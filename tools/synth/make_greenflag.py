#!/usr/bin/env python3
"""make_greenflag.py -- the GREEN-FLAG set: spec-driven synthetic statements for the
content-reading auto wizard, each PDF paired with the ground truth it was drawn from.

WHAT THIS IS. One JSON spec file = one statement. The spec says where every column
is, what kind of content it holds, how every figure and date is printed, where the
(made-up) headings sit, what free text surrounds the table, which cells are redacted,
and whether a scanned copy is wanted. This script renders the PDF, writes the truth
file, and immediately re-checks the PDF against the truth with pdftotext and pymupdf.

It was written WITHOUT reading the reader it measures (R/wizard_auto.R and friends):
the layouts are what a bank could plausibly print, pushed to the weird end, not what
some algorithm happens to handle.

Full spec reference and worked examples: tools/synth/greenflag/SPEC.md.

    python3 tools/synth/make_greenflag.py --specs tools/synth/greenflag/specs --out OUT
    python3 tools/synth/make_greenflag.py --specs ... --out OUT --only example_03
    python3 tools/synth/make_greenflag.py --check OUT
    python3 tools/synth/make_greenflag.py --specs tools/synth/greenflag/specs --list

TRUTH FILE (<case>.truth.json) -- the make_corpus / make_layouts format, plus extras:
    case, generator, note, opening_balance, closing_balance, row_count,
    rows: [{date "YYYY-MM-DD"|null, description, debit, credit, balance, ...}]
debit is money OUT and credit money IN for the account holder, both positive (or 0
for a zero-value line), else null. balance is the running balance printed ON THAT ROW
(null when not printed). Rows are in PRINTED order. Opening / closing / brought- and
carried-forward / page-total lines are never rows. description = every text-column
value of the row, left to right, joined by one space (a wrapped cell is its lines
joined by one space). See SPEC.md for the added keys.

Deterministic: crc32 seeds (never hash()), reportlab invariant mode, fixed scan noise.
Dev-time only. Nothing here ships to the server. Python 3.9+, reportlab, pymupdf;
Pillow + numpy only for scans; poppler's pdftotext for --check.
"""

import argparse
import copy
import datetime as dt
import glob
import io
import json
import math
import os
import random
import re
import subprocess
import sys
import zlib

from reportlab.lib.colors import HexColor
from reportlab.lib.pagesizes import A4, LEGAL, LETTER
from reportlab.pdfbase.pdfmetrics import stringWidth
from reportlab.pdfgen import canvas

GENERATOR = "make_greenflag.py v1"
FOOTER = "SYNTHETIC TEST DOCUMENT - NOT A REAL STATEMENT"
CUES = ("running_balance", "printed_totals", "sign_markers", "indicator_column",
        "description_semantics", "legend")


class SpecError(Exception):
    pass


def crc(*parts):
    return zlib.crc32(":".join(str(p) for p in parts).encode("utf-8"))


def rng_for(*parts):
    return random.Random(crc(*parts))


def check_keys(obj, allowed, where):
    if not isinstance(obj, dict):
        raise SpecError("%s: expected an object, got %r" % (where, obj))
    bad = [k for k in obj if k not in allowed and not k.startswith("_")]
    if bad:
        raise SpecError("%s: unknown key(s) %s; allowed: %s"
                        % (where, bad, ", ".join(sorted(allowed))))


def deep_merge(a, b):
    out = copy.deepcopy(a) if isinstance(a, dict) else {}
    for k, v in (b or {}).items():
        if isinstance(v, dict) and isinstance(out.get(k), dict):
            out[k] = deep_merge(out[k], v)
        else:
            out[k] = copy.deepcopy(v)
    return out


def cents_of(x):
    return None if x is None else int(round(float(x) * 100))


def dollars(c):
    return None if c is None else round(c / 100.0, 2)


def norm_ws(s):
    return re.sub(r"\s+", " ", s).strip()


# ---------------------------------------------------------------------------
# Fonts
# ---------------------------------------------------------------------------

FONT_NAMES = {
    ("Helvetica", False, False): "Helvetica",
    ("Helvetica", True, False): "Helvetica-Bold",
    ("Helvetica", False, True): "Helvetica-Oblique",
    ("Helvetica", True, True): "Helvetica-BoldOblique",
    ("Times", False, False): "Times-Roman",
    ("Times", True, False): "Times-Bold",
    ("Times", False, True): "Times-Italic",
    ("Times", True, True): "Times-BoldItalic",
    ("Courier", False, False): "Courier",
    ("Courier", True, False): "Courier-Bold",
    ("Courier", False, True): "Courier-Oblique",
    ("Courier", True, True): "Courier-BoldOblique",
}
FONT_KEYS = {"family", "size", "bold", "italic", "oblique"}
DEFAULT_FONT = {"family": "Helvetica", "size": 8.0, "bold": False, "italic": False}


def resolve_font(base, over, where):
    f = dict(base)
    if over:
        check_keys(over, FONT_KEYS, where)
        for k, v in over.items():
            f["italic" if k == "oblique" else k] = v
    fam = f["family"]
    if fam not in ("Helvetica", "Times", "Courier"):
        raise SpecError("%s: font family must be Helvetica, Times or Courier, not %r" % (where, fam))
    sz = float(f["size"])
    if not 4 <= sz <= 30:
        raise SpecError("%s: font size %s outside 4..30" % (where, sz))
    f["size"] = sz
    f["name"] = FONT_NAMES[(fam, bool(f["bold"]), bool(f["italic"]))]
    return f


def sw(s, font):
    return stringWidth(s, font["name"], font["size"])


# ---------------------------------------------------------------------------
# Dates
# ---------------------------------------------------------------------------

MONTHS = ["January", "February", "March", "April", "May", "June", "July", "August",
          "September", "October", "November", "December"]
DAYS = ["Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"]
DATE_TOK = re.compile(r"\[[^\]]*\]|YYYY|YY|MONTH|Month|MON|Mon|DOW|Dow|DD|Do|D|MM|M")
TOK_RE = {"YYYY": r"(\d{4})", "YY": r"(\d{2})", "MONTH": r"([A-Za-z]+)", "Month": r"([A-Za-z]+)",
          "MON": r"([A-Za-z]{3})", "Mon": r"([A-Za-z]{3})", "DOW": r"([A-Za-z]{3})",
          "Dow": r"([A-Za-z]{3})", "DD": r"(\d{2})", "Do": r"(\d{1,2})(?:st|nd|rd|th)",
          "D": r"(\d{1,2})", "MM": r"(\d{2})", "M": r"(\d{1,2})"}


def ordinal(n):
    if 10 <= n % 100 <= 20:
        return "%dth" % n
    return "%d%s" % (n, {1: "st", 2: "nd", 3: "rd"}.get(n % 10, "th"))


def fmt_date(d, pat, case=None):
    out, pos = [], 0
    for m in DATE_TOK.finditer(pat):
        out.append(pat[pos:m.start()])
        t = m.group(0)
        if t.startswith("["):
            out.append(t[1:-1])
        elif t == "YYYY":
            out.append("%04d" % d.year)
        elif t == "YY":
            out.append("%02d" % (d.year % 100))
        elif t == "MONTH":
            out.append(MONTHS[d.month - 1].upper())
        elif t == "Month":
            out.append(MONTHS[d.month - 1])
        elif t == "MON":
            out.append(MONTHS[d.month - 1][:3].upper())
        elif t == "Mon":
            out.append(MONTHS[d.month - 1][:3])
        elif t == "DOW":
            out.append(DAYS[d.weekday()][:3].upper())
        elif t == "Dow":
            out.append(DAYS[d.weekday()][:3])
        elif t == "DD":
            out.append("%02d" % d.day)
        elif t == "Do":
            out.append(ordinal(d.day))
        elif t == "D":
            out.append(str(d.day))
        elif t == "MM":
            out.append("%02d" % d.month)
        elif t == "M":
            out.append(str(d.month))
        pos = m.end()
    out.append(pat[pos:])
    s = "".join(out)
    if case == "upper":
        s = s.upper()
    elif case == "lower":
        s = s.lower()
    return s


def date_pattern_ok(pat):
    toks = [m.group(0) for m in DATE_TOK.finditer(pat) if not m.group(0).startswith("[")]
    has_d = any(t in ("DD", "D", "Do") for t in toks)
    has_m = any(t in ("MM", "M", "MON", "Mon", "MONTH", "Month") for t in toks)
    return has_d and has_m, any(t in ("YYYY", "YY") for t in toks)


def parse_date(s, pat):
    """Independent read-back: (year|None, month, day) or None."""
    parts, groups, pos = [], [], 0
    for m in DATE_TOK.finditer(pat):
        parts.append(re.escape(pat[pos:m.start()]))
        t = m.group(0)
        if t.startswith("["):
            parts.append(re.escape(t[1:-1]))
        else:
            parts.append(TOK_RE[t])
            groups.append(t)
        pos = m.end()
    parts.append(re.escape(pat[pos:]))
    mm = re.fullmatch("".join(parts), s, re.I)
    if not mm:
        return None
    y = mo = d = dow = None
    for t, v in zip(groups, mm.groups()):
        if t == "YYYY":
            y = int(v)
        elif t == "YY":
            y = 2000 + int(v)
        elif t in ("MONTH", "Month"):
            names = [x.lower() for x in MONTHS]
            if v.lower() not in names:
                return None
            mo = names.index(v.lower()) + 1
        elif t in ("MON", "Mon"):
            names = [x[:3].lower() for x in MONTHS]
            if v.lower() not in names:
                return None
            mo = names.index(v.lower()) + 1
        elif t in ("DOW", "Dow"):
            dow = v.lower()
        elif t in ("DD", "Do", "D"):
            d = int(v)
        elif t in ("MM", "M"):
            mo = int(v)
    return (y, mo, d, dow)


# ---------------------------------------------------------------------------
# Money
# ---------------------------------------------------------------------------

MONEY_DEFAULTS = {"neg_prefix": "-", "neg_suffix": "", "pos_prefix": "", "pos_suffix": "",
                  "thousands": ",", "decimal": ".", "currency": "", "currency_suffix": "",
                  "currency_position": "after_sign", "zero": None, "vector_minus": False}
MONEY_PRESETS = {
    "minus": {},
    "trailing_minus": {"neg_prefix": "", "neg_suffix": "-"},
    "parens": {"neg_prefix": "(", "neg_suffix": ")"},
    "crdr": {"neg_prefix": "", "neg_suffix": " DR", "pos_suffix": " CR"},
    "crdr_attached": {"neg_prefix": "", "neg_suffix": "DR", "pos_suffix": "CR"},
    "crdr_lower": {"neg_prefix": "", "neg_suffix": " dr", "pos_suffix": " cr"},
    "cr_only": {"neg_prefix": "", "pos_suffix": " CR"},
    "dr_only": {"neg_prefix": "", "neg_suffix": " DR"},
    "drcr_prefix": {"neg_prefix": "DR ", "pos_prefix": "CR "},
    "plusminus": {"pos_prefix": "+"},
    "plus_only": {"neg_prefix": "", "pos_prefix": "+"},
    "od": {"neg_prefix": "", "neg_suffix": " OD"},
    "od_attached": {"neg_prefix": "", "neg_suffix": "OD"},
    "unsigned": {"neg_prefix": ""},
    "vector_minus": {"vector_minus": True},
}
MONEY_KEYS = set(MONEY_DEFAULTS) | {"style"}


def resolve_money(f, formats, where, depth=0):
    if depth > 5:
        raise SpecError("%s: money format references loop" % where)
    if f is None:
        f = "minus"
    if isinstance(f, str):
        if f in formats:
            return resolve_money(formats[f], formats, "formats.%s" % f, depth + 1)
        if f in MONEY_PRESETS:
            f = {"style": f}
        else:
            raise SpecError("%s: unknown money format %r (presets: %s; or a name in 'formats')"
                            % (where, f, ", ".join(sorted(MONEY_PRESETS))))
    check_keys(f, MONEY_KEYS, where)
    style = f.get("style", "minus")
    if style in formats:
        F = dict(resolve_money(formats[style], formats, "formats.%s" % style, depth + 1))
    elif style in MONEY_PRESETS:
        F = dict(MONEY_DEFAULTS)
        F.update(MONEY_PRESETS[style])
    else:
        raise SpecError("%s: unknown money style %r" % (where, style))
    for k, v in f.items():
        if k != "style":
            F[k] = v
    F["style"] = style
    if F["decimal"] not in (".", ","):
        raise SpecError("%s: decimal must be '.' or ','" % where)
    if F["thousands"] == F["decimal"]:
        raise SpecError("%s: thousands and decimal separators are the same" % where)
    if F["currency_position"] not in ("after_sign", "before_sign"):
        raise SpecError("%s: currency_position must be after_sign or before_sign" % where)
    return F


def neg_markable(F):
    return (not F["vector_minus"]) and bool(F["neg_prefix"] or F["neg_suffix"])


def group3(n, sep):
    s = str(n)
    if not sep:
        return s
    out = []
    while len(s) > 3:
        out.insert(0, s[-3:])
        s = s[:-3]
    out.insert(0, s)
    return sep.join(out)


def money_text(c, F):
    """(text, vector_minus_needed)."""
    a = abs(c)
    if a == 0 and F["zero"] is not None:
        return F["zero"], False
    num = group3(a // 100, F["thousands"]) + F["decimal"] + "%02d" % (a % 100) + F["currency_suffix"]
    if a == 0:
        pre, suf = "", ""
    elif c < 0:
        pre, suf = F["neg_prefix"], F["neg_suffix"]
    else:
        pre, suf = F["pos_prefix"], F["pos_suffix"]
    vec = False
    if c < 0 and F["vector_minus"]:
        pre, suf, vec = "", "", True
    cur = F["currency"]
    if F["currency_position"] == "before_sign":
        return cur + pre + num + suf, vec
    return pre + cur + num + suf, vec


def parse_money(s, F):
    """Independent read-back of a printed figure. Returns cents or raises ValueError."""
    t = s
    if F["zero"] is not None and t == F["zero"]:
        return 0
    if F["currency"]:
        t = t.replace(F["currency"], "", 1)
    if F["currency_suffix"]:
        t = t.replace(F["currency_suffix"], "", 1)
    sign = 1
    pairs = [(F["neg_prefix"], F["neg_suffix"], -1), (F["pos_prefix"], F["pos_suffix"], 1)]
    pairs = [p for p in pairs if p[0] or p[1]]
    pairs.sort(key=lambda p: -len(p[0] + p[1]))
    numre = r"[0-9][0-9 ,.']*"
    for pre, suf, g in pairs:
        if t.startswith(pre) and t.endswith(suf) and len(t) > len(pre) + len(suf):
            core = t[len(pre):len(t) - len(suf)] if suf else t[len(pre):]
            if re.fullmatch(numre, core):
                t, sign = core, g
                break
    if not re.fullmatch(numre, t):
        raise ValueError("figure %r does not parse" % s)
    if F["thousands"]:
        t = t.replace(F["thousands"], "")
    if F["decimal"] != ".":
        t = t.replace(F["decimal"], ".")
    if not re.fullmatch(r"\d+\.\d\d", t):
        raise ValueError("figure %r does not parse (core %r)" % (s, t))
    return sign * int(t.replace(".", ""))


# ---------------------------------------------------------------------------
# Description pools (all names fictional)
# ---------------------------------------------------------------------------

TOWNS = ["PONSONBY", "PETONE", "HAMILTON", "TAURANGA", "NAPIER", "NELSON", "TIMARU",
         "WHANGAREI", "ROTORUA", "DUNEDIN", "PALMERSTON NTH", "MASTERTON", "OAMARU",
         "GISBORNE", "LEVIN", "FEILDING", "TE AWAMUTU", "MOTUEKA", "WANAKA", "KAIKOHE",
         "PUKEKOHE", "HASTINGS", "INVERCARGILL", "ASHBURTON", "THAMES", "TOKOROA"]
MERCHANTS = ["KUMARA KITCHEN", "HARBOURSIDE FUEL", "TOTARA HARDWARE", "KIWI KAI MART",
             "PIPI PHARMACY", "FERNLEAF BAKERY", "TUATARA TYRES", "WAIRUA WINES", "KORU CAFE",
             "SOUTHERLY SUPERMARKET", "MANUKA MEATS", "BELLBIRD BOOKS", "GANNET GROCERS",
             "RIVERSTONE MOTORS", "KAIMAI KEBABS", "TAKAHE TAKEAWAYS", "WEKA WATERWORKS",
             "KEA KIDS CLOTHING", "RAUPO RENTALS", "HUIA HOME SUPPLIES", "MOKO MOTEL",
             "PUHA PIZZA", "KARAKA CINEMAS"]
BILLERS = ["PAUA POWER LTD", "TUI TELECOM", "WAITAHA WATER", "KORORA INSURANCE",
           "AWA DISTRICT RATES", "PUKEKO PHONE CO", "ROHE BROADBAND", "TAIAO GAS"]
EMPLOYERS = ["KAWAKAWA CONSTRUCTION", "RANGI LOGISTICS LTD", "MOANA SEAFOODS", "TE PUNA HEALTH",
             "KOWHAI ENGINEERING", "PATIKI PRINTING", "MIRO MEDIA LTD"]
PEOPLE = ["J R HENDERSON", "A K TAMIHANA", "S M PATEL", "L WONG", "T NGATA", "R OSULLIVAN",
          "M FALEOLO", "K BRIGHTWATER", "H VAN DER BERG", "P TE AHO", "D MCKAY",
          "C RAMASWAMY", "E HOHAIA", "B KOWALSKI"]
COMPANIES = ["KAHU HOLDINGS LTD", "TAKAHE TRUST", "RIMU VENTURES", "PAPA AND SONS",
             "ORONGO PARTNERS", "NGAIO GROUP", "KAITIAKI SERVICES", "MAHOE LIMITED",
             "AROHA ASSOCIATES", "TAWA COLLECTIVE"]
CODES = ["QX", "TR", "KP", "MZ", "AB", "RL", "WN", "HB"]
PARTICULARS = ["RENT", "BOND", "FLAT", "CAR", "INV", "WK", "JOB", "LOT", "ORDER", "SITE", "PROJ", "ACC"]
THINGS = ["fencing materials", "garden supplies", "plumbing parts", "scaffold hire",
          "catering for staff function", "printer toner cartridges", "roof repairs",
          "firewood delivery", "school camp costs", "boat trailer parts", "kitchen benchtop",
          "survey work"]
STREETS = ["Rata Street", "Kowhai Road", "Tui Crescent", "Harakeke Lane", "Totara Avenue",
           "Ponga Place", "Kereru Drive", "Matai Terrace"]
MEMO_TEMPLATES = [
    "Invoice {n} {thing} {num} {street} {town}",
    "{thing} job {n} at {num} {street} {town}",
    "Ref {n} {thing} for {person} {town}",
    "Quote {n} {thing} stage {k} of 3",
    "{thing} batch {n} site {num} {street}",
]
NEUTRAL_TYPES = ["TXN", "ONL", "BRN", "ELEC", "MISC", "XFR", "OTH", "BAT"]

# (template, type code, lo, hi, whole-twenties?)
SEM_OUT = [
    ("EFTPOS {merchant} {town}", "EFTPOS", 4, 280, False),
    ("POS {merchant}", "POS", 3, 190, False),
    ("POS W/D {merchant} {town}", "POS", 10, 320, False),
    ("ATM WITHDRAWAL {town}", "ATM", 20, 400, True),
    ("ATM {town} BRANCH", "ATM", 20, 300, True),
    ("PAYMENT TO {person}", "PAYMENT", 15, 1500, False),
    ("PAYMENT TO {biller} {ref}", "PAYMENT", 30, 600, False),
    ("DIRECT DEBIT {biller}", "DIRECT DEBIT", 25, 450, False),
    ("DIRECT DEBIT {biller} {ref}", "DIRECT DEBIT", 25, 450, False),
    ("MONTHLY ACCOUNT FEE", "FEE", 2, 15, False),
    ("EFTPOS FEE", "FEE", 0.2, 3, False),
    ("OVERSEAS TXN FEE", "FEE", 0.5, 9, False),
]
SEM_IN = [
    ("SALARY {employer}", "SALARY", 1800, 6200, False),
    ("WAGES {employer}", "WAGES", 600, 2400, False),
    ("INTEREST CREDIT", "INTEREST CREDIT", 0.1, 45, False),
    ("REFUND {merchant}", "REFUND", 5, 250, False),
    ("TRANSFER FROM {person}", "TRANSFER FROM", 20, 2000, False),
    ("TRANSFER FROM SAVINGS {acct}", "TRANSFER FROM", 50, 3000, False),
    ("WAGES {employer} {ref}", "WAGES", 600, 2400, False),
]
NAMES_OUT = [
    ("{merchant} {town}", "", 4, 300, False),
    ("{merchant}", "", 4, 250, False),
    ("{biller}", "", 25, 450, False),
]
NAMES_IN = [
    ("{employer}", "", 600, 5000, False),
    ("{person}", "", 20, 1500, False),
]
NEUTRAL = [
    ("{person}", "", 10, 1500, False),
    ("{company}", "", 10, 3000, False),
    ("{company} {ref}", "", 10, 2500, False),
    ("REF {ref} {person}", "", 10, 900, False),
    ("BATCH {ref}", "", 10, 1200, False),
    ("{code} {company}", "", 10, 2000, False),
    ("{person} {code}", "", 10, 800, False),
]
POOLS = ("semantic", "names", "neutral", "mixed", "custom")
PH_FILL = re.compile(r"\{([a-z]+)\}")


def fill_template(t, fills):
    def sub(m):
        k = m.group(1)
        if k not in fills:
            raise SpecError("unknown placeholder {%s} in description template %r" % (k, t))
        return fills[k]
    return PH_FILL.sub(sub, t)


# ---------------------------------------------------------------------------
# Spec schema
# ---------------------------------------------------------------------------

TOP_KEYS = {"case", "theme", "note", "seed", "features", "decidable", "decidable_by",
            "undecidable_reason", "bank", "holder", "account_no", "account_name", "period",
            "page", "font", "formats", "default_money", "table", "transactions", "accounts",
            "items", "redactions", "redact_random", "scan", "newest_first", "allow_collisions"}
PAGE_KEYS = {"size", "orientation", "margins", "offsets", "drift", "jitter", "footer"}
TABLE_KEYS = {"top", "top_cont", "bottom", "header_height", "header_height_after",
              "line_height", "row_gap", "rows_per_page", "heading_pages", "heading_font",
              "font", "columns", "extra_headings", "repeat_headings_every", "balance_mode",
              "rules", "opening_line", "closing_line", "page_totals", "carry", "allow_overlap"}
COL_KEYS = {"id", "kind", "x", "w", "align", "valign", "heading", "font", "format", "case",
            "source", "wrap", "max_lines", "fill", "values", "group_once", "offset", "tokens",
            "of", "invert", "value_sign", "empty", "page_dx", "prefix"}
KINDS = ("date", "date2", "text", "debit", "credit", "amount_signed", "amount_unsigned",
         "indicator", "balance", "ignore_text")
MONEY_KINDS = ("debit", "credit", "amount_signed", "amount_unsigned", "balance")
HEAD_KEYS = {"text", "dx", "dy", "align", "rotate", "font", "pages", "below_first_row",
             "span_to", "leading", "color", "x"}
TX_KEYS = {"count", "in_ratio", "pool", "semantic_rate", "custom_pool", "amount", "amount_in",
           "amount_out", "round_rate", "zero_count", "huge_count", "huge_dir", "same_day",
           "opening", "overdraft", "literal", "literal_only"}
LIT_KEYS = {"date", "date2", "debit", "credit", "text"}
ACCOUNT_KEYS = {"name", "number", "holder", "period", "transactions", "table", "newest_first",
                "new_page", "gap", "title_items", "title_height", "items", "after_items",
                "after_height", "bank"}
ITEM_KEYS = {"text", "x", "y", "y_ref", "pages", "align", "font", "size", "bold", "italic",
             "rotate", "color", "box", "redact", "legend", "role", "shape", "w", "h", "x2", "y2",
             "width", "fill", "stroke", "money", "leading", "family"}
RULE_KEYS = {"heading_above", "heading_below", "row_lines", "zebra", "column_lines", "box",
             "width", "color"}
LINE_KEYS = {"label", "label_col", "value_col", "date", "position", "bold"}
PT_KEYS = {"label", "label_col", "cols", "on_last", "bold"}
CARRY_KEYS = {"bf_label", "cf_label", "label_col", "value_col", "bold"}
SCAN_KEYS = {"dpi", "skew", "noise", "blur", "jpeg_quality"}
REDACT_KEYS = {"account", "row", "cols", "kind"}
RRANDOM_KEYS = {"count", "cols", "kind", "account"}
FOOTER_KEYS = {"x", "y", "align", "size"}
PAGE_SIZES = {"A4": A4, "LETTER": LETTER, "LEGAL": LEGAL}
MONEY_PH = ("opening", "closing", "total_in", "total_out", "net")
DATE_PH = ("period_start", "period_end")
PLAIN_PH = ("page", "pages", "bank", "holder", "account_no", "account_name", "year", "count",
            "count_in", "count_out")
PH = re.compile(r"\{\{|\}\}|\{([a-z_0-9]+)(?::([^{}]*))?\}")


def parse_iso(s, where):
    try:
        return dt.date.fromisoformat(s)
    except Exception:
        raise SpecError("%s: %r is not a YYYY-MM-DD date" % (where, s))


def pages_ok(sel, docpage, acct_page, where):
    """Page selector for headings (acct_page = 1-based page within the account's table)."""
    if sel is None or sel == "all":
        return True
    if sel == "none":
        return False
    if sel == "first":
        return acct_page == 1
    if sel in ("cont", "not_first"):
        return acct_page > 1
    if sel == "odd":
        return docpage % 2 == 1
    if sel == "even":
        return docpage % 2 == 0
    if isinstance(sel, list):
        return docpage in sel
    raise SpecError("%s: bad page selector %r" % (where, sel))


# ---------------------------------------------------------------------------
# The builder
# ---------------------------------------------------------------------------

class Builder:
    def __init__(self, spec, spec_file):
        check_keys(spec, TOP_KEYS, "spec")
        if "case" not in spec or not re.fullmatch(r"[A-Za-z0-9_\-]+", str(spec["case"])):
            raise SpecError("spec: 'case' is required and must be [A-Za-z0-9_-]+")
        self.spec = spec
        self.spec_file = spec_file
        self.case = spec["case"]
        self.seed = int(spec.get("seed", crc(self.case)))
        pg = spec.get("page", {})
        check_keys(pg, PAGE_KEYS, "page")
        size = str(pg.get("size", "A4")).upper()
        if size not in PAGE_SIZES:
            raise SpecError("page.size must be A4, Letter or Legal")
        w, h = PAGE_SIZES[size]
        orient = pg.get("orientation", "portrait")
        if orient not in ("portrait", "landscape"):
            raise SpecError("page.orientation must be portrait or landscape")
        self.W, self.H = (max(w, h), min(w, h)) if orient == "landscape" else (min(w, h), max(w, h))
        self.page_size_name = size
        self.orientation = orient
        self.margins = {"left": 36, "right": 36, "top": 36, "bottom": 36}
        self.margins.update(pg.get("margins", {}))
        self.page_cfg = pg
        self.footer_cfg = pg.get("footer", {})
        check_keys(self.footer_cfg, FOOTER_KEYS, "page.footer")
        self.base_font = resolve_font(DEFAULT_FONT, spec.get("font"), "font")
        self.formats = spec.get("formats", {})
        if not isinstance(self.formats, dict):
            raise SpecError("formats must be an object of name -> money format")
        self.default_money = spec.get("default_money", "minus")
        self.pages = []
        self.cur = None
        self.y = 0.0
        self.redaction_boxes = []
        self.items_out = []
        self.warnings = []
        self.auto = set()
        self.cue_totals = False
        self.cue_legend = False
        accs = spec.get("accounts") or [{}]
        if not isinstance(accs, list):
            raise SpecError("accounts must be a list")
        self.accounts = []
        for ai, a in enumerate(accs):
            check_keys(a, ACCOUNT_KEYS, "accounts[%d]" % ai)
            A = {"index": ai, "spec": a, "pages": []}
            A["name"] = a.get("name", spec.get("account_name", "EVERYDAY ACCOUNT"))
            A["number"] = a.get("number", spec.get("account_no", "00-0000-0000000-00"))
            A["holder"] = a.get("holder", spec.get("holder", "A N EXAMPLE"))
            A["bank"] = a.get("bank", spec.get("bank", "SYNTHETIC BANK"))
            per = a.get("period", spec.get("period", {"start": "2026-03-01", "end": "2026-03-31"}))
            check_keys(per, {"start", "end"}, "period")
            A["start"] = parse_iso(per["start"], "period.start")
            A["end"] = parse_iso(per["end"], "period.end")
            if A["end"] < A["start"]:
                raise SpecError("period ends before it starts")
            A["T"] = deep_merge(spec.get("table", {}), a.get("table", {}))
            A["tx"] = deep_merge(spec.get("transactions", {}), a.get("transactions", {}))
            A["newest_first"] = bool(a.get("newest_first", spec.get("newest_first", False)))
            self.resolve_table(A)
            self.gen_rows(A)
            self.accounts.append(A)
        self.plan_redactions()

    # ---------------- table / columns ----------------
    def resolve_table(self, A):
        T = A["T"]
        where = "table" if A["index"] == 0 else "accounts[%d].table" % A["index"]
        check_keys(T, TABLE_KEYS, where)
        T["font"] = resolve_font(self.base_font, T.get("font"), where + ".font")
        hb = dict(T["font"])
        hb["bold"] = True
        T["heading_font"] = resolve_font(hb, T.get("heading_font"), where + ".heading_font")
        T["line_height"] = float(T.get("line_height", round(T["font"]["size"] * 1.25, 2)))
        T["row_gap"] = float(T.get("row_gap", 2.0))
        T["top"] = float(T.get("top", self.margins["top"] + 130))
        T["top_cont"] = float(T.get("top_cont", self.margins["top"] + 40))
        T["bottom"] = float(T.get("bottom", self.H - self.margins["bottom"] - 22))
        rules = T.get("rules", {})
        check_keys(rules, RULE_KEYS, where + ".rules")
        T["rules"] = rules
        if "columns" not in T or not T["columns"]:
            raise SpecError("%s.columns is required" % where)
        cols, used = [], {}
        for ci, c in enumerate(T["columns"]):
            cw = "%s.columns[%d]" % (where, ci)
            check_keys(c, COL_KEYS, cw)
            kind = c.get("kind")
            if kind not in KINDS:
                raise SpecError("%s: kind must be one of %s" % (cw, ", ".join(KINDS)))
            cid = c.get("id")
            if not cid:
                used[kind] = used.get(kind, 0) + 1
                cid = kind if used[kind] == 1 else "%s%d" % (kind, used[kind])
            if any(x["id"] == cid for x in cols):
                raise SpecError("%s: duplicate column id %r" % (cw, cid))
            if "x" not in c or "w" not in c:
                raise SpecError("%s: x and w are required" % cw)
            C = {"id": cid, "kind": kind, "x": float(c["x"]), "w": float(c["w"])}
            C["align"] = c.get("align", "right" if kind in MONEY_KINDS else "left")
            if C["align"] == "center":
                C["align"] = "centre"
            if C["align"] not in ("left", "right", "centre"):
                raise SpecError("%s: align must be left, right or centre" % cw)
            C["valign"] = c.get("valign", "top")
            if C["valign"] not in ("top", "bottom"):
                raise SpecError("%s: valign must be top or bottom" % cw)
            C["font"] = resolve_font(T["font"], c.get("font"), cw + ".font")
            C["case"] = c.get("case")
            C["empty"] = c.get("empty", "")
            C["page_dx"] = {int(k): float(v) for k, v in c.get("page_dx", {}).items()}
            if kind in ("date", "date2"):
                C["format"] = c.get("format", "DD/MM/YYYY")
                ok, has_year = date_pattern_ok(C["format"])
                if not ok:
                    raise SpecError("%s: date format %r needs a day and a month token" % (cw, C["format"]))
                C["has_year"] = has_year
                C["group_once"] = bool(c.get("group_once", False))
                off = c.get("offset", [0, 2] if kind == "date2" else [0, 0])
                C["offset"] = [int(off[0]), int(off[1])]
            elif kind in MONEY_KINDS:
                C["format"] = resolve_money(c.get("format", self.default_money), self.formats, cw + ".format")
                C["invert"] = bool(c.get("invert", False))
                C["value_sign"] = c.get("value_sign", "positive")
                if C["value_sign"] not in ("positive", "negative"):
                    raise SpecError("%s: value_sign must be positive or negative" % cw)
            elif kind in ("text", "ignore_text"):
                src = c.get("source", "payee" if kind == "text" else "seq")
                srcs = src if isinstance(src, list) else [src]
                for s in srcs:
                    if s not in TEXT_SOURCES:
                        raise SpecError("%s: unknown source %r (one of %s)" % (cw, s, ", ".join(TEXT_SOURCES)))
                if "custom" in srcs and "values" not in c:
                    raise SpecError("%s: source 'custom' needs 'values'" % cw)
                C["source"] = srcs
                C["values"] = c.get("values")
                C["wrap"] = bool(c.get("wrap", kind == "text"))
                C["max_lines"] = int(c.get("max_lines", 4))
                C["fill"] = float(c.get("fill", 1.0))
                C["prefix"] = c.get("prefix", "")
            elif kind == "indicator":
                C["of"] = c.get("of", "amount")
                if C["of"] not in ("amount", "balance"):
                    raise SpecError("%s: of must be amount or balance" % cw)
                dflt = {"out": "D", "in": "C"} if C["of"] == "amount" else {"neg": "DR", "pos": "CR"}
                C["tokens"] = dict(dflt, **c.get("tokens", {}))
                need = ("out", "in") if C["of"] == "amount" else ("neg", "pos")
                if set(C["tokens"]) != set(need):
                    raise SpecError("%s: tokens must have exactly the keys %s" % (cw, need))
                if C["tokens"][need[0]] == C["tokens"][need[1]]:
                    raise SpecError("%s: the two indicator tokens are identical" % cw)
            C["heading"] = self.norm_heading(c.get("heading"), cw + ".heading")
            if C["x"] < 0 or C["x"] + C["w"] > self.W:
                raise SpecError("%s: column [%g, %g] leaves the page (width %g)" % (cw, C["x"], C["x"] + C["w"], self.W))
            cols.append(C)
        kinds = [c["kind"] for c in cols]
        for k in ("date", "date2", "balance", "amount_signed", "amount_unsigned", "debit", "credit"):
            if kinds.count(k) > 1:
                raise SpecError("%s: at most one %s column" % (where, k))
        if kinds.count("date") != 1:
            raise SpecError("%s: exactly one 'date' column is required" % where)
        if "text" not in kinds:
            raise SpecError("%s: at least one 'text' column is required" % where)
        money_modes = [("debit" in kinds) and ("credit" in kinds), "amount_signed" in kinds,
                       "amount_unsigned" in kinds]
        if sum(money_modes) != 1 or (("debit" in kinds) != ("credit" in kinds)):
            raise SpecError("%s: amounts need EITHER debit+credit columns, OR one amount_signed, "
                            "OR one amount_unsigned (optionally with an indicator)" % where)
        for c in cols:
            if c["kind"] == "indicator" and c["of"] == "balance" and "balance" not in kinds:
                raise SpecError("%s: indicator of balance needs a balance column" % where)
        if not T.get("allow_overlap"):
            sc = sorted(cols, key=lambda c: c["x"])
            for a, b in zip(sc, sc[1:]):
                if a["x"] + a["w"] > b["x"] + 0.01:
                    raise SpecError("%s: columns %r [%g-%g] and %r [%g-%g] overlap (set allow_overlap)"
                                    % (where, a["id"], a["x"], a["x"] + a["w"], b["id"], b["x"], b["x"] + b["w"]))
        bm = T.get("balance_mode", "every_row" if "balance" in kinds else "none")
        if bm not in ("every_row", "last_of_day", "none"):
            raise SpecError("%s.balance_mode must be every_row, last_of_day or none" % where)
        if ("balance" in kinds) != (bm != "none"):
            raise SpecError("%s: balance_mode %r but %s balance column" % (where, bm, "a" if "balance" in kinds else "no"))
        T["balance_mode"] = bm
        for c in cols:
            h = c["heading"]
            if h and h.get("span_to") and h["span_to"] not in [x["id"] for x in cols]:
                raise SpecError("%s: heading span_to %r is not a column id" % (where, h["span_to"]))
        T["cols"] = cols
        T["colmap"] = {c["id"]: c for c in cols}
        T["extra"] = [self.norm_heading(h, "%s.extra_headings[%d]" % (where, i), extra=True)
                      for i, h in enumerate(T.get("extra_headings", []))]
        # text keys: heading text if unique and non-empty, else column id
        tcols = [c for c in cols if c["kind"] in ("text", "ignore_text")]
        htxt = [norm_ws(c["heading"]["text"]) if c["heading"] else "" for c in tcols]
        for c, ht in zip(tcols, htxt):
            c["key"] = ht if ht and htxt.count(ht) == 1 else c["id"]
        # heading band heights
        lf = T["heading_font"]

        def band_need(hs):
            need = 0.0
            for h in hs:
                f = resolve_font(lf, h.get("font"), "heading.font")
                lines = h["text"].split("\n")
                lead = float(h.get("leading", f["size"] * 1.15))
                if h["rotate"]:
                    need = max(need, max(sw(l, f) for l in lines) + 6)
                else:
                    dy = h["dy"] if h["dy"] is not None else f["size"] + 1
                    need = max(need, dy + (len(lines) - 1) * lead + 4)
            return need
        main = [c["heading"] for c in cols if c["heading"] and not c["heading"]["below_first_row"]]
        main += [h for h in T["extra"] if not h["below_first_row"]]
        after = [c["heading"] for c in cols if c["heading"] and c["heading"]["below_first_row"]]
        after += [h for h in T["extra"] if h["below_first_row"]]
        T["header_height"] = float(T.get("header_height", max(band_need(main), 0.0)))
        T["header_height_after"] = float(T.get("header_height_after", band_need(after)))
        T["has_after"] = bool(after)
        for key, keys in (("opening_line", LINE_KEYS), ("closing_line", LINE_KEYS),
                          ("page_totals", PT_KEYS), ("carry", CARRY_KEYS)):
            if key in T:
                check_keys(T[key], keys, "%s.%s" % (where, key))
        bal = [c for c in cols if c["kind"] == "balance"]
        firsttext = [c for c in sorted(cols, key=lambda c: c["x"]) if c["kind"] == "text"][0]
        for key in ("opening_line", "closing_line", "carry"):
            if key in T:
                L = T[key]
                L.setdefault("label_col", firsttext["id"])
                if "value_col" not in L:
                    if not bal:
                        raise SpecError("%s.%s: no balance column, so value_col is required" % (where, key))
                    L["value_col"] = bal[0]["id"]
                for k in ("label_col", "value_col"):
                    if L[k] not in T["colmap"]:
                        raise SpecError("%s.%s.%s %r is not a column id" % (where, key, k, L[k]))
                if T["colmap"][L["value_col"]]["kind"] not in MONEY_KINDS:
                    raise SpecError("%s.%s.value_col must be a money column" % (where, key))
        if "page_totals" in T:
            P = T["page_totals"]
            P.setdefault("label_col", firsttext["id"])
            P.setdefault("cols", [c["id"] for c in cols if c["kind"] in ("debit", "credit", "amount_signed")])
            for cid in P["cols"] + [P["label_col"]]:
                if cid not in T["colmap"]:
                    raise SpecError("%s.page_totals: %r is not a column id" % (where, cid))
            for cid in P["cols"]:
                if T["colmap"][cid]["kind"] not in ("debit", "credit", "amount_signed", "amount_unsigned"):
                    raise SpecError("%s.page_totals.cols must be amount columns" % where)

    def norm_heading(self, h, where, extra=False):
        if h is None or h == "":
            return None
        if isinstance(h, str):
            h = {"text": h}
        check_keys(h, HEAD_KEYS, where)
        if "text" not in h:
            raise SpecError("%s: heading needs text" % where)
        if extra and "x" not in h:
            raise SpecError("%s: an extra heading needs x" % where)
        H = {"text": str(h["text"]), "dx": float(h.get("dx", 0)), "dy": h.get("dy"),
             "align": h.get("align"), "rotate": float(h.get("rotate", 0)), "font": h.get("font"),
             "pages": h.get("pages", "all"), "below_first_row": bool(h.get("below_first_row", False)),
             "span_to": h.get("span_to"), "color": h.get("color"), "x": h.get("x")}
        if H["dy"] is not None:
            H["dy"] = float(H["dy"])
        if "leading" in h:
            H["leading"] = float(h["leading"])
        if H["align"] == "center":
            H["align"] = "centre"
        if H["pages"] == "last":
            raise SpecError("%s: headings cannot use pages 'last'" % where)
        return H

    # ---------------- transactions ----------------
    def gen_rows(self, A):
        tx = A["tx"]
        where = "transactions"
        check_keys(tx, TX_KEYS, where)
        ai = A["index"]
        ra = rng_for(self.seed, ai, "amounts")
        pool = tx.get("pool", "semantic")
        if pool not in POOLS:
            raise SpecError("transactions.pool must be one of %s" % ", ".join(POOLS))
        if pool == "custom" and "custom_pool" not in tx:
            raise SpecError("transactions.pool 'custom' needs custom_pool")
        in_ratio = float(tx.get("in_ratio", 0.3))
        rows = []
        days = (A["end"] - A["start"]).days + 1
        if not tx.get("literal_only"):
            n = int(tx.get("count", 25))
            if n < 1:
                raise SpecError("transactions.count must be >= 1")
            same = float(tx.get("same_day", 0.3))
            k = max(1, min(days, n, int(round(n * (1 - same)))))
            day_idx = sorted(ra.sample(range(days), k))
            counts = [1] * k
            for _ in range(n - k):
                counts[ra.randrange(k)] += 1
            dates = []
            for di, cnt in zip(day_idx, counts):
                dates += [A["start"] + dt.timedelta(days=di)] * cnt
            for d in dates:
                dirn = "in" if ra.random() < in_ratio else "out"
                tmpl = self.pick_template(ra, pool, dirn, tx)
                c = self.pick_amount(ra, tmpl, dirn, tx)
                rows.append({"date": d, "dir": dirn, "cents": c, "tmpl": tmpl, "lit": None})
            gen_idx = list(range(len(rows)))
            zc = int(tx.get("zero_count", 0))
            hc = int(tx.get("huge_count", 0))
            if zc + hc > len(rows):
                raise SpecError("zero_count + huge_count exceed the row count")
            pick = ra.sample(gen_idx, zc + hc)
            for j in pick[:zc]:
                rows[j]["cents"] = 0
            hd = tx.get("huge_dir", "in")
            for j in pick[zc:]:
                rows[j]["cents"] = ra.randint(100000000, 999999999)
                if hd in ("in", "out"):
                    rows[j]["dir"] = hd
                    rows[j]["tmpl"] = self.pick_template(ra, pool, hd, tx)
        for li, L in enumerate(tx.get("literal", [])):
            lw = "transactions.literal[%d]" % li
            check_keys(L, LIT_KEYS, lw)
            if ("debit" in L) == ("credit" in L):
                raise SpecError("%s: exactly one of debit / credit" % lw)
            dirn = "out" if "debit" in L else "in"
            c = cents_of(L["debit"] if "debit" in L else L["credit"])
            if c < 0:
                raise SpecError("%s: amounts are positive (debit = money out)" % lw)
            rows.append({"date": parse_iso(L["date"], lw + ".date"), "dir": dirn, "cents": c,
                         "tmpl": None, "lit": L,
                         "date2_lit": parse_iso(L["date2"], lw + ".date2") if "date2" in L else None})
        if not rows:
            raise SpecError("no transactions (literal_only with no literal rows?)")
        rows.sort(key=lambda r: r["date"])  # stable: generated before literal on a day
        for i, r in enumerate(rows):
            r["i"] = i
        # opening balance and overdraft policy
        op = tx.get("opening", [200, 5000])
        explicit = not isinstance(op, list)
        opening = cents_of(op) if explicit else int(round(ra.uniform(float(op[0]), float(op[1])) * 100))
        pol = tx.get("overdraft", "allow" if explicit else "never")
        if pol not in ("allow", "never", "force"):
            raise SpecError("transactions.overdraft must be allow, never or force")
        run, mn = 0, 0
        for r in rows:
            run += r["cents"] if r["dir"] == "in" else -r["cents"]
            mn = min(mn, run)
        if pol == "never" and opening + mn < 0:
            opening = -mn + ra.randint(1000, 50000)
        elif pol == "force" and opening + mn >= 0:
            opening = -mn - ra.randint(5000, 80000)
        A["opening"] = opening
        run = opening
        for r in rows:
            r["signed"] = r["cents"] if r["dir"] == "in" else -r["cents"]
            run += r["signed"]
            r["bal"] = run
        A["closing"] = run
        # date2
        T = A["T"]
        for r in rows:
            for c in T["cols"]:
                if c["kind"] == "date2":
                    if r.get("date2_lit"):
                        r["date2"] = r["date2_lit"]
                    else:
                        rr = rng_for(self.seed, ai, r["i"], "date2")
                        r["date2"] = r["date"] + dt.timedelta(days=rr.randint(*c["offset"]))
        # balance visibility
        bm = T["balance_mode"]
        for j, r in enumerate(rows):
            if bm == "every_row":
                r["show_bal"] = True
            elif bm == "last_of_day":
                r["show_bal"] = (j == len(rows) - 1) or rows[j + 1]["date"] != r["date"]
            else:
                r["show_bal"] = False
        A["rows_chrono"] = rows
        A["rows"] = list(reversed(rows)) if A["newest_first"] else list(rows)
        for pi, r in enumerate(A["rows"]):
            r["pi"] = pi
        for r in A["rows"]:
            self.build_cells(A, r)

    def pick_template(self, ra, pool, dirn, tx):
        if pool == "mixed":
            pool = "semantic" if ra.random() < float(tx.get("semantic_rate", 0.5)) else "neutral"
        if pool == "semantic":
            return ra.choice(SEM_IN if dirn == "in" else SEM_OUT)
        if pool == "names":
            return ra.choice(NAMES_IN if dirn == "in" else NAMES_OUT)
        if pool == "neutral":
            return ra.choice(NEUTRAL)
        cp = tx["custom_pool"]
        lst = cp if isinstance(cp, list) else cp.get(dirn, [])
        if not lst:
            raise SpecError("transactions.custom_pool has no %r entries" % dirn)
        return (ra.choice(lst), "", 5, 500, False)

    def pick_amount(self, ra, tmpl, dirn, tx):
        rng = tx.get("amount_in" if dirn == "in" else "amount_out", tx.get("amount"))
        lo, hi = (float(rng[0]), float(rng[1])) if rng else (tmpl[2], tmpl[3])
        v = ra.uniform(lo, hi)
        if tmpl[4] and not rng:
            return int(max(1, round(v / 20.0)) * 2000)
        if ra.random() < float(tx.get("round_rate", 0.1)):
            return int(max(1, round(v))) * 100
        return max(1, int(round(v * 100)))

    # ---------------- text sources ----------------
    def fills(self, A, r):
        if "fills" not in r:
            g = rng_for(self.seed, A["index"], r["i"], "fill")
            r["fills"] = {
                "merchant": g.choice(MERCHANTS), "town": g.choice(TOWNS), "person": g.choice(PEOPLE),
                "biller": g.choice(BILLERS), "employer": g.choice(EMPLOYERS),
                "company": g.choice(COMPANIES), "ref": str(g.randint(10000, 9999999)),
                "code": g.choice(CODES) + str(g.randint(1, 99)),
                "acct": "%02d-%04d-%07d-%02d" % (g.randint(10, 39), g.randint(1000, 9999),
                                                  g.randint(1000000, 9999999), g.randint(0, 99)),
                "month": MONTHS[r["date"].month - 1][:3].upper(),
            }
        return r["fills"]

    def source_value(self, A, r, C, src):
        g = rng_for(self.seed, A["index"], r["i"], src, C["id"])
        tm = r["tmpl"]
        if src == "payee":
            return fill_template(tm[0], self.fills(A, r)) if tm else ""
        if src == "name":
            if not tm:
                return ""
            m = PH_FILL.search(tm[0])
            return self.fills(A, r)[m.group(1)] if m else tm[0]
        if src == "type":
            if not tm:
                return ""
            return tm[1] if tm[1] else g.choice(NEUTRAL_TYPES)
        if src == "particulars":
            return g.choice(PARTICULARS) + str(g.randint(1, 999))
        if src == "code":
            return self.fills(A, r)["code"]
        if src == "reference":
            return g.choice(["", "REF", "R", "#"]) + str(g.randint(1000, 99999999))
        if src == "memo":
            f = self.fills(A, r)
            return fill_template(g.choice(MEMO_TEMPLATES), {
                "n": str(g.randint(100, 99999)), "thing": g.choice(THINGS), "num": str(g.randint(1, 240)),
                "street": g.choice(STREETS), "town": f["town"].title(), "person": f["person"],
                "k": str(g.randint(1, 3))})
        if src == "location":
            return self.fills(A, r)["town"]
        if src == "card":
            return "CARD %04d" % rng_for(self.seed, A["index"], "card").randint(1000, 9999)
        if src == "seq":
            return "%04d" % (r["pi"] + 1)
        if src == "serial":
            return str(g.randint(1000000, 9999999))
        if src == "custom":
            v = C["values"]
            if isinstance(v, dict):
                lst = v.get(r["dir"], [])
            else:
                lst = v
            if not lst:
                return ""
            return fill_template(g.choice(lst), self.fills(A, r))
        raise SpecError("unknown text source %r" % src)

    def text_value(self, A, r, C):
        if r["lit"] is not None and C["id"] in r["lit"].get("text", {}):
            return str(r["lit"]["text"][C["id"]])
        if r["lit"] is not None and not r["tmpl"]:
            # literal row without text for this column: fall back to a neutral fill
            r["tmpl"] = ("{company}", "", 0, 0, False)
        g = rng_for(self.seed, A["index"], r["i"], C["id"], "fillprob")
        if C["fill"] < 1.0 and g.random() >= C["fill"]:
            return ""
        parts = [self.source_value(A, r, C, s) for s in C["source"]]
        v = " ".join(p for p in parts if p)
        if C["case"] == "upper":
            v = v.upper()
        elif C["case"] == "lower":
            v = v.lower()
        elif C["case"] == "title":
            v = v.title()
        if v and C["prefix"]:
            v = C["prefix"] + v
        return norm_ws(v)

    # ---------------- cells ----------------
    def wrap(self, text, C):
        f = C["font"]
        width = C["w"]
        if not text:
            return []
        if not C.get("wrap"):
            s = text
            while s and sw(s, f) > width:
                s = s[:-1].rstrip()
            return [s] if s else []
        lines, cur = [], ""
        for word in text.split():
            trial = cur + " " + word if cur else word
            if sw(trial, f) <= width:
                cur = trial
                continue
            if cur:
                lines.append(cur)
            while sw(word, f) > width:
                k = len(word)
                while k > 1 and sw(word[:k], f) > width:
                    k -= 1
                lines.append(word[:k])
                word = word[k:]
            cur = word
        if cur:
            lines.append(cur)
        return lines[:C["max_lines"]]

    def build_cells(self, A, r):
        cells = {}
        for C in A["T"]["cols"]:
            k = C["kind"]
            cell = None
            if k in ("date", "date2"):
                d = r["date"] if k == "date" else r["date2"]
                cell = {"type": "plain", "lines": [fmt_date(d, C["format"], C["case"])]}
            elif k in ("text", "ignore_text"):
                v = self.text_value(A, r, C)
                cell = {"type": "plain", "lines": self.wrap(v, C)}
            elif k == "indicator":
                if C["of"] == "amount":
                    tok = C["tokens"][r["dir"]]
                else:
                    tok = (C["tokens"]["neg" if r["bal"] < 0 else "pos"]) if r["show_bal"] else ""
                cell = {"type": "plain", "lines": [tok] if tok else []}
            else:
                v = None
                if k == "debit" and r["dir"] == "out":
                    v = -r["cents"] if C["value_sign"] == "negative" else r["cents"]
                elif k == "credit" and r["dir"] == "in":
                    v = -r["cents"] if C["value_sign"] == "negative" else r["cents"]
                elif k == "amount_signed":
                    v = -r["signed"] if C["invert"] else r["signed"]
                elif k == "amount_unsigned":
                    v = r["cents"]
                elif k == "balance" and r["show_bal"]:
                    v = -r["bal"] if C["invert"] else r["bal"]
                if v is None:
                    cell = {"type": "plain", "lines": [], "empty": True}
                else:
                    s, vec = money_text(v, C["format"])
                    cell = {"type": "money", "lines": [s], "vec": vec, "value": v}
            if not cell["lines"] and C["empty"] and k not in ("text", "ignore_text"):
                cell = {"type": "plain", "lines": [C["empty"]], "filler": True}
            cells[C["id"]] = cell
        r["cells"] = cells
        r["nlines"] = max([1] + [len(c["lines"]) for c in cells.values()])

    # ---------------- redactions ----------------
    def plan_redactions(self):
        self.redact = {}
        for i, R in enumerate(self.spec.get("redactions", [])):
            w = "redactions[%d]" % i
            check_keys(R, REDACT_KEYS, w)
            ai = int(R.get("account", 0))
            if ai >= len(self.accounts):
                raise SpecError("%s: no account %d" % (w, ai))
            A = self.accounts[ai]
            row = int(R["row"])
            if not 0 <= row < len(A["rows"]):
                raise SpecError("%s: row %d out of range (0..%d, printed order)" % (w, row, len(A["rows"]) - 1))
            self.add_redaction(A, row, R.get("cols", []), R.get("kind", "remove"), w)
        rr = self.spec.get("redact_random")
        if rr:
            check_keys(rr, RRANDOM_KEYS, "redact_random")
            accs = self.accounts if rr.get("account", "all") == "all" else [self.accounts[int(rr["account"])]]
            g = rng_for(self.seed, "redact")
            cand = []
            for A in accs:
                for r in A["rows"]:
                    if any(r["cells"].get(cid, {}).get("lines") and not r["cells"][cid].get("filler")
                           for cid in rr["cols"]):
                        cand.append((A, r["pi"]))
            n = min(int(rr.get("count", 3)), len(cand))
            for A, pi in sorted(g.sample(cand, n), key=lambda t: (t[0]["index"], t[1])):
                self.add_redaction(A, pi, rr["cols"], rr.get("kind", "remove"), "redact_random")

    def add_redaction(self, A, row, cols, kind, w):
        if kind not in ("remove", "overlay"):
            raise SpecError("%s: kind must be remove or overlay" % w)
        for cid in cols:
            if cid not in A["T"]["colmap"]:
                raise SpecError("%s: %r is not a column id" % (w, cid))
            self.redact.setdefault((A["index"], row), {})[cid] = kind

    # ---------------- drawing primitives (top-down coordinates) ----------------
    def op_text(self, x, y, s, font, align="left", rotate=0.0, color=None, tag=None):
        w = sw(s, font)
        off = {"left": 0.0, "right": w, "centre": w / 2.0}[align]
        op = {"t": "text", "x": x, "y": y, "s": s, "font": font, "off": off, "w": w,
              "rot": rotate, "color": color, "tag": tag}
        self.cur["ops"].append(op)
        return op

    def op_rect(self, x, y, w, h, fill=None, stroke=None, width=0.6, tag=None):
        op = {"t": "rect", "x": x, "y": y, "w": w, "h": h, "fill": fill, "stroke": stroke,
              "width": width, "tag": tag}
        self.cur["ops"].append(op)
        return op

    def op_line(self, x0, y0, x1, y1, width=0.6, color=None):
        op = {"t": "line", "x0": x0, "y0": y0, "x1": x1, "y1": y1, "width": width, "color": color}
        self.cur["ops"].append(op)
        return op

    def new_page(self, A):
        p = {"n": len(self.pages) + 1, "ops": [], "acct": A["index"]}
        self.pages.append(p)
        self.cur = p
        A["pages"].append(p["n"])
        return p

    def colx(self, C):
        return C["x"] + C["page_dx"].get(self.cur["n"], 0.0)

    # ---------------- headings ----------------
    def draw_band(self, A, y, after, acct_page):
        T = A["T"]
        hs = []
        for C in T["cols"]:
            h = C["heading"]
            if h and h["below_first_row"] == after:
                hs.append((h, C))
        for h in T["extra"]:
            if h["below_first_row"] == after:
                hs.append((h, None))
        for h, C in hs:
            if not pages_ok(h["pages"], self.cur["n"], acct_page, "heading"):
                continue
            f = resolve_font(T["heading_font"], h["font"], "heading.font")
            lines = h["text"].split("\n")
            lead = h.get("leading", f["size"] * 1.15)
            if C is None:
                align = h["align"] or "left"
                ax = float(h["x"]) + h["dx"]
            else:
                x0, x1 = self.colx(C), self.colx(C) + C["w"]
                if h["span_to"]:
                    O = T["colmap"][h["span_to"]]
                    x0, x1 = min(x0, self.colx(O)), max(x1, self.colx(O) + O["w"])
                    align = h["align"] or "centre"
                else:
                    align = h["align"] or C["align"]
                ax = {"left": x0, "right": x1, "centre": (x0 + x1) / 2.0}[align] + h["dx"]
            if h["rotate"]:
                dy = h["dy"] if h["dy"] is not None else (T["header_height_after"] if after else T["header_height"]) - 3
                for k, ln in enumerate(lines):
                    self.op_text(ax + k * lead, y + dy, ln, f, align="left", rotate=h["rotate"],
                                 color=h["color"], tag="heading")
            else:
                dy = h["dy"] if h["dy"] is not None else f["size"] + 1
                for k, ln in enumerate(lines):
                    self.op_text(ax, y + dy + k * lead, ln, f, align=align, color=h["color"], tag="heading")
        R = T["rules"]
        bh = T["header_height_after"] if after else T["header_height"]
        if not after and bh > 0:
            x0, x1 = self.table_x(A)
            if R.get("heading_above"):
                self.op_line(x0, y, x1, y, R.get("width", 0.6), R.get("color"))
            if R.get("heading_below", True):
                self.op_line(x0, y + bh - 1.5, x1, y + bh - 1.5, R.get("width", 0.6), R.get("color"))

    def table_x(self, A):
        cols = A["T"]["cols"]
        return (min(self.colx(c) for c in cols) - 2, max(self.colx(c) + c["w"] for c in cols) + 2)

    # ---------------- rows and lines ----------------
    def draw_cell_lines(self, A, C, lines, y_base, lh, bold=False, tag=None):
        f = C["font"]
        if bold:
            f = resolve_font(f, {"bold": True}, "font")
        x0 = self.colx(C)
        ax = {"left": x0, "right": x0 + C["w"], "centre": x0 + C["w"] / 2.0}[C["align"]]
        ops = []
        for k, ln in enumerate(lines):
            ops.append(self.op_text(ax, y_base + k * lh, ln, f, align=C["align"], tag=tag))
        return ops

    def draw_money(self, A, C, value, y_base, bold=False, tag=None, check=True):
        f = C["font"]
        if bold:
            f = resolve_font(f, {"bold": True}, "font")
        s, vec = money_text(value, C["format"])
        x0 = self.colx(C)
        dw = sw("-", f) if vec else 0.0
        tw = sw(s, f) + dw
        if check and tw > C["w"] + 0.01:
            raise SpecError("column %r (w=%g) is too narrow for %r (needs %.1f)" % (C["id"], C["w"], s, tw))
        left = {"left": x0, "right": x0 + C["w"] - tw, "centre": x0 + (C["w"] - tw) / 2.0}[C["align"]]
        op = self.op_text(left + dw, y_base, s, f, align="left", tag=tag)
        ops = [op]
        if vec:
            yy = y_base - f["size"] * 0.3
            ops.append(self.op_line(left + dw * 0.12, yy, left + dw * 0.88, yy, max(0.4, f["size"] * 0.075)))
        return ops, s, left, tw

    def text_bbox(self, op):
        f = op["font"]
        x0 = op["x"] - op["off"]
        return [x0, op["y"] - f["size"] * 0.8, x0 + op["w"], op["y"] + f["size"] * 0.25]

    def draw_row(self, A, r, y, show_dates, acct_page):
        T = A["T"]
        lh = T["line_height"]
        base = y + T["font"]["size"]
        red = self.redact.get((A["index"], r["pi"]), {})
        r["page"] = self.cur["n"]
        r["printed"] = {}
        r["red_fields"] = {}
        R = T["rules"]
        rh = r["nlines"] * lh + T["row_gap"]
        if R.get("zebra") and r["pi"] % 2 == 1:
            x0, x1 = self.table_x(A)
            self.op_rect(x0, y - 1, x1 - x0, rh, fill=R["zebra"])
        for C in sorted(T["cols"], key=lambda c: c["x"]):
            cell = r["cells"][C["id"]]
            lines = cell["lines"]
            if C["kind"] in ("date", "date2") and C["group_once"] and not show_dates.get(C["id"], True):
                lines = []
            if not lines:
                continue
            if C["kind"] in ("date", "date2", "indicator", "ignore_text") or cell.get("filler"):
                for ln in lines:
                    if sw(ln, C["font"]) > C["w"] + 0.01:
                        raise SpecError("column %r (w=%g) is too narrow for %r" % (C["id"], C["w"], ln))
            start = base + (r["nlines"] - len(lines)) * lh if C["valign"] == "bottom" else base
            kind = red.get(C["id"]) if not cell.get("filler") else None
            if cell["type"] == "money":
                ops, s, left, tw = self.draw_money(A, C, cell["value"], start)
                bboxes = [[left, start - C["font"]["size"] * 0.8, left + tw, start + C["font"]["size"] * 0.25]]
                texts = [s]
            else:
                ops = self.draw_cell_lines(A, C, lines, start, lh)
                bboxes = [self.text_bbox(o) for o in ops]
                texts = list(lines)
            if kind == "remove":
                for o in ops:
                    self.cur["ops"].remove(o)
            if kind:
                for bb, tx in zip(bboxes, texts):
                    pad = 1.3
                    rect = [bb[0] - pad, bb[1] - pad, bb[2] + pad, bb[3] + pad]
                    self.op_rect(rect[0], rect[1], rect[2] - rect[0], rect[3] - rect[1], fill="#000000",
                                 tag={"kind": kind, "text": tx, "account": A["index"], "row": r["pi"],
                                      "col": C["id"]})
                r["red_fields"][C["id"]] = kind
            if kind != "remove" and not cell.get("filler"):
                r["printed"][C["id"]] = texts if C["kind"] in ("text", "ignore_text") else texts[0]
        if R.get("row_lines"):
            x0, x1 = self.table_x(A)
            yy = y + rh - T["row_gap"] / 2.0 + 0.5
            self.op_line(x0, yy, x1, yy, R.get("width", 0.3), R.get("color", "#999999"))
        return rh

    def draw_special(self, A, label, cfg, value, y, date=None):
        T = A["T"]
        base = y + T["font"]["size"]
        bold = cfg.get("bold", True)
        LC = T["colmap"][cfg["label_col"]]
        if label:
            f = resolve_font(LC["font"], {"bold": bold}, "font")
            if sw(label, f) > LC["w"] + 0.01 and LC["kind"] not in ("text",):
                raise SpecError("label %r does not fit column %r" % (label, LC["id"]))
            self.draw_cell_lines(A, LC, [label], base, T["line_height"], bold=bold, tag="special")
        if value is not None:
            VC = T["colmap"][cfg["value_col"]]
            v = -value if VC.get("invert") else value
            self.draw_money(A, VC, v, base, bold=bold, tag="special")
        if date is not None:
            DC = [c for c in T["cols"] if c["kind"] == "date"][0]
            self.draw_cell_lines(A, DC, [fmt_date(date, DC["format"], DC["case"])], base, T["line_height"],
                                 bold=bold, tag="special")
        self.auto.add("table_balance_lines")
        return T["line_height"] + T["row_gap"]

    def draw_page_totals(self, A, sums, y):
        T = A["T"]
        P = T["page_totals"]
        base = y + T["font"]["size"]
        bold = P.get("bold", True)
        LC = T["colmap"][P["label_col"]]
        self.draw_cell_lines(A, LC, [P.get("label", "Page total")], base, T["line_height"], bold=bold, tag="special")
        for cid in P["cols"]:
            self.draw_money(A, T["colmap"][cid], sums.get(cid, 0), base, bold=bold, tag="special")
        return T["line_height"] + T["row_gap"]

    # ---------------- flow ----------------
    def layout(self):
        for A in self.accounts:
            self.layout_account(A)
        for p in self.pages:
            self.close_rules(p)

    def layout_account(self, A):
        T = A["T"]
        S = A["spec"]
        lh = T["line_height"]
        title_items = S.get("title_items", [])
        title_h = float(S.get("title_height", (max([float(i.get("y", 0)) for i in title_items]) + 8) if title_items else 0))
        if A["index"] == 0 or S.get("new_page") or self.cur is None:
            self.new_page(A)
            y = T["top"]
        else:
            y = self.y + float(S.get("gap", 18))
            if y + title_h + T["header_height"] + 3 * lh > T["bottom"]:
                self.new_page(A)
                y = T["top_cont"]
        A["first_page"] = self.cur["n"]
        for it in title_items:
            self.place_item(it, A, y_base=y)
        y += title_h
        state = {"acct_page": 1, "rows_on_page": 0, "after_drawn": False, "sums": {}, "last_dates": {},
                 "span_top": y, "boundary": A["closing"] if A["newest_first"] else A["opening"]}
        A["spans"] = []

        def start_band(y):
            if pages_ok(T.get("heading_pages", "all"), self.cur["n"], state["acct_page"], "heading_pages"):
                self.draw_band(A, y, False, state["acct_page"])
                return y + T["header_height"]
            return y

        def footer_reserve():
            n = 0
            if "page_totals" in T:
                n += 1
            if "carry" in T:
                n += 1
            return n * (lh + T["row_gap"])

        def end_page(y, last):
            if "page_totals" in T and (not last or T["page_totals"].get("on_last", True)):
                y += self.draw_page_totals(A, state["sums"], y)
            if "carry" in T and not last:
                y += self.draw_special(A, T["carry"].get("cf_label", "CARRIED FORWARD"), T["carry"],
                                       state["boundary"], y)
            A["spans"].append((self.cur["n"], state["span_top"], y))
            return y

        y = start_band(y)
        blocks = []
        op_pos = T.get("opening_line", {}).get("position", "bottom" if A["newest_first"] else "top")
        cl_pos = T.get("closing_line", {}).get("position", "top" if A["newest_first"] else "bottom")
        top_lines = [k for k, pos in (("closing_line", cl_pos), ("opening_line", op_pos))
                     if k in T and pos == "top"]
        if not A["newest_first"]:
            top_lines.sort(key=lambda k: 0 if k == "opening_line" else 1)
        bot_lines = [k for k, pos in (("opening_line", op_pos), ("closing_line", cl_pos))
                     if k in T and pos == "bottom"]
        if not A["newest_first"]:
            bot_lines.sort(key=lambda k: 0 if k == "opening_line" else 1)
        blocks += [("line", k) for k in top_lines]
        blocks += [("row", r) for r in A["rows"]]
        blocks += [("line", k) for k in bot_lines]
        rpp = T.get("rows_per_page")
        rep = T.get("repeat_headings_every")
        for kind, b in blocks:
            if kind == "row":
                h = b["nlines"] * lh + T["row_gap"]
                extra = 0.0
                if state["rows_on_page"] == 0 and T["has_after"]:
                    extra += T["header_height_after"]
                if rep and state["rows_on_page"] > 0 and state["rows_on_page"] % int(rep) == 0:
                    extra += T["header_height"]
                brk = (y + h + extra > T["bottom"] - footer_reserve()) or \
                      (rpp and state["rows_on_page"] >= int(rpp))
            else:
                h = lh + T["row_gap"]
                brk = y + h > T["bottom"] - footer_reserve()
            if brk:
                if state["rows_on_page"] == 0 and kind == "row":
                    raise SpecError("a single row does not fit between table top and bottom")
                end_page(y, False)
                self.new_page(A)
                state.update(acct_page=state["acct_page"] + 1, rows_on_page=0, after_drawn=False,
                             sums={}, last_dates={})
                y = T["top_cont"]
                state["span_top"] = y
                y = start_band(y)
                if "carry" in T:
                    y += self.draw_special(A, T["carry"].get("bf_label", "BROUGHT FORWARD"), T["carry"],
                                           state["boundary"], y)
                if kind == "row" and rep:
                    pass
            if kind == "line":
                cfg = T[b]
                val = A["opening"] if b == "opening_line" else A["closing"]
                dsel = cfg.get("date")
                d = A["start"] if dsel == "start" else (A["end"] if dsel == "end" else None)
                if dsel not in (None, "start", "end"):
                    raise SpecError("%s.date must be start or end" % b)
                lbl = cfg.get("label", "OPENING BALANCE" if b == "opening_line" else "CLOSING BALANCE")
                y += self.draw_special(A, lbl, cfg, val, y, date=d)
                continue
            r = b
            if rep and state["rows_on_page"] > 0 and state["rows_on_page"] % int(rep) == 0:
                self.draw_band(A, y, False, state["acct_page"])
                y += T["header_height"]
                self.auto.add("repeated_headings")
            show = {}
            for C in T["cols"]:
                if C["kind"] in ("date", "date2"):
                    key = r["date"] if C["kind"] == "date" else r["date2"]
                    show[C["id"]] = state["last_dates"].get(C["id"]) != key
                    state["last_dates"][C["id"]] = key
            y += self.draw_row(A, r, y, show, state["acct_page"])
            for cid, cell in r["cells"].items():
                if cell["type"] == "money":
                    state["sums"][cid] = state["sums"].get(cid, 0) + cell["value"]
            state["boundary"] = r["bal"] if not A["newest_first"] else r["bal"] - r["signed"]
            state["rows_on_page"] += 1
            if state["rows_on_page"] == 1 and T["has_after"]:
                self.draw_band(A, y, True, state["acct_page"])
                y += T["header_height_after"]
        y = end_page(y, True)
        A["last_page"] = self.cur["n"]
        after = S.get("after_items", [])
        if after:
            need = float(S.get("after_height", max(float(i.get("y", 0)) for i in after) + 14))
            if y + need > self.H - self.margins["bottom"] - 14:
                self.new_page(A)
                y = T["top_cont"]
            for it in after:
                self.place_item(it, A, y_base=y)
            y += need
        self.y = y

    def close_rules(self, p):
        for A in self.accounts:
            T = A["T"]
            R = T["rules"]
            if not (R.get("column_lines") or R.get("box")):
                continue
            for (pn, y0, y1) in A.get("spans", []):
                if pn != p["n"]:
                    continue
                saved = self.cur
                self.cur = p
                x0, x1 = self.table_x(A)
                if R.get("box"):
                    self.op_rect(x0, y0, x1 - x0, y1 - y0, stroke=R.get("color", "#000000"), width=R.get("width", 0.6))
                if R.get("column_lines"):
                    sc = sorted(T["cols"], key=lambda c: c["x"])
                    for a, b in zip(sc, sc[1:]):
                        xm = (self.colx(a) + a["w"] + self.colx(b)) / 2.0
                        self.op_line(xm, y0, xm, y1, R.get("width", 0.3), R.get("color", "#777777"))
                self.cur = saved

    # ---------------- free items ----------------
    def place_item(self, it, A, y_base=None, page=None):
        """Record an item op now; text placeholders are filled in finalize()."""
        check_keys(it, ITEM_KEYS, "item")
        pg = page or self.cur
        y = float(it.get("y", 0))
        ref = it.get("y_ref", "block" if y_base is not None else "top")
        if ref == "bottom":
            y = self.H - y
        elif ref in ("block", "table_end"):
            if y_base is None:
                raise SpecError("item y_ref %r only works in title_items / after_items" % ref)
            y = y_base + y
        elif ref != "top":
            raise SpecError("item y_ref must be top, bottom, block or table_end")
        pg["ops"].append({"t": "item", "it": it, "y": y, "acct": A["index"] if A else None})

    def place_page_items(self):
        n = len(self.pages)
        for it in self.spec.get("items", []):
            sel = it.get("pages", "all")
            for p in self.pages:
                if self.item_page_ok(sel, p["n"], 1, n, p["n"] == 1, p["n"] == n):
                    self.place_item(it, None, page=p)
        for A in self.accounts:
            ap = A["pages"]
            for it in A["spec"].get("items", []):
                sel = it.get("pages", "all")
                for k, pn in enumerate(ap):
                    if self.item_page_ok(sel, pn, k + 1, len(ap), k == 0, k == len(ap) - 1):
                        self.place_item(it, A, page=self.pages[pn - 1])

    def item_page_ok(self, sel, docpage, k, n, first, last):
        if sel == "all":
            return True
        if sel == "first":
            return first
        if sel == "last":
            return last
        if sel in ("not_first", "cont"):
            return not first
        if sel == "not_last":
            return not last
        if sel == "odd":
            return docpage % 2 == 1
        if sel == "even":
            return docpage % 2 == 0
        if isinstance(sel, list):
            return docpage in sel
        raise SpecError("item pages: bad selector %r" % (sel,))

    def ctx_for(self, A, pn, npages):
        ctx = {"page": str(pn), "pages": str(npages)}
        ctx.update(self.acct_values(A if A is not None else self.accounts[0]))
        if A is None:
            for X in self.accounts:
                for k, v in self.acct_values(X).items():
                    ctx["a%d_%s" % (X["index"], k)] = v
        return ctx

    def acct_values(self, X):
        rows = X["rows_chrono"]
        tin = sum(r["cents"] for r in rows if r["dir"] == "in")
        tout = sum(r["cents"] for r in rows if r["dir"] == "out")
        return {"opening": X["opening"], "closing": X["closing"], "total_in": tin, "total_out": tout,
                "net": tin - tout, "period_start": X["start"], "period_end": X["end"],
                "bank": X["bank"], "holder": X["holder"], "account_no": X["number"],
                "account_name": X["name"], "year": str(X["end"].year), "count": str(len(rows)),
                "count_in": str(sum(1 for r in rows if r["dir"] == "in")),
                "count_out": str(sum(1 for r in rows if r["dir"] == "out"))}

    def fill_text(self, text, ctx, money_fmt):
        def sub(m):
            g = m.group(0)
            if g == "{{":
                return "{"
            if g == "}}":
                return "}"
            name, arg = m.group(1), m.group(2)
            base = re.sub(r"^a\d+_", "", name)
            if name not in ctx:
                raise SpecError("unknown placeholder {%s} in %r" % (name, text))
            v = ctx[name]
            if base in MONEY_PH:
                self.cue_totals = True
                F = resolve_money(arg if arg else money_fmt, self.formats, "placeholder {%s}" % name)
                s, vec = money_text(v, F)
                return ("-" + s) if vec else s
            if base in DATE_PH:
                return fmt_date(v, arg or "DD/MM/YYYY")
            return str(v)
        return PH.sub(sub, text)

    def finalize(self):
        n = len(self.pages)
        for p in self.pages:
            ops = []
            for op in p["ops"]:
                if op["t"] != "item":
                    ops.append(op)
                    continue
                it = op["it"]
                A = self.accounts[op["acct"]] if op["acct"] is not None else None
                shape = it.get("shape", "text")
                x = float(it.get("x", 0))
                y = op["y"]
                if shape == "rect":
                    ops.append({"t": "rect", "x": x, "y": y, "w": float(it["w"]), "h": float(it["h"]),
                                "fill": it.get("fill"), "stroke": it.get("stroke", "#000000" if not it.get("fill") else None),
                                "width": float(it.get("width", 0.6)), "tag": None})
                    continue
                if shape == "line":
                    y2 = float(it.get("y2", it.get("y", 0)))
                    y2 = y + (y2 - float(it.get("y", 0)))
                    ops.append({"t": "line", "x0": x, "y0": y, "x1": float(it["x2"]), "y1": y2,
                                "width": float(it.get("width", 0.6)), "color": it.get("stroke")})
                    continue
                if shape != "text":
                    raise SpecError("item shape must be text, rect or line")
                fo = {"size": it.get("size", self.base_font["size"]), "bold": it.get("bold", False),
                      "italic": it.get("italic", False)}
                if "family" in it:
                    fo["family"] = it["family"]
                if it.get("font"):
                    fo.update(it["font"])
                f = resolve_font(self.base_font, fo, "item.font")
                ctx = self.ctx_for(A, p["n"], n)
                txt = self.fill_text(str(it.get("text", "")), ctx, it.get("money", self.default_money))
                align = it.get("align", "left")
                if align == "center":
                    align = "centre"
                lead = float(it.get("leading", f["size"] * 1.2))
                rot = float(it.get("rotate", 0))
                lines = txt.split("\n")
                bb = None
                tops = []
                for k, ln in enumerate(lines):
                    if not ln:
                        continue
                    yy = y + k * lead
                    w = sw(ln, f)
                    off = {"left": 0.0, "right": w, "centre": w / 2.0}[align]
                    top = {"t": "text", "x": x + (k * lead if rot else 0.0), "y": yy if not rot else y,
                           "s": ln, "font": f, "off": off, "w": w, "rot": rot, "color": it.get("color"),
                           "tag": "item"}
                    tops.append(top)
                    if not rot:
                        b = [x - off, yy - f["size"] * 0.8, x - off + w, yy + f["size"] * 0.25]
                        bb = b if bb is None else [min(bb[0], b[0]), min(bb[1], b[1]), max(bb[2], b[2]), max(bb[3], b[3])]
                    self.items_out.append({"page": p["n"], "x": round(x, 1), "y": round(yy, 1), "text": ln,
                                           "role": it.get("role", "legend" if it.get("legend") else "note"),
                                           "account_index": op["acct"]})
                if it.get("legend") or it.get("role") == "legend":
                    self.cue_legend = True
                box = it.get("box")
                if box and bb:
                    pad = float(box.get("pad", 3))
                    ops.append({"t": "rect", "x": bb[0] - pad, "y": bb[1] - pad, "w": bb[2] - bb[0] + 2 * pad,
                                "h": bb[3] - bb[1] + 2 * pad, "fill": box.get("fill"),
                                "stroke": box.get("stroke", "#000000"), "width": float(box.get("width", 0.6)),
                                "tag": None})
                red = it.get("redact")
                if red not in (None, "overlay", "remove"):
                    raise SpecError("item redact must be overlay or remove")
                if red != "remove":
                    ops.extend(tops)
                if red and bb:
                    ops.append({"t": "rect", "x": bb[0] - 1.5, "y": bb[1] - 1.5, "w": bb[2] - bb[0] + 3,
                                "h": bb[3] - bb[1] + 3, "fill": "#000000", "stroke": None, "width": 0,
                                "tag": {"kind": red, "item": True, "text": txt.replace("\n", " ")}})
            p["ops"] = ops

    # ---------------- page offsets ----------------
    def page_offset(self, n):
        pg = self.page_cfg
        offs = pg.get("offsets", [])
        dx, dy = (float(offs[n - 1][0]), float(offs[n - 1][1])) if n - 1 < len(offs) else (0.0, 0.0)
        dr = pg.get("drift", [0, 0])
        dx += float(dr[0]) * (n - 1)
        dy += float(dr[1]) * (n - 1)
        jt = pg.get("jitter")
        if jt:
            g = rng_for(self.seed, "jitter", n)
            dx += g.uniform(-float(jt[0]), float(jt[0]))
            dy += g.uniform(-float(jt[1]), float(jt[1]))
        return round(dx, 2), round(dy, 2)

    # ---------------- render ----------------
    def render(self, path):
        c = canvas.Canvas(path, pagesize=(self.W, self.H), invariant=1)
        c.setTitle(self.case)
        c.setAuthor("make_greenflag.py (synthetic)")
        c.setSubject(FOOTER)
        fc = self.footer_cfg
        for p in self.pages:
            dx, dy = self.page_offset(p["n"])
            p["offset"] = (dx, dy)
            fs = float(fc.get("size", 6.5))
            ff = resolve_font(DEFAULT_FONT, {"size": fs}, "footer")
            fy = float(fc.get("y", self.H - 12))
            fa = fc.get("align", "centre")
            fx = float(fc.get("x", self.W / 2.0 if fa == "centre" else (self.margins["left"] if fa == "left" else self.W - self.margins["right"])))
            w = sw(FOOTER, ff)
            p["ops"].append({"t": "text", "x": fx, "y": fy, "s": FOOTER, "font": ff,
                             "off": {"left": 0, "right": w, "centre": w / 2.0}[fa], "w": w, "rot": 0,
                             "color": "#555555", "tag": "footer"})
            self.check_page(p, dx, dy)
            for op in p["ops"]:
                t = op["t"]
                if t == "text":
                    c.saveState()
                    c.setFillColor(HexColor(op["color"]) if op["color"] else HexColor("#000000"))
                    c.setFont(op["font"]["name"], op["font"]["size"])
                    if op["rot"]:
                        c.translate(op["x"] + dx, self.H - (op["y"] + dy))
                        c.rotate(op["rot"])
                        c.drawString(-op["off"], 0, op["s"])
                    else:
                        c.drawString(op["x"] - op["off"] + dx, self.H - (op["y"] + dy), op["s"])
                    c.restoreState()
                elif t == "rect":
                    c.saveState()
                    if op["fill"]:
                        c.setFillColor(HexColor(op["fill"]))
                    if op["stroke"]:
                        c.setStrokeColor(HexColor(op["stroke"]))
                        c.setLineWidth(op["width"])
                    c.rect(op["x"] + dx, self.H - (op["y"] + dy) - op["h"], op["w"], op["h"],
                           fill=1 if op["fill"] else 0, stroke=1 if op["stroke"] else 0)
                    c.restoreState()
                    tag = op.get("tag")
                    if isinstance(tag, dict):
                        rec = {"page": p["n"], "rect": [round(op["x"] + dx, 2), round(op["y"] + dy, 2),
                                                        round(op["x"] + op["w"] + dx, 2), round(op["y"] + op["h"] + dy, 2)]}
                        rec.update(tag)
                        self.redaction_boxes.append(rec)
                elif t == "line":
                    c.saveState()
                    c.setStrokeColor(HexColor(op["color"]) if op["color"] else HexColor("#000000"))
                    c.setLineWidth(op["width"])
                    c.line(op["x0"] + dx, self.H - (op["y0"] + dy), op["x1"] + dx, self.H - (op["y1"] + dy))
                    c.restoreState()
            c.showPage()
        c.save()

    def check_page(self, p, dx, dy):
        boxes = []
        for op in p["ops"]:
            if op["t"] != "text":
                continue
            f = op["font"]
            if op["rot"]:
                a = math.radians(op["rot"])
                pts = []
                for (u, v) in ((-op["off"], -f["size"] * 0.25), (op["w"] - op["off"], -f["size"] * 0.25),
                               (-op["off"], f["size"] * 0.8), (op["w"] - op["off"], f["size"] * 0.8)):
                    pts.append((op["x"] + u * math.cos(a) - v * math.sin(a),
                                op["y"] - (u * math.sin(a) + v * math.cos(a))))
                x0, x1 = min(q[0] for q in pts), max(q[0] for q in pts)
                y0, y1 = min(q[1] for q in pts), max(q[1] for q in pts)
            else:
                x0 = op["x"] - op["off"]
                x1 = x0 + op["w"]
                y0, y1 = op["y"] - f["size"] * 0.75, op["y"] + f["size"] * 0.2
            if x0 + dx < -0.5 or x1 + dx > self.W + 0.5 or y0 + dy < -0.5 or y1 + dy > self.H + 0.5:
                raise SpecError("page %d: %r is drawn off the page (x %.1f..%.1f, y %.1f..%.1f incl. offset)"
                                % (p["n"], op["s"], x0 + dx, x1 + dx, y0 + dy, y1 + dy))
            boxes.append((y0, y1, x0, x1, op["s"]))
        if self.spec.get("allow_collisions"):
            return
        boxes.sort()
        for i, (y0, y1, x0, x1, s) in enumerate(boxes):
            for (b0, b1, c0, c1, t) in boxes[i + 1:]:
                if b0 >= y1 - 0.6:
                    break
                if min(x1, c1) - max(x0, c0) > 0.6 and min(y1, b1) - max(y0, b0) > 0.6:
                    raise SpecError("page %d: text %r collides with %r (set allow_collisions to permit)"
                                    % (p["n"], s, t))

    # ---------------- truth ----------------
    def truth(self, scan_name):
        rows_out = []
        accounts_out = []
        all_cols = []
        for A in self.accounts:
            T = A["T"]
            cols_sorted = sorted(T["cols"], key=lambda c: c["x"])
            cols_out = []
            for C in cols_sorted:
                rec = {"id": C["id"], "kind": C["kind"],
                       "heading": norm_ws(C["heading"]["text"]) if C["heading"] else "",
                       "x0": round(C["x"], 2), "x1": round(C["x"] + C["w"], 2), "align": C["align"]}
                if C["page_dx"]:
                    rec["page_dx"] = {str(k): v for k, v in C["page_dx"].items()}
                if C["kind"] in ("date", "date2"):
                    rec["format"] = C["format"]
                    rec["group_once"] = C["group_once"]
                elif C["kind"] in MONEY_KINDS:
                    rec["format"] = {k: v for k, v in C["format"].items()}
                    rec["invert"] = C["invert"]
                    rec["value_sign"] = C["value_sign"]
                elif C["kind"] == "indicator":
                    rec["of"] = C["of"]
                    rec["tokens"] = C["tokens"]
                elif C["kind"] in ("text", "ignore_text"):
                    rec["key"] = C["key"]
                    rec["source"] = C["source"]
                if C["heading"]:
                    h = C["heading"]
                    rec["heading_placement"] = {"dx": h["dx"], "dy": h["dy"], "align": h["align"] or C["align"],
                                                "rotate": h["rotate"], "below_first_row": h["below_first_row"],
                                                "span_to": h["span_to"], "pages": h["pages"]}
                cols_out.append(rec)
            all_cols.append(cols_out)
            for r in A["rows"]:
                red = r.get("red_fields", {})
                removed, overlaid = [], []
                text, ign = {}, {}
                for C in cols_sorted:
                    if C["kind"] in ("text", "ignore_text"):
                        v = " ".join(r["cells"][C["id"]]["lines"])
                        if red.get(C["id"]) == "remove" and r["cells"][C["id"]]["lines"]:
                            v = None
                        (text if C["kind"] == "text" else ign)[C["key"]] = v
                debit = dollars(r["cents"]) if r["dir"] == "out" else None
                credit = dollars(r["cents"]) if r["dir"] == "in" else None
                bal = dollars(r["bal"]) if r["show_bal"] else None
                date = r["date"].isoformat()
                date2 = r["date2"].isoformat() if "date2" in r else None
                for C in cols_sorted:
                    k = red.get(C["id"])
                    if not k:
                        continue
                    if C["kind"] in ("debit", "credit", "amount_signed", "amount_unsigned"):
                        fld = "debit" if r["dir"] == "out" else "credit"
                    elif C["kind"] == "balance":
                        fld = "balance"
                    elif C["kind"] in ("date", "date2"):
                        fld = C["kind"]
                    elif C["kind"] == "text":
                        fld = "text." + C["key"]
                    elif C["kind"] == "ignore_text":
                        fld = "ignore_text." + C["key"]
                    else:
                        fld = "indicator"
                    if k == "overlay":
                        overlaid.append(fld)
                        continue
                    removed.append(fld)
                    if fld == "debit":
                        debit = None
                    elif fld == "credit":
                        credit = None
                    elif fld == "balance":
                        bal = None
                    elif fld == "date":
                        date = None
                    elif fld == "date2":
                        date2 = None
                desc = " ".join(v for v in text.values() if v)
                row = {"date": date, "description": desc, "debit": debit, "credit": credit, "balance": bal}
                if date2 is not None or any(c["kind"] == "date2" for c in T["cols"]):
                    row["date2"] = date2
                row["text"] = text
                if ign:
                    row["ignore_text"] = ign
                row["page"] = r["page"]
                row["account_index"] = A["index"]
                row["redacted"] = removed
                row["overlay_redacted"] = overlaid
                row["printed"] = r["printed"]
                rows_out.append(row)
            accounts_out.append({"index": A["index"], "name": A["name"], "number": A["number"],
                                 "holder": A["holder"], "period": {"start": A["start"].isoformat(), "end": A["end"].isoformat()},
                                 "opening_balance": dollars(A["opening"]), "closing_balance": dollars(A["closing"]),
                                 "row_count": len(A["rows"]), "newest_first": A["newest_first"],
                                 "pages": A["pages"], "balance_mode": T["balance_mode"], "columns": cols_out})
        cues = self.detect_cues(rows_out)
        feats = sorted(set(self.spec.get("features", [])) | self.derive_features(rows_out))
        A0 = self.accounts[0]
        tr = {
            "case": self.case, "generator": GENERATOR, "note": self.spec.get("note", ""),
            "opening_balance": dollars(A0["opening"]), "closing_balance": dollars(A0["closing"]),
            "row_count": len(rows_out), "theme": self.spec.get("theme", ""), "features": feats,
            "decidable": bool(self.spec.get("decidable", True)),
            "decidable_by": list(self.spec.get("decidable_by", [])),
            "undecidable_reason": self.spec.get("undecidable_reason"),
            "auto_cues": cues, "newest_first": A0["newest_first"], "pages": len(self.pages),
            "page_size": [round(self.W, 2), round(self.H, 2)], "page_offsets": [list(p["offset"]) for p in self.pages],
            "period": {"start": A0["start"].isoformat(), "end": A0["end"].isoformat()},
            "spec_file": os.path.basename(self.spec_file), "seed": self.seed,
            "columns": all_cols[0], "accounts": accounts_out, "redaction_boxes": self.redaction_boxes,
            "items": self.items_out, "scan_pdf": scan_name, "rows": rows_out,
        }
        return tr

    def detect_cues(self, rows):
        cues = set()
        if any(r["balance"] is not None for r in rows):
            cues.add("running_balance")
        if self.cue_totals or any(k in A["T"] for A in self.accounts
                                  for k in ("opening_line", "closing_line", "page_totals", "carry")):
            cues.add("printed_totals")
        for A in self.accounts:
            tx = A["tx"]
            for C in A["T"]["cols"]:
                if C["kind"] in ("amount_signed", "debit", "credit"):
                    F = C["format"]
                    if C["kind"] == "amount_signed" and (neg_markable(F) or F["pos_prefix"] or F["pos_suffix"] or F["vector_minus"]):
                        cues.add("sign_markers")
                    if C["kind"] in ("debit", "credit") and (F["pos_prefix"] or F["pos_suffix"] or
                                                             (C["value_sign"] == "negative")):
                        cues.add("sign_markers")
                if C["kind"] == "indicator" and C["of"] == "amount":
                    cues.add("indicator_column")
                if C["kind"] == "text" and isinstance(C.get("values"), dict):
                    cues.add("description_semantics")
            if tx.get("pool", "semantic") in ("semantic", "names", "mixed") or tx.get("literal") or \
                    isinstance(tx.get("custom_pool"), dict):
                if any(C["kind"] == "text" and set(C["source"]) & {"payee", "name", "type", "custom"}
                       for C in A["T"]["cols"]) or tx.get("literal"):
                    cues.add("description_semantics")
        if self.cue_legend:
            cues.add("legend")
        return [c for c in CUES if c in cues]

    def derive_features(self, rows):
        f = set(self.auto)
        if len(self.accounts) > 1:
            f.add("multi_account")
            if any(A["spec"].get("new_page") for A in self.accounts[1:]):
                f.add("bundled_statements")
        if self.orientation == "landscape":
            f.add("landscape")
        f.add("page_" + self.page_size_name.lower())
        for A in self.accounts:
            T = A["T"]
            cols = T["cols"]
            kinds = {c["kind"]: c for c in cols}
            fam = T["font"]["family"].lower()
            f.add("font_" + fam)
            ntext = sum(1 for c in cols if c["kind"] == "text")
            f.add("text_cols_%d" % ntext)
            if any(r["nlines"] > 1 for r in A["rows"]):
                f.add("multiline_rows")
            d = kinds["date"]
            money_x = [c["x"] for c in cols if c["kind"] in MONEY_KINDS]
            if money_x and d["x"] > max(money_x):
                f.add("date_right")
            elif money_x and d["x"] > min(c["x"] for c in cols if c["kind"] == "text"):
                f.add("date_middle")
            else:
                f.add("date_left")
            if "date2" in kinds:
                f.add("two_dates")
            if any(c.get("group_once") for c in cols):
                f.add("date_once_per_day")
            if any(c["kind"] in ("date", "date2") and not c["has_year"] for c in cols):
                f.add("yearless_dates")
            f.add("balance_" + T["balance_mode"])
            if "balance" in kinds:
                b = kinds["balance"]
                amt = [c["x"] for c in cols if c["kind"] in ("debit", "credit", "amount_signed", "amount_unsigned")]
                if b["x"] < min(amt):
                    f.add("balance_left_of_amounts")
            if A["newest_first"]:
                f.add("newest_first")
            if any(r["bal"] < 0 for r in A["rows"]) or A["opening"] < 0:
                f.add("overdrawn")
            if "debit" in kinds:
                f.add("debit_credit_columns")
            if "amount_signed" in kinds:
                f.add("signed_amount_column")
            if "amount_unsigned" in kinds:
                f.add("unsigned_amount_column")
            for c in cols:
                if c["kind"] == "indicator":
                    f.add("indicator_of_" + c["of"])
                if c["kind"] == "ignore_text":
                    f.add("ignore_text_column")
                if c["kind"] in MONEY_KINDS:
                    F = c["format"]
                    f.add("money_" + F["style"])
                    if F["thousands"] == " ":
                        f.add("thousands_space")
                    elif F["thousands"] == "'":
                        f.add("thousands_apostrophe")
                    elif F["thousands"] == "":
                        f.add("thousands_none")
                    elif F["thousands"] == ".":
                        f.add("thousands_dot")
                    if F["decimal"] == ",":
                        f.add("decimal_comma")
                    if F["currency"] or F["currency_suffix"]:
                        f.add("currency_marker")
                    if F["vector_minus"]:
                        f.add("vector_minus")
                    if c.get("invert"):
                        f.add("inverted_sign")
                h = c["heading"]
                if h:
                    if h["rotate"]:
                        f.add("rotated_heading")
                    if h["below_first_row"]:
                        f.add("heading_below_first_row")
                    if h["span_to"]:
                        f.add("heading_spans_columns")
                    if "\n" in h["text"]:
                        f.add("multiline_heading")
                    if h["pages"] != "all":
                        f.add("heading_missing_some_pages")
                    if abs(h["dx"]) > 8:
                        f.add("heading_offset")
                else:
                    f.add("column_without_heading")
                if c["page_dx"]:
                    f.add("column_page_shift")
            if T.get("heading_pages", "all") != "all":
                f.add("heading_missing_some_pages")
            if T["extra"]:
                f.add("extra_headings")
            for k in ("opening_line", "closing_line", "page_totals", "carry"):
                if k in T:
                    f.add(k)
            if any(r["cents"] == 0 for r in A["rows"]):
                f.add("zero_amounts")
            if any(r["cents"] >= 100000000 for r in A["rows"]):
                f.add("huge_amounts")
        pg = self.page_cfg
        if pg.get("offsets") or pg.get("drift") or pg.get("jitter"):
            f.add("page_offsets")
        kinds_red = {b["kind"] for b in self.redaction_boxes}
        for k in kinds_red:
            f.add("redaction_" + k)
        if self.spec.get("scan"):
            f.add("scan")
        if len(self.pages) > 1:
            f.add("multi_page")
        return f


TEXT_SOURCES = ("payee", "name", "type", "particulars", "code", "reference", "memo", "location",
                "card", "seq", "serial", "custom")


# ---------------------------------------------------------------------------
# Scan
# ---------------------------------------------------------------------------

def make_scan(src, dst, sc, seed):
    try:
        import pymupdf
    except ImportError:
        import fitz as pymupdf
    import numpy as np
    from PIL import Image, ImageFilter
    check_keys(sc if isinstance(sc, dict) else {}, SCAN_KEYS, "scan")
    sc = sc if isinstance(sc, dict) else {}
    dpi = int(sc.get("dpi", 200))
    if not 150 <= dpi <= 400:
        raise SpecError("scan.dpi must be 150..400")
    skew = float(sc.get("skew", 0.6))
    if abs(skew) > 1.5:
        raise SpecError("scan.skew must be <= 1.5 degrees")
    noise = float(sc.get("noise", 0.03))
    blur = float(sc.get("blur", 0.5))
    q = int(sc.get("jpeg_quality", 70))
    doc = pymupdf.open(src)
    out = pymupdf.open()
    for i, pg in enumerate(doc):
        pix = pg.get_pixmap(dpi=dpi, colorspace=pymupdf.csGRAY, alpha=False)
        img = Image.frombytes("L", (pix.width, pix.height), pix.samples)
        g = rng_for(seed, "scan", i)
        ang = skew * (1 if g.random() < 0.5 else -1) * (0.6 + 0.4 * g.random())
        if ang:
            img = img.rotate(ang, resample=Image.BICUBIC, expand=False, fillcolor=255)
        if blur:
            img = img.filter(ImageFilter.GaussianBlur(blur))
        if noise:
            arr = np.asarray(img, dtype=np.int16)
            rs = np.random.RandomState(crc(seed, "noise", i) & 0x7FFFFFFF)
            arr = arr + rs.normal(0, noise * 255, arr.shape).astype(np.int16)
            img = Image.fromarray(np.clip(arr, 0, 255).astype(np.uint8), "L")
        buf = io.BytesIO()
        img.save(buf, "JPEG", quality=q)
        np_ = out.new_page(width=pg.rect.width, height=pg.rect.height)
        np_.insert_image(np_.rect, stream=buf.getvalue())
    out.set_metadata({"title": os.path.basename(dst), "subject": FOOTER, "producer": "make_greenflag.py",
                      "creator": "make_greenflag.py"})
    out.save(dst, garbage=4, deflate=True, no_new_id=True)
    out.close()
    doc.close()


# ---------------------------------------------------------------------------
# Build one case
# ---------------------------------------------------------------------------

def build_case(spec_path, out_dir):
    with open(spec_path) as fh:
        try:
            spec = json.load(fh)
        except json.JSONDecodeError as e:
            raise SpecError("invalid JSON: %s" % e)
    B = Builder(spec, spec_path)
    B.layout()
    B.place_page_items()
    B.finalize()
    pdf = os.path.join(out_dir, B.case + ".pdf")
    B.render(pdf)
    scan_name = None
    if spec.get("scan"):
        scan_name = B.case + "_scan.pdf"
        make_scan(pdf, os.path.join(out_dir, scan_name), spec["scan"] if isinstance(spec["scan"], dict) else {}, B.seed)
    tr = B.truth(scan_name)
    # claims about decidability must be backed by detected content
    if tr["decidable"]:
        if not tr["decidable_by"]:
            raise SpecError("decidable=true needs decidable_by (one or more of %s)" % ", ".join(CUES))
        bad = [c for c in tr["decidable_by"] if c not in CUES]
        if bad:
            raise SpecError("decidable_by has unknown cue(s) %s" % bad)
        missing = [c for c in tr["decidable_by"] if c not in tr["auto_cues"]]
        if missing:
            raise SpecError("decidable_by claims %s but the content does not show it (detected: %s)"
                            % (missing, tr["auto_cues"]))
    else:
        if not tr["undecidable_reason"]:
            raise SpecError("decidable=false needs undecidable_reason")
    with open(os.path.join(out_dir, B.case + ".truth.json"), "w") as fh:
        json.dump(tr, fh, indent=1)
        fh.write("\n")
    return B.case, tr


# ---------------------------------------------------------------------------
# Checker
# ---------------------------------------------------------------------------

def pdftotext(pdf):
    try:
        out = subprocess.run(["pdftotext", "-layout", "-enc", "UTF-8", pdf, "-"], capture_output=True,
                             check=True)
    except FileNotFoundError:
        raise SystemExit("--check needs poppler's pdftotext on PATH")
    return out.stdout.decode("utf-8", "replace")


def check_case(tpath):
    errs = []
    with open(tpath) as fh:
        T = json.load(fh)
    d = os.path.dirname(tpath)
    pdf = os.path.join(d, T["case"] + ".pdf")
    if not os.path.exists(pdf):
        return ["PDF missing: %s" % pdf], T
    txt = pdftotext(pdf)
    pages_txt = txt.split("\f")
    hay = "\n".join(norm_ws(l) for l in txt.splitlines())
    try:
        import pymupdf
    except ImportError:
        import fitz as pymupdf
    doc = pymupdf.open(pdf)
    if doc.page_count != T["pages"]:
        errs.append("PDF has %d pages, truth says %d" % (doc.page_count, T["pages"]))
    for i in range(doc.page_count):
        if i >= len(pages_txt) or FOOTER not in norm_ws(pages_txt[i]):
            errs.append("page %d: footer missing" % (i + 1))
    rows = T["rows"]
    if T["row_count"] != len(rows):
        errs.append("row_count %d != %d rows" % (T["row_count"], len(rows)))
    for key in ("case", "generator", "note", "opening_balance", "closing_balance", "row_count", "rows",
                "theme", "features", "decidable", "decidable_by", "columns", "newest_first"):
        if key not in T:
            errs.append("truth lacks %r" % key)
    accts = {a["index"]: a for a in T["accounts"]}
    if T["accounts"][0]["opening_balance"] != T["opening_balance"] or \
            T["accounts"][0]["closing_balance"] != T["closing_balance"]:
        errs.append("top-level opening/closing differ from account 0")
    # arithmetic
    for ai, A in accts.items():
        ar = [r for r in rows if r["account_index"] == ai]
        if len(ar) != A["row_count"]:
            errs.append("account %d: row_count %d != %d" % (ai, A["row_count"], len(ar)))
        seq = list(reversed(ar)) if A["newest_first"] else ar
        run = cents_of(A["opening_balance"])
        last_date = None
        for r in seq:
            if r["debit"] is not None and r["credit"] is not None:
                errs.append("row has both debit and credit: %r" % r["description"])
            if r["date"]:
                if last_date and r["date"] < last_date:
                    errs.append("account %d: dates out of order at %s (newest_first=%s)"
                                % (ai, r["date"], A["newest_first"]))
                last_date = r["date"]
            if r["debit"] is None and r["credit"] is None:
                run = None
            elif run is not None:
                run += cents_of(r["credit"] or 0) - cents_of(r["debit"] or 0)
            if r["balance"] is not None:
                b = cents_of(r["balance"])
                if run is not None and run != b:
                    errs.append("account %d: balance %.2f printed, arithmetic gives %.2f (%s)"
                                % (ai, b / 100.0, run / 100.0, r["description"]))
                run = b
        if run is not None and run != cents_of(A["closing_balance"]):
            errs.append("account %d: closing %.2f but rows give %.2f" % (ai, A["closing_balance"], run / 100.0))
    # printed values present and parsing back to the truth
    for idx, r in enumerate(rows):
        A = accts[r["account_index"]]
        where = "row %d (%s)" % (idx, r["description"][:30])
        pr = r["printed"]
        for C in A["columns"]:
            p = pr.get(C["id"])
            k = C["kind"]
            if p is None:
                if k == "text" and r["text"].get(C["key"]) and "text." + C["key"] not in r["redacted"]:
                    errs.append("%s: text %r has no printed record" % (where, C["key"]))
                continue
            lines = p if isinstance(p, list) else [p]
            for ln in lines:
                if ln and norm_ws(ln) not in hay:
                    errs.append("%s: printed %r not found in pdftotext output" % (where, ln))
            if k in ("text", "ignore_text"):
                tv = (r["text"] if k == "text" else r.get("ignore_text", {})).get(C["key"])
                if tv != " ".join(lines):
                    errs.append("%s: %s %r != printed %r" % (where, C["key"], tv, lines))
            elif k in ("date", "date2"):
                want = r[k]
                got = parse_date(p, C["format"])
                if got is None:
                    errs.append("%s: date %r does not parse with %r" % (where, p, C["format"]))
                elif want is not None:
                    wd = dt.date.fromisoformat(want)
                    y, m, dd, dow = got
                    if (y is not None and y != wd.year) or m != wd.month or dd != wd.day or \
                            (dow and dow != DAYS[wd.weekday()][:3].lower()):
                        errs.append("%s: printed date %r != truth %s" % (where, p, want))
            elif k == "indicator":
                if C["of"] == "amount":
                    if r["debit"] is None and r["credit"] is None:
                        continue
                    want = C["tokens"]["in" if r["credit"] is not None else "out"]
                else:
                    if r["balance"] is None:
                        continue
                    want = C["tokens"]["neg" if r["balance"] < 0 else "pos"]
                if p != want:
                    errs.append("%s: indicator %r != expected %r" % (where, p, want))
            else:
                F = C["format"]
                try:
                    got = parse_money(p, F)
                except ValueError as e:
                    errs.append("%s: %s" % (where, e))
                    continue
                db, cr = cents_of(r["debit"]), cents_of(r["credit"])
                if k == "debit":
                    want = None if db is None else (-db if C["value_sign"] == "negative" else db)
                    if db is None and cr is not None:
                        errs.append("%s: debit column printed on a credit row" % where)
                elif k == "credit":
                    want = None if cr is None else (-cr if C["value_sign"] == "negative" else cr)
                    if cr is None and db is not None:
                        errs.append("%s: credit column printed on a debit row" % where)
                elif k == "amount_signed":
                    want = None if (db is None and cr is None) else (cr if cr is not None else -db)
                    if want is not None and C["invert"]:
                        want = -want
                elif k == "amount_unsigned":
                    want = db if db is not None else cr
                else:
                    want = cents_of(r["balance"])
                    if want is not None and C["invert"]:
                        want = -want
                if want is None:
                    continue
                if neg_markable(F):
                    ok = got == want
                else:
                    ok = abs(got) == abs(want)
                if not ok:
                    errs.append("%s: %s printed %r reads %.2f, truth %.2f" % (where, C["id"], p, got / 100.0, want / 100.0))
        desc = " ".join(v for v in r["text"].values() if v)
        if desc != r["description"]:
            errs.append("%s: description %r != joined text %r" % (where, r["description"], desc))
    # redaction boxes, by position
    for b in T.get("redaction_boxes", []):
        page = doc[b["page"] - 1]
        x0, y0, x1, y1 = b["rect"]
        inside = [w[4] for w in page.get_text("words")
                  if x0 <= (w[0] + w[2]) / 2 <= x1 and y0 <= (w[1] + w[3]) / 2 <= y1]
        if b["kind"] == "remove" and inside:
            errs.append("page %d: removed redaction still has text under it: %r" % (b["page"], inside))
        if b["kind"] == "overlay" and norm_ws(" ".join(inside)) != norm_ws(b["text"]):
            errs.append("page %d: overlay redaction text %r != %r" % (b["page"], " ".join(inside), b["text"]))
    for r in rows:
        for f in r["redacted"]:
            if f in ("debit", "credit", "balance", "date") and r.get(f) is not None:
                errs.append("redacted field %s still has a value" % f)
    # decidability record
    if T["decidable"]:
        if not T["decidable_by"] or any(c not in T.get("auto_cues", []) for c in T["decidable_by"]):
            errs.append("decidable_by %r not backed by detected cues %r" % (T["decidable_by"], T.get("auto_cues")))
    elif not T.get("undecidable_reason"):
        errs.append("undecidable without a reason")
    if T.get("scan_pdf"):
        sp = os.path.join(d, T["scan_pdf"])
        if not os.path.exists(sp):
            errs.append("scan PDF missing")
        else:
            sd = pymupdf.open(sp)
            if sd.page_count != T["pages"]:
                errs.append("scan has %d pages, not %d" % (sd.page_count, T["pages"]))
            if any(pg.get_text().strip() for pg in sd):
                errs.append("scan PDF has a text layer")
            sd.close()
    doc.close()
    return errs, T


def run_check(d, only=None):
    paths = sorted(glob.glob(os.path.join(d, "*.truth.json")))
    if only:
        paths = [p for p in paths if only in os.path.basename(p)]
    if not paths:
        print("no truth files in %s" % d)
        return 1
    bad = 0
    for p in paths:
        errs, T = check_case(p)
        name = os.path.basename(p)[:-len(".truth.json")]
        if errs:
            bad += 1
            print("FAIL %-40s %d problem(s)" % (name, len(errs)))
            for e in errs[:12]:
                print("     - " + e)
            if len(errs) > 12:
                print("     ... %d more" % (len(errs) - 12))
        else:
            print("ok   %-40s %3d rows %d pg  %-9s %s" % (name, T["row_count"], T["pages"],
                                                          "decidable" if T["decidable"] else "UNDECIDABLE",
                                                          ",".join(T["decidable_by"]) or "-"))
    print("%d/%d cases clean%s" % (len(paths) - bad, len(paths), "" if not bad else "  <-- %d FAILED" % bad))
    return 1 if bad else 0


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------

def main(argv=None):
    ap = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    ap.add_argument("--specs", help="directory of *.json spec files")
    ap.add_argument("--out", help="output directory for PDFs and truth files")
    ap.add_argument("--only", help="only specs/cases whose file name contains this")
    ap.add_argument("--check", metavar="DIR", help="verify every truth file in DIR against its PDF")
    ap.add_argument("--list", action="store_true", help="list the specs")
    a = ap.parse_args(argv)
    if a.check:
        return run_check(a.check, a.only)
    if not a.specs:
        ap.error("--specs is required (or --check DIR)")
    specs = sorted(glob.glob(os.path.join(a.specs, "*.json")))
    if a.only:
        specs = [s for s in specs if a.only in os.path.basename(s)]
    if a.list:
        for s in specs:
            try:
                with open(s) as fh:
                    sp = json.load(fh)
                print("%-34s %-14s %-11s %s" % (sp.get("case", "?"), sp.get("theme", ""),
                                                "decidable" if sp.get("decidable", True) else "UNDECIDABLE",
                                                sp.get("note", "")[:90]))
            except Exception as e:
                print("%-34s BROKEN: %s" % (os.path.basename(s), e))
        return 0
    if not a.out:
        ap.error("--out is required")
    os.makedirs(a.out, exist_ok=True)
    failed = 0
    seen = {}
    for s in specs:
        try:
            case, tr = build_case(s, a.out)
        except SpecError as e:
            failed += 1
            print("SPEC ERROR %s: %s" % (os.path.basename(s), e))
            continue
        if case in seen:
            failed += 1
            print("SPEC ERROR %s: case name %r already used by %s" % (os.path.basename(s), case, seen[case]))
            continue
        seen[case] = os.path.basename(s)
        errs, _ = check_case(os.path.join(a.out, case + ".truth.json"))
        if errs:
            failed += 1
            print("FAIL %-40s" % case)
            for e in errs[:12]:
                print("     - " + e)
        else:
            print("ok   %-40s %3d rows %d pg  %s%s" % (case, tr["row_count"], tr["pages"],
                                                     "decidable by " + ",".join(tr["decidable_by"]) if tr["decidable"] else "UNDECIDABLE",
                                                     "  +scan" if tr["scan_pdf"] else ""))
    print("%d/%d specs built clean%s" % (len(specs) - failed, len(specs), "" if not failed else "  <-- %d FAILED" % failed))
    return 1 if failed else 0


if __name__ == "__main__":
    sys.exit(main())
