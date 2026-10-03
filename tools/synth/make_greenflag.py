#!/usr/bin/env python3
"""make_greenflag.py -- the GREEN-FLAG set: 100 deliberately weird synthetic bank
statements, the acceptance test for the content-reading auto wizard.

WHAT THIS IS. Each statement is drawn from a one-line recipe (CASES, below) layered
over seeded random choices. The recipes push every axis a template reader leans on:
made-up column headings in odd places (offset from their data, centred over two
columns, split over lines, below the first row, at the bottom, missing on later
pages, a heading row that lines up with nothing, headings that lie), 1-5 text
columns that wrap independently, dates on the left / right / middle / twice / once
per day group in many formats, every money convention (leading / trailing minus,
brackets, CR/DR suffix or prefix in either case, explicit +/-, unsigned figures with
an indicator column -- including made-up tokens explained only by a printed legend --
separate in/out columns in either order, decimal comma, space / apostrophe / no
thousands separators, $ / NZ$ / NZD prefixes, zero and huge amounts), running balance
on every row / day-end only / absent / left of the amounts / newest-first /
overdrawn, opening and closing lines inside the table, totals boxes with made-up
labels, per-page offsets and drift, landscape and Letter, 6-11pt type, both kinds of
redaction, and fictional banks with silly names. A handful get image-only scans.

It was written WITHOUT reading the reader it measures (R/wizard_auto.R and friends).
It reuses tools/synth/make_layouts.py for the drawing sheet (collision and off-page
checks), the date formats, and the scan rasteriser.

DECIDABILITY. The correct reading must follow from the page content alone, ignoring
the made-up headings. The generator COMPUTES which cues settle the direction of money
(it does not take the recipe's word for it) and refuses to write a case whose
computed answer disagrees with the recipe:
    running_balance        a balance printed on (nearly) every row, or day-end
                           balances whose sign assignment is unique per day
    printed_totals         opening AND closing printed, amounts fall into two groups
                           (two columns / indicator tokens / sign) and the group sums
                           differ, so closing = opening + in - out names the groups
    sign_markers           minus / brackets / CR-DR / +- on the figure itself
    indicator_column       a standard indicator token (CR/DR, C/D, +/-, IN/OUT)
    legend                 made-up tokens explained by a printed legend
    description_semantics  SALARY / WAGES / REFUND / INTEREST CREDIT / TRANSFER FROM
                           = money in; EFTPOS / POS / FEE / ATM / PAYMENT TO /
                           DIRECT DEBIT = money out
Ten recipes are deliberately UNDECIDABLE (decidable false + undecidable_reason): the
only correct reader behaviour there is to ask a person.

TRUTH FILE (<case>.truth.json), compatible with tools/synth/truth.R:
    case, generator, note, opening_balance, closing_balance, row_count,
    rows: [{date "YYYY-MM-DD", description, debit, credit, balance, ...}]
  debit = money OUT, credit = money IN, both positive (0.0 for a zero-value line,
  which counts as a debit) else null; balance = the balance printed ON THAT ROW else
  null; rows in PRINTED order; description = every text-column value left to right
  joined by one space (a wrapped cell's lines joined by one space); opening, closing,
  brought/carried-forward and column-total lines are never rows. With two date
  columns `date` is the transaction date (the earlier one) and `date2` the other.
  Added keys: features[], decidable, decidable_by[], undecidable_reason,
  newest_first, columns[] ({heading, kind, source}), money_format, balance_format,
  amount_layout, indicator_tokens, legend, opening_printed, closing_printed,
  page_count, period; per row: text {heading: value}, page, redacted[] (value
  genuinely removed under a black box: field null), overlay_redacted[] (box over
  text still in the text layer: value kept), printed {date, date2, amount,
  indicator, balance} (the exact strings drawn, null when not drawn).
  A _scan truth turns every overlay into a removal (a scan cannot see under a box).

CHECKS. Before each truth is written: the balance chain (opening -> every printed
balance -> closing, through every redaction), every printed figure parsed back to
the truth, neutral text free of direction words, every drawn string inside the page
and clear of its neighbours (make_layouts.Sheet), and the computed decidability equal
to the recipe's. After the run (or alone with --check DIR): every non-redacted truth
amount, balance and date string is found in `pdftotext -layout` output on the row's
page, the footer is on every page, and each scan has no text layer.

Run:  python3 tools/synth/make_greenflag.py --out DIR [--only SUBSTR] [--check DIR]

Deterministic (crc32 seeds, reportlab invariant mode). ASCII-only source. Dev-time
only; nothing here ships. Python 3.9+, reportlab, pymupdf + numpy + Pillow (scans),
poppler pdftotext (checks).
"""

import argparse
import datetime as dt
import json
import os
import random
import re
import subprocess
import sys
import zlib

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import make_layouts as ML  # noqa: E402  (Sheet, GenError, dates, make_scan)
from make_layouts import GenError, Sheet  # noqa: E402
from reportlab.lib.pagesizes import A4, landscape, letter  # noqa: E402
from reportlab.pdfbase.pdfmetrics import stringWidth  # noqa: E402

GENERATOR = "tools/synth/make_greenflag.py"
FOOTER = ML.SYNTHETIC
PAGES = {"A4": A4, "A4L": landscape(A4), "Letter": letter, "LetterL": landscape(letter)}
MON, MONTH, DOW = ML.MON, ML.MONTH, ML.DOW
DAYNAME = ["Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"]
BLACK, PALE, RED = (0, 0, 0), (0.93, 0.93, 0.93), (0.78, 0.05, 0.05)

# ---------------------------------------------------------------------------
# Dates
# ---------------------------------------------------------------------------

DATES = dict(ML.DATE_FMTS)
DATES.update({
    "dd.mm.yyyy": lambda d: "%02d.%02d.%d" % (d.day, d.month, d.year),
    "d.m.yy": lambda d: "%d.%d.%02d" % (d.day, d.month, d.year % 100),
    "mm/dd/yyyy": lambda d: "%02d/%02d/%d" % (d.month, d.day, d.year),
    "Mon d, yyyy": lambda d: "%s %d, %d" % (MON[d.month - 1], d.day, d.year),
    "dd": lambda d: "%02d" % d.day,
    "DOW dd/mm": lambda d: "%s %02d/%02d" % (DOW[d.weekday()].upper(), d.day, d.month),
    "Month d": lambda d: "%s %d" % (MONTH[d.month - 1], d.day),
    "d-MON": lambda d: "%d-%s" % (d.day, MON[d.month - 1].upper()),
    "yyyy.mm.dd": lambda d: "%d.%02d.%02d" % (d.year, d.month, d.day),
    "dd Mon 'yy": lambda d: "%02d %s '%02d" % (d.day, MON[d.month - 1], d.year % 100),
    "Dow, d Month": lambda d: "%s, %d %s" % (DAYNAME[d.weekday()], d.day, MONTH[d.month - 1]),
})
# day and month both numeric: only readable when some printed day exceeds 12
DM_NUMERIC = {"dd/mm/yyyy", "d/mm/yyyy", "dd/mm/yy", "dd-mm-yy", "dd-mm-yyyy", "d/m/yyyy",
              "d/m/yy", "dd.mm.yyyy", "d.m.yy", "mm/dd/yyyy", "DOW dd/mm"}
COMMON_DATES = ["dd/mm/yyyy", "dd Mon", "dd Mon yyyy", "d Mon", "dd-Mon-yy", "dd/mm/yy",
                "dd MON yy", "d/m/yyyy", "Dow dd Mon", "d Month yyyy", "d-MON", "dd Mon 'yy",
                "d.m.yy", "Month d", "DOW dd/mm"]

# ---------------------------------------------------------------------------
# Money: format and parse back. M = {thou, dec, cur, neg, tok, sep}
#   neg: lead "-1.00" | trail "1.00-" | paren "(1.00)" | plus "+1.00/-1.00"
#        suf "1.00 DR" | pre "DR 1.00" | none (unsigned; a negative is an error)
# ---------------------------------------------------------------------------

M_DEFAULT = {"thou": ",", "dec": ".", "cur": "", "neg": "lead", "tok": ["CR", "DR"], "sep": " "}


def group3(d, sep):
    s = str(d)
    if not sep:
        return s
    parts = []
    while len(s) > 3:
        parts.insert(0, s[-3:])
        s = s[:-3]
    parts.insert(0, s)
    return sep.join(parts)


def fmt_money(c, M, unsigned=False, zero_neg=False):
    d, r = divmod(abs(int(c)), 100)
    body = M["cur"] + group3(d, M["thou"]) + M["dec"] + "%02d" % r
    st = "none" if unsigned else M["neg"]
    if st == "none":
        if c < 0:
            raise GenError("unsigned style asked to print a negative figure")
        return body
    neg = c < 0 or (c == 0 and zero_neg)
    if st in ("suf", "pre"):
        t = M["tok"][1] if neg else M["tok"][0]
        if not t:
            return body
        return body + M["sep"] + t if st == "suf" else t + M["sep"] + body
    if c == 0:
        return body
    if st == "lead":
        return ("-" if neg else "") + body
    if st == "plus":
        return ("-" if neg else "+") + body
    if st == "trail":
        return body + ("-" if neg else "")
    if st == "paren":
        return "(" + body + ")" if neg else body
    raise GenError("unknown money style %r" % st)


def parse_money(s, M, unsigned=False):
    """Read a printed figure back to signed cents, by stripping marks, not by
    re-formatting -- so a formatter that drops a sign is caught."""
    t = s.strip()
    neg = False
    st = "none" if unsigned else M["neg"]
    if st == "suf":
        pos, ng = M["tok"]
        if ng and t.endswith(ng):
            neg, t = True, t[:-len(ng)].rstrip()
        elif pos and t.endswith(pos):
            t = t[:-len(pos)].rstrip()
    elif st == "pre":
        pos, ng = M["tok"]
        if ng and t.startswith(ng):
            neg, t = True, t[len(ng):].lstrip()
        elif pos and t.startswith(pos):
            t = t[len(pos):].lstrip()
    if t.startswith("(") and t.endswith(")"):
        neg, t = True, t[1:-1]
    if t[:1] in ("+", "-"):
        neg, t = t[0] == "-", t[1:]
    if t.endswith("-"):
        neg, t = True, t[:-1]
    cur = M["cur"]
    if cur:
        if not t.startswith(cur):
            raise GenError("figure %r lacks its currency mark %r" % (s, cur))
        t = t[len(cur):]
    num = (r"\d{1,3}(?:" + re.escape(M["thou"]) + r"\d{3})*") if M["thou"] else r"\d+"
    if not re.fullmatch(num + re.escape(M["dec"]) + r"\d\d", t):
        raise GenError("printed figure %r does not parse under %r" % (s, M))
    c = int(re.sub(r"\D", "", t))
    if unsigned and neg:
        raise GenError("unsigned figure %r carries a sign" % s)
    return -c if neg else c


# ---------------------------------------------------------------------------
# Made-up world
# ---------------------------------------------------------------------------

BANKS = ["Bank of the Wobbling Kea", "Quorble Savings & Loan", "Fennimore Mutual Thrift",
         "Grand Duchy Spoon Bank", "Thrumcap Ledger Company", "Pickled Herring Building Society",
         "Orbweaver Treasury", "Snollygoster Bank of Commerce", "First Bank of Lower Puddleby",
         "Ziggurat Credit Union", "Marmalade & Partners Private Bank", "Kumquat Cooperative",
         "Nebula Neobank", "Hobnail Trust Bank", "Bumblebrook Savings", "Gargoyle Federal",
         "Wibblesford Penny Bank", "Octopus Ink Bank", "Sasquatch Home Loans", "Krakatoa Commercial"]
TAGLINES = ["Est. 1874 - Purveyors of Fine Ledgers", "Banking, but sideways", "Your money, our hobby",
            "Proudly counting since last Tuesday", "Member of no association whatsoever",
            "We put the fun in funds", "Ledgers lovingly hand-tallied"]
HOLDERS = ["Ms Pernilla Q Featherstonehaugh", "Mr Barnaby Zwicky", "Dr Ottoline Grubb",
           "Wigglesworth Family Trust", "Ignatius Plum Ltd", "Mx Juniper Ratchet",
           "Sir Reginald Bumbleton III", "Gertrude & Horace Snodgrass", "Fizzpop Studios Ltd",
           "Captain Wilhelmina Starboard", "Ottoway Noodle Collective", "Prof Ambrose Quibble"]
STREET_ADDR = ["14 Wobble Street", "7B Kettle Road", "221 Puddle Crescent", "3 Haddock Lane",
               "88 Grommet Avenue", "Flat 2, 19 Teacup Terrace", "1 Lighthouse Lookout"]
TOWNS = ["WHAKATIKI", "PUDDLEBY", "GLOAMING BAY", "KNOTTY ASH", "OWLSWICK", "TE MANGO",
         "BRISKET HILL", "LOWER SNODWELL", "FROGMORTON", "PORT QUIBBLE"]
MERCHANTS = ["WOBBLY GOOSE CAFE", "PICKLED NEWT DELI", "GRUMBLEWORTH HARDWARE", "ZIGZAG NOODLE BAR",
             "MOTH AND LANTERN BOOKS", "SPROCKET BIKES", "THE DAPPER TURNIP", "QUOKKA KAI MART",
             "FIZZWICK FUEL", "BRAMBLE AND BOLT", "SOGGY BISCUIT BAKERY", "TOAD HALL GARAGE"]
EMPLOYERS = ["KERFUFFLE ENGINEERING", "SNORKEL LOGISTICS LTD", "PLUMTREE HEALTH BOARD",
             "OBLONG SOFTWARE", "TINDERBOX JOINERY", "GADZOOKS MEDIA"]
BILLERS = ["SQUELCH WATER CO", "ZAPPO POWER", "HOOTENANNY TELECOM", "MUMBLE MUTUAL INSURANCE",
           "GLIMMER GAS", "RUMPUS COUNCIL RATES"]
PEOPLE = ["P Q FLINTWHISTLE", "A ZOGBERT", "M T NOODLEMAN", "R VAN DER SPROCKET", "K OBRAMBLE",
          "T HUMPERDOO", "J J WIGGINS", "L MCSNOOD"]
COMPANIES = ["WIDGETRY HOLDINGS", "NIMBUS & NARWHAL LTD", "GROMMET TRUST", "BLUSTER PARTNERS",
             "CRUMPET VENTURES", "HULLABALOO LTD", "SKEDADDLE CARTAGE"]
THINGS = ["garden gnome repairs", "llama grooming", "kayak storage", "bagpipe lessons",
          "hedge sculpture", "trampoline netting", "beehive extension", "unicycle tyres",
          "goat feed", "telescope lens", "pottery kiln hire", "sheep dip"]
MEMOS = ["for the {thing} at {addr}", "re lot {n2} {thing} part {n1}", "half share of the {thing} as agreed",
         "see note about the {thing}", "instalment {n1} of 4 for the {thing}",
         "money for the {thing} and the van", "split with Bertie for the {thing}",
         "{thing} from the night market on {dayname}"]

SEM_IN = [("SALARY {emp}", 180000, 520000, 2), ("WAGES {emp}", 60000, 150000, 2),
          ("REFUND {mer}", 900, 18000, 2), ("INTEREST CREDIT", 40, 2600, 1),
          ("TRANSFER FROM {per}", 2000, 180000, 3)]
SEM_OUT = [("EFTPOS {mer} {town}", 350, 24000, 4), ("POS {mer}", 300, 16000, 2),
           ("{fee}", 100, 1500, 1), ("ATM {town}", 2000, 40000, 2),
           ("PAYMENT TO {bil}", 2500, 90000, 2), ("DIRECT DEBIT {bil}", 1500, 32000, 2)]
FEES = ["ACCOUNT FEE", "MONTHLY FEE", "OVERSEAS FEE", "DISHONOUR FEE"]
NEUTRAL = ["{com}", "{per}", "{mer} {town}", "ONL {n4} {com}", "TXN {n5}", "{bil}",
           "BATCH {n4} {com}", "{com} {town}", "{per} {n4}", "MOB {n5} {mer}"]

IN_RE = re.compile(r"\b(SALARY|WAGES|REFUND|INTEREST CREDIT|TRANSFER FROM)\b", re.I)
OUT_RE = re.compile(r"\b(EFTPOS|POS|FEE|ATM|PAYMENT TO|DIRECT DEBIT)\b", re.I)
# neutral text must not even hint
HINT_RE = re.compile(r"\b(SALARY|WAGES|REFUND|INTEREST|TRANSFER|EFTPOS|POS|FEE|ATM|PAYMENT|DEBIT|"
                     r"CREDIT|DEPOSIT|WITHDRAWAL|PURCHASE|IN|OUT)\b", re.I)


def semantic_dir(text):
    i, o = bool(IN_RE.search(text or "")), bool(OUT_RE.search(text or ""))
    return "in" if i and not o else ("out" if o and not i else None)


HEADS = {
    "date": ["When-ish", "Stamp", "Epoch", "Tick", "Blip Day", "Zeit", "Sun Count", "Daymark", "Occurred"],
    "date2": ["Echo Day", "Moon Tick", "Afterglow", "Second Stamp"],
    "text": ["Gist", "Whatsit", "Lore", "Scribble", "Saga", "Squiggle", "Yarn", "Doohickey", "Glyphs",
             "Tale Bits", "Murmur", "Babble", "Palaver", "Rigmarole", "Kerfuffle"],
    "money": ["Flux A", "Flux B", "Quanta", "Ripple", "Plink", "Clunk", "Zorch", "Wobble", "Spline",
              "Thud", "Boing", "Glint"],
    "amount": ["Swing", "Lurch", "Ebb Flow", "Wiggle"],
    "ind": ["Vibe", "Spin", "Mood", "Tilt"],
    "balance": ["Pile", "Heap", "Tide Mark", "Stockpile", "Hoard", "Kitty", "Level"],
}
MULTI = {
    "date": ["Grand Old Stamp", "When It Was", "Tick Of Clock"],
    "date2": ["Other Tick Thing", "Echo Of Stamp"],
    "text": ["Tale Of Woe", "Small Print Bits", "Odds And Sods", "Words Go Here", "Bits And Bobs"],
    "money": ["Wobbly Number One", "Wobbly Number Two", "Third Wobbly Thing"],
    "amount": ["Big Swingy Figure"], "ind": ["Which Way Up"],
    "balance": ["Heap At Rest", "Pile After That"],
}
SHORT_HEADS = ["Fip", "Zog", "Wuz", "Blip", "Kraw", "Tuft", "Moop", "Nib", "Glo", "Yex", "Snib", "Dap",
               "Vorp", "Quib"]
LAB = {
    "open": ["Treasury at dawn", "Starting heap", "Kickoff kitty", "Prelude figure", "Where we began"],
    "close": ["Treasury at dusk", "Final heap", "Parting kitty", "Coda figure", "Where we ended up"],
    "in": ["Glorp received", "Inbound wiggles", "Sum of arrivals", "Plinks total"],
    "out": ["Glorp dispatched", "Outbound wiggles", "Sum of departures", "Clunks total"],
    "bf": ["Hauled over from previous leaf", "Arrived from yonder", "Carried hither"],
    "cf": ["Shoved onto next leaf", "Onward to yonder", "Carried thither"],
    "coltot": ["Sums thereof", "Tally-ho", "Column heaps"],
}
STD_TOKENS = [("CR", "DR"), ("Cr", "Dr"), ("cr", "dr"), ("C", "D"), ("+", "-"), ("IN", "OUT")]
TEXT_SRCS = ("details", "longdetails", "party", "ref", "code", "memo", "place", "chan", "card", "seq")
MONEY_COLS = ("debit", "credit", "amount", "uamount", "balance")
KIND_TRUTH = {"date": "date", "date2": "date_value", "debit": "debit", "credit": "credit",
              "amount": "amount_signed", "uamount": "amount_unsigned", "ind": "indicator",
              "balance": "balance", "text": "text"}
HEAD_POOL = {"date": "date", "date2": "date2", "text": "text", "debit": "money", "credit": "money",
             "uamount": "money", "amount": "amount", "ind": "ind", "balance": "balance"}

# ---------------------------------------------------------------------------
# The 100 recipes. cols: printed order; text sources are any of TEXT_SRCS.
# ---------------------------------------------------------------------------

CASES = []


def C(name, **kw):
    kw["name"] = "gf%03d_%s" % (len(CASES) + 1, name)
    CASES.append(kw)


U_NONE = dict(bal="none", pool="neutral", openclose="none")

# A. headings in weird places
C("head_offset_right", cols="date details ref debit credit balance", head=dict(dx=38))
C("head_offset_left_letter", page="Letter", cols="date details party amount balance",
  head=dict(dx=-30), money=dict(neg="trail"))
C("head_centred_over_money_pair", cols="date details ref debit credit balance",
  head=dict(merge=[(3, 4)]), pool="semantic")
C("head_centred_over_date_and_text", cols="date details code amount balance",
  head=dict(merge=[(0, 1)]), money=dict(neg="paren"))
C("head_split_three_lines", cols="date details party ref debit credit balance",
  head=dict(split=True), size=8, scan=True)
C("head_below_first_row", cols="date details debit credit balance", head=dict(pos="below_first"))
C("head_first_page_only", n=95, cols="date details ref amount balance", head=dict(pages="first"),
  money=dict(neg="suf", tok=["CR", "DR"]))
C("head_later_pages_only", n=85, cols="details date debit credit balance", head=dict(pages="later"))
C("head_lines_up_with_nothing", cols="date details party debit credit balance", head=dict(free=0))
C("head_shuffled_lies", cols="date details ref debit credit balance", head=dict(shuffle=True),
  pool="semantic")
C("head_none_at_all", cols="date details amount balance", head=dict(pos="none"),
  money=dict(neg="suf", tok=["Cr", "Dr"]))
C("head_at_bottom", cols="date details code debit credit balance", head=dict(pos="bottom"), n=60)
C("head_far_above_with_advert", cols="date details debit credit balance", head=dict(pos="far"),
  promo=True)
C("head_phantom_extra_words", cols="date details uamount ind balance", head=dict(free=2),
  tokens=["C", "D"])
C("head_all_flush_right", cols="date details party ref amount balance", head=dict(align="right", dx=12))
C("head_lower_split_below_row", cols="date details memo debit credit balance",
  head=dict(pos="below_first", split=True, case="lower"))
C("head_offset_varies_per_page", n=90, cols="date details ref debit credit balance",
  head=dict(dx=20, dxpages=True), pdx=14)
C("head_two_tier_groups", cols="date details party ref debit credit balance",
  head=dict(group=[("Whodunnit", 1, 3), ("Coinage", 4, 6)]))

# B. text columns and wrapping
C("five_text_cols_landscape", page="A4L", cols="date chan details party ref memo debit credit balance",
  size=8, scan=True)
C("four_text_cols_narrow_wrap", cols="date details party code memo amount balance", narrow=8, size=7)
C("five_text_cols_letter_6pt", page="Letter", size=6,
  cols="date card details party ref memo debit credit balance")
C("text_right_of_amounts", cols="date debit credit balance details ref")
C("amounts_on_last_wrapped_line", cols="date details memo amount balance", mvalign="bottom", narrow=9)
C("columns_spread_huge_gaps", cols="date details debit credit balance", narrow=7, page="A4L")
C("text_between_out_and_in", cols="date debit details party credit balance", pool="semantic")
C("five_text_no_balance_semantic", page="A4L", cols="details party ref code place date debit credit",
  bal="none", pool="semantic", totals="after", openclose="header")

# C. dates
C("date_on_right", cols="details ref debit credit balance date")
C("date_in_middle", cols="details date party amount balance")
C("two_date_columns", cols="date date2 details debit credit balance")
C("two_dates_far_apart", cols="date details ref amount balance date2")
C("date_once_per_day", cols="date details debit credit balance", dgroup=True, n=40, scan=True)
C("date_once_per_day_no_reprint", cols="date details ref amount balance", dgroup=True, dreprint=False,
  n=110, size=8)
C("date_day_only", dfmt="dd", cols="date details debit credit balance")
C("date_iso_right", dfmt="yyyy-mm-dd", cols="details party amount balance date")
C("date_us_mmdd_letter", dfmt="mm/dd/yyyy", page="Letter", money=dict(cur="$"))
C("date_dotted_euro", dfmt="dd.mm.yyyy", money=dict(dec=",", thou="."))
C("date_compact_yyyymmdd", dfmt="yyyymmdd", cols="date details ref uamount ind balance",
  tokens=["CR", "DR"])
C("date_long_weekday", dfmt="Dow, d Month", cols="date details debit credit balance", page="A4L")
C("date_month_name_first", dfmt="Mon d, yyyy", cols="details date debit credit balance")
C("date_grouped_on_right", dfmt="dd MON yy", dgroup=True, cols="details ref debit credit balance date",
  n=36)

# D. money conventions
C("money_leading_minus", cols="date details amount balance", money=dict(neg="lead"))
C("money_trailing_minus", cols="date details ref amount balance", money=dict(neg="trail"))
C("money_brackets", cols="date details party amount balance", money=dict(neg="paren"))
C("money_crdr_suffix", cols="date details ref amount balance", money=dict(neg="suf", tok=["CR", "DR"]))
C("money_crdr_suffix_lower_attached", cols="date details amount balance",
  money=dict(neg="suf", tok=["cr", "dr"], sep=""))
C("money_crdr_prefix", cols="date details party amount balance", money=dict(neg="pre", tok=["CR", "DR"]))
C("money_dr_suffix_only", cols="date details amount balance", money=dict(neg="suf", tok=["", "DR"]))
C("money_explicit_plus_minus", cols="date details ref amount balance", money=dict(neg="plus"))
C("indicator_crdr_column", cols="date details uamount ind balance", tokens=["CR", "DR"])
C("indicator_cd_left_of_amount", cols="date details ind uamount balance", tokens=["C", "D"])
C("indicator_plus_minus_column", cols="date details uamount ind", tokens=["+", "-"], bal="none")
C("indicator_in_out_far_left", cols="ind date details ref uamount balance", tokens=["IN", "OUT"])
C("indicator_madeup_legend_top", cols="date details uamount ind", tokens=["K", "Z"], legend="top",
  bal="none", pool="neutral", openclose="none", scan=True)
C("indicator_madeup_legend_bottom", cols="date details party ind uamount", tokens=["<<", ">>"],
  legend="bottom", bal="none", pool="neutral", openclose="none")
C("indicator_madeup_hash_star_dayend", cols="date details uamount ind balance", tokens=["#", "*"],
  legend="top", bal="dayend")
C("separate_in_then_out", cols="date details ref credit debit balance")
C("separate_out_signed_no_balance", cols="date details debit credit", deb_signed=True, bal="none",
  pool="neutral", openclose="none")
C("decimal_comma_dot_thousands", cols="date details debit credit balance", money=dict(dec=",", thou="."),
  huge=True, scan=True)
C("decimal_comma_space_thousands", cols="date details ref amount balance",
  money=dict(dec=",", thou=" ", neg="trail"))
C("apostrophe_thousands_lower_prefix", cols="date details amount balance",
  money=dict(thou="'", neg="pre", tok=["cr", "dr"]), huge=True)
C("no_thousands_separator", cols="date details debit credit balance", money=dict(thou=""), huge=True)
C("dollar_prefix", cols="date details party amount balance", money=dict(cur="$"))
C("nz_dollar_prefix_brackets", cols="date details amount balance", money=dict(cur="NZ$", neg="paren"))
C("nzd_prefix_dr_suffix", cols="date details ref amount balance",
  money=dict(cur="NZD ", neg="suf", tok=["", "DR"]), page="A4L")
C("space_thousands_dot_decimal", cols="date details debit credit balance", money=dict(thou=" "),
  huge=True)
C("zero_and_huge_amounts", cols="date details ref debit credit balance", zero=True, huge=True)
C("dollar_brackets_overdrawn", cols="date details amount balance", money=dict(cur="$", neg="paren"),
  overdrawn=True)

# E. balances, opening/closing, totals
C("balance_left_of_amounts", cols="date details balance debit credit")
C("balance_day_end_only", cols="date details amount balance", bal="dayend")
C("no_balance_semantic_only", cols="date details ref debit credit", bal="none", pool="semantic",
  openclose="none")
C("newest_first", cols="date details ref debit credit balance", newest=True, scan=True)
C("newest_first_day_end", cols="date details debit credit balance", newest=True, bal="dayend",
  pool="semantic")
C("overdrawn_od_suffix", cols="date details debit credit balance", overdrawn=True,
  bmoney=dict(neg="suf", tok=["", "OD"]))
C("overdrawn_trailing_minus", cols="date details amount balance", overdrawn=True,
  bmoney=dict(neg="trail"))
C("overdrawn_brackets_balance", cols="date details debit credit balance", overdrawn=True,
  bmoney=dict(neg="paren"))
C("balance_first_column", cols="balance date details debit credit")
C("open_close_inside_table", cols="date details ref debit credit balance", openclose="table_dated")
C("bf_cf_lines_multi_page", cols="date details debit credit balance", bf=True, n=100, size=9)
C("totals_box_after_no_balance", cols="date details ref debit credit", totals="after", tot_oc=True,
  bal="none", pool="neutral", openclose="none")
C("totals_box_top_right", cols="date details party amount balance", totals="top", tot_oc=True)
C("column_totals_with_open_close", cols="date details ref debit credit", totals="coltot",
  openclose="table", bal="none", pool="neutral")

# F. page geometry and type size
C("letter_landscape", page="LetterL", cols="date chan details party debit credit balance")
C("page_offsets", cols="date details ref debit credit balance", pdx=22, pdy=30, n=90)
C("row_drift", cols="date details amount balance", drift=0.35, n=34)
C("tiny_6pt_dense_newest", cols="date details party ref amount balance", size=6, n=150, newest=True)
C("big_11pt_long_text", cols="date longdetails amount balance", size=11, n=30)
C("money_left_aligned", cols="date details debit credit balance", malign="left")
C("money_centred", cols="date details ref amount balance", malign="center")

# G. redactions
C("redact_removed_mix", cols="date details party debit credit balance",
  redact=[("removed", "amount", 2), ("removed", "balance", 3), ("removed", "text", 3)], acct_red="removed")
C("redact_overlay_mix", cols="date details ref amount balance",
  redact=[("overlay", "amount", 3), ("overlay", "text", 4), ("overlay", "balance", 2)], acct_red="overlay",
  scan=True)

# H. deliberately undecidable
C("undecidable_two_plain_columns", cols="date details ref debit credit", scan=True,
  undecidable="two unsigned money columns, no balance, no opening/closing, no totals and neutral "
              "descriptions: nothing on the page says which column is money in", **U_NONE)
C("undecidable_two_columns_decimal_comma", page="A4L", cols="details date party credit debit",
  money=dict(dec=",", thou=" "),
  undecidable="two unsigned money columns (decimal comma) with neutral text and no balance or "
              "totals: the in/out columns cannot be told apart", **U_NONE)
C("undecidable_single_unsigned", cols="date details party uamount",
  undecidable="one unsigned amount column, no indicator, no balance, neutral descriptions: the "
              "direction of every row is unknowable", **U_NONE)
C("undecidable_madeup_tokens_no_legend", cols="date details uamount ind", tokens=["Q", "W"],
  undecidable="indicator tokens Q / W are made up and no legend, balance or totals explain them",
  **U_NONE)
C("undecidable_balances_blacked_out", cols="date details debit credit balance", pool="neutral",
  openclose="none", redact=[("removed", "balance", "all")],
  undecidable="the balance column exists but every balance is removed under a black box; two "
              "unsigned columns and neutral text remain")
C("undecidable_column_totals_no_ends", cols="date details ref debit credit", totals="coltot",
  undecidable="column totals are printed but no opening or closing balance, so they only restate "
              "the columns; neutral text, no balance", **U_NONE)
C("undecidable_card_side_minus", cols="date details ref amount", card_side=True,
  undecidable="minus signs follow the card-side convention (minus = payment received) but nothing "
              "on the page says so; no balance, totals or descriptive words", **U_NONE)
C("undecidable_legend_redacted", cols="date details uamount ind", tokens=["~", "="], legend="removed",
  undecidable="the legend that would explain the made-up ~ / = tokens is removed under a black box",
  **U_NONE)
C("undecidable_closing_only", cols="date details uamount", bal="none", pool="neutral",
  openclose="closing_header",
  undecidable="only a closing balance is printed (no opening), amounts unsigned, neutral text")
C("undecidable_colour_only", cols="date details party uamount", color_only=True,
  undecidable="money out is printed in red ink and nothing else marks it; colour is not in the "
              "text layer and is not an accepted cue", **U_NONE)

# ---------------------------------------------------------------------------
# Recipe resolution and data
# ---------------------------------------------------------------------------

DEFAULTS = dict(cols="date details ref debit credit balance", page=None, font=None, size=None,
                dfmt=None, dgroup=False, dreprint=True, money=None, bmoney=None, deb_signed=False,
                tokens=None, legend=None, bal="every", newest=False, overdrawn=False, openclose=None,
                bf=False, totals=None, tot_oc=False, pool="mixed", n=None, months=1, head=None,
                pdx=0.0, pdy=0.0, drift=0.0, malign="right", mvalign="top", redact=(), acct_red=None,
                scan=False, huge=False, zero=False, undecidable=None, card_side=False,
                color_only=False, promo=None, narrow=None, note=None)
HEAD_DEFAULT = dict(pos="above", pages="all", dx=0.0, dxpages=False, split=False, merge=(), group=(),
                    free=None, shuffle=False, align="auto", case="title")


def resolve(spec):
    S = dict(DEFAULTS)
    S.update(spec)
    rng = random.Random(zlib.crc32(spec["name"].encode()))
    S["rng"] = rng
    S["page"] = S["page"] or rng.choice(["A4", "A4", "A4", "Letter"])
    S["font"] = S["font"] or rng.choice(["Helvetica", "Helvetica", "Times", "Courier"])
    S["size"] = S["size"] or rng.choice([7, 7.5, 8, 8, 8.5, 9, 10])
    S["dfmt"] = S["dfmt"] or rng.choice(COMMON_DATES)
    M = dict(M_DEFAULT)
    M.update(S["money"] or {})
    if M["dec"] == ",":
        if M["thou"] == ",":
            M["thou"] = "."
    MB = dict(M)
    if MB["neg"] == "none":
        MB["neg"] = "lead"
    MB.update(S["bmoney"] or {})
    S["M"], S["MB"] = M, MB
    H = dict(HEAD_DEFAULT)
    H.update(S["head"] or {})
    S["H"] = H
    if S["openclose"] is None:
        S["openclose"] = rng.choice(["header", "header", "table", "none"])
    if S["promo"] is None:
        S["promo"] = rng.random() < 0.3
    S["toff"] = rng.choice([0, 0, 0, 18, 46, 90])
    S["bankpos"] = rng.choice(["tl", "tr", "c", "bottom"])
    S["cont"] = rng.choice(["left", "right", "none"])
    S["rules"] = rng.choice(["none", "none", "rows", "zebra", "headline"])
    S["bank"] = rng.choice(BANKS)
    S["holder"] = rng.choice(HOLDERS)
    S["cols"] = S["cols"].split()
    for c in S["cols"]:
        if c not in TEXT_SRCS and c not in ("date", "date2", "ind") + MONEY_COLS:
            raise GenError("%s: unknown column %r" % (S["name"], c))
    return S


def fill(rng, t):
    return t.format(emp=rng.choice(EMPLOYERS), mer=rng.choice(MERCHANTS), town=rng.choice(TOWNS),
                    bil=rng.choice(BILLERS), per=rng.choice(PEOPLE), com=rng.choice(COMPANIES),
                    fee=rng.choice(FEES), n4="%04d" % rng.randint(1, 9999),
                    n5="%05d" % rng.randint(1, 99999), thing=rng.choice(THINGS),
                    addr=rng.choice(STREET_ADDR), n1=rng.randint(1, 4), n2=rng.randint(2, 40),
                    dayname=rng.choice(DAYNAME))


def phrase(rng, di, sem):
    if sem:
        pool = SEM_IN if di == "in" else SEM_OUT
        t, lo, hi, _ = rng.choices(pool, weights=[p[3] for p in pool])[0]
        c = rng.randint(lo, hi)
        if t.startswith("ATM"):
            c = c // 2000 * 2000
        return fill(rng, t), c
    t = rng.choice(NEUTRAL)
    c = rng.randint(1500, 260000) if di == "in" else rng.randint(300, 60000)
    return fill(rng, t), c


def side_text(rng, src):
    if src == "party":
        return rng.choice(PEOPLE + COMPANIES)
    if src == "ref":
        return rng.choice(["REF %05d" % rng.randint(1, 99999), "INV-%04d" % rng.randint(1, 9999),
                           "#%06d" % rng.randint(1, 999999), "%04d/%02d" % (rng.randint(1, 9999),
                                                                           rng.randint(1, 99)),
                           "PO %d%s" % (rng.randint(10, 999), rng.choice("ABCXYZ")), "", ""])
    if src == "code":
        return rng.choice(["QX%d" % rng.randint(1, 99), "BR-%04d" % rng.randint(1, 9999),
                           "Z%02d" % rng.randint(1, 99), "KP", "MZ-%d" % rng.randint(1, 9), ""])
    if src == "memo":
        return fill(rng, rng.choice(MEMOS))
    if src == "place":
        return rng.choice(TOWNS) + rng.choice([" NZ", "", " NI", " SI"])
    if src == "chan":
        return rng.choice(["ONLINE", "BRANCH", "APP", "PHONE", "KIOSK", "CARRIER PIGEON", "TELEGRAM",
                           "COUNTER 3", "SMOKE SIGNAL"])
    if src == "card":
        return rng.choice(["CARD %04d", "****%04d", "C/%04d"]) % rng.randint(1, 9999)
    if src == "seq":
        return rng.choice(["SEQ %06d", "#%05d", "N%04d"]) % rng.randint(1, 9999)
    raise GenError("no side text for %r" % src)


def gen_data(S):
    rng = S["rng"]
    y = rng.choice([2025, 2026])
    m = rng.randint(1, 12) if y == 2025 else rng.randint(1, 8)
    start = dt.date(y, m, 1)
    n = S["n"] or rng.randint(14, 30)
    if S["dfmt"] != "dd":
        S["months"] = max(S["months"], -(-n // 40))
    end = ML.add_months(start, S["months"]) - dt.timedelta(days=1)
    days = list(ML.daterange(start, end))
    pool_days = sorted(rng.sample(days, max(5, n // 3))) if S["dgroup"] else days
    dates = sorted(rng.choice(pool_days) for _ in range(n))
    if not any(d.day > 12 for d in dates):
        dates[-1] = days[-1]
        dates.sort()
    n_in = max(2, int(round(n * rng.uniform(0.22, 0.36))))
    dirs = ["in"] * n_in + ["out"] * (n - n_in)
    rng.shuffle(dirs)
    if S["overdrawn"]:      # the big bill comes early, the pay comes later
        k = max(1, n // 5)
        dirs[k] = "out"
    rows = []
    for d, di in zip(dates, dirs):
        sem = S["pool"] == "semantic" or (S["pool"] == "mixed" and rng.random() < 0.5)
        main, c = phrase(rng, di, sem)
        rows.append(dict(date=d, dir=di, c=c, main=main, sem=sem))
    if S["zero"]:
        k = next(i for i, r in enumerate(rows) if r["dir"] == "out" and i > 2)
        rows[k].update(c=0, main="MONTHLY FEE WAIVED" if rows[k]["sem"] else "ADJ %04d NIL" % rng.randint(1, 9999))
    if S["huge"]:
        ins = [i for i, r in enumerate(rows) if r["dir"] == "in" and i < n - 3]
        i = ins[0] if ins else 1
        rows[i].update(dir="in", c=rng.randint(100000000, 480000000))
        rows[i]["main"] = (fill(rng, "TRANSFER FROM {com}") if rows[i]["sem"]
                           else fill(rng, "{com} SETTLEMENT A"))
        j = next((k for k in range(i + 1, n) if rows[k]["dir"] == "out"), n - 1)
        rows[j].update(dir="out", c=rng.randint(rows[i]["c"] // 3, rows[i]["c"] * 9 // 10))
        rows[j]["main"] = (fill(rng, "PAYMENT TO {com}") if rows[j]["sem"]
                           else fill(rng, "{com} SETTLEMENT B"))
    if S["overdrawn"]:
        k = max(1, n // 5)
        rows[k]["c"] = rng.randint(180000, 420000)
        rows[k]["main"] = (fill(rng, "PAYMENT TO {bil}") if rows[k]["sem"] else fill(rng, "{com}"))
    for r in rows:
        if not r["sem"] and HINT_RE.search(r["main"]):
            raise GenError("neutral text %r hints at a direction" % r["main"])
        if r["sem"] and semantic_dir(r["main"]) != r["dir"]:
            raise GenError("semantic text %r does not say %s" % (r["main"], r["dir"]))
    # opening balance
    if S["overdrawn"]:
        opening = rng.randint(2000, 60000)
        k = max(1, n // 5)
        pre = sum(r["c"] if r["dir"] == "in" else -r["c"] for r in rows[:k])
        rows[k]["c"] = max(rows[k]["c"], opening + pre + rng.randint(20000, 150000))
    run, lo = 0, 0
    for r in rows:
        run += r["c"] if r["dir"] == "in" else -r["c"]
        lo = min(lo, run)
    if S["overdrawn"]:
        if opening + lo >= -5000:
            raise GenError("overdrawn recipe never goes overdrawn")
    else:
        opening = rng.randint(5000, 900000) + (-lo if lo < 0 else 0)
    bal = opening
    for r in rows:
        bal += r["c"] if r["dir"] == "in" else -r["c"]
        r["bal"] = bal
        r["sa"] = r["c"] if r["dir"] == "in" else -r["c"]
        r["date2"] = r["date"] + dt.timedelta(days=rng.choice([0, 0, 1, 1, 2, 3]))
    # text columns
    texts = [c for c in S["cols"] if c in TEXT_SRCS]
    main_src = "details" if "details" in texts else ("longdetails" if "longdetails" in texts else texts[0])
    for r in rows:
        r["texts"] = {}
        for src in texts:
            if src == main_src:
                v = r["main"]
                if src == "longdetails":
                    v = v + " " + fill(rng, rng.choice(MEMOS))
            else:
                v = side_text(rng, src)
            if src != main_src or src == "longdetails":
                extra = v if src != "longdetails" else v[len(r["main"]):]
                if HINT_RE.search(extra):
                    raise GenError("side text %r hints at a direction" % extra)
            r["texts"][src] = v
    # day-end flag (chronological last of each date)
    for i, r in enumerate(rows):
        r["dayend"] = i == len(rows) - 1 or rows[i + 1]["date"] != r["date"]
    return dict(start=start, end=end, rows=rows, opening=opening, closing=bal, main_src=main_src)


# ---------------------------------------------------------------------------
# Layout
# ---------------------------------------------------------------------------

class Col:
    def __init__(self, i, kind, src=None):
        self.i, self.kind, self.src = i, kind, src
        self.x = self.w = 0.0
        self.align = "left"
        self.head = None


def wrap(text, width, font, size):
    lines, cur = [], ""
    for w in text.split():
        t = w if not cur else cur + " " + w
        if not cur or stringWidth(t, font, size) <= width:
            cur = t
        else:
            lines.append(cur)
            cur = w
    if cur:
        lines.append(cur)
    return lines


def printed_strings(S, D):
    """The exact strings each row prints, per column kind (before date grouping)."""
    M, MB = S["M"], S["MB"]
    kinds = set(S["cols"])
    tok = S["tokens"]
    for r in D["rows"]:
        P = {}
        P["date"] = DATES[S["dfmt"]](r["date"])
        P["date2"] = DATES[S["dfmt"]](r["date2"]) if "date2" in kinds else None
        out = r["dir"] == "out"
        if "debit" in kinds:
            if out:
                P["debit"] = (fmt_money(-r["c"], M) if S["deb_signed"] else fmt_money(r["c"], M, unsigned=True))
                P["credit"] = None
            else:
                P["debit"], P["credit"] = None, fmt_money(r["c"], M, unsigned=True)
        if "amount" in kinds:
            v = -r["sa"] if S["card_side"] else r["sa"]
            P["amount"] = fmt_money(v, M, zero_neg=(out != S["card_side"]))
        if "uamount" in kinds:
            P["uamount"] = fmt_money(r["c"], M, unsigned=True)
        if "ind" in kinds:
            P["ind"] = tok[1] if out else tok[0]
        if "balance" in kinds:
            show = S["bal"] == "every" or (S["bal"] == "dayend" and r["dayend"])
            P["balance"] = fmt_money(r["bal"], S["MB"]) if show else None
        r["P"] = P


def layout(S, D):
    rng = S["rng"]
    fam = ML.FONTS[S["font"]]
    font, bold = fam
    size = float(S["size"])
    PW, PH = PAGES[S["page"]]
    cols = []
    for i, k in enumerate(S["cols"]):
        cols.append(Col(i, "text", k) if k in TEXT_SRCS else Col(i, k))
    # headings (made up)
    H = S["H"]
    used = set()
    for c in cols:
        pool = (MULTI if H["split"] else HEADS)[HEAD_POOL[c.kind]]
        cand = [h for h in pool if h not in used] or pool
        c.head = rng.choice(cand)
        used.add(c.head)
        if H["case"] == "upper":
            c.head = c.head.upper()
        elif H["case"] == "lower":
            c.head = c.head.lower()
    hsize = size
    gap = max(7.0, 1.1 * size)
    rows = D["rows"]
    # extra strings that land in money columns
    val_col = next((c for c in cols if c.kind == "balance"), None) or \
        [c for c in cols if c.kind in MONEY_COLS][-1]
    extra = {val_col.i: [fmt_money(D["opening"], S["MB"]), fmt_money(D["closing"], S["MB"])]}
    if S["totals"] == "coltot":
        tin = sum(r["c"] for r in rows if r["dir"] == "in")
        tout = sum(r["c"] for r in rows if r["dir"] == "out")
        for c in cols:
            if c.kind == "debit":
                extra.setdefault(c.i, []).append(fmt_money(tout, S["M"], unsigned=True))
            if c.kind == "credit":
                extra.setdefault(c.i, []).append(fmt_money(tin, S["M"], unsigned=True))
    # natural widths
    fixed, mins, weights = 0.0, {}, {}
    for c in cols:
        if c.kind == "text":
            words = [w for r in rows for w in r["texts"][c.src].split()]
            mins[c.i] = max(stringWidth(w, font, size) for w in words) + 3
            weights[c.i] = {"details": 3.0, "longdetails": 5.0, "memo": 3.2, "party": 2.0,
                            "place": 1.5}.get(c.src, 1.1)
            c.natural = max(stringWidth(r["texts"][c.src], font, size) for r in rows) + 4
            c.align = "left"
            continue
        if c.kind in ("date", "date2"):
            ss = [r["P"][c.kind] for r in rows]
            c.align = S.get("dalign", "left")
        elif c.kind == "ind":
            ss = list(S["tokens"])
            c.align = "left"
        else:
            ss = [r["P"].get(c.kind) or "" for r in rows] + extra.get(c.i, [])
            c.align = S["malign"]
        c.w = max(stringWidth(s, font, size) for s in ss) + 4
        hw = max(stringWidth(w, bold, hsize) for w in c.head.split()) if (H["split"]) else \
            stringWidth(c.head, bold, hsize)
        if H["free"] is None and not H["shuffle"] and not any(a <= c.i <= b for a, b in H["merge"]):
            c.w = max(c.w, hw + 2)
        fixed += c.w
    for c in cols:
        if c.kind == "text":
            hw = max(stringWidth(w, bold, hsize) for w in c.head.split()) if H["split"] else \
                stringWidth(c.head, bold, hsize)
            if H["free"] is None and not H["shuffle"]:
                mins[c.i] = max(mins[c.i], hw + 2)
    room = S["pdx"] + 2
    ml = rng.uniform(30, 48) + room
    mr = rng.uniform(30, 48) + room + S["drift"] * 60
    CW = PW - ml - mr
    avail = CW - fixed - gap * (len(cols) - 1)
    tcols = [c for c in cols if c.kind == "text"]
    if sum(mins.values()) > avail:
        raise GenError("%s: text columns do not fit (%.0f > %.0f)" % (S["name"], sum(mins.values()), avail))
    left = dict(weights)
    width = {}
    while left:     # proportional share, honouring minimums
        share = avail - sum(width.values())
        tw = sum(left.values())
        lows = [i for i in left if share * left[i] / tw < mins[i]]
        if not lows:
            for i in left:
                width[i] = share * left[i] / tw
            break
        for i in lows:
            width[i] = mins[i]
            del left[i]
    spare = 0.0
    for c in tcols:
        cap = c.natural + 6
        if S["narrow"]:
            cap = min(cap, S["narrow"] * size * 1.0)
        w = max(mins[c.i], min(width[c.i], cap))
        spare += width[c.i] - w
        c.w = w
    gaps = gap + (spare / (len(cols) - 1) if len(cols) > 1 else 0)
    x = ml
    for c in cols:
        c.x = x
        x += c.w + gaps
    lead = size * 1.22
    rowgap = size * 0.32
    for r in rows:
        r["lines"] = {}
        for c in tcols:
            r["lines"][c.i] = wrap(r["texts"][c.src], c.w, font, size)
        nl = max([1] + [len(v) for v in r["lines"].values()])
        r["nl"] = nl
        r["h"] = nl * lead + rowgap
    L = dict(cols=cols, font=font, bold=bold, size=size, hsize=hsize, PW=PW, PH=PH, ml=ml, mr=mr,
             CW=CW, lead=lead, rowgap=rowgap, val_col=val_col, gap=gaps)
    L["heads"] = head_entries(S, L)
    return L


def head_entries(S, L):
    """Heading tiers: list of tiers, each a list of (lines, anchor_x, align, col_indexes)."""
    H, cols = S["H"], L["cols"]
    texts = [c.head for c in cols]
    rng = S["rng"]
    tiers = []
    if H["group"]:
        tier = []
        for t, i, j in H["group"]:
            a, b = cols[i], cols[j]
            tier.append(([t], (a.x + b.x + b.w) / 2.0, "center", ()))
        tiers.append(tier)
    tier = []
    if H["free"] is not None:
        m = len(cols) + H["free"]
        words = rng.sample(SHORT_HEADS, m)
        x0, x1 = L["ml"], L["ml"] + L["CW"]
        for k in range(m):
            al = "left" if k == 0 else ("right" if k == m - 1 else "center")
            tier.append(([words[k]], x0 + k * (x1 - x0) / (m - 1), al, (k,) if k < len(cols) else ()))
        for k, c in enumerate(cols):
            c.head = words[k]
        L["phantom"] = words[len(cols):]
    else:
        if H["shuffle"]:
            perm = list(range(len(cols)))
            while any(p == q for p, q in zip(perm, range(len(cols)))):
                rng.shuffle(perm)
            texts = [texts[p] for p in perm]
            for c, t in zip(cols, texts):
                c.head = t
        merged = set()
        for i, j in H["merge"]:
            t = rng.choice(["Flux Zone", "The Numbers", "Who And When", "Stuff Happened", "Gubbins"])
            if H["case"] == "upper":
                t = t.upper()
            a, b = cols[i], cols[j]
            tier.append(([t], (a.x + b.x + b.w) / 2.0, "center", tuple(range(i, j + 1))))
            for k in range(i, j + 1):
                merged.add(k)
                cols[k].head = t
        for c in cols:
            if c.i in merged:
                continue
            t = c.head
            lines = t.split() if H["split"] else [t]
            al = H["align"]
            if al == "auto":
                al = "left" if c.kind in ("text", "date", "date2", "ind") else \
                    {"right": "right", "left": "left", "center": "center"}[S["malign"]]
            ax = c.x if al == "left" else (c.x + c.w if al == "right" else c.x + c.w / 2.0)
            tier.append((lines, ax, al, (c.i,)))
        L["phantom"] = []
    tiers.append(tier)
    nl = sum(max(len(e[0]) for e in t) for t in tiers)
    L["head_h"] = nl * L["hsize"] * 1.18 + 5
    return tiers


# ---------------------------------------------------------------------------
# Pagination and drawing
# ---------------------------------------------------------------------------

def heads_on(S, p, npages):
    H = S["H"]
    if H["pos"] == "none":
        return False
    if H["pages"] == "first":
        return p == 0
    if H["pages"] == "later":
        return p > 0
    return True


def header_block_height(S, L):
    h = 120
    if S["openclose"] in ("header", "closing_header"):
        h += 20
    if S["legend"] in ("top",):
        h += 12
    if S["totals"] == "top":
        h += 62
    if S["promo"]:
        h += 26
    if S["H"]["pos"] == "far":
        h += 0
    return h + S["toff"]


def build_items(S, L, D):
    rows = D["rows"]
    printed = list(reversed(rows)) if S["newest"] else list(rows)
    line_h = L["lead"] + L["rowgap"]
    items = []
    oc_table = S["openclose"] in ("table", "table_dated")
    first, last = ("close", "open") if S["newest"] else ("open", "close")
    if oc_table:
        items.append(dict(kind=first, h=line_h))
    for r in printed:
        items.append(dict(kind="row", r=r, h=r["h"]))
    if oc_table:
        items.append(dict(kind=last, h=line_h))
    if S["totals"] == "coltot":
        items.append(dict(kind="coltot", h=line_h * 1.5))
    if S["totals"] == "after":
        items.append(dict(kind="totbox", h=L["lead"] * 1.35 * 4 + 16))
    if S["legend"] in ("bottom", "removed"):
        items.append(dict(kind="legend", h=line_h * 1.8))
    return printed, items


def paginate(S, L, items):
    PH = L["PH"]
    bottom = PH - 40 - (14 if S["bankpos"] == "bottom" else 0)
    line_h = L["lead"] + L["rowgap"]
    top1 = header_block_height(S, L) + (40 if S["H"]["pos"] == "far" else 0)
    rng = random.Random(zlib.crc32((S["name"] + ":geo").encode()))
    pages, cur, p = [], [], 0
    y = None

    def cap(p):
        t = top1 if p == 0 else 56.0
        t += abs(S["pdy"])
        a = bottom - t
        if S["H"]["pos"] != "none":
            a -= L["head_h"]
        if S["bf"]:
            a -= 2 * line_h
        return a

    used = 0.0
    for it in items:
        if cur and used + it["h"] > cap(p):
            pages.append(cur)
            cur, used, p = [], 0.0, p + 1
        cur.append(it)
        used += it["h"]
    pages.append(cur)
    n = len(pages)
    pdx = [round(rng.uniform(-S["pdx"], S["pdx"]), 1) if S["pdx"] else 0.0 for _ in range(n)]
    pdy = [round(rng.uniform(0, S["pdy"]), 1) if S["pdy"] else 0.0 for _ in range(n)]
    hdx = [S["H"]["dx"] * (rng.uniform(-1.2, 1.2) if S["H"]["dxpages"] and k else 1.0) for k in range(n)]
    return pages, dict(top1=top1, pdx=pdx, pdy=pdy, hdx=hdx, bottom=bottom)


class Draw:
    def __init__(self, S, L, D, path):
        self.S, self.L, self.D = S, L, D
        self.sh = Sheet(path, PAGES[S["page"]], S["font"])
        self.size = L["size"]
        self.acct = "%02d-%04d-%07d-%02d" % (S["rng"].randint(90, 99), S["rng"].randint(1, 9999),
                                             S["rng"].randint(1, 9999999), S["rng"].randint(0, 99))

    # -- primitives
    def cell(self, x, y, s, align, mode=None, color=None):
        sh = self.sh
        if not s:
            return None
        if mode == "removed":
            x0, x1 = sh.place(x, s, self.size, False, align)
            sh.blackout(x0, x1, y, self.size)
            return (x0, x1)
        x0, x1 = sh.text(x, y, s, self.size, align=align, color=color)
        if mode == "overlay":
            sh.blackout(x0, x1, y, self.size)
        return (x0, x1)

    def anchor(self, c, dx=0.0):
        return (c.x + dx if c.align == "left" else
                (c.x + c.w + dx if c.align == "right" else c.x + c.w / 2.0 + dx))

    # -- page furniture
    def header(self, pno, npages):
        S, L, sh = self.S, self.L, self.sh
        PW, ml, mr = L["PW"], L["ml"] - S["pdx"], L["mr"] - S["pdx"] - S["drift"] * 60
        bank = S["bank"]
        rng = random.Random(zlib.crc32((S["name"] + ":hdr").encode()))
        if pno == 0:
            bs = 15
            if S["bankpos"] == "tl":
                sh.text(ml, 40, bank, bs, bold=True)
                sh.text(ml, 54, rng.choice(TAGLINES), 7)
            elif S["bankpos"] == "tr":
                sh.text(PW - mr, 40, bank, bs, bold=True, align="right")
                sh.text(PW - mr, 54, rng.choice(TAGLINES), 7, align="right")
            elif S["bankpos"] == "c":
                sh.text(PW / 2, 40, bank, bs, bold=True, align="center")
                sh.text(PW / 2, 54, rng.choice(TAGLINES), 7, align="center")
            else:
                sh.text(ml, 40, "STATEMENT OF GOINGS-ON", 13, bold=True)
            y = 78
            hl = [S["holder"], rng.choice(STREET_ADDR), rng.choice(TOWNS).title() + " %04d" % rng.randint(1000, 9999)]
            for k, t in enumerate(hl):
                sh.text(ml, y + 11 * k, t, 8, bold=(k == 0))
            p1, p2 = ML.period_text("long" if S["dfmt"] == "mm/dd/yyyy" else rng.choice(["long", "short", "upper"]),
                                    self.D["start"], self.D["end"])
            right = [("Account number: ", self.acct), ("Statement period: %s to %s" % (p1, p2), None),
                     ("Statement no. %04d" % rng.randint(1, 999), None)]
            if S["openclose"] == "header":
                right.append(("%s: %s" % (rng.choice(LAB["open"]), fmt_money(self.D["opening"], S["MB"])), None))
            if S["openclose"] in ("header", "closing_header"):
                right.append(("%s: %s" % (rng.choice(LAB["close"]), fmt_money(self.D["closing"], S["MB"])), None))
            for k, (t, v) in enumerate(right):
                yy = y + 11 * k
                if v is None:
                    sh.text(PW - mr, yy, t, 8, align="right")
                    continue
                vw = sh.width(v, 8)
                if S["acct_red"] == "removed":
                    x1 = PW - mr
                    sh.blackout(x1 - vw, x1, yy, 8)
                    sh.text(x1 - vw - 4, yy, t.rstrip(), 8, align="right")
                else:
                    sh.text(PW - mr, yy, v, 8, align="right")
                    sh.text(PW - mr - vw - 4, yy, t.rstrip(), 8, align="right")
                    if S["acct_red"] == "overlay":
                        sh.blackout(PW - mr - vw, PW - mr, yy, 8)
            y += 11 * max(len(hl), len(right)) + 8
            if S["legend"] == "top":
                sh.text(ml, y, self.legend_text(), 7.5)
                y += 12
            if S["totals"] == "top":
                y = self.totbox(PW - mr - 210, y, 210) + 6
            if S["promo"]:
                sh.text(ml, y, "NEW! Earn 4.25% p.a. on hoards over $5,000 with a Kitty-Plus saver.", 7.5)
                sh.text(ml, y + 10, "Call 0800 555 019 or visit any of our 3 branches before 31/12.", 7.5)
                y += 26
            return y
        # continuation pages
        t = "%s - account %s - continued" % (bank, self.acct[:-6] + "XXXX-XX")
        if S["cont"] == "left":
            sh.text(ml, 30, t, 7)
        elif S["cont"] == "right":
            sh.text(PW - mr, 30, t, 7, align="right")
        return None

    def footer(self, pno, npages):
        S, L, sh = self.S, self.L, self.sh
        PW, PH = L["PW"], L["PH"]
        sh.text(PW / 2, PH - 14, FOOTER, 6.5, align="center")
        lab = ["Leaf %d of %d", "p.%d/%d", "Page %d (of %d)"][zlib.crc32(S["name"].encode()) % 3]
        sh.text(PW - 30, PH - 14, lab % (pno + 1, npages), 6.5, align="right")
        if S["bankpos"] == "bottom":
            sh.text(PW / 2, PH - 30, S["bank"], 9, bold=True, align="center")

    def legend_text(self):
        a, b = self.S["tokens"]
        return "Key to the squiggles:  %s = money received into this account;  %s = money paid out" % (a, b)

    def totbox(self, x, y, w):
        S, sh, D = self.S, self.sh, self.D
        rng = random.Random(zlib.crc32((S["name"] + ":tot").encode()))
        tin = sum(r["c"] for r in D["rows"] if r["dir"] == "in")
        tout = sum(r["c"] for r in D["rows"] if r["dir"] == "out")
        lines = []
        if S["tot_oc"]:
            lines.append((rng.choice(LAB["open"]), fmt_money(D["opening"], S["MB"])))
        lines.append((rng.choice(LAB["in"]), fmt_money(tin, S["M"], unsigned=True)))
        lines.append((rng.choice(LAB["out"]), fmt_money(tout, S["M"], unsigned=True)))
        if S["tot_oc"]:
            lines.append((rng.choice(LAB["close"]), fmt_money(D["closing"], S["MB"])))
        sz = 8
        h = len(lines) * sz * 1.35 + 8
        sh.rect(x, y, w, h, stroke=(0.3, 0.3, 0.3))
        for k, (a, b) in enumerate(lines):
            yy = y + 4 + sz + k * sz * 1.35
            sh.text(x + 5, yy, a, sz)
            sh.text(x + w - 5, yy, b, sz, align="right")
        self.tot_printed = lines
        return y + h

    # -- table pieces
    def draw_heads(self, y, hdx):
        L, sh = self.L, self.sh
        hs = L["hsize"]
        lh = hs * 1.18
        for tier in L["heads"]:
            nl = max(len(e[0]) for e in tier)
            for lines, ax, al, _ in tier:
                off = nl - len(lines)
                for k, t in enumerate(lines):
                    sh.text(ax + hdx, y + (off + k + 1) * lh, t, hs, bold=True, align=al)
            y += nl * lh
        if self.S["rules"] == "headline":
            sh.line(L["ml"], y + 3, L["ml"] + L["CW"], y + 3, width=0.5)
        return y + 5

    def label_line(self, y, label_pool, value, date=None):
        L, S = self.L, self.S
        base = y + self.size * 0.95
        vc = L["val_col"]
        spans = []
        vx = self.cell(self.anchor(vc), base, value, vc.align)
        spans.append(vx)
        dcol = next((c for c in L["cols"] if c.kind == "date"), None)
        if date and dcol is not None:
            spans.append(self.cell(self.anchor(dcol), base, date, dcol.align))
        tc = next(c for c in L["cols"] if c.kind == "text")
        lx = tc.x
        limit = min([s[0] for s in spans if s and s[0] > lx] + [L["ml"] + L["CW"] + 20]) - 4
        lowest = [s for s in spans if s and s[0] <= lx]
        if lowest and max(s[1] for s in lowest) > lx - 3:
            raise GenError("label line: value sits on the label")
        fits = [t for t in label_pool if self.sh.width(t, self.size, True) <= limit - lx]
        if not fits:
            raise GenError("%s: no label fits in %.0fpt" % (S["name"], limit - lx))
        self.cell_bold(lx, base, fits[0])

    def cell_bold(self, x, y, s):
        self.sh.text(x, y, s, self.size, bold=True)

    def draw_row(self, r, y, dx):
        S, L = self.S, self.L
        size, lead = self.size, L["lead"]
        base = y + size * 0.95
        red = r.get("red", {})
        mline = (r["nl"] - 1) if S["mvalign"] == "bottom" else 0
        if S["rules"] == "zebra" and r.get("zebra"):
            self.sh.rect(L["ml"] - 2 + dx, y - 1, L["CW"] + 4, r["h"] - 1, fill=PALE)
        for c in L["cols"]:
            if c.kind == "text":
                mode = red.get("text:" + c.src)
                for k, ln in enumerate(r["lines"][c.i]):
                    self.cell(c.x + dx, base + k * lead, ln, "left", mode)
                continue
            s = r["P"].get(c.kind)
            if c.kind in ("date",):
                s = r["shown_date"]
            if s is None:
                continue
            mode = None
            if c.kind in ("debit", "credit", "amount", "uamount"):
                mode = red.get("amount")
            elif c.kind == "balance":
                mode = red.get("balance")
            color = RED if (S["color_only"] and c.kind == "uamount" and r["dir"] == "out") else None
            yy = base + (mline * lead if c.kind in MONEY_COLS or c.kind == "ind" else 0)
            self.cell(self.anchor(c, dx), yy, s, c.align, mode, color)
        if S["rules"] == "rows":
            self.sh.line(L["ml"] + dx, y + r["h"] - 1.2, L["ml"] + L["CW"] + dx, y + r["h"] - 1.2,
                         width=0.25, color=(0.7, 0.7, 0.7))


def render(S, L, D, printed, pages, geo, path):
    dr = Draw(S, L, D, path)
    sh = dr.sh
    n = len(pages)
    line_h = L["lead"] + L["rowgap"]
    run_bal = D["opening"]
    prev_date = None
    zebra = False
    rng = random.Random(zlib.crc32((S["name"] + ":labels").encode()))
    labs = {k: rng.sample(v, len(v)) for k, v in LAB.items()}
    for pno, items in enumerate(pages):
        sh.begin_page()
        top = dr.header(pno, n)
        dr.footer(pno, n)
        y = (top if pno == 0 else 56.0) + geo["pdy"][pno]
        sh.push_dx(geo["pdx"][pno])
        H = S["H"]
        show_h = heads_on(S, pno, n)
        if pno == 0 and H["pos"] == "far" and show_h:
            y = dr.draw_heads(y, geo["hdx"][pno])
            sh.text(L["ml"], y + 14, "(Everything below is listed in the order it reached us. Probably.)", 7.5)
            y += 40
        elif show_h and H["pos"] in ("above", "far"):
            y = dr.draw_heads(y, geo["hdx"][pno])
        if S["bf"] and pno > 0:
            dr.label_line(y, labs["bf"], fmt_money(page_bal_start[pno], S["MB"]))
            y += line_h
        if pno == 0:
            page_bal_start = {}
        drew_head_below = False
        k = 0
        for it in items:
            kind = it["kind"]
            if kind == "row":
                r = it["r"]
                r["page"] = pno + 1
                first_on_page = k == 0
                if not S["dgroup"]:
                    r["shown_date"] = r["P"]["date"]
                else:
                    r["shown_date"] = r["P"]["date"] if (r["date"] != prev_date or
                                                         (first_on_page and S["dreprint"])) else None
                prev_date = r["date"]
                r["zebra"] = zebra
                zebra = not zebra
                dr.draw_row(r, y, S["drift"] * k)
                k += 1
                run_bal = r["bal"] if not S["newest"] else r["bal"] - r["sa"]
                y += it["h"]
                if show_h and H["pos"] == "below_first" and not drew_head_below:
                    y = dr.draw_heads(y, geo["hdx"][pno])
                    drew_head_below = True
            elif kind in ("open", "close"):
                v = D["opening"] if kind == "open" else D["closing"]
                d = None
                if S["openclose"] == "table_dated":
                    d = DATES[S["dfmt"]](D["start"] if kind == "open" else D["end"])
                dr.label_line(y, labs[kind], fmt_money(v, S["MB"]), d)
                y += it["h"]
            elif kind == "coltot":
                tin = sum(r["c"] for r in D["rows"] if r["dir"] == "in")
                tout = sum(r["c"] for r in D["rows"] if r["dir"] == "out")
                base = y + L["size"] * 0.95 + 4
                sh.line(L["ml"], y + 1, L["ml"] + L["CW"], y + 1, width=0.4)
                for c in L["cols"]:
                    if c.kind in ("debit", "credit"):
                        v = fmt_money(tout if c.kind == "debit" else tin, S["M"], unsigned=True)
                        dr.cell(dr.anchor(c), base, v, c.align)
                tc = next(c for c in L["cols"] if c.kind == "text")
                sh.text(tc.x, base, labs["coltot"][0], L["size"], bold=True)
                y += it["h"]
            elif kind == "totbox":
                dr.totbox(L["ml"] + L["CW"] - 220, y + 6, 220)
                y += it["h"]
            elif kind == "legend":
                if S["legend"] == "bottom":
                    sh.text(L["ml"], y + 12, dr.legend_text(), 7.5)
                else:
                    lab = "Key to the squiggles:"
                    x1 = sh.text(L["ml"], y + 12, lab, 7.5)[1]
                    rest = dr.legend_text()[len(lab):]
                    sh.blackout(x1 + 4, x1 + 4 + sh.width(rest.strip(), 7.5), y + 12, 7.5)
                y += it["h"]
        if S["bf"] and pno < n - 1:
            nxt = next((it["r"] for it in pages[pno + 1] if it["kind"] == "row"), None)
            # balance carried = balance before the next page's first printed row
            cb = (nxt["bal"] - nxt["sa"]) if (nxt is not None and not S["newest"]) else (
                nxt["bal"] if nxt is not None else run_bal)
            page_bal_start[pno + 1] = cb
            dr.label_line(y, labs["cf"], fmt_money(cb, S["MB"]))
            y += line_h
        if show_h and H["pos"] == "bottom":
            dr.draw_heads(y + 4, geo["hdx"][pno])
        sh.pop_dx()
        sh.end_page()
    sh.save()
    return dr


# ---------------------------------------------------------------------------
# Redactions, cues, truth
# ---------------------------------------------------------------------------

def apply_redactions(S, D):
    rng = random.Random(zlib.crc32((S["name"] + ":red").encode()))
    rows = D["rows"]
    texts = [c for c in S["cols"] if c in TEXT_SRCS]
    side = [t for t in texts if t != D["main_src"]] or texts
    for mode, field, count in S["redact"]:
        if field == "balance":
            cand = [r for r in rows if r["P"].get("balance")]
        else:
            cand = rows[1:]
        cand = [r for r in cand if not any(k.split(":")[0] == field for k in r.get("red", {}))]
        pick = cand if count == "all" else rng.sample(cand, count)
        for r in pick:
            key = field if field != "text" else "text:" + rng.choice(side)
            if key.startswith("text:") and not r["texts"][key[5:]]:
                key = "text:" + D["main_src"]
            r.setdefault("red", {})[key] = mode


def visible(r, key):
    return r.get("red", {}).get(key) != "removed"


def unique_dayend(S, D, visible_bal):
    """Day-end balances decide every row iff each day's sign pattern is unique."""
    rows = D["rows"]
    prev = D["opening"] if S["opening_printed"] else None
    i = 0
    while i < len(rows):
        j = i
        while not rows[j]["dayend"]:
            j += 1
        grp = rows[i:j + 1]
        end = rows[j]["bal"] if visible_bal(rows[j]) else None
        if prev is None or end is None:
            return False
        mags = [r["c"] for r in grp]
        hits = 0
        for mask in range(1 << len(mags)):
            if sum(m if mask >> k & 1 else -m for k, m in enumerate(mags)) == end - prev:
                hits += 1
        if hits != 1 and any(mags):
            return False
        prev = end
        i = j + 1
    return True


def compute_cues(S, D):
    rows = D["rows"]
    kinds = set(S["cols"])
    sep = "debit" in kinds and "credit" in kinds
    ind = "ind" in kinds
    signed = "amount" in kinds
    groups = sep or ind or signed
    cues = []
    vis_bal = [r for r in rows if r["P"].get("balance") and visible(r, "balance")]
    if S["bal"] == "every" and len(vis_bal) >= 0.8 * len(rows):
        cues.append("running_balance")
    elif S["bal"] == "dayend" and unique_dayend(S, D, lambda r: visible(r, "balance")):
        cues.append("running_balance")
    tin = sum(r["c"] for r in rows if r["dir"] == "in")
    tout = sum(r["c"] for r in rows if r["dir"] == "out")
    if groups and S["opening_printed"] and S["closing_printed"] and tin != tout:
        cues.append("printed_totals")
    if (signed and not S["card_side"]) or (sep and S["deb_signed"]):
        cues.append("sign_markers")
    if ind:
        if tuple(S["tokens"]) in STD_TOKENS:
            cues.append("indicator_column")
        elif S["legend"] in ("top", "bottom"):
            cues += ["indicator_column", "legend"]
    sem = [semantic_dir(" ".join(v for k, v in r["texts"].items() if visible(r, "text:" + k)))
           for r in rows]
    for s, r in zip(sem, rows):
        if s is not None and s != r["dir"] and r["c"]:
            raise GenError("row %r reads as %s but is %s" % (r["main"], s, r["dir"]))
    nz = [(s, r) for s, r in zip(sem, rows) if r["c"]]
    if groups:
        k_in = sum(1 for s, r in nz if s == "in")
        k_out = sum(1 for s, r in nz if s == "out")
        if k_in >= 2 and k_out >= 2:
            cues.append("description_semantics")
    elif all(s is not None for s, r in nz):
        cues.append("description_semantics")
    return cues


def features_of(S, D, L, npages):
    f = []
    kinds = S["cols"]
    H = S["H"]
    f.append("page:" + S["page"])
    f.append("font:%s/%gpt" % (S["font"], S["size"]))
    if S["page"].endswith("L"):
        f.append("landscape")
    if S["page"].startswith("Letter"):
        f.append("letter")
    if npages > 1:
        f.append("multi_page:%d" % npages)
    nt = sum(1 for k in kinds if k in TEXT_SRCS)
    f.append("text_columns:%d" % nt)
    if any(r["nl"] > 1 for r in D["rows"]):
        f.append("multi_line_wrap")
    hd = ["headings:" + H["pos"]]
    if H["pages"] != "all":
        hd.append("headings_pages:" + H["pages"])
    if H["dx"]:
        hd.append("headings_offset%s" % ("_per_page" if H["dxpages"] else ""))
    if H["merge"]:
        hd.append("heading_centred_over_two_columns")
    if H["split"]:
        hd.append("headings_split_over_lines")
    if H["free"] is not None:
        hd.append("heading_row_lines_up_with_nothing")
    if H["shuffle"]:
        hd.append("headings_lie")
    if H["group"]:
        hd.append("two_tier_headings")
    if H["align"] != "auto":
        hd.append("headings_align:" + H["align"])
    f += hd
    di = kinds.index("date")
    f.append("date_position:" + ("left" if di == 0 else ("right" if di == len(kinds) - 1 else "middle")))
    f.append("date_format:" + S["dfmt"])
    if "date2" in kinds:
        f.append("two_date_columns")
    if S["dgroup"]:
        f.append("date_once_per_day" + ("" if S["dreprint"] else "_not_reprinted_on_new_page"))
    M = S["M"]
    if "debit" in kinds:
        f.append("separate_in_out" + ("_in_first" if kinds.index("credit") < kinds.index("debit") else ""))
        if S["deb_signed"]:
            f.append("debit_column_signed")
    if "amount" in kinds:
        f.append("signed_amount:" + M["neg"] + (":" + "/".join(M["tok"]) if M["neg"] in ("suf", "pre") else ""))
    if "uamount" in kinds:
        f.append("unsigned_amount")
    if "ind" in kinds:
        f.append("indicator:" + "/".join(S["tokens"]))
    if S["legend"]:
        f.append("legend:" + S["legend"])
    f.append("thousands:" + {",": "comma", ".": "dot", " ": "space", "'": "apostrophe", "": "none"}[M["thou"]])
    if M["dec"] == ",":
        f.append("decimal_comma")
    if M["cur"]:
        f.append("currency:" + M["cur"].strip())
    f.append("balance:" + S["bal"] if "balance" in kinds else "balance:none")
    if "balance" in kinds:
        bi = kinds.index("balance")
        mi = [kinds.index(k) for k in kinds if k in ("debit", "credit", "amount", "uamount")]
        if mi and bi < min(mi):
            f.append("balance_left_of_amounts")
        f.append("balance_neg:" + S["MB"]["neg"])
    if S["newest"]:
        f.append("newest_first")
    if min(r["bal"] for r in D["rows"]) < 0:
        f.append("overdrawn")
    f.append("opening_closing:" + S["openclose"])
    if S["bf"]:
        f.append("brought_carried_forward")
    if S["totals"]:
        f.append("totals:" + S["totals"])
    if S["pdx"] or S["pdy"]:
        f.append("page_offsets")
    if S["drift"]:
        f.append("row_drift")
    if S["malign"] != "right":
        f.append("money_align:" + S["malign"])
    if S["mvalign"] == "bottom":
        f.append("amounts_on_last_line")
    if S["narrow"]:
        f.append("narrow_text_columns")
    if S["huge"]:
        f.append("huge_amounts")
    if S["zero"]:
        f.append("zero_amount")
    if S["card_side"]:
        f.append("card_side_signs")
    if S["color_only"]:
        f.append("colour_only_cue")
    if S["promo"]:
        f.append("promo_text_with_numbers")
    if S["toff"]:
        f.append("table_starts_low")
    modes = {m for r in D["rows"] for m in r.get("red", {}).values()}
    for m in sorted(modes):
        f.append("redaction:" + m)
    if S["acct_red"]:
        f.append("account_number_redaction:" + S["acct_red"])
    f.append("bank_position:" + S["bankpos"])
    return f


def text_keys(L):
    tcols = [c for c in L["cols"] if c.kind == "text"]
    heads = [c.head for c in tcols]
    keys = {}
    for k, c in enumerate(tcols):
        keys[c.src] = c.head if (c.head and heads.count(c.head) == 1 and
                                 c.head not in L.get("phantom", [])) else "col%d" % (c.i + 1)
    return keys


def truth_rows(S, D, L, printed):
    keys = text_keys(L)
    tsrc = [c.src for c in L["cols"] if c.kind == "text"]
    out = []
    for r in printed:
        red = r.get("red", {})
        rem = sorted(k.split(":")[0] if not k.startswith("text:") else "text:" + keys[k[5:]]
                     for k, m in red.items() if m == "removed")
        ov = sorted(k.split(":")[0] if not k.startswith("text:") else "text:" + keys[k[5:]]
                    for k, m in red.items() if m == "overlay")
        amt_ok = visible(r, "amount")
        bal_s = r["P"].get("balance")
        bal_ok = bal_s is not None and visible(r, "balance")
        text = {}
        for src in tsrc:
            text[keys[src]] = r["texts"][src] if visible(r, "text:" + src) else None
        desc = " ".join(v for v in (text[keys[s]] for s in tsrc) if v)
        amt_s = r["P"].get("debit") or r["P"].get("credit") or r["P"].get("amount") or r["P"].get("uamount")
        row = {
            "date": r["date"].isoformat(),
            "description": desc,
            "debit": (r["c"] / 100.0 if r["dir"] == "out" else None) if amt_ok else None,
            "credit": (r["c"] / 100.0 if r["dir"] == "in" else None) if amt_ok else None,
            "balance": r["bal"] / 100.0 if bal_ok else None,
            "text": text,
            "page": r["page"],
            "redacted": rem,
            "overlay_redacted": ov,
            "printed": {"date": r["shown_date"], "date2": r["P"].get("date2"),
                        "amount": amt_s if amt_ok else None, "indicator": r["P"].get("ind"),
                        "balance": bal_s if bal_ok else None},
        }
        if "date2" in S["cols"]:
            row["date2"] = r["date2"].isoformat()
        out.append(row)
    return out


def check_truth_chain(t):
    """opening -> every printed balance -> closing, in date order, through redactions."""
    rows = list(reversed(t["rows"])) if t["newest_first"] else list(t["rows"])
    b = round(t["opening_balance"] * 100)
    for r in rows:
        if r["debit"] is None and r["credit"] is None:
            b = None
        elif b is not None:
            b += round((r["credit"] or 0) * 100) - round((r["debit"] or 0) * 100)
        if r["balance"] is not None:
            if b is not None and b != round(r["balance"] * 100):
                raise GenError("%s: balance chain broken at %s %r (%s vs %s)"
                               % (t["case"], r["date"], r["description"], b, r["balance"]))
            b = round(r["balance"] * 100)
    if b is not None and b != round(t["closing_balance"] * 100):
        raise GenError("%s: closing %s != chain %s" % (t["case"], t["closing_balance"], b))


def check_truth_parse(t):
    """Every printed figure parses back to the truth row's figure."""
    M, MB = t["money_format"], t["balance_format"]
    lay = t["amount_layout"]
    for r in t["rows"]:
        p = r["printed"]
        if p["amount"] is not None:
            if lay == "signed":
                v = parse_money(p["amount"], M)
                if t["card_side"]:
                    v = -v
            elif lay == "separate":
                v = parse_money(p["amount"], M)
                v = abs(v) if r["credit"] is not None else -abs(v)
            elif lay == "indicator":
                v = parse_money(p["amount"], M, unsigned=True)
                tok = t["indicator_tokens"]
                if p["indicator"] not in tok.values():
                    raise GenError("%s: unknown token %r" % (t["case"], p["indicator"]))
                v = v if p["indicator"] == tok["in"] else -v
            else:
                v = parse_money(p["amount"], M, unsigned=True)
                v = v if r["credit"] is not None else -v
            want = round((r["credit"] or 0) * 100) - round((r["debit"] or 0) * 100)
            if v != want:
                raise GenError("%s: printed %r parses to %d, truth %d" % (t["case"], p["amount"], v, want))
        if p["balance"] is not None and parse_money(p["balance"], MB) != round(r["balance"] * 100):
            raise GenError("%s: printed balance %r != %s" % (t["case"], p["balance"], r["balance"]))


def write_json(path, obj):
    with open(path, "w") as f:
        json.dump(obj, f, indent=1, sort_keys=True)


def build_case(spec, out_dir):
    S = resolve(spec)
    D = gen_data(S)
    printed_strings(S, D)
    if S["dfmt"] in DM_NUMERIC and not any(r["date"].day > 12 for r in D["rows"]):
        raise GenError("%s: numeric day/month dates never show a day > 12" % S["name"])
    if S["dfmt"] == "dd" and S["months"] != 1:
        raise GenError("day-only dates need a one-month period")
    apply_redactions(S, D)
    L = layout(S, D)
    printed, items = build_items(S, L, D)
    pages, geo = paginate(S, L, items)
    if S["H"]["pages"] != "all" and len(pages) < 2:
        raise GenError("%s: headings_pages needs several pages" % S["name"])
    path = os.path.join(out_dir, S["name"] + ".pdf")
    render(S, L, D, printed, pages, geo, path)
    oc = S["openclose"]
    S["opening_printed"] = oc in ("header", "table", "table_dated") or (S["totals"] in ("after", "top") and S["tot_oc"])
    S["closing_printed"] = S["opening_printed"] or oc == "closing_header"
    cues = compute_cues(S, D)
    want_dec = S["undecidable"] is None
    if bool(cues) != want_dec:
        raise GenError("%s: recipe says decidable=%s but the cues are %s" % (S["name"], want_dec, cues))
    kinds = S["cols"]
    lay = ("separate" if "debit" in kinds else "signed" if "amount" in kinds else
           "indicator" if "ind" in kinds else "unsigned")
    columns = [{"heading": c.head if S["H"]["pos"] != "none" else None, "kind": KIND_TRUTH[c.kind],
                "source": c.src} for c in L["cols"]]
    feats = features_of(S, D, L, len(pages))
    note = S["note"] or ("Weird layout: " + ", ".join(x for x in feats if not x.startswith(("page:", "font:"))))
    truth = {
        "case": S["name"], "generator": GENERATOR, "note": note,
        "bank": S["bank"], "source_format": "pdf",
        "opening_balance": D["opening"] / 100.0, "closing_balance": D["closing"] / 100.0,
        "opening_printed": S["opening_printed"], "closing_printed": S["closing_printed"],
        "period": {"start": D["start"].isoformat(), "end": D["end"].isoformat()},
        "rows": truth_rows(S, D, L, printed), "features": feats,
        "decidable": want_dec, "decidable_by": cues, "undecidable_reason": S["undecidable"],
        "newest_first": S["newest"], "columns": columns, "phantom_headings": L.get("phantom", []),
        "money_format": S["M"], "balance_format": S["MB"], "amount_layout": lay,
        "card_side": S["card_side"], "date_format": S["dfmt"],
        "indicator_tokens": ({"in": S["tokens"][0], "out": S["tokens"][1]} if "ind" in kinds else None),
        "legend": S["legend"], "page_count": len(pages), "page_size": S["page"],
        "font": S["font"], "font_size": S["size"],
    }
    truth["row_count"] = len(truth["rows"])
    check_truth_chain(truth)
    check_truth_parse(truth)
    write_json(os.path.join(out_dir, S["name"] + ".truth.json"), truth)
    if S["scan"]:
        sname = S["name"] + "_scan"
        settings = ML.make_scan(path, os.path.join(out_dir, sname + ".pdf"), zlib.crc32(sname.encode()))
        write_json(os.path.join(out_dir, sname + ".truth.json"), scan_truth(truth, settings))
    return truth


def scan_truth(truth, settings):
    t = json.loads(json.dumps(truth))
    t["case"] = truth["case"] + "_scan"
    t["source_format"] = "scan"
    t["note"] = truth["note"] + " IMAGE-ONLY SCAN of %s.pdf (%d dpi, skew %.2f deg)." % (
        truth["case"], settings["dpi"], settings["skew"])
    t["features"] = truth["features"] + ["image_only_scan", "scan_dpi:%d" % settings["dpi"]]
    for r in t["rows"]:
        ov = r.pop("overlay_redacted", [])
        for f in ov:
            if f == "amount":
                r["debit"] = r["credit"] = None
                r["printed"]["amount"] = None
            elif f == "balance":
                r["balance"] = None
                r["printed"]["balance"] = None
            elif f.startswith("text:"):
                r["text"][f[5:]] = None
        if ov:
            r["description"] = " ".join(v for v in r["text"].values() if v)
            r["redacted"] = sorted(set(r["redacted"] + ov))
        r["overlay_redacted"] = []
    return t


# ---------------------------------------------------------------------------
# The pdftotext pass
# ---------------------------------------------------------------------------

def norm(s):
    return re.sub(r"\s+", " ", s)


def check_dir(d, only=None):
    import pymupdf
    names = sorted(f[:-len(".truth.json")] for f in os.listdir(d) if f.endswith(".truth.json"))
    if only:
        names = [n for n in names if only in n]
    bad, stats = [], dict(cases=0, scans=0, strings=0, pages=0, undecidable=0)
    for name in names:
        t = json.load(open(os.path.join(d, name + ".truth.json")))
        pdf = os.path.join(d, name + ".pdf")
        if not os.path.exists(pdf):
            bad.append("%s: no pdf" % name)
            continue
        if t["source_format"] == "scan":
            stats["scans"] += 1
            doc = pymupdf.open(pdf)
            if any(pg.get_text().strip() for pg in doc):
                bad.append("%s: scan has a text layer" % name)
            if len(doc) != t["page_count"]:
                bad.append("%s: scan has %d pages, truth %d" % (name, len(doc), t["page_count"]))
            doc.close()
            continue
        stats["cases"] += 1
        stats["undecidable"] += 0 if t["decidable"] else 1
        txt = subprocess.run(["pdftotext", "-layout", pdf, "-"], capture_output=True, text=True,
                             check=True).stdout
        pages = [norm(p) for p in txt.split("\f")]
        if pages and not pages[-1].strip():
            pages = pages[:-1]
        if len(pages) != t["page_count"]:
            bad.append("%s: %d pages, truth says %d" % (name, len(pages), t["page_count"]))
        for k, p in enumerate(pages):
            stats["pages"] += 1
            if FOOTER not in p:
                bad.append("%s: footer missing on page %d" % (name, k + 1))
        try:
            check_truth_chain(t)
            check_truth_parse(t)
        except GenError as e:
            bad.append(str(e))
        if t["row_count"] != len(t["rows"]) or t["decidable"] != bool(t["decidable_by"]):
            bad.append("%s: row_count or decidable inconsistent" % name)
        for r in t["rows"]:
            page = pages[r["page"] - 1] if r["page"] - 1 < len(pages) else ""
            for key, s in r["printed"].items():
                if s is None:
                    continue
                stats["strings"] += 1
                if norm(s) not in page:
                    bad.append("%s: %s %r not on page %d" % (name, key, s, r["page"]))
            for key, v in r["text"].items():
                if v is None or "text:" + key in r["overlay_redacted"]:
                    continue
                for w in v.split():
                    if w not in page:
                        bad.append("%s: text word %r not on page %d" % (name, w, r["page"]))
                        break
    print("check: %d text PDFs (%d undecidable), %d scans, %d pages, %d printed strings found"
          % (stats["cases"], stats["undecidable"], stats["scans"], stats["pages"], stats["strings"]))
    for b in bad[:40]:
        print("  FAIL", b)
    if bad:
        print("check: %d problem(s)" % len(bad))
        return 1
    print("check: all good")
    return 0


def main(argv=None):
    ap = argparse.ArgumentParser(description="Build the green-flag statement set.")
    ap.add_argument("--out", help="directory to write PDFs and truth files into")
    ap.add_argument("--only", help="only cases whose name contains this")
    ap.add_argument("--check", help="re-check a built directory against pdftotext")
    a = ap.parse_args(argv)
    if not a.out and not a.check:
        ap.error("give --out DIR and/or --check DIR")
    if len(CASES) != 100 or sum(1 for c in CASES if c.get("undecidable")) != 10:
        sys.exit("expected 100 recipes with 10 undecidable, have %d / %d"
                 % (len(CASES), sum(1 for c in CASES if c.get("undecidable"))))
    if a.out:
        os.makedirs(a.out, exist_ok=True)
        n = 0
        for spec in CASES:
            if a.only and a.only not in spec["name"]:
                continue
            try:
                t = build_case(spec, a.out)
            except GenError as e:
                sys.exit("GENERATOR SELF-CHECK FAILED in %s: %s" % (spec["name"], e))
            n += 1
            print("%-48s %3d rows %d pg  %s" % (t["case"], t["row_count"], t["page_count"],
                                                ",".join(t["decidable_by"]) or "UNDECIDABLE"))
        print("built %d case(s) into %s" % (n, a.out))
    if a.check:
        return check_dir(a.check, a.only)
    return 0


if __name__ == "__main__":
    sys.exit(main())
