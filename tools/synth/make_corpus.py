#!/usr/bin/env python3
"""make_corpus.py -- generate an ADVERSARIAL synthetic bank-statement corpus,
each PDF paired with the ground truth it was drawn from.

WHY THIS EXISTS, and why it is Python when the product is R.

The shipped fixtures (tests/testthat/fixtures/make_pdf_fixtures.R) prove that
three templates read three one-page statements. That is a regression net, not a
measurement: it cannot say how accurate the reader IS, only that it has not
changed. Nothing in the suite could answer "does centre-of-word band assignment
survive a column drawn 3pt too narrow", because there was no case where the right
answer was known independently of the reader.

Every PDF here is emitted with a .truth.json holding the EXACT rows it was drawn
from -- date, description, debit, credit, balance -- so a parse can be scored
rather than eyeballed. That is the whole point. A corpus without ground truth can
only find crashes; this one can find WRONG FIGURES, which is the failure this tool
exists to prevent.

Python because reportlab can place a glyph at an exact point, draw a minus sign as
a VECTOR LINE rather than as text, rotate a page, and emit a 120-page file in a
second. R's pdf() device cannot do the second of those at all, and that case --
a negative amount drawn as a line, which silently inverts a transaction -- is one
of the faults worth hunting.

NOTHING HERE SHIPS TO THE SERVER. The offline box runs R only. This writes PDFs
and JSON; the R harness (tools/synth/score.R) reads them. A subset of the PDFs is
committed as fixtures, the generator is a dev-time tool, and the product gains no
Python dependency.

Run:  python3 tools/synth/make_corpus.py --out /tmp/corpus
      python3 tools/synth/make_corpus.py --out /tmp/corpus --only band_cliff
"""

import argparse, json, os, random, sys
from reportlab.pdfgen import canvas
from reportlab.lib.pagesizes import A4

# A4 in points. The y axis in reportlab runs UP from the bottom; every layout
# below is written in TOP-DOWN coordinates (y from the top of the page) and
# converted once, in _text, because that is how a template's y bands read and
# mixing the two is how a fixture ends up 19pt out.
PW, PH = A4                      # 595.276 x 841.89

# The ANZ everyday band layout, which the shipped template declares. The corpus
# draws AT these coordinates so a case exercises the real template rather than
# one invented to suit it.
BANDS = {
    "date":        (40, 80),
    "description": (80, 330),
    "debit":       (330, 395),
    "credit":      (395, 472),
    "balance":     (472, 545),
}
HEADER = ["Date", "Transaction type and details", "Withdrawals", "Deposits", "Balance"]
# The fingerprint phrases anz_everyday_pdf matches on. A case that drops one is
# testing detection, not parsing, and says so in its name.
FINGERPRINT = ["Transaction type and details", "Withdrawals", "Deposits"]

MONTHS = ["Jan", "Feb", "Mar", "Apr", "May", "Jun",
          "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]

DESCRIPTIONS = [
    "EFTPOS RIVERSIDE DAIRY", "SALARY MATAI HOLDINGS", "DD CITY COUNCIL RATES",
    "TFR TO SAVINGS", "ATM WITHDRAWAL QUEEN ST", "DIRECT CREDIT IRD REFUND",
    "AP KIWISAVER CONTRIB", "EFTPOS FUEL STOP 114", "INTEREST PAID",
    "ONLINE PAYMENT POWERCO", "EFTPOS SUPERMARKET 2201", "BANK FEE MONTHLY",
]
# Long enough to spill out of the description band (80-330) into the DEBIT band
# (330-395) and stop there, leaving the credit and balance columns clean. This is
# the common real shape: a reference number tacked onto a description runs a little
# wide, not clear across the page. It is the case where the balance column can
# still rescue the amount, so it has to be tested separately from the extreme one.
MEDIUM_DESCRIPTIONS = [
    "EFTPOS PURCHASE RIVERSIDE DAIRY AND CONVENIENCE STORE 0114",
    "AUTOMATIC PAYMENT TO MATAI HOLDINGS TRUST REFERENCE 88412-00",
    "DIRECT DEBIT CITY COUNCIL RATES ASSESSMENT NUMBER 2291104A",
    "ONLINE BILL PAYMENT POWERCO CUSTOMER ACCOUNT NO 55120-03",
]
LONG_DESCRIPTIONS = [
    "EFTPOS PURCHASE AT RIVERSIDE DAIRY AND CONVENIENCE STORE LIMITED BRANCH 0114",
    "AUTOMATIC PAYMENT TO MATAI HOLDINGS TRUST ACCOUNT REFERENCE 88412-00 PARTICULARS RENT",
    "DIRECT DEBIT CITY COUNCIL RATES INSTALMENT THREE OF FOUR ASSESSMENT 2291104A",
]


def money(x, od=False):
    """The printed form: 1,234.56. None prints as nothing (an empty cell).

    A NEGATIVE NUMBER MUST NOT LOSE ITS SIGN HERE. This function used to return
    abs(x), and the first scoring run blamed the engine for 221 wrong balances on
    a 260-row statement: the invented balance went overdrawn at row 40 and the
    page then printed "196.16" for a balance of -196.16. The reader was right and
    the generator was lying -- the same wrong-figure fault this corpus exists to
    hunt, in the harness itself. A negative prints the way a real NZ statement
    prints one: a trailing OD, or a leading minus.
    """
    if x is None:
        return ""
    s = "{:,.2f}".format(abs(x))
    if x < 0:
        return s + " OD" if od else "-" + s
    return s


class Sheet:
    """One PDF being drawn, in TOP-DOWN y coordinates."""

    def __init__(self, path, pagesize=(PW, PH)):
        self.c = canvas.Canvas(path, pagesize=pagesize)
        self.w, self.h = pagesize
        self.c.setFont("Helvetica", 9)

    def text(self, x, y_top, s, size=9, bold=False, align="left"):
        """Place a string with its BASELINE at y_top from the top of the page."""
        if s == "":
            return
        self.c.setFont("Helvetica-Bold" if bold else "Helvetica", size)
        y = self.h - y_top
        if align == "right":
            self.c.drawRightString(x, y, s)
        else:
            self.c.drawString(x, y, s)

    def line(self, x0, y0_top, x1, y1_top, width=0.7):
        self.c.setLineWidth(width)
        self.c.line(x0, self.h - y0_top, x1, self.h - y1_top)

    def page_break(self):
        self.c.showPage()
        self.c.setFont("Helvetica", 9)

    def save(self):
        self.c.save()


# ---------------------------------------------------------------------------
# The statement body. One function, parameterised, because every case below is
# the SAME statement drawn differently -- which is what makes a difference in the
# score attributable to the one thing the case changed.
# ---------------------------------------------------------------------------

def make_rows(n, seed, year=2026, month=2, long_desc=False, with_balance=True,
              allow_overdraft=False, days_in_month=28, day_step=3):
    """Invent n transactions and the running balance they produce.

    Returns (opening, rows, closing). Every row reconciles: opening, minus each
    debit, plus each credit, equals the printed balance and the final closing.
    Invented people, invented accounts, invented figures -- no real data.

    THE BALANCE STAYS POSITIVE unless the case asks for an overdraft. A statement
    that wanders overdrawn halfway through is a legitimate shape, but it is a
    SEPARATE thing to test (how the sign is printed) and letting it happen by
    accident in every long case buries whatever the case was actually about.

    DATES ADVANCE ACROSS MONTHS rather than clamping at the 28th. Clamping meant a
    260-row statement printed "28 Feb" on 250 of its rows, so any date fault after
    row 10 was invisible and the period check had nothing to bite on.
    """
    rng = random.Random(seed)
    opening = round(rng.uniform(2000, 8000), 2)
    bal = opening
    rows = []
    day, mon, yr = 1, month, year
    for i in range(n):
        # day_step 0 packs several transactions onto one day, which is how a busy
        # account really prints 260 rows. Stepping 1-3 days per row instead made a
        # 260-row case span TWENTY MONTHS -- "Statement period 1 Feb 2026 to 5 Sep
        # 2027" -- and then blamed the reader for 92 wrong years on dates printed
        # with no year at all. No bank issues a 20-month statement; the case was
        # measuring an impossible document.
        day += rng.randint(0 if day_step == 0 else 1, max(day_step, 1))
        while day > days_in_month:
            day -= days_in_month
            mon += 1
            if mon > 12:
                mon = 1
                yr += 1
        is_credit = rng.random() < 0.3
        amt = round(rng.uniform(5, 950), 2)
        if not is_credit and not allow_overdraft and bal - amt < 50:
            is_credit = True          # keep it out of overdraft on purpose
        if is_credit:
            debit, credit = None, amt
            bal = round(bal + amt, 2)
        else:
            debit, credit = amt, None
            bal = round(bal - amt, 2)
        pool = (LONG_DESCRIPTIONS if long_desc == "long" else
                MEDIUM_DESCRIPTIONS if long_desc == "medium" else DESCRIPTIONS)
        rows.append({
            "date": "%02d %s" % (day, MONTHS[mon - 1]),
            "iso_date": "%04d-%02d-%02d" % (yr, mon, day),
            "description": pool[i % len(pool)],
            "debit": debit,
            "credit": credit,
            "balance": bal if with_balance else None,
        })
    return opening, rows, bal


def draw_statement(sh, opening, rows, closing, year=2026, month=2,
                   y_table=None, rows_per_page=30, row_pitch=14,
                   header_every_page=True, page_footer=True,
                   start_page=1, wrap_desc=False, amount_align="right",
                   negatives_as_line=False, no_header=False,
                   band_nudge=None, decimal_comma=False, dr_cr_suffix=False,
                   parens_negative=False, blank_stub_wrap=False,
                   drop_fingerprint=False, tight_pitch=False, no_opening=False,
                   total_row=False, footer_looks_like_row=False,
                   overdraft_od=False):
    """Draw one statement. Returns the list of rows ACTUALLY drawn as data rows,
    which is the ground truth for this file."""
    if y_table is None:
        y_table = 190
    last = rows[-1]["iso_date"] if rows else "%04d-%02d-28" % (year, month)
    ly, lm, ld = (int(x) for x in last.split("-"))
    period = "Statement period 1 %s %d to %d %s %d" % (
        MONTHS[month - 1], year, ld, MONTHS[lm - 1], ly)

    def page_head(first):
        sh.text(40, 60, "Kowhai Bank of Aotearoa", size=14, bold=True)
        sh.text(40, 78, "Everyday Account", size=10)
        sh.text(40, 96, "A R TAMATI and J P TAMATI", size=9)
        sh.text(40, 110, "Account number 01-9988-0043217-00", size=9)
        sh.text(40, 128, period, size=9)
        if first and not no_opening:
            sh.text(40, 146, "Opening balance", size=9)
            sh.text(BANDS["balance"][1], 146, money(opening), size=9, align="right")

    def col_heads(y):
        if no_header:
            return
        names = list(HEADER)
        if drop_fingerprint:
            names[1] = "Details"          # breaks page_contains_all on purpose
        sh.text(BANDS["date"][0], y, names[0], size=9, bold=True)
        sh.text(BANDS["description"][0], y, names[1], size=9, bold=True)
        sh.text(BANDS["debit"][1], y, names[2], size=9, bold=True, align="right")
        sh.text(BANDS["credit"][1], y, names[3], size=9, bold=True, align="right")
        sh.text(BANDS["balance"][1], y, names[4], size=9, bold=True, align="right")
        sh.line(BANDS["date"][0], y + 4, BANDS["balance"][1], y + 4)

    def amount(x_band, y, val, kind):
        """One money cell, printed however this case prints money."""
        if val is None:
            return
        s = money(val)
        if decimal_comma:
            s = s.replace(",", " ").replace(".", ",")
        if dr_cr_suffix:
            s = s + (" CR" if kind == "credit" else " DR")
        if parens_negative and kind == "debit":
            s = "(" + s + ")"
        x0, x1 = x_band
        if negatives_as_line and kind == "debit":
            # THE FAULT WORTH HUNTING: the minus is a drawn LINE, not a glyph, so
            # the text layer holds "40.00" and the sign exists only as vector ink.
            # A reader that trusts the text layer records +40.00 for a -40.00
            # transaction, which is the worst single error this tool can make.
            if amount_align == "right":
                sh.text(x1, y, s, size=9, align="right")
                tw = sh.c.stringWidth(s, "Helvetica", 9)
                sh.line(x1 - tw - 6, y - 3, x1 - tw - 2, y - 3, width=0.9)
            else:
                sh.text(x0 + 6, y, s, size=9)
                sh.line(x0, y - 3, x0 + 4, y - 3, width=0.9)
            return
        if amount_align == "right":
            sh.text(x1, y, s, size=9, align="right")
        else:
            sh.text(x0 + 2, y, s, size=9)

    bands = dict(BANDS)
    if band_nudge:
        for k, dx in band_nudge.items():
            x0, x1 = bands[k]
            bands[k] = (x0 + dx, x1 + dx)

    pitch = 9 if tight_pitch else row_pitch
    drawn = []
    i = 0
    page = 0
    while i < len(rows):
        if page or start_page > 1:
            pass
        page_head(first=(page == 0))
        y = y_table
        if page == 0 or header_every_page:
            col_heads(y)
            y += 18
        n_this = min(rows_per_page, len(rows) - i)
        for r in rows[i:i + n_this]:
            desc = r["description"]
            if wrap_desc and len(desc) > 40:
                # A description over two or three lines. The CONTINUATION lines
                # carry no date and no figures -- Nurminen's blank-stub shape --
                # so a reader that groups by line pitch alone emits three rows
                # where one is right.
                words = desc.split()
                lines, cur = [], ""
                for w in words:
                    if len(cur) + len(w) + 1 > 34:
                        lines.append(cur); cur = w
                    else:
                        cur = (cur + " " + w).strip()
                if cur:
                    lines.append(cur)
            else:
                lines = [desc]
            sh.text(bands["date"][0], y, r["date"], size=9)
            sh.text(bands["description"][0], y, lines[0], size=9)
            amount(bands["debit"], y, r["debit"], "debit")
            amount(bands["credit"], y, r["credit"], "credit")
            if r["balance"] is not None:
                sh.text(bands["balance"][1], y, money(r["balance"], od=overdraft_od),
                        size=9, align="right")
            for extra in lines[1:]:
                y += pitch
                if blank_stub_wrap:
                    sh.text(bands["description"][0], y, extra, size=9)
                else:
                    sh.text(bands["description"][0] + 8, y, extra, size=9)
            drawn.append(r)
            y += pitch
            if y > PH - 90:
                break
        i += n_this
        if total_row:
            # A TOTAL ROW WITH NO HEADING OVER IT. It is not a transaction and
            # must not become one; a reader that keeps any line with a figure in
            # the amount band adds a phantom transaction the size of the month.
            y += 6
            sh.text(bands["description"][0], y, "Total for period", size=9, bold=True)
            tot_d = sum(r["debit"] or 0 for r in rows)
            sh.text(bands["debit"][1], y, money(tot_d), size=9, bold=True, align="right")
        if footer_looks_like_row:
            # A FOOTER SHAPED LIKE A TRANSACTION: a date and a figure on one line
            # below the table. The keep-rule decides whether this is data.
            sh.text(bands["date"][0], PH - 70, "28 %s" % MONTHS[month - 1], size=8)
            sh.text(bands["description"][0], PH - 70,
                    "Interest rate effective this period", size=8)
            sh.text(bands["balance"][1], PH - 70, "4.25", size=8, align="right")
        if page_footer:
            sh.text(40, PH - 50, "Page %d of %d" % (
                page + 1, (len(rows) + rows_per_page - 1) // rows_per_page), size=8)
        i_done = i >= len(rows)
        if not i_done:
            sh.page_break()
        page += 1
    # The closing balance, printed where a statement prints it.
    sh.text(40, PH - 70 if not footer_looks_like_row else PH - 58,
            "Closing balance", size=9, bold=True)
    sh.text(bands["balance"][1], PH - 70 if not footer_looks_like_row else PH - 58,
            money(closing), size=9, bold=True, align="right")
    return drawn


# ---------------------------------------------------------------------------
# THE CASES. Each is (name, what it is testing, builder). The builder returns the
# ground truth dict written beside the PDF.
# ---------------------------------------------------------------------------

def case(name, note, **kw):
    """Declare one case as a (name, note, kwargs) triple."""
    return (name, note, kw)


CASES = [
    # ---- the baseline every other case is measured against --------------------
    case("baseline_1page", "6 rows, one page, right-aligned, nothing unusual",
         n=6),
    case("baseline_2page", "40 rows over two pages, header repeated",
         n=40, rows_per_page=30),
    case("baseline_long", "260 rows over nine pages, one busy month",
         n=260, rows_per_page=30, day_step=0),

    # ---- the offsets the brief named explicitly -------------------------------
    case("offset_single_page", "the whole table drawn 60pt lower on every page",
         n=20, y_table=250),
    case("offset_odd_pages", "the table starts lower on page 1 than on page 2+",
         n=50, rows_per_page=25, y_table=250),
    case("offset_table_on_page2", "page 1 is a cover; the table starts on page 2",
         n=20, cover_page=True),
    case("offset_table_on_page3", "two cover pages before the table",
         n=20, cover_page=2),

    # ---- the band model under stress -----------------------------------------
    case("band_cliff_3pt", "amounts drawn 3pt into the NEXT band",
         n=20, band_nudge={"debit": 3, "credit": 3}),
    case("band_cliff_1pt", "amounts drawn 1pt over -- inside the rounding error",
         n=20, band_nudge={"debit": 1, "credit": 1, "balance": 1}),
    # A CASE THE READER CANNOT GET RIGHT, AND MUST NOT GET WRONG QUIETLY.
    # The debit is drawn so far over that it sits inside the CREDIT band. No reader
    # working from column positions can know that a figure in the credit column is
    # really a debit -- the page says otherwise -- so scoring it on figure accuracy
    # would be scoring an impossibility. What it CAN be scored on is whether the
    # run is caught: the sign flips, the running balance stops adding up, and the
    # statement must come back flagged rather than clean. `mustflag_` tells the
    # scorer that is the whole test.
    case("mustflag_debit_in_credit_column",
         "a debit drawn inside the credit band: unknowable, so it must be caught",
         n=20, band_nudge={"debit": 32, "credit": 38}),
    case("band_cliff_10pt", "amounts drawn 10pt over, centre now in the wrong band",
         n=20, band_nudge={"debit": 10, "credit": 10}),
    case("band_left_aligned", "amounts LEFT-aligned in their band, not right",
         n=20, amount_align="left"),
    case("band_narrow_desc", "description overflows across ALL the numeric bands",
         n=20, long_desc="long"),
    case("band_overflow_debit_only",
         "description spills into the debit band only; credit and balance stay clean",
         n=20, long_desc="medium"),

    # ---- the wrapped row -----------------------------------------------------
    case("wrap_indented", "long descriptions over 2-3 INDENTED lines",
         n=14, long_desc="long", wrap_desc=True),
    case("wrap_blank_stub", "the same, NOT indented -- Nurminen's blank-stub shape",
         n=14, long_desc="long", wrap_desc=True, blank_stub_wrap=True),

    # ---- how money is printed ------------------------------------------------
    case("money_dr_cr_suffix", "amounts carry a DR / CR suffix",
         n=16, dr_cr_suffix=True),
    case("money_parens_negative", "debits printed in parentheses",
         n=16, parens_negative=True),
    case("money_decimal_comma", "1.234,56 -- comma decimal, space thousands",
         n=16, decimal_comma=True),
    case("money_minus_as_line", "the minus sign is VECTOR INK, absent from the text layer",
         n=16, negatives_as_line=True),

    # ---- structure the reader has to refuse or absorb ------------------------
    case("struct_total_row", "an unheaded TOTAL row under the transactions",
         n=16, total_row=True),
    case("struct_footer_like_row", "a footer line shaped exactly like a transaction",
         n=16, footer_looks_like_row=True),
    case("struct_no_col_header", "no column headings at all",
         n=16, no_header=True),
    case("struct_no_header_page2", "headings on page 1 only, not on continuation pages",
         n=50, rows_per_page=25, header_every_page=False),
    case("struct_tight_pitch", "9pt row pitch -- tighter than the 3pt row_tol assumes",
         n=20, tight_pitch=True),
    case("struct_no_opening", "no opening balance printed anywhere",
         n=16, no_opening=True),
    case("struct_no_balance_col", "no running balance column at all",
         n=16, with_balance=False),

    case("money_overdrawn_minus", "the balance goes overdrawn, printed with a minus",
         n=30, allow_overdraft=True),
    case("money_overdrawn_od", "the balance goes overdrawn, printed as '196.16 OD'",
         n=30, allow_overdraft=True, overdraft_od=True),
    case("dates_across_months", "90 rows spanning four months",
         n=90, rows_per_page=30),
    case("dates_across_new_year", "a period that crosses 31 Dec, dates printed with no year",
         n=60, rows_per_page=30, start_month=11, day_step=2),

    # ---- detection, not parsing ----------------------------------------------
    case("detect_phrase_missing", "a fingerprint phrase is reworded",
         n=16, drop_fingerprint=True),

    # ---- the page itself -----------------------------------------------------
    case("page_landscape", "A4 landscape -- the bands are in portrait points",
         n=16, landscape=True),
    case("page_rotated_90", "the page carries /Rotate 90",
         n=16, rotate=90),
]


def build(case_name, note, out_dir, seed=4242, **kw):
    """Draw one case and write <name>.pdf + <name>.truth.json.

    The kwargs split three ways: some steer make_rows (what the data IS), some
    steer the page (size, rotation, cover pages), and the rest go through to
    draw_statement (how the data is PRINTED). Keeping the split explicit is what
    stops a case silently passing an argument nothing reads.
    """
    n = kw.pop("n", 20)
    cover = int(kw.pop("cover_page", 0) or 0)
    landscape = kw.pop("landscape", False)
    rotate = kw.pop("rotate", 0)
    long_desc = kw.pop("long_desc", False)   # False | "medium" | "long"
    with_balance = kw.pop("with_balance", True)
    allow_overdraft = kw.pop("allow_overdraft", False)
    day_step = kw.pop("day_step", 3)
    start_month = kw.pop("start_month", 2)
    start_year = kw.pop("start_year", 2026)

    # A per-case seed, derived from the name rather than from the iteration order,
    # so adding a case does not change the figures in every other one.
    case_seed = seed + (abs(hash(case_name)) % 9973)
    opening, rows, closing = make_rows(n, seed=case_seed, long_desc=long_desc,
                                       with_balance=with_balance,
                                       allow_overdraft=allow_overdraft,
                                       day_step=day_step, month=start_month,
                                       year=start_year)
    kw.setdefault("month", start_month)
    kw.setdefault("year", start_year)

    pagesize = (PH, PW) if landscape else (PW, PH)
    pdf = os.path.join(out_dir, case_name + ".pdf")
    sh = Sheet(pdf, pagesize=pagesize)

    for _ in range(cover):
        sh.text(40, 300, "Kowhai Bank of Aotearoa", size=20, bold=True)
        sh.text(40, 330, "Your statement is enclosed.", size=11)
        sh.text(40, 360, "This page carries no transactions.", size=10)
        sh.page_break()

    drawn = draw_statement(sh, opening, rows, closing, **kw)
    sh.save()

    if rotate:
        _apply_rotate(pdf, rotate)

    truth = {
        "case": case_name,
        "note": note,
        "generator": "tools/synth/make_corpus.py",
        "opening_balance": opening,
        "closing_balance": closing,
        "row_count": len(drawn),
        "rows": [{"date": r["iso_date"], "description": r["description"],
                  "debit": r["debit"], "credit": r["credit"],
                  "balance": r["balance"]} for r in drawn],
    }
    with open(os.path.join(out_dir, case_name + ".truth.json"), "w") as f:
        json.dump(truth, f, indent=1, sort_keys=True)
    return pdf, truth


def _apply_rotate(pdf, deg):
    """Set /Rotate on every page, in place. pymupdf, because reportlab has no
    way to say it and a rotated page is a real shape a scanner produces."""
    import pymupdf
    d = pymupdf.open(pdf)
    for page in d:
        page.set_rotation(deg)
    d.saveIncr()
    d.close()


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out", required=True)
    ap.add_argument("--only", default=None,
                    help="build just the cases whose name contains this")
    ap.add_argument("--list", action="store_true")
    a = ap.parse_args()

    if a.list:
        for name, note, _ in CASES:
            print("%-26s %s" % (name, note))
        return 0

    os.makedirs(a.out, exist_ok=True)
    built = 0
    index = []
    for name, note, kw in CASES:
        if a.only and a.only not in name:
            continue
        try:
            pdf, truth = build(name, note, a.out, **kw)
        except Exception as e:                       # noqa: BLE001
            print("FAILED  %-26s %s: %s" % (name, type(e).__name__, e))
            continue
        size = os.path.getsize(pdf)
        print("%-26s %4d rows  %7d bytes  %s" % (name, truth["row_count"], size, note))
        index.append({"case": name, "note": note, "rows": truth["row_count"]})
        built += 1
    with open(os.path.join(a.out, "index.json"), "w") as f:
        json.dump(index, f, indent=1)
    print("\n%d case(s) written to %s" % (built, a.out))
    return 0


if __name__ == "__main__":
    sys.exit(main())
