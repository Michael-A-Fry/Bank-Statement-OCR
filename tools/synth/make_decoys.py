#!/usr/bin/env python3
"""make_decoys.py -- the DECOY-TABLE set: ~80 realistic synthetic bank statements in
which the real transaction table is surrounded by other tables that LOOK like
transactions. A held-back test for the content-reading auto wizard.

WHAT THIS IS. Each statement is a plausible NZ-style bank document: 2-4 pages of
front matter (cover letter, account overview, multi-account summaries, interest-rate
change notices, term-deposit and loan schedules, fee schedules, automatic payments,
pending transactions, rewards / FX tables, "important changes" notices), then the
real statement (which may start partway down a page and span several pages, with
decoys above it, between its pages and below it), then 1-5 pages of back matter
(terms and conditions, fee tables, complaint / dispute information, marketing, a
linked account's mini-statement, tax / RWT summaries, near-blank pages). Many
decoys carry dates, money and even a correctly CHAINING balance column (loan
amortisation, term-deposit activity, linked account mini-statement, rewards points,
the sample table in a "your new statement design" notice).

It was written WITHOUT reading the reader it measures (R/auto_read*.R,
R/parse_pdf_table.R, R/wizard_auto.R). It reuses make_layouts.py (Sheet: collision
and off-page checks, fonts, period text) and make_greenflag.py (date formats, money
formatting and parse-back, word wrap).

REAL BANK NAMES, FAKE EVERYTHING ELSE. Real NZ bank names appear as plain text only
(no logos, no brand colours) beside three fictional banks; every person, address,
account number, merchant and figure is invented, and every page prints
"SYNTHETIC TEST DOCUMENT - NOT A REAL STATEMENT".

TRUTH FILE (<case>.truth.json), make_greenflag.py's format (tools/synth/truth.R reads
it): case, generator, note, bank, opening_balance, closing_balance, row_count,
rows [{date, description, debit, credit, balance, text, page, redacted,
overlay_redacted, printed{date, date2, amount, indicator, balance}}], features,
decidable, decidable_by, newest_first, columns, money_format, balance_format,
amount_layout, date_format, page_count, period ... plus:
    decoys        [{kind, position (front|top|mid|below|back), pages, title, rows,
                    chains, dated_rows [{date, amounts (cents), mirror}], probes}]
    decoy_kinds   sorted distinct kinds
    front_pages   pages wholly before the page on which the real table starts
    back_pages    pages wholly after the page on which the real table ends
    statement_pages, statement_starts_partway
    accounts      (genuine multi-account statements only: rows then carry
                   account_index and the top-level opening/closing are null)
  rows = ONLY the real statement's transactions, in printed order. debit = money
  OUT, credit = money IN, both positive. On a credit card the figures are from the
  holder's side (purchase = debit, payment = credit; opening/closing = amount owed
  NEGATED, as make_layouts.py does). Opening / closing / brought- and
  carried-forward / total lines are never rows; nothing in any decoy is a row.

CHECKS. Before each truth is written: the balance chain (opening -> every printed
balance -> closing, per account), every printed figure parsed back to the truth,
every decoy chain re-added, no decoy dated row sharing (date, amount) with a real
row unless it is a deliberate mirror (the other side of a transfer to a linked
account), every string inside the page and clear of its neighbours
(make_layouts.Sheet). After the run (or alone with --check DIR): every truth date,
amount, balance and description word is found in `pdftotext -layout` output on the
row's page, each decoy's probe strings are on its pages, the footer is on every
page, front_pages >= 2 and 1 <= back_pages <= 5.

PITFALLS ALREADY MET (keep them fixed): a block must draw inside its own height or
the Sheet collision check fires on the next block; a decoy's (date, amount) can
collide by chance with a real row -- the case is then rebuilt with the next attempt
seed rather than written with an ambiguous truth; year-less dates (dd Mon) rely on
the period printed in the statement header.

Run:  python3 tools/synth/make_decoys.py --out DIR [--only SUBSTR] [--check DIR]

Deterministic (crc32 seeds, reportlab invariant mode). ASCII-only source. Dev-time
only; nothing here ships. Python 3.9+, reportlab, poppler pdftotext (checks).
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
import make_layouts as ML  # noqa: E402
import make_greenflag as G  # noqa: E402
from make_layouts import GenError, Sheet  # noqa: E402
from reportlab.lib.pagesizes import A4, landscape, letter  # noqa: E402
from reportlab.pdfbase.pdfmetrics import stringWidth  # noqa: E402

GENERATOR = "tools/synth/make_decoys.py"
FOOTER = ML.SYNTHETIC
PAGES = {"A4": A4, "A4L": landscape(A4), "Letter": letter}
MON, MONTH = ML.MON, ML.MONTH
DATES = G.DATES
GREY = (0.88, 0.88, 0.88)
PALE = (0.95, 0.95, 0.95)
MIDG = (0.45, 0.45, 0.45)
MAX_PAGES = 13


class Retry(GenError):
    """A chance collision (decoy figure equal to a real row, too many pages): the
    case is rebuilt with the next attempt seed instead of writing a muddled truth."""


# ---------------------------------------------------------------------------
# Made-up world (realistic, but every name invented)
# ---------------------------------------------------------------------------

BANKS = dict(ML.BANKS)
BANKS["totara"] = dict(name="Totara Building Society", mast="Totara Building Society",
                       legal="Totara Building Society Limited", codes=("98",), fictional=True)
BANKS["kahu"] = dict(name="Kahu Credit Union", mast="Kahu Credit Union",
                     legal="Kahu Credit Union Incorporated", codes=("97",), fictional=True)

TITLES = ["Mr", "Ms", "Mrs", "Dr", "Mx"]
FIRST = ["Aroha", "Benjamin", "Charlotte", "Daniel", "Eleanor", "Finn", "Grace", "Hemi", "Isla",
         "James", "Kiri", "Liam", "Mere", "Noah", "Olivia", "Priya", "Quentin", "Ruby", "Samuel",
         "Tama", "Ursula", "Vikram", "Wiremu", "Xanthe", "Yusuf", "Zara"]
LAST = ["Ashworth", "Bramley", "Calloway", "Dunmore", "Ellerby", "Fairweather", "Gillanders",
        "Harcourt", "Inglewood", "Jessop", "Kettering", "Lockhart", "Marchetti", "Netherby",
        "Oakridge", "Pennington", "Quarrie", "Rothwell", "Sandilands", "Tremaine", "Underhill",
        "Wetherall", "Yardley"]
STREETS = ["Kowhai Street", "Rata Road", "Matai Crescent", "Huia Avenue", "Karaka Lane",
           "Ngaio Terrace", "Puriri Place", "Tawa Drive", "Miro Way", "Hinau Grove"]
TOWNS = ["Ashhurst", "Brightwater", "Cromwell", "Dannevirke", "Eltham", "Feilding", "Geraldine",
         "Hokitika", "Katikati", "Levin", "Martinborough", "Ngatea", "Opotiki", "Pahiatua",
         "Raglan", "Taihape", "Waipawa", "Wanaka", "Whitianga", "Te Puke"]
MERCH = ["HARBOURVIEW SUPERMARKET", "KOWHAI CORNER DAIRY", "TUI LANE PHARMACY", "RIVERSTONE FUEL",
         "MATAI ST BAKERY", "SEVEN PINES HARDWARE", "BLUE HERON CAFE", "PAUA BAY FISH SHOP",
         "TOTARA BOOKS", "NORTHGATE CINEMAS", "KEA PARK GARDEN CTR", "SALTWATER TAKEAWAYS",
         "CLOVERDALE BUTCHERY", "LONGBEACH LIQUOR", "SUMMIT SPORTS", "WILLOW TREE FLORIST",
         "GREENSTONE NOODLE BAR", "RED SHED VET CLINIC"]
BILLERS = ["WAIRAU POWER LTD", "TASMAN WATER SERVICES", "NORTHLINK BROADBAND",
           "KERERU MUTUAL INSURANCE", "HILLSIDE GAS CO", "DISTRICT COUNCIL RATES", "PEAK MOBILE",
           "SUNRISE GYM", "ORCHARD KINDERGARTEN"]
EMPLOYERS = ["HOROWHENUA ENGINEERING", "COASTAL FREIGHT LTD", "LAKESIDE HEALTH TRUST",
             "ALPINE TIMBER LTD", "BAYSIDE DENTAL GROUP", "ORCHARD VALLEY PACKHOUSE",
             "SUMMERHILL SCHOOL"]
PEOPLE = ["J M HARCOURT", "A K BRAMLEY", "S R LOCKHART", "T WETHERALL", "P QUARRIE",
          "R & L DUNMORE", "H TREMAINE", "M NETHERBY"]

# (type code, long prefix, body template, lo cents, hi cents, weight)
OUT_T = [("EFTPOS", "EFTPOS", "{mer} {town}", 350, 22000, 7),
         ("POS", "POS W/D", "{mer}", 400, 15000, 3),
         ("D/D", "DIRECT DEBIT", "{bil}", 2500, 32000, 2),
         ("A/P", "AUTOMATIC PAYMENT", "{per}", 20000, 65000, 1),
         ("BILL PMT", "BILL PAYMENT", "{bil}", 3000, 26000, 2),
         ("ATM", "ATM WITHDRAWAL", "{town}", 2000, 40000, 1),
         ("ONLINE", "ONLINE PAYMENT", "{per}", 1500, 48000, 1),
         ("VISA", "VISA DEBIT", "{mer}", 600, 16000, 2)]
IN_T = [("D/C", "SALARY", "{emp}", 180000, 420000, 2),
        ("D/C", "DIRECT CREDIT", "{per}", 2000, 90000, 2),
        ("DEP", "DEPOSIT", "{town} BRANCH", 2000, 50000, 1),
        ("D/C", "IRD REFUND", "", 3000, 90000, 1),
        ("INT", "INTEREST CREDIT", "", 10, 900, 1)]
SAV_T = [("in", "INT", "INTEREST CREDIT", 50, 4000), ("in", "TFR", "TRANSFER FROM EVERYDAY", 5000, 80000),
         ("in", "DEP", "DEPOSIT", 2000, 30000), ("out", "TFR", "TRANSFER TO EVERYDAY", 5000, 60000),
         ("out", "W/D", "WITHDRAWAL", 2000, 30000)]

PRODUCTS = {"bank": ["Everyday Account", "Cheque Account", "Current Account", "Transaction Account",
                     "Business Current Account", "Everyday Plus"],
            "savings": ["Online Saver", "Bonus Saver", "Notice Saver", "Savings Account"],
            "card": ["Visa Classic", "Low Rate Visa", "Mastercard Platinum", "Visa Rewards"]}

HEADS_MAIN = {
    "date": ["Date", "Date", "Transaction date", "Processed", "Date posted"],
    "type": ["Type", "Tran type", "Code"],
    "details": ["Transaction details", "Details", "Description", "Particulars", "Transaction"],
    "ref": ["Reference", "Ref", "Reference no."],
    "amount": ["Amount", "Transaction amount", "Amount $"],
    "balance": ["Balance", "Balance", "Running balance", "Balance $"],
}
PAIRS_MAIN = [("Withdrawals", "Deposits"), ("Debits", "Credits"), ("Payments", "Deposits"),
              ("Money out", "Money in"), ("Debit", "Credit"), ("Withdrawals", "Credits")]

LAYOUTS = {
    "sep": ["date", "details", "debit", "credit", "balance"],
    "sep_in_first": ["date", "details", "credit", "debit", "balance"],
    "sep_type": ["date", "type", "details", "debit", "credit", "balance"],
    "sep_ref": ["date", "details", "ref", "debit", "credit", "balance"],
    "signed": ["date", "details", "amount", "balance"],
    "signed_ref": ["date", "details", "ref", "amount", "balance"],
    "nobal_sep": ["date", "details", "ref", "debit", "credit"],
    "nobal_signed": ["date", "type", "details", "amount"],
    "card": ["date", "details", "amount"],
    "card_ref": ["date", "ref", "details", "amount"],
}
MAIN_DATES = ["dd/mm/yyyy", "dd Mon", "dd Mon yyyy", "d Mon", "dd-Mon-yy", "dd/mm/yy", "dd MON yy",
              "d/mm/yyyy", "dd MON", "Dow dd Mon", "d Month yyyy", "dd-Mon-yyyy"]

# ---------------------------------------------------------------------------
# Recipes: "kind@pos" with pos front|top|mid|below|back. Front and back matter is
# then topped up with filler decoys to the case's front / back page targets.
# ---------------------------------------------------------------------------

CASES = []


def C(theme, bank, decoys, **kw):
    kw.update(name="dc%03d_%s_%s" % (len(CASES) + 1, bank, theme), bank=bank,
              decoys=[tuple(t.split("@")) for t in decoys.split()])
    CASES.append(kw)


C("summary_pending", "anz", "account_summary@top pending@below")
C("rates_loan", "anz", "rate_change@front loan_schedule@back")
C("heavy_letter_rates", "anz", "cover_letter@front rate_change@front account_summary@top "
  "fees_charged@below standing_orders@back terms@back", heavy=True)
C("card_rewards_fx", "anz", "card_min_payment@top rewards_points@below fx_rates@back", card=True)
C("linked_fees", "anz", "linked_ministatement@below fee_schedule@back", n=34)
C("multi_interest", "anz", "account_summary@top interest_by_month@back", multi=True)
C("td_between", "anz", "term_deposit@mid marketing@back", n=40)
C("design_loan_between", "anz", "new_design@front loan_schedule@mid", n=36)
C("summary_rates_top", "asb", "account_summary@top rate_change@top", dgroup=True)
C("heavy_rate_pile", "asb", "cover_letter@front rate_change@front term_deposit@front fx_rates@front "
  "fee_schedule@front account_summary@top", heavy=True)
C("pending_linked", "asb", "pending@below linked_ministatement@below")
C("loan_front_orders", "asb", "loan_schedule@front standing_orders@below", layout="sep_type")
C("card_minpay_fees", "asb", "card_min_payment@below rewards_points@back fees_charged@back", card=True)
C("interest_rwt", "asb", "interest_by_month@top rwt_summary@back", layout="signed")
C("spending_design", "asb", "spending_summary@below new_design@back")
C("td_pending", "asb", "term_deposit@top pending@below", dgroup=True)
C("rates_fees", "bnz", "rate_change@top fee_schedule@below", layout="sep_ref")
C("heavy_everything_after", "bnz", "account_summary@top linked_ministatement@below pending@below "
  "loan_schedule@back term_deposit@back rwt_summary@back", heavy=True, n=30)
C("marketing_linked_between", "bnz", "marketing@front linked_ministatement@mid", n=44)
C("orders_fees", "bnz", "standing_orders@top fees_charged@below", layout="signed_ref")
C("multi_pending", "bnz", "account_summary@top pending@below", multi=True)
C("card_fx", "bnz", "card_min_payment@top fx_rates@below", card=True, layout="card_ref")
C("loan_chain_above", "bnz", "loan_schedule@top", n=22)
C("letter_interest", "bnz", "cover_letter@front interest_by_month@below", layout="nobal_sep")
C("summary_loan", "westpac", "account_summary@top loan_schedule@below")
C("heavy_design_mid", "westpac", "new_design@front account_summary@top rate_change@mid "
  "standing_orders@below pending@below fee_schedule@back terms@back", heavy=True, n=42)
C("td_rwt", "westpac", "term_deposit@below rwt_summary@below", layout="sep_in_first")
C("linked_above", "westpac", "linked_ministatement@top", n=24)
C("fx_spending", "westpac", "fx_rates@top spending_summary@below", page="Letter")
C("card_heavyish", "westpac", "card_min_payment@top rewards_points@top fees_charged@below "
  "marketing@back", card=True)
C("rates_pending", "westpac", "rate_change@front pending@below", overdrawn=True)
C("loan_between_fees", "westpac", "loan_schedule@mid fees_charged@back", n=46, bf=True)
C("summary_interest", "kiwibank", "account_summary@top interest_by_month@below", layout="signed")
C("rates_td", "kiwibank", "rate_change@top term_deposit@below")
C("heavy_letter_loan", "kiwibank", "cover_letter@front rate_change@front loan_schedule@front "
  "account_summary@top pending@below linked_ministatement@back terms@back", heavy=True)
C("orders_linked", "kiwibank", "standing_orders@front linked_ministatement@below", dgroup=True)
C("multi_rates", "kiwibank", "rate_change@front rwt_summary@back", multi=True)
C("spending_fees", "kiwibank", "spending_summary@top fee_schedule@below", page="A4L", layout="sep_type")
C("design_pending", "kiwibank", "new_design@front pending@below", layout="signed")
C("card_minpay_fx", "kiwibank", "card_min_payment@below fx_rates@back", card=True)
C("summary_td", "tsb", "account_summary@top term_deposit@top")
C("loan_rwt", "tsb", "loan_schedule@back rwt_summary@back", layout="nobal_signed")
C("heavy_tsb", "tsb", "rate_change@front fee_schedule@front account_summary@top fees_charged@below "
  "standing_orders@below interest_by_month@back marketing@back", heavy=True)
C("pending_linked_back", "tsb", "pending@below linked_ministatement@back", bf=True, n=58)
C("letter_orders", "tsb", "cover_letter@front standing_orders@top")
C("td_between_rates", "tsb", "term_deposit@mid rate_change@back", n=38, layout="sep_ref")
C("summary_fx", "coop", "account_summary@top fx_rates@below", page="Letter")
C("loan_pending", "coop", "loan_schedule@top pending@below", layout="signed")
C("heavy_coop", "coop", "account_summary@top rate_change@top term_deposit@below loan_schedule@below "
  "rwt_summary@back fee_schedule@back", heavy=True)
C("linked_between_spending", "coop", "linked_ministatement@mid spending_summary@back", n=40)
C("design_interest", "coop", "new_design@front interest_by_month@top", dgroup=True)
C("card_rewards", "coop", "card_min_payment@top rewards_points@below", card=True)
C("summary_rates_below", "rimu", "account_summary@top rate_change@below", layout="sep_in_first")
C("loan_td_front", "rimu", "loan_schedule@front term_deposit@front")
C("heavy_rimu", "rimu", "marketing@front cover_letter@front account_summary@top "
  "linked_ministatement@below pending@below standing_orders@back loan_schedule@back", heavy=True)
C("fees_both", "rimu", "fees_charged@top fee_schedule@below", overdrawn=True)
C("multi_linked", "rimu", "account_summary@top linked_ministatement@below", multi=True)
C("interest_rwt_front", "rimu", "interest_by_month@front rwt_summary@front", page="Letter")
C("pending_loan_between", "rimu", "pending@below loan_schedule@mid", n=40, bf=True)
C("card_all", "rimu", "card_min_payment@below rewards_points@back fx_rates@back", card=True)
C("summary_orders", "totara", "account_summary@top standing_orders@below", layout="nobal_sep")
C("heavy_totara", "totara", "rate_change@front term_deposit@front fx_rates@front "
  "interest_by_month@front account_summary@top pending@below", heavy=True)
C("linked_rates", "totara", "linked_ministatement@below rate_change@back", layout="signed_ref")
C("loan_spending", "totara", "loan_schedule@below spending_summary@below", page="A4L")
C("summary_fees", "kahu", "account_summary@top fees_charged@below", dgroup=True)
C("design_linked", "kahu", "new_design@front linked_ministatement@below")
C("td_loan", "kahu", "term_deposit@top loan_schedule@back", layout="sep_type")
C("heavy_kahu", "kahu", "cover_letter@front account_summary@top rate_change@mid "
  "linked_ministatement@below fees_charged@below fee_schedule@back terms@back", heavy=True, n=40)
C("spending_pending", "anz", "spending_summary@top pending@below", layout="signed")
C("multi_rates_top", "asb", "rate_change@top interest_by_month@back", multi=True)
C("rates_between_linked", "bnz", "rate_change@mid linked_ministatement@below", n=42)
C("multi_loan", "westpac", "account_summary@top loan_schedule@back", multi=True)
C("loan_below_fx", "kiwibank", "loan_schedule@below fx_rates@back", dgroup=True)
C("card_fees", "tsb", "card_min_payment@top fees_charged@below rewards_points@back", card=True)
C("orders_td", "coop", "standing_orders@top term_deposit@back", layout="sep_ref")
C("three_top", "rimu", "account_summary@top rate_change@top pending@below")
C("marketing_loan_top", "asb", "marketing@front loan_schedule@top")
C("design_td_rwt", "bnz", "new_design@front term_deposit@below rwt_summary@back", overdrawn=True)
C("terms_first", "westpac", "terms@front account_summary@top", layout="signed")
C("pending_above", "kiwibank", "pending@top linked_ministatement@below")

FULLPAGE = {"cover_letter", "terms", "marketing", "new_design", "blank_page", "complaints"}
FRONT_FILL = ["account_summary", "rate_change", "important_changes", "term_deposit", "loan_schedule",
              "fee_schedule", "standing_orders", "account_overview", "rewards_points", "fx_rates",
              "interest_by_month", "pending", "spending_summary", "rwt_summary"]
CARD_FRONT_FILL = ["card_min_payment", "rewards_points", "fx_rates", "important_changes",
                   "account_overview", "fee_schedule", "rate_change", "standing_orders"]
BACK_FILL = ["terms", "fee_schedule", "complaints", "marketing", "linked_ministatement",
             "rwt_summary", "blank_page", "interest_by_month", "term_deposit", "loan_schedule",
             "important_changes", "fx_rates"]

# ---------------------------------------------------------------------------
# Style and data
# ---------------------------------------------------------------------------

DEFAULTS = dict(layout=None, page=None, font=None, size=None, dfmt=None, n=None, multi=False,
                card=False, dgroup=False, bf=None, overdrawn=False, heavy=False)


def resolve(spec, attempt, shrink):
    S = dict(DEFAULTS)
    S.update(spec)
    S["attempt"] = attempt
    rng = random.Random(zlib.crc32(("%s:%d" % (spec["name"], attempt)).encode()))
    S["rng"] = rng
    idx = int(spec["name"][2:5])
    S["front_target"] = [2, 3, 2, 3, 4, 2, 3][idx % 7]
    S["back_target"] = 1 + (idx * 3) % 5
    B = BANKS[S["bank"]]
    S["B"] = B
    S["page"] = S["page"] or rng.choice(["A4", "A4", "A4", "A4", "Letter"])
    S["font"] = S["font"] or rng.choice(["Helvetica", "Helvetica", "Helvetica", "Times", "Courier"])
    S["size"] = S["size"] or rng.choice([7.5, 8, 8, 8.5, 9])
    if S["font"] == "Courier":
        S["size"] = min(S["size"], 8)
    S["dsize"] = max(6.5, S["size"] + rng.choice([-1, -0.5, 0, 0]))
    if S["layout"] is None:
        S["layout"] = ("card" if S["card"] else
                       rng.choice(["sep", "sep", "sep", "sep_type", "sep_ref", "signed", "sep_in_first",
                                   "signed_ref"]))
    S["cols"] = list(LAYOUTS[S["layout"]])
    S["dfmt"] = S["dfmt"] or rng.choice(MAIN_DATES)
    S["ddfmt"] = S["dfmt"] if rng.random() < 0.65 else rng.choice(MAIN_DATES)
    M = dict(G.M_DEFAULT)
    M["thou"] = rng.choice([",", ",", ",", ",", ""])
    M["cur"] = rng.choice(["", "", "", "$"])
    M["neg"] = rng.choice(["lead", "lead", "paren", "trail"])
    if "amount" in S["cols"] and not S["card"] and rng.random() < 0.35:
        M["neg"], M["tok"] = "suf", rng.choice([["", "DR"], ["CR", "DR"]])
    MB = dict(M)
    MB["neg"], MB["tok"] = rng.choice([("lead", ["CR", "DR"]), ("suf", ["", "OD"]),
                                       ("suf", ["CR", "DR"]), ("lead", ["CR", "DR"])])
    S["M"], S["MB"] = M, MB
    S["card_style"] = rng.choice(["cr", "minus"])
    S["mstyle"] = rng.choice(["band", "rule", "plain", "band"])
    S["dstyle"] = S["mstyle"] if rng.random() < 0.5 else rng.choice(["band", "rule", "box", "plain"])
    S["rules"] = rng.choice(["none", "none", "rows", "zebra"])
    S["bankpos"] = rng.choice(["left", "right", "left"])
    if S["bf"] is None:
        S["bf"] = rng.random() < 0.3
    S["dated_open"] = rng.random() < 0.4
    S["totline"] = rng.random() < 0.35
    S["partway"] = rng.random() < 0.45
    PW, PH = PAGES[S["page"]]
    S["PW"], S["PH"] = PW, PH
    S["ml"] = rng.uniform(34, 52)
    S["mr"] = rng.uniform(34, 52)
    S["n"] = S["n"] or rng.randint(12, 34)
    S["n"] = max(8, int(S["n"] * shrink))
    S["holder"] = "%s %s %s" % (rng.choice(TITLES), rng.choice(FIRST), rng.choice(LAST))
    S["addr"] = ["%d %s" % (rng.randint(1, 240), rng.choice(STREETS)),
                 "%s %04d" % (rng.choice(TOWNS), rng.randint(1000, 9899))]
    code = rng.choice(B["codes"])
    S["acct"] = "%s-%04d-%07d-%s" % (code, rng.randint(1, 9999), rng.randint(0, 9999999),
                                     rng.choice(["00", "000", "01", "001", "02"]))
    S["product"] = rng.choice(PRODUCTS["card" if S["card"] else "bank"])
    if S["card"]:
        S["acct"] = "%04d %04d %04d %04d" % (rng.randint(4000, 5599), rng.randint(1000, 9999),
                                             rng.randint(1000, 9999), rng.randint(0, 9999))
    kinds = [k for k, _ in S["decoys"]]
    S["linked"] = None
    if "linked_ministatement" in kinds or S["multi"]:
        S["linked"] = dict(product=rng.choice(PRODUCTS["savings"]),
                           acct="%s-%04d-%07d-%s" % (code, rng.randint(1, 9999), rng.randint(0, 9999999),
                                                     rng.choice(["01", "002", "50"])))
    return S


def fill(rng, t):
    return t.format(mer=rng.choice(MERCH), town=rng.choice(TOWNS).upper(), bil=rng.choice(BILLERS),
                    per=rng.choice(PEOPLE), emp=rng.choice(EMPLOYERS))


def period(S, rng):
    y = rng.choice([2025, 2026])
    m = rng.randint(1, 12) if y == 2025 else rng.randint(1, 8)
    sd = rng.choice([1, 1, 1, 15])
    start = dt.date(y, m, sd)
    end = ML.add_months(start, 1) - dt.timedelta(days=1)
    return start, end


def pick_dates(rng, start, end, n):
    days = list(ML.daterange(start, end))
    dates = sorted(rng.choice(days) for _ in range(n))
    if not any(d.day > 12 for d in dates):
        dates[-1] = max(d for d in days if d.day > 12)
        dates.sort()
    return dates


def gen_bank_rows(S, rng, start, end, n, has_type, linked):
    dates = pick_dates(rng, start, end, n)
    n_in = max(2, int(round(n * rng.uniform(0.2, 0.33))))
    dirs = ["in"] * n_in + ["out"] * (n - n_in)
    rng.shuffle(dirs)
    rows = []
    for d, di in zip(dates, dirs):
        pool = IN_T if di == "in" else OUT_T
        code, pre, body, lo, hi, _ = rng.choices(pool, weights=[p[5] for p in pool])[0]
        c = rng.randint(lo, hi)
        if code == "ATM":
            c = max(2000, c // 2000 * 2000)
        b = fill(rng, body)
        if has_type:
            texts = dict(type=code, details=(b or pre))
        else:
            texts = dict(details=(pre + " " + b).strip())
        if "ref" in S["cols"]:
            texts["ref"] = rng.choice(["", "", "INV %04d" % rng.randint(1, 9999), "%06d" % rng.randint(1, 999999),
                                       "RENT", "WK %d" % rng.randint(1, 52), "CARD %04d" % rng.randint(1, 9999),
                                       "0%d-%04d" % (rng.randint(1, 9), rng.randint(1, 9999))])
        rows.append(dict(date=d, dir=di, c=c, texts=texts, mirror=False))
    if linked and rng.random() < 0.8:
        tail = linked["acct"][-6:]
        for k in rng.sample(range(len(rows)), min(len(rows), rng.randint(1, 2))):
            r = rows[k]
            r["mirror"] = True
            r["c"] = rng.randint(50, 600) * 100
            word = "TRANSFER TO" if r["dir"] == "out" else "TRANSFER FROM"
            if has_type:
                r["texts"] = dict(type="TFR", details="%s %s" % (word, tail))
            else:
                r["texts"] = dict(details="%s %s %s" % (word, linked["product"].upper(), tail))
    return rows


def gen_savings_rows(S, rng, start, end, n):
    dates = pick_dates(rng, start, end, n)
    rows = []
    for d in dates:
        di, code, txt, lo, hi = rng.choice(SAV_T)
        rows.append(dict(date=d, dir=di, c=rng.randint(lo, hi), texts=dict(type=code, details=txt),
                         mirror=False))
    return rows


def gen_card_rows(S, rng, start, end, n):
    dates = pick_dates(rng, start, end, n)
    rows = []
    pay_at = rng.randint(1, max(1, n // 2))
    for i, d in enumerate(dates):
        if i == pay_at:
            rows.append(dict(date=d, dir="in", c=0, texts=dict(details=rng.choice(
                ["PAYMENT RECEIVED - THANK YOU", "DIRECT DEBIT PAYMENT - THANK YOU"])), pay=True))
            continue
        k = rng.random()
        if k < 0.07:
            t, di, c = "REFUND " + rng.choice(MERCH), "in", rng.randint(900, 9000)
        elif k < 0.12:
            t, di, c = rng.choice(["INTEREST CHARGED ON PURCHASES", "OVERSEAS TRANSACTION FEE"]), "out", \
                rng.randint(150, 4500)
        else:
            t = rng.choice(["{mer} {town} NZ", "ONLINE {mer}", "{mer}"])
            t, di, c = fill(rng, t), "out", rng.randint(400, 28000)
        rows.append(dict(date=d, dir=di, c=c, texts=dict(details=t)))
    for r in rows:
        r["mirror"] = False
        if "ref" in S["cols"]:
            r["texts"]["ref"] = "%011d" % rng.randint(10 ** 9, 10 ** 11 - 1)
    return rows


def gen_data(S):
    rng = S["rng"]
    start, end = period(S, rng)
    has_type = "type" in S["cols"]
    accts = []
    if S["card"]:
        rows = gen_card_rows(S, rng, start, end, S["n"])
        owed0 = rng.randint(30000, 400000)
        spend = sum(r["c"] for r in rows if r["dir"] == "out")
        pay = next(r for r in rows if r.get("pay"))
        pay["c"] = max(2000, min(owed0, owed0 - rng.randint(0, owed0 // 3)))
        opening = -owed0
        accts.append(dict(kind="card", product=S["product"], acct=S["acct"], rows=rows, opening=opening))
        del spend
    else:
        rows = gen_bank_rows(S, rng, start, end, S["n"], has_type, S["linked"] if not S["multi"] else None)
        accts.append(dict(kind="bank", product=S["product"], acct=S["acct"], rows=rows))
        if S["multi"]:
            L = S["linked"]
            r2 = gen_savings_rows(S, rng, start, end, rng.randint(4, 9))
            if not has_type:
                for r in r2:
                    r["texts"] = dict(details=r["texts"]["details"])
            accts.append(dict(kind="savings", product=L["product"], acct=L["acct"], rows=r2))
            S["linked"] = None    # the savings account is genuine here, not a decoy
    for a in accts:
        if a["kind"] == "card":
            o = a["opening"]
        else:
            run, lo = 0, 0
            for r in a["rows"]:
                run += r["c"] if r["dir"] == "in" else -r["c"]
                lo = min(lo, run)
            if S["overdrawn"] and a is accts[0]:
                o = rng.randint(2000, 40000)
                big = next(r for r in a["rows"][len(a["rows"]) // 4:] if r["dir"] == "out")
                pre = sum(r["c"] if r["dir"] == "in" else -r["c"]
                          for r in a["rows"][:a["rows"].index(big)])
                big["c"] = max(big["c"], o + pre + rng.randint(20000, 120000))
                if big["texts"].get("details", "").startswith(("EFTPOS", "POS", "VISA")) or \
                        big["texts"].get("type") in ("EFTPOS", "POS", "VISA"):
                    b = rng.choice(BILLERS)
                    big["texts"] = (dict(type="BILL PMT", details=b) if has_type
                                    else dict(details="BILL PAYMENT " + b))
            else:
                o = rng.randint(20000, 900000) + (-lo if lo < 0 else 0)
        a["opening"] = o
        bal = o
        for r in a["rows"]:
            r["sa"] = r["c"] if r["dir"] == "in" else -r["c"]
            bal += r["sa"]
            r["bal"] = bal
        a["closing"] = bal
        a["tin"] = sum(r["c"] for r in a["rows"] if r["dir"] == "in")
        a["tout"] = sum(r["c"] for r in a["rows"] if r["dir"] == "out")
    if S["card"] and accts[0]["closing"] > 0:
        raise Retry("card ended in credit")
    taken = set()
    for a in accts:
        for r in a["rows"]:
            taken.add((r["date"].isoformat(), r["c"]))
    return dict(start=start, end=end, accounts=accts, taken=taken)


# ---------------------------------------------------------------------------
# Drawing blocks
# ---------------------------------------------------------------------------

class Block:
    def __init__(self, h, draw, kind=None, newpage=False, w=None):
        self.h, self.draw, self.kind, self.newpage, self.w = float(h), draw, kind, newpage, w
        self.meta = None
        self.pages = []


class Ctx:
    """What a decoy builder needs: fonts, formats, the period, the real data."""

    def __init__(self, S, D):
        self.S, self.D = S, D
        self.reg, self.bold = ML.FONTS[S["font"]]
        self.size = S["dsize"]
        self.tstyle = S["dstyle"]
        self.taken = D["taken"]
        self.period = ML.period_text("long", D["start"], D["end"])
        self.CW = S["PW"] - S["ml"] - S["mr"]

    def money(self, c, cur=None):
        M = dict(self.S["M"])
        if cur is not None:
            M["cur"] = cur
        return G.fmt_money(abs(c), M, unsigned=True)

    def smoney(self, c):
        return G.fmt_money(c, self.S["MB"])

    def date(self, d, fmt=None):
        return DATES[fmt or self.S["ddfmt"]](d)

    def lond(self, d):
        return "%d %s %d" % (d.day, MONTH[d.month - 1], d.year)

    def pct(self, r, pa=True):
        return "%.2f%%%s" % (r, " p.a." if pa else "")

    def fresh(self, d, c):
        while (d.isoformat(), c) in self.taken:
            c += 1
        return c


def table(C, x, W, title, heads, rows, aligns, intro=None, foot=None, total=None, size=None,
          compact=True, style=None, bold_rows=()):
    size = size or C.size
    style = style or C.tstyle
    reg, bold = C.reg, C.bold
    n = len(heads)
    hl = [h.split("\n") for h in heads]
    allr = [list(r) for r in rows] + ([list(total)] if total else [])
    for r in allr:
        if len(r) != n:
            raise GenError("table %r: row has %d cells, want %d" % (title, len(r), n))
    gap = max(9.0, size * 1.5)
    pad = 6.0 if style == "box" else 0.0
    iw = W - 2 * pad
    nat, mn = [], []
    for j in range(n):
        hw = max(stringWidth(t, bold, size) for t in hl[j])
        cw = [stringWidth(r[j], bold if (total and i == len(allr) - 1) or i in bold_rows else reg, size)
              for i, r in enumerate(allr) if r[j]]
        nat.append(max([hw] + cw) + 1.0)
        if aligns[j] == "L":
            ws = [stringWidth(w, bold, size) for r in allr for w in r[j].split()]
            mn.append(max([hw] + ws) + 1.0)
        else:
            mn.append(nat[-1])
    wid = list(nat)
    need = sum(wid) + gap * (n - 1)
    if need > iw:
        ex = need - iw
        sl = [wid[j] - mn[j] for j in range(n)]
        if sum(sl) < ex:
            raise GenError("table %r does not fit in %.0fpt" % (title, iw))
        tot = sum(sl)
        wid = [wid[j] - ex * sl[j] / tot for j in range(n)]
    elif not compact:
        js = [j for j in range(n) if aligns[j] == "L"]
        if js:
            wid[js[0]] += iw - need
        elif n > 1:
            gap += (iw - need) / (n - 1)
    xs, cx = [], x + pad
    for j in range(n):
        xs.append(cx)
        cx += wid[j] + gap
    tw = cx - gap - x - pad
    bw = tw + 2 * pad
    cells = []
    for i, r in enumerate(allr):
        fn = bold if (total and i == len(allr) - 1) or i in bold_rows else reg
        cells.append([G.wrap(r[j], wid[j] + 0.5, fn, size) if (aligns[j] == "L" and r[j]) else [r[j]]
                      for j in range(n)])
    lead, rg = size * 1.22, size * 0.36
    tsz = size + 1.5
    tw_text = bw if style == "box" else max(bw, min(W, 380))
    tl = G.wrap(title, tw_text - 2 * pad, bold, tsz) if title else []
    il = G.wrap(intro, tw_text - 2 * pad, reg, size) if intro else []
    fsz = size - 1
    fl = G.wrap(foot, tw_text - 2 * pad, reg, fsz) if foot else []
    nh = max(len(t) for t in hl)
    hh = nh * size * 1.18 + 5
    rh = [max(len(c) for c in cr) * lead + rg for cr in cells]
    h = pad + len(tl) * tsz * 1.35 + (2 if tl else 0) + len(il) * size * 1.3 + (3 if il else 0)
    h += hh + 2 + sum(rh) + (3 if total else 0)
    h += (4 + len(fl) * fsz * 1.3) if fl else 0
    h += 5 + pad

    def draw(sh, y0):
        y = y0 + pad
        for t in tl:
            sh.text(x + pad, y + tsz, t, tsz, bold=True)
            y += tsz * 1.35
        if tl:
            y += 2
        for ln in il:
            sh.text(x + pad, y + size, ln, size)
            y += size * 1.3
        if il:
            y += 3
        if style == "band":
            sh.rect(x + pad - 2, y, tw + 4, hh, fill=GREY)
        elif style in ("rule", "box"):
            sh.line(x + pad, y + hh, x + pad + tw, y + hh, width=0.5)
        for j in range(n):
            for k, t in enumerate(hl[j]):
                bl = y + (nh - len(hl[j]) + k + 1) * size * 1.18
                if aligns[j] == "L":
                    sh.text(xs[j], bl, t, size, bold=True)
                else:
                    sh.text(xs[j] + wid[j], bl, t, size, bold=True, align="right")
        y += hh + 2
        for i, cr in enumerate(cells):
            is_tot = bool(total) and i == len(cells) - 1
            if is_tot:
                sh.line(x + pad, y + 1, x + pad + tw, y + 1, width=0.4)
                y += 3
            b = is_tot or i in bold_rows
            for j, lines in enumerate(cr):
                for k, t in enumerate(lines):
                    if not t:
                        continue
                    bl = y + size * 0.95 + k * lead
                    if aligns[j] == "L":
                        sh.text(xs[j], bl, t, size, bold=b)
                    else:
                        sh.text(xs[j] + wid[j], bl, t, size, bold=b, align="right")
            y += rh[i]
        if fl:
            y += 4
            for ln in fl:
                sh.text(x + pad, y + fsz, ln, fsz)
                y += fsz * 1.3
        if style == "box":
            sh.rect(x, y0, bw, h - 1, stroke=MIDG, width=0.6)
    return Block(h, draw, w=bw)


def kv_block(C, x, W, title, pairs, size=None, box=True):
    size = size or C.size
    lh = size * 1.45
    pad = 6.0
    vw = max(stringWidth(v, C.reg, size) for _, v in pairs)
    lw = max(stringWidth(k, C.reg, size) for k, _ in pairs)
    tw = stringWidth(title, C.bold, size + 1) if title else 0
    bw = min(W, max(lw + vw + 24, tw + 12) + 2 * pad)
    if lw + vw + 4 > bw - 2 * pad:
        raise GenError("kv box %r too narrow" % title)
    h = pad + (size * 1.7 if title else 0) + len(pairs) * lh + pad

    def draw(sh, y0):
        y = y0 + pad
        if title:
            sh.text(x + pad, y + size + 1, title, size + 1, bold=True)
            y += size * 1.7
        for k, v in pairs:
            sh.text(x + pad, y + size, k, size)
            sh.text(x + bw - pad, y + size, v, size, align="right")
            y += lh
        if box:
            sh.rect(x, y0, bw, h - 1, stroke=MIDG, width=0.6)
    return Block(h, draw, w=bw)


def para_block(C, x, W, paras, size=None, title=None, tsize=None, gap=None):
    size = size or C.size
    tsize = tsize or size + 3
    gap = size * 0.7 if gap is None else gap
    lines = []
    if title:
        for t in G.wrap(title, W, C.bold, tsize):
            lines.append((t, tsize, True, 0.0))
    for p in paras:
        bold = p.startswith("**")
        p = p.lstrip("*")
        for k, t in enumerate(G.wrap(p, W, C.bold if bold else C.reg, size)):
            lines.append((t, size, bold, gap if k == 0 and lines else 0.0))
    h = sum(sz * 1.3 + g for _, sz, _, g in lines) + 3

    def draw(sh, y0):
        y = y0
        for t, sz, b, g in lines:
            y += g
            sh.text(x, y + sz, t, sz, bold=b)
            y += sz * 1.3
    return Block(h, draw, w=W)


def stack(blocks, gap=8.0, kind=None, newpage=False):
    h = sum(b.h for b in blocks) + gap * (len(blocks) - 1)

    def draw(sh, y0):
        y = y0
        for b in blocks:
            b.draw(sh, y)
            y += b.h + gap
    return Block(h, draw, kind=kind, newpage=newpage)


def side_by_side(b1, b2):
    def draw(sh, y0):
        b1.draw(sh, y0)
        b2.draw(sh, y0)
    return Block(max(b1.h, b2.h), draw)


# ---------------------------------------------------------------------------
# Decoy builders: (C, rng, x, W) -> Block with .meta
# ---------------------------------------------------------------------------

def meta(kind, title, rows=0, chains=False, dated=(), probes=()):
    return dict(kind=kind, title=title, rows=rows, chains=chains,
                dated_rows=[dict(date=d.isoformat(), amounts=[int(a) for a in am], mirror=bool(mi))
                            for d, am, mi in dated],
                probes=[p for p in probes if p][:4])


def other_accounts(C, r, k):
    out = []
    pool = ["savings", "card", "loan", "td", "savings2"]
    r.shuffle(pool)
    for kind in pool[:k]:
        if kind.startswith("savings"):
            o = r.randint(50000, 2500000)
            i, u = r.randint(100, 120000), r.randint(0, 60000)
            have = {a["product"] for a in C.D["accounts"]} | {z["product"] for z in out}
            name = r.choice([p for p in PRODUCTS["savings"] if p not in have] or ["Kids Saver"])
        elif kind == "card":
            o = -r.randint(20000, 400000)
            i, u = r.randint(10000, 200000), r.randint(10000, 300000)
            name = r.choice(PRODUCTS["card"])
        elif kind == "loan":
            o = -r.randint(15000000, 65000000)
            i, u = r.randint(150000, 400000), r.randint(60000, 300000)
            name = r.choice(["Home Loan", "Fixed Rate Home Loan", "Floating Home Loan"])
        else:
            o = r.randint(1000000, 8000000)
            i, u = r.randint(0, 40000), 0
            name = "Term Deposit"
        acct = "%s-%04d-%07d-%02d" % (C.S["acct"][:2] if not C.S["card"] else "12", r.randint(1, 9999),
                                      r.randint(0, 9999999), r.randint(0, 90))
        out.append(dict(product=name, acct=acct, opening=o, tin=i, tout=u, closing=o + i - u))
    return out


def d_account_summary(C, r, x, W):
    D = C.D
    accts = [dict(product=a["product"], acct=a["acct"], opening=a["opening"], tin=a["tin"],
                  tout=a["tout"], closing=a["closing"]) for a in D["accounts"]]
    accts += other_accounts(C, r, r.randint(1, 3))
    hv = r.choice([("Opening\nbalance", "Money in", "Money out", "Closing\nbalance"),
                   ("Balance at\nstart", "Total\ncredits", "Total\ndebits", "Balance at\nend"),
                   ("Opening balance", "Deposits", "Withdrawals", "Closing balance")])
    title = r.choice(["Summary of your accounts %s to %s" % C.period, "Your accounts at a glance",
                      "Account summary for this period"])
    rows = [[a["product"], a["acct"], C.smoney(a["opening"]), C.money(a["tin"]), C.money(a["tout"]),
             C.smoney(a["closing"])] for a in accts]
    tot = None
    if r.random() < 0.5:
        net_o = sum(a["opening"] for a in accts)
        net_c = sum(a["closing"] for a in accts)
        tot = ["Net position", "", C.smoney(net_o), "", "", C.smoney(net_c)]
    b = table(C, x, W, title, ["Account", "Account number"] + list(hv), rows, "LLRRRR", total=tot,
              compact=r.random() < 0.5)
    b.meta = meta("account_summary", title, len(rows), probes=[rows[-1][2], rows[-1][5]])
    return b


def d_account_overview(C, r, x, W):
    D = C.D
    A = D["accounts"][0]
    accts = [dict(product=a["product"], acct=a["acct"], closing=a["closing"]) for a in D["accounts"]]
    accts += other_accounts(C, r, r.randint(2, 3))
    title = r.choice(["Your account overview", "Overview of your banking with us", "Your relationship summary"])
    rows = []
    for a in accts:
        lim = r.choice([0, 0, 100000, 500000, 1000000])
        avail = a["closing"] + lim
        rows.append([a["product"], a["acct"], C.smoney(a["closing"]), C.money(lim) if lim else "-",
                     C.smoney(avail), C.pct(r.choice([0.05, 0.10, 2.35, 3.10, 4.25, 6.49, 19.95, 22.95]),
                                            pa=False)])
    tb = table(C, x, W, title, ["Account", "Number", "Balance at\n%s" % C.date(D["end"]),
                                "Limit", "Available", "Interest\nrate"], rows, "LLRRRR",
               intro="Balances shown are as at the close of business on %s. Interest rates are per "
                     "annum and may change; see the notice in this pack." % C.lond(D["end"]),
               compact=False)
    pairs = [("Total deposits", C.money(sum(max(0, a["closing"]) for a in accts))),
             ("Total lending", C.money(sum(-min(0, a["closing"]) for a in accts))),
             ("Statement issued", C.lond(D["end"] + dt.timedelta(days=r.randint(1, 4))))]
    kb = kv_block(C, x, W, "At a glance", pairs)
    b = stack([tb, kb])
    b.meta = meta("account_overview", title, len(rows), probes=[rows[0][2], pairs[0][1]])
    return b


def d_rate_change(C, r, x, W):
    D = C.D
    eff = D["end"] + dt.timedelta(days=r.randint(5, 40)) if r.random() < 0.6 else \
        D["start"] + dt.timedelta(days=r.randint(3, 20))
    prod = r.choice(PRODUCTS["savings"] + [C.S["product"]])
    title = r.choice(["Changes to interest rates", "Interest rate update", "Your interest rates are changing",
                      "Important: new interest rates from %s" % C.lond(eff)])
    intro = ("From %s the interest rates on your %s will change as shown below. You do not need to do "
             "anything." % (C.lond(eff), prod))
    var = r.choice(["tiers", "history", "products"])
    dated, probes = [], []
    if var == "tiers":
        th = [0, r.choice([100000, 500000]), r.choice([1000000, 2000000]), 5000000, 10000000]
        th = th[:r.randint(3, 5)]
        rows = []
        base = r.uniform(0.5, 2.5)
        for k, lo in enumerate(th):
            hi = th[k + 1] - 1 if k + 1 < len(th) else None
            tier = ("%s - %s" % (C.money(lo, "$"), C.money(hi, "$")) if hi is not None
                    else "%s and over" % C.money(lo, "$"))
            old = base + k * r.uniform(0.2, 0.7)
            new = old + r.choice([-0.5, -0.25, 0.15, 0.25, 0.4])
            rows.append([tier, C.pct(old), C.pct(max(0.05, new)), C.date(eff)])
            dated.append((eff, [lo] + ([hi] if hi is not None else []), False))
        heads = ["Balance", "Current rate", "New rate", "Effective\ndate"]
        al = "LRRR"
        probes = [rows[0][3], rows[-1][1]]
    elif var == "history":
        rows = []
        d = eff
        rate = r.uniform(1.0, 5.5)
        for k in range(r.randint(4, 7)):
            bonus = r.choice([0.0, 0.5, 1.0, 1.25])
            rows.append([C.date(d), C.pct(rate, False), C.pct(bonus, False), C.pct(rate + bonus)])
            d = d - dt.timedelta(days=r.randint(25, 70))
            rate = max(0.1, rate + r.choice([-0.25, 0.25, 0.5, -0.5]))
        heads = ["Effective date", "Base rate", "Bonus rate", "Total rate"]
        al = "LRRR"
        title = r.choice(["Interest rate history - %s" % prod, title])
        probes = [rows[0][0], rows[-1][3]]
    else:
        rows = []
        for p in r.sample(PRODUCTS["savings"] + ["Term Deposit 6 months", "Term Deposit 12 months"], 4):
            lo = r.choice([0, 100000, 1000000, 500000])
            old = r.uniform(0.5, 5.2)
            dd = eff + dt.timedelta(days=r.choice([0, 0, 7, 14]))
            rows.append([p, "%s+" % C.money(lo, "$"), C.pct(old), C.pct(old + r.choice([-0.3, 0.2, 0.35])),
                         C.date(dd)])
            dated.append((dd, [lo], False))
        heads = ["Product", "Balance", "Old rate", "New rate", "From"]
        al = "LRRRL"
        probes = [rows[0][4], rows[1][3]]
    b = table(C, x, W, title, heads, rows, al, intro=intro,
              foot="Rates are variable and are subject to change. Interest is calculated daily and paid "
                   "monthly.", compact=r.random() < 0.6)
    b.meta = meta("rate_change", title, len(rows), dated=dated, probes=probes)
    return b


def d_important_changes(C, r, x, W):
    D = C.D
    title = r.choice(["Important changes to your account", "Changes you need to know about",
                      "Upcoming changes to fees and limits"])
    pool = [("Minimum balance to earn bonus interest", 100000, 500000),
            ("Daily ATM withdrawal limit", 100000, 150000), ("Daily EFTPOS spending limit", 200000, 300000),
            ("Free monthly transfers above", 25000, 50000), ("Overdraft review threshold", 200000, 250000),
            ("Unarranged overdraft fee", 1500, 1000), ("Monthly account fee", 500, 650),
            ("Large cash deposit notice above", 1000000, 500000), ("Contactless payment limit", 20000, 30000)]
    rows, dated = [], []
    for name, a, b in r.sample(pool, r.randint(4, 6)):
        d = D["end"] + dt.timedelta(days=r.randint(10, 75))
        rows.append([C.date(d), name, C.money(a, "$"), C.money(b, "$")])
        dated.append((d, [a, b], False))
    rows.sort(key=lambda z: z[0])
    tb = table(C, x, W, title, ["Effective\ndate", "What is changing", "Now", "From the\neffective date"],
               rows, "LLRR", intro="We are making the following changes. If you are unhappy with a change "
               "you can close your account without charge within 30 days of %s." % C.lond(D["end"]),
               compact=r.random() < 0.5)
    tb.meta = meta("important_changes", title, len(rows), dated=dated, probes=[rows[0][0], rows[0][2]])
    return tb


FEE_POOL = [("Monthly account fee", "per month", 500), ("Electronic transactions", "each", 0),
            ("Over-the-counter withdrawal", "each", 300), ("Overseas ATM withdrawal", "each", 350),
            ("Dishonour fee", "each", 1500), ("Bank cheque", "each", 1000),
            ("Statement reprint", "per copy", 500), ("International payment (outward)", "each", 2500),
            ("Unarranged overdraft fee", "per month", 1500), ("Coin deposit", "per bag", 300),
            ("Stop payment request", "each", 1000), ("Replacement card", "each", 1000),
            ("Account closure", "each", 0), ("Cash advance (credit card)", "each", 250)]


def d_fee_schedule(C, r, x, W):
    title = r.choice(["Standard fees and charges", "Our fees", "Fee schedule - effective %s"
                      % C.lond(C.D["start"] - dt.timedelta(days=r.randint(20, 300)))])
    items = r.sample(FEE_POOL, r.randint(5, 9))
    var = r.choice(["two", "three", "change"])
    if var == "two":
        rows = [[a, C.money(c, "$") if c else "Free"] for a, b, c in items]
        heads, al = ["Service", "Fee"], "LR"
    elif var == "three":
        rows = [[a, b, C.money(c, "$") if c else "Free"] for a, b, c in items]
        heads, al = ["Service", "How it is charged", "Fee"], "LLR"
    else:
        rows = [[a, C.money(c, "$") if c else "Free", C.money(c + r.choice([0, 50, 100, 250]), "$")]
                for a, b, c in items]
        heads, al = ["Service", "Current fee", "New fee"], "LRR"
    b = table(C, x, W, title, heads, rows, al, compact=r.random() < 0.6,
              foot="Fees are GST inclusive where applicable. A full list of fees is available at any branch.")
    b.meta = meta("fee_schedule", title, len(rows), probes=[rows[0][-1], rows[-1][0].split()[0]])
    return b


def d_fees_charged(C, r, x, W):
    D = C.D
    deb = D["end"] + dt.timedelta(days=r.randint(1, 5))
    title = r.choice(["Fees charged this period", "Fees for this statement period",
                      "Fees to be debited on %s" % C.lond(deb)])
    rows, dated, tot = [], [], 0
    days = list(ML.daterange(D["start"], D["end"]))
    for name, unit in r.sample([("Over-the-counter withdrawal", 300), ("Overseas ATM withdrawal", 350),
                                ("Dishonour fee", 1500), ("Electronic transaction", 50),
                                ("Paper statement fee", 200), ("Bank cheque", 1000), ("Coin deposit", 300)],
                               r.randint(2, 5)):
        d = r.choice(days)
        q = r.randint(1, 4) if unit < 1000 else 1
        amt = C.fresh(d, unit * q)
        unit_s = C.money(unit)
        rows.append([C.date(d), name, str(q), unit_s, C.money(amt)])
        dated.append((d, [amt, unit], False))
        tot += amt
    rows.sort(key=lambda z: z[0])
    b = table(C, x, W, title, ["Date", "Fee", "Number", "Fee each", "Amount"], rows, "LLRRR",
              intro="These fees will be debited from your account on %s and will appear on your next "
                    "statement." % C.lond(deb),
              total=["", "Total fees", "", "", C.money(tot)], compact=r.random() < 0.5)
    b.meta = meta("fees_charged", title, len(rows), dated=dated, probes=[rows[0][0], rows[0][4]])
    return b


def d_term_deposit(C, r, x, W):
    D = C.D
    if r.random() < 0.55:
        title = r.choice(["Your term deposits", "Term deposit holdings", "Term deposits held with us"])
        rows, dated = [], []
        for k in range(r.randint(2, 4)):
            start = D["start"] - dt.timedelta(days=r.randint(15, 320))
            term = r.choice([3, 6, 9, 12, 18, 24])
            mat = ML.add_months(start, term)
            rate = r.choice([3.85, 4.10, 4.25, 4.50, 4.65, 4.95, 5.10, 5.35])
            p = r.choice([500000, 1000000, 2000000, 2500000, 5000000, 7500000]) + r.randint(0, 99) * 100
            intr = int(round(p * rate / 100.0 * (mat - start).days / 365.0))
            rows.append(["TD-%06d" % r.randint(1, 999999), C.date(start), C.date(mat), "%d months" % term,
                         C.pct(rate, False), C.money(p), C.money(intr)])
            dated.append((start, [p, intr], False))
            dated.append((mat, [p, intr], False))
        b = table(C, x, W, title, ["Deposit no.", "Start date", "Maturity\ndate", "Term", "Rate p.a.",
                                   "Principal", "Interest at\nmaturity"], rows, "LLLLRRR",
                  foot="Interest shown is before resident withholding tax. Early withdrawal may incur an "
                       "interest adjustment.", compact=r.random() < 0.5)
        b.meta = meta("term_deposit", title, len(rows), dated=dated, probes=[rows[0][1], rows[0][5]])
        return b
    # activity with a chaining balance
    acct = "TD-%06d" % r.randint(1, 999999)
    title = "Term deposit %s - activity this period" % acct
    bal = r.choice([1000000, 2000000, 3500000, 5000000]) + r.randint(0, 999) * 100
    o = bal
    days = list(ML.daterange(D["start"], D["end"]))
    ds = sorted(r.sample(days, 3))
    rows, dated = [["", "Opening balance", "", "", C.money(bal)]], []
    rate = r.choice([4.10, 4.50, 4.95, 5.20])
    i1 = C.fresh(ds[0], int(round(bal * rate / 100 / 12)))
    rwt = int(round(i1 * r.choice([0.105, 0.175, 0.30, 0.33])))
    steps = [(ds[0], "Interest credited", i1), (ds[0], "Resident withholding tax", -rwt),
             (ds[1], "Additional deposit", r.randint(50, 500) * 1000), (ds[2], "Interest credited",
                                                                         int(round(bal * rate / 100 / 52)))]
    for d, t, v in steps:
        v = v if v < 0 else C.fresh(d, v)
        if v < 0 and (d.isoformat(), -v) in C.taken:
            raise Retry("td rwt collides")
        bal += v
        rows.append([C.date(d), t, C.money(-v) if v < 0 else "", C.money(v) if v > 0 else "", C.money(bal)])
        dated.append((d, [abs(v), bal], False))
    rows.append(["", "Closing balance", "", "", C.money(bal)])
    chain_check([o] + [s[2] for s in steps], bal, "term_deposit")
    b = table(C, x, W, title, ["Date", "Transaction", "Debit", "Credit", "Balance"], rows, "LLRRR",
              bold_rows=(0, len(rows) - 1), compact=r.random() < 0.5)
    b.meta = meta("term_deposit", title, len(rows) - 2, chains=True, dated=dated,
                  probes=[rows[1][0], rows[1][4]])
    return b


def chain_check(steps, end, what):
    if sum(steps) != end:
        raise GenError("%s decoy does not chain" % what)


def d_loan_schedule(C, r, x, W):
    D = C.D
    lacct = "%s-%04d-%07d-%02d" % ("12", r.randint(1, 9999), r.randint(0, 9999999), r.randint(50, 92))
    if r.random() < 0.7:
        P = r.randint(1500, 6500) * 10000 + r.randint(0, 9999) * 100
        rate = r.choice([5.49, 5.79, 6.15, 6.29, 6.75, 7.19])
        fort = r.random() < 0.3
        per = 26 if fort else 12
        i = rate / 100.0 / per
        N = 25 * per
        pay = int(round(P * i / (1 - (1 + i) ** -N)))
        d = (D["start"] + dt.timedelta(days=r.randint(2, 20))) if r.random() < 0.5 else \
            (D["end"] + dt.timedelta(days=r.randint(1, 20)))
        bal = P - r.randint(0, 40) * pay // 3
        b0 = bal
        rows, dated, princ = [], [], []
        k = r.randint(8, 16)
        for _ in range(k):
            it = int(round(bal * i))
            pr = pay - it
            bal -= pr
            princ.append(-pr)
            rows.append([C.date(d), C.money(pay), C.money(it), C.money(pr), C.money(bal)])
            dated.append((d, [pay, it, pr, bal], False))
            d = d + dt.timedelta(days=14) if fort else ML.add_months(d, 1)
        chain_check([b0] + princ, bal, "loan")
        title = r.choice(["Indicative repayment schedule - %s" % lacct, "Your home loan repayment schedule",
                          "Loan amortisation schedule (estimate)"])
        intro = ("Loan balance %s at %s fixed, %s repayments of %s. This schedule is an estimate only and "
                 "assumes no changes to your rate or repayments." % (C.money(b0, "$"), C.pct(rate),
                                                                     "fortnightly" if fort else "monthly",
                                                                     C.money(pay, "$")))
        heads = r.choice([["Payment\ndate", "Repayment", "Interest", "Principal", "Loan\nbalance"],
                          ["Date", "Payment", "Interest", "Principal", "Balance"]])
        b = table(C, x, W, title, heads, rows, "LRRRR", intro=intro, compact=r.random() < 0.5)
        b.meta = meta("loan_schedule", title, len(rows), chains=True, dated=dated,
                      probes=[rows[0][0], rows[-1][4]])
        return b
    # loan account activity: owed balance printed with DR, chaining
    title = "Home loan %s - transactions this period" % lacct
    owed = r.randint(1500, 6000) * 10000 + r.randint(0, 9999) * 100
    o = owed
    days = list(ML.daterange(D["start"], D["end"]))
    ds = sorted(r.sample(days, 4))
    pay = r.randint(900, 3200) * 100
    rows = [["", "Opening balance", "", "", C.money(owed) + " DR"]]
    dated, steps = [], []
    for k, d in enumerate(ds):
        if k % 2 == 0:
            v = C.fresh(d, pay)
            owed -= v
            rows.append([C.date(d), "LOAN REPAYMENT", "", C.money(v), C.money(owed) + " DR"])
            steps.append(-v)
        else:
            v = C.fresh(d, int(round(owed * 0.0625 / 12)))
            owed += v
            rows.append([C.date(d), "INTEREST", C.money(v), "", C.money(owed) + " DR"])
            steps.append(v)
        dated.append((d, [v, owed], False))
    rows.append(["", "Closing balance", "", "", C.money(owed) + " DR"])
    chain_check([o] + steps, owed, "loan activity")
    b = table(C, x, W, title, ["Date", "Transaction", "Debit", "Credit", "Balance"], rows, "LLRRR",
              bold_rows=(0, len(rows) - 1), compact=r.random() < 0.5)
    b.meta = meta("loan_schedule", title, len(ds), chains=True, dated=dated, probes=[rows[1][0], rows[1][4]])
    return b


def d_standing_orders(C, r, x, W):
    D = C.D
    title = r.choice(["Your automatic payments", "Regular payments from this account",
                      "Automatic payments and direct debits"])
    rows, dated = [], []
    for k in range(r.randint(3, 7)):
        freq = r.choice(["Weekly", "Fortnightly", "Monthly", "Monthly", "Quarterly"])
        start = D["start"] - dt.timedelta(days=r.randint(40, 900))
        nxt = D["end"] + dt.timedelta(days=r.randint(1, 30))
        amt = r.randint(10, 900) * 100 + r.choice([0, 0, 50, 99])
        rows.append([r.choice(PEOPLE + BILLERS), r.choice(["RENT", "POWER", "SAVINGS", "INSURANCE", "KOHA",
                                                           "SCHOOL FEES", "GYM"]),
                     freq, C.date(start), C.date(nxt), C.money(amt)])
        dated.append((start, [amt], False))
        dated.append((nxt, [amt], False))
    b = table(C, x, W, title, ["Paid to", "Reference", "Frequency", "First\npayment", "Next\npayment",
                               "Amount"], rows, "LLLLLR", compact=r.random() < 0.5,
              foot="To change or cancel an automatic payment, use online banking or call us.")
    b.meta = meta("standing_orders", title, len(rows), dated=dated, probes=[rows[0][4], rows[0][5]])
    return b


def d_pending(C, r, x, W):
    D, S = C.D, C.S
    title = r.choice(["Pending transactions", "Transactions not yet processed", "Uncleared items"])
    rows, dated = [], []
    sep = r.random() < 0.5
    for k in range(r.randint(2, 6)):
        d = D["end"] + dt.timedelta(days=r.randint(-3, 3))
        if S["card"]:
            t, di, c = fill(r, "{mer} {town} NZ"), "out", r.randint(500, 20000)
        else:
            di = "out" if r.random() < 0.8 else "in"
            code, pre, body, lo, hi, _ = r.choice(OUT_T if di == "out" else IN_T)
            t, c = (pre + " " + fill(r, body)).strip(), r.randint(lo, hi)
        c = C.fresh(d, c)
        if sep:
            rows.append([C.date(d), t, C.money(c) if di == "out" else "", C.money(c) if di == "in" else ""])
        else:
            rows.append([C.date(d), t, C.smoney(-c if di == "out" else c)])
        dated.append((d, [c], False))
    rows.sort(key=lambda z: z[0])
    intro = ("The following transactions had not been processed by %s and are not included in your "
             "closing balance. They will appear on your next statement." % C.lond(D["end"]))
    p1, p2 = PAIRS_MAIN[r.randrange(len(PAIRS_MAIN))]
    heads = (["Date", "Transaction details", p1, p2] if sep else ["Date", "Transaction details", "Amount"])
    b = table(C, x, W, title, heads, rows, "LLRR" if sep else "LLR", intro=intro, compact=r.random() < 0.4)
    b.meta = meta("pending", title, len(rows), dated=dated, probes=[rows[0][0], rows[0][2] or rows[0][3]])
    return b


def d_linked_ministatement(C, r, x, W):
    D, S = C.D, C.S
    L = S["linked"] or dict(product=r.choice(PRODUCTS["savings"]),
                            acct="%s-%04d-%07d-01" % (S["acct"][:2], r.randint(1, 9999), r.randint(0, 9999999)))
    title = r.choice(["Linked account: %s %s" % (L["product"], L["acct"]),
                      "%s %s - recent activity" % (L["product"], L["acct"])])
    intro = r.choice(["Shown for your information only. This account is reported in full on its own "
                      "statement and is not included in the balances above.",
                      "For information only - not part of this statement. See your separate %s statement."
                      % L["product"]])
    days = list(ML.daterange(D["start"], D["end"]))
    items = []
    tail = re.split(r"[- ]", S["acct"])[2 if "-" in S["acct"] else -1][-4:]
    for a in D["accounts"][:1]:
        for row in a["rows"]:
            if row.get("mirror"):
                items.append((row["date"], "in" if row["dir"] == "out" else "out", row["c"], True,
                              "TRANSFER %s %s ...%s" % ("FROM" if row["dir"] == "out" else "TO",
                                                        S["product"].upper(), tail)))
    for k in range(r.randint(2, 5)):
        d = r.choice(days)
        di, code, txt, lo, hi = r.choice(SAV_T)
        items.append((d, di, C.fresh(d, r.randint(lo, hi)), False, txt))
    items.sort(key=lambda z: z[0])
    bal = r.randint(100000, 3000000)
    run, lo = 0, 0
    for d, di, c, mi, t in items:
        run += c if di == "in" else -c
        lo = min(lo, run)
    bal += -lo
    o = bal
    rows = [["", "Opening balance", "", "", C.money(bal)]]
    dated, steps = [], []
    for d, di, c, mi, t in items:
        v = c if di == "in" else -c
        bal += v
        steps.append(v)
        rows.append([C.date(d), t, C.money(c) if di == "out" else "", C.money(c) if di == "in" else "",
                     C.money(bal)])
        dated.append((d, [c, bal], mi))
    rows.append(["", "Closing balance", "", "", C.money(bal)])
    chain_check([o] + steps, bal, "linked")
    p1, p2 = PAIRS_MAIN[r.randrange(len(PAIRS_MAIN))]
    b = table(C, x, W, title, ["Date", "Details", p1, p2, "Balance"], rows, "LLRRR", intro=intro,
              bold_rows=(0, len(rows) - 1), compact=r.random() < 0.4)
    b.meta = meta("linked_ministatement", title, len(items), chains=True, dated=dated,
                  probes=[rows[1][0], rows[-1][4]])
    b.meta["mirrors"] = sum(1 for z in items if z[3])
    return b


def d_card_min_payment(C, r, x, W):
    owed = -C.D["accounts"][0]["closing"] if C.S["card"] else r.randint(80000, 600000)
    owed = max(owed, 20000)
    yrs = r.randint(8, 26)
    tot1 = int(owed * r.uniform(1.9, 3.4))
    fixed = r.randint(5, 30) * 1000
    tot2 = int(owed * r.uniform(1.1, 1.4))
    rows = [["Only the minimum payment", "%d years %d months" % (yrs, r.randint(0, 11)), C.money(tot1, "$")],
            [C.money(fixed, "$"), "3 years", C.money(tot2, "$")]]
    b = table(C, x, W, "Minimum payment warning",
              ["If you make no additional\ncharges and each month\nyou pay...",
               "You will pay off the\nclosing balance in...", "And you will end up\npaying an estimated\ntotal of..."],
              rows, "LLR", foot="If you pay %s each month you would save an estimated %s."
              % (C.money(fixed, "$"), C.money(tot1 - tot2, "$")), compact=r.random() < 0.5, style="box")
    b.meta = meta("card_min_payment", "Minimum payment warning", 2, probes=[rows[0][2], rows[1][0]])
    return b


def d_rewards_points(C, r, x, W):
    D = C.D
    title = r.choice(["Your rewards summary", "Kowhai Rewards points", "Rewards points this period"])
    if r.random() < 0.5:
        o = r.randint(500, 60000)
        e, bn, rd = r.randint(100, 4000), r.choice([0, 0, 250, 500, 1000]), r.choice([0, 0, 5000, 10000])
        rd = min(rd, o)
        rows = [["{:,}".format(o), "{:,}".format(e), "{:,}".format(bn), "{:,}".format(rd), "{:,}".format(o + e + bn - rd)]]
        b = table(C, x, W, title, ["Points brought\nforward", "Points\nearned", "Bonus\npoints", "Points\nredeemed",
                                   "Points\nbalance"], rows, "RRRRR", compact=True,
                  foot="Points expire 36 months after the month they are earned. 1,000 points = $5.00 reward.")
        b.meta = meta("rewards_points", title, 1, probes=[rows[0][4]])
        return b
    days = list(ML.daterange(D["start"], D["end"]))
    bal = r.randint(2000, 80000)
    o = bal
    rows = [["", "Points brought forward", "", "{:,}".format(bal)]]
    dated, steps = [], []
    for d in sorted(r.sample(days, r.randint(3, 6))):
        v = r.choice([r.randint(50, 900), r.randint(50, 900), -r.choice([5000, 10000, 2000])])
        if bal + v < 0:
            v = -v
        bal += v
        steps.append(v)
        what = "Points earned on purchases" if v > 0 else "Redeemed for %s voucher" % C.money(-v // 2, "$")
        rows.append([C.date(d), what, "{:+,}".format(v), "{:,}".format(bal)])
        dated.append((d, [], False))
    chain_check([o] + steps, bal, "points")
    b = table(C, x, W, title, ["Date", "Activity", "Points", "Points\nbalance"], rows, "LLRR",
              bold_rows=(0,), compact=r.random() < 0.6)
    b.meta = meta("rewards_points", title, len(rows) - 1, chains=True, dated=dated, probes=[rows[1][0], rows[-1][3]])
    return b


FX = [("United States dollar", "USD", 0.5934), ("Australian dollar", "AUD", 0.9071),
      ("British pound", "GBP", 0.4512), ("Euro", "EUR", 0.5468), ("Japanese yen", "JPY", 89.45),
      ("Fiji dollar", "FJD", 1.3215), ("Chinese yuan", "CNY", 4.2731), ("Singapore dollar", "SGD", 0.7912),
      ("Canadian dollar", "CAD", 0.8233), ("Hong Kong dollar", "HKD", 4.6411)]


def d_fx_rates(C, r, x, W):
    d = C.D["end"] - dt.timedelta(days=r.randint(0, 3))
    title = "Foreign exchange rates as at %s" % C.lond(d)
    rows = []
    for name, code, mid in r.sample(FX, r.randint(5, 8)):
        m = mid * r.uniform(0.97, 1.03)
        dp = 2 if mid > 50 else 4
        rows.append([name, code, "%.*f" % (dp, m * 1.012), "%.*f" % (dp, m * 0.988),
                     "%.*f" % (dp, m * 1.03)])
    b = table(C, x, W, title, ["Currency", "Code", "We buy\n(TT)", "We sell\n(TT)", "Notes\nbuy"], rows, "LLRRR",
              foot="Rates are indicative only, per NZ$1.00, and change throughout the day.", compact=True)
    b.meta = meta("fx_rates", title, len(rows), probes=[rows[0][2]])
    return b


def d_rwt_summary(C, r, x, W):
    y = C.D["end"].year + (1 if C.D["end"].month > 3 else 0)
    title = r.choice(["Resident withholding tax summary - year ended 31 March %d" % y,
                      "Your tax summary for the year to 31 March %d" % y])
    rows, tg, tr = [], 0, 0
    for a in [C.S["product"]] + r.sample(PRODUCTS["savings"] + ["Term Deposit"], r.randint(1, 3)):
        g = r.randint(500, 250000)
        rate = r.choice([10.5, 17.5, 30.0, 33.0, 39.0])
        t = int(round(g * rate / 100))
        rows.append([a, C.money(g), "%.1f%%" % rate, C.money(t), C.money(g - t)])
        tg += g
        tr += t
    b = table(C, x, W, title, ["Account", "Gross interest", "RWT rate", "RWT deducted", "Net interest"], rows,
              "LRRRR", total=["Total", C.money(tg), "", C.money(tr), C.money(tg - tr)],
              foot="Keep this summary for your tax records. The figures are reported to Inland Revenue.",
              compact=r.random() < 0.5)
    b.meta = meta("rwt_summary", title, len(rows), probes=[rows[0][1], C.money(tg)])
    return b


def d_interest_by_month(C, r, x, W):
    D = C.D
    title = r.choice(["Interest earned by month", "Interest history this financial year",
                      "Interest paid to you - year to date"])
    m = ML.add_months(D["start"].replace(day=1), -1)
    months = []
    for k in range(r.randint(4, 10)):
        months.append(m)
        m = ML.add_months(m, -1)
    months.reverse()
    rows, dated, tg, tt = [], [], 0, 0
    rate = r.choice([0.105, 0.175, 0.30, 0.33])
    for m in months:
        last = ML.add_months(m, 1) - dt.timedelta(days=1)
        g = r.randint(50, 9000)
        t = int(round(g * rate))
        rows.append(["%s %d" % (MONTH[m.month - 1], m.year), C.date(last), C.money(g), C.money(t), C.money(g - t)])
        dated.append((last, [g, t, g - t], False))
        tg += g
        tt += t
    b = table(C, x, W, title, ["Month", "Date paid", "Interest\nearned", "RWT\ndeducted", "Net interest\ncredited"],
              rows, "LLRRR", total=["Year to date", "", C.money(tg), C.money(tt), C.money(tg - tt)],
              compact=r.random() < 0.5)
    b.meta = meta("interest_by_month", title, len(rows), dated=dated, probes=[rows[0][1], rows[0][2]])
    return b


def d_spending_summary(C, r, x, W):
    title = r.choice(["Where your money went", "Spending by category this period", "Your spending snapshot"])
    rows = []
    for cat in r.sample(["Groceries", "Eating out", "Transport", "Utilities", "Insurance", "Entertainment",
                         "Health", "Home and garden", "Shopping", "Transfers"], r.randint(5, 8)):
        a, b = r.randint(2000, 90000), r.randint(2000, 90000)
        rows.append([cat, C.money(a), C.money(b), G.fmt_money(a - b, dict(C.S["M"], neg="plus"))])
    b = table(C, x, W, title, ["Category", "This period", "Last period", "Change"], rows, "LRRR",
              compact=True, foot="Categories are assigned automatically and may not be exact.")
    b.meta = meta("spending_summary", title, len(rows), probes=[rows[0][1]])
    return b


def d_cover_letter(C, r, x, W):
    S, D = C.S, C.D
    issued = D["end"] + dt.timedelta(days=r.randint(2, 6))
    eff = D["end"] + dt.timedelta(days=r.randint(20, 60))
    last = S["holder"].split()[-1]
    head = para_block(C, x, W, ["%s, PO Box %d, %s %04d. Freephone 0800 %03d %03d."
                                % (S["B"]["legal"], r.randint(100, 9999), r.choice(TOWNS), r.randint(1000, 9899),
                                   r.randint(100, 999), r.randint(100, 999))],
                      size=7.5, title=S["B"]["mast"], tsize=15)
    addr = para_block(C, x, W, [C.lond(issued), "", S["holder"]] + S["addr"], size=9, gap=0)
    A = D["accounts"][0]
    paras = ["Dear %s %s," % (S["holder"].split()[0], last),
             "**Changes to your %s" % S["product"],
             "We are writing to let you know about some changes to the interest rates and fees on your "
             "accounts, and to enclose your statement for the period %s to %s." % C.period,
             "From %s the following changes will apply:" % C.lond(eff)]
    p1 = para_block(C, x, W, paras, size=9)
    rows = []
    dated = []
    for prod in r.sample(PRODUCTS["savings"] + [S["product"]], 3):
        old = r.uniform(0.1, 5.0)
        rows.append([prod, C.pct(old), C.pct(max(0.05, old + r.choice([-0.4, -0.25, 0.2]))), C.date(eff)])
    rows.append(["Monthly account fee", C.money(500, "$"), C.money(r.choice([600, 650, 700]), "$"), C.date(eff)])
    tb = table(C, x + 18, W - 36, None, ["", "Now", "From", "Effective"], rows, "LRRL", size=9, style="rule")
    p2 = para_block(C, x, W, [
        "Your closing balance on %s was %s. If you have set up automatic payments of more than %s a month, "
        "please check that you will have enough money in your account on the due dates."
        % (C.lond(D["end"]), C.money(abs(A["closing"]), "$"), C.money(r.choice([50000, 100000]), "$")),
        "You do not need to do anything. If you would like to talk about these changes, call us on the number "
        "above or visit any branch before %s." % C.lond(eff),
        "Yours sincerely,", "", "%s %s" % (r.choice(FIRST), r.choice(LAST)),
        r.choice(["Head of Everyday Banking", "General Manager, Personal Banking", "Customer Experience Lead"])],
        size=9)
    b = stack([head, addr, p1, tb, p2], gap=10, kind="cover_letter", newpage=True)
    b.meta = meta("cover_letter", "Letter: changes to your %s" % S["product"], len(rows), dated=dated,
                  probes=[C.money(abs(A["closing"]), "$"), rows[0][1]])
    return b


CLAUSES = [
    "Interest on credit balances is calculated daily on the end-of-day balance and paid monthly. Rates "
    "are variable and we may change them at any time; we will tell you within {d} days of a change.",
    "A dishonour fee of {f1} applies each time a payment is declined because there is not enough money "
    "in your account. An unarranged overdraft fee of {f2} applies in any month your account is overdrawn "
    "without an arrangement.",
    "You must check your statement and tell us about any error or unauthorised transaction within {d} "
    "days of the statement date. Disputed card transactions over {f3} may need a written claim.",
    "We may change these terms by giving you at least 14 days notice. Changes to fees take effect from "
    "{date1}; changes to transaction limits take effect from {date2}.",
    "Automatic payments are made on the due date or the next business day. If an automatic payment "
    "fails three times in a row we may cancel it and charge {f1}.",
    "Daily limits: ATM withdrawals {f4}; EFTPOS purchases {f5}; online transfers to other people {f6}. "
    "You can ask us to lower these limits at any time.",
    "If your account has a credit balance below {f3} and no customer-initiated transactions for 12 "
    "months it may be classed as dormant and a fee of {f2} per year may apply.",
    "Resident withholding tax is deducted from interest at the rate you have told us. If you have not "
    "given us your IRD number the non-declaration rate of 45% applies.",
    "Statements are issued monthly where there have been transactions, and at least every six months "
    "otherwise. A replacement statement costs {f7} per page.",
    "We are not liable for any loss caused by a system failure beyond our reasonable control, but this "
    "does not limit your rights under the Consumer Guarantees Act.",
    "Overseas transactions are converted to New Zealand dollars at our rate on the day they are "
    "processed, plus a currency conversion fee of 2.10% of the amount.",
    "Cheques deposited are usually cleared within 2 business days. Funds from cheques over {f8} may be "
    "held for up to 7 business days.",
]


def d_terms(C, r, x, W, avail=None):
    D = C.D
    title = r.choice(["Terms and conditions (extract)", "General terms and conditions", "Important terms"])
    vals = dict(d=r.choice([30, 60, 90]), f1=C.money(1500, "$"), f2=C.money(r.choice([1000, 1500]), "$"),
                f3=C.money(r.choice([5000, 10000, 20000]), "$"), f4=C.money(100000, "$"),
                f5=C.money(r.choice([200000, 500000]), "$"), f6=C.money(r.choice([500000, 1000000]), "$"),
                f7=C.money(500, "$"), f8=C.money(500000, "$"),
                date1=C.lond(D["end"] + dt.timedelta(days=r.randint(15, 45))),
                date2=C.lond(D["end"] + dt.timedelta(days=r.randint(45, 90))))
    order = list(range(len(CLAUSES)))
    r.shuffle(order)
    sz = r.choice([6.5, 7, 7.5])
    avail = avail or 600
    paras, k = [], 0
    blocks = []
    while k < len(order):
        p = "%d. %s" % (k + 1, CLAUSES[order[k]].format(**vals))
        trial = para_block(C, x, W, paras + [p], size=sz, title=title, tsize=12)
        if trial.h > avail - 150:
            break
        paras.append(p)
        k += 1
    blocks.append(para_block(C, x, W, paras, size=sz, title=title, tsize=12))
    fees = [["Dishonour fee", vals["f1"]], ["Unarranged overdraft fee", vals["f2"]],
            ["Replacement statement (per page)", vals["f7"]], ["Currency conversion", "2.10%"]]
    tb = table(C, x, W, "Fees referred to above", ["Fee", "Amount"], fees, "LR", size=sz, compact=True)
    blocks.append(tb)
    b = stack(blocks, gap=8, kind="terms", newpage=True)
    if b.h > avail:
        raise GenError("terms block taller than the page")
    b.meta = meta("terms", title, len(fees), probes=[vals["f1"], vals["date1"]])
    return b


def d_complaints(C, r, x, W, avail=None):
    D = C.D
    title = r.choice(["Problems, complaints and disputed transactions", "If something goes wrong"])
    paras = ["If you think there is a mistake on your statement, or a transaction you did not make, tell us "
             "as soon as possible. We will acknowledge your complaint within 2 business days and aim to "
             "resolve it within 10 business days.",
             "If we cannot resolve your complaint you can refer it, free of charge, to an independent "
             "dispute resolution scheme. You must usually do this within 2 months of our final response.",
             "Card disputes (chargebacks) must be raised within 75 days of the transaction date. Below is a "
             "summary of disputes you have open with us."]
    pb = para_block(C, x, W, paras, size=C.size + 0.5, title=title, tsize=12)
    rows, dated = [], []
    days = list(ML.daterange(D["start"] - dt.timedelta(days=60), D["end"]))
    for k in range(r.randint(2, 4)):
        d = r.choice(days)
        amt = C.fresh(d, r.randint(1500, 60000))
        rows.append([C.date(d), "CD-%05d" % r.randint(1, 99999), fill(r, "{mer}"), C.money(amt),
                     r.choice(["Under review", "Resolved - credited", "Awaiting merchant", "Closed"])])
        dated.append((d, [amt], False))
    rows.sort(key=lambda z: z[0])
    tb = table(C, x, W, "Your open and recent disputes", ["Date raised", "Reference", "Merchant",
                                                           "Amount\ndisputed", "Status"], rows, "LLLRL",
               compact=r.random() < 0.5)
    b = stack([pb, tb], gap=10, kind="complaints", newpage=True)
    b.meta = meta("complaints", title, len(rows), dated=dated, probes=[rows[0][0], rows[0][3]])
    return b


def d_new_design(C, r, x, W):
    S, D = C.S, C.D
    title = r.choice(["Your new-look statement", "We have refreshed your statement design",
                      "Introducing your new statement"])
    pb = para_block(C, x, W, ["From %s your statement looks a little different. Here is a sample of how "
                              "your transactions now appear, and what each part means." % C.lond(D["start"])],
                    size=C.size + 1, title=title, tsize=14)
    days = list(ML.daterange(D["start"], D["end"]))
    bal = r.randint(50000, 400000)
    o = bal
    rows = [["", "Opening balance", "", "", C.money(bal)]]
    dated, steps = [], []
    for d, t, di in [(None, "SAMPLE CAFE (EXAMPLE)", "out"), (None, "SALARY (EXAMPLE)", "in"),
                     (None, "POWER COMPANY (EXAMPLE)", "out")]:
        d = r.choice(days)
        c = C.fresh(d, r.randint(400, 9000) if di == "out" else r.randint(100000, 300000))
        v = c if di == "in" else -c
        bal += v
        steps.append(v)
        rows.append([C.date(d), t, C.money(c) if di == "out" else "", C.money(c) if di == "in" else "",
                     C.money(bal)])
        dated.append((d, [c, bal], False))
    chain_check([o] + steps, bal, "sample")
    p1, p2 = PAIRS_MAIN[r.randrange(len(PAIRS_MAIN))]
    tb = table(C, x + 20, W - 40, None, ["Date", "Transaction details", p1, p2, "Balance"], rows, "LLRRR",
               bold_rows=(0,), compact=False, style="box")
    notes = para_block(C, x, W, ["1  Date - the day the transaction was processed.",
                                 "2  Transaction details - who you paid or who paid you.",
                                 "3  %s and %s - money leaving and arriving in your account." % (p1, p2),
                                 "4  Balance - your balance after each transaction."], size=C.size, gap=0)
    b = stack([pb, tb, notes], gap=10, kind="new_design", newpage=True)
    b.meta = meta("new_design", title, 3, chains=True, dated=dated, probes=[rows[1][0], rows[-1][4]])
    return b


def d_marketing(C, r, x, W):
    D = C.D
    ends = D["end"] + dt.timedelta(days=r.randint(20, 80))
    title = r.choice(["Grow your savings faster", "Special offer: term deposit rates",
                      "Thinking about a new home?"])
    pb = para_block(C, x, W, ["Lock in a great rate today. Offer ends %s. Minimum deposit %s; terms and "
                              "conditions apply." % (C.lond(ends), C.money(500000, "$"))],
                    size=C.size + 1.5, title=title, tsize=18)
    rows = []
    for term in r.sample(["3 months", "6 months", "9 months", "1 year", "18 months", "2 years"], 5):
        rate = r.uniform(3.6, 5.4)
        rows.append([term, C.pct(rate), C.money(int(1000000 * rate / 100)), C.money(int(2500000 * rate / 100))])
    t1 = table(C, x, W, "Term deposit specials", ["Term", "Rate", "Interest on\n$10,000 (1 yr)",
                                                  "Interest on\n$25,000 (1 yr)"], rows, "LRRR", compact=True,
               size=C.size + 0.5)
    rows2 = []
    rate = r.choice([5.49, 5.69, 5.99])
    for P in [40000000, 55000000, 70000000]:
        i = rate / 100 / 52
        pay = int(round(P * i / (1 - (1 + i) ** -(25 * 52))))
        rows2.append([C.money(P, "$"), C.money(pay, "$"), C.money(pay * 25 * 52 - P, "$")])
    t2 = table(C, x, W, "Home loan special %s fixed for 1 year" % C.pct(rate),
               ["Loan amount", "Weekly\nrepayment", "Total interest\nover 25 years"], rows2, "RRR", compact=True,
               size=C.size + 0.5, foot="Example only. Lending criteria, fees and terms apply.")
    b = stack([pb, t1, t2], gap=14, kind="marketing", newpage=True)
    b.meta = meta("marketing", title, len(rows) + len(rows2), probes=[rows[0][1], rows2[0][1]])
    return b


def d_blank_page(C, r, x, W, avail=None):
    text = r.choice(["This page has been left blank intentionally.", "", "Notes"])
    pb = para_block(C, x, W, [text] if text else [], size=8) if text else Block(10, lambda sh, y: None)
    h = (avail or 600) - 5

    def draw(sh, y0):
        if text:
            pb.draw(sh, y0 + h / 2.0)
    b = Block(h, draw, kind="blank_page", newpage=True)
    b.meta = meta("blank_page", text or "(blank)", 0, probes=[text] if text else [])
    return b


BUILDERS = {k[2:]: v for k, v in globals().items() if k.startswith("d_") and callable(v)}
NEEDS_AVAIL = {"terms", "complaints", "blank_page"}


# ---------------------------------------------------------------------------
# The real statement table
# ---------------------------------------------------------------------------

def card_fmt(c_signed, S):
    body = G.fmt_money(abs(c_signed), S["M"], unsigned=True)
    if c_signed >= 0:      # payment / refund (money in for the holder)
        return body + " CR" if S["card_style"] == "cr" else "-" + body
    return body


def card_parse(s, S):
    if s.endswith(" CR"):
        return G.parse_money(s[:-3], S["M"], unsigned=True)
    if s.startswith("-"):
        return G.parse_money(s[1:], S["M"], unsigned=True)
    return -G.parse_money(s, S["M"], unsigned=True)


class Main:
    def __init__(self, S, D, C):
        self.S, self.D, self.C = S, D, C
        rng = S["rng"]
        self.reg, self.bold = C.reg, C.bold
        self.size = float(S["size"])
        cols = S["cols"]
        self.cols = cols
        heads = {k: rng.choice(HEADS_MAIN[k]) for k in cols if k in HEADS_MAIN}
        p1, p2 = PAIRS_MAIN[rng.randrange(len(PAIRS_MAIN))]
        heads["debit"], heads["credit"] = p1, p2
        if S["card"]:
            heads["amount"] = rng.choice(["Amount", "Amount NZ$", "Amount $"])
        self.heads = heads
        self.has_bal = "balance" in cols
        M, MB = S["M"], S["MB"]
        lab = dict(open=rng.choice(["Opening balance", "Balance brought forward", "OPENING BALANCE",
                                    "Balance at start of period"]),
                   close=rng.choice(["Closing balance", "CLOSING BALANCE", "Balance at end of period"]),
                   cf=rng.choice(["Balance carried forward", "Carried forward"]),
                   bf=rng.choice(["Balance brought forward", "Brought forward"]),
                   tot=rng.choice(["Total", "Totals this period", "Total movements"]))
        self.lab = lab
        for a in D["accounts"]:
            for r in a["rows"]:
                P = dict(date=DATES[S["dfmt"]](r["date"]))
                if "debit" in cols:
                    P["debit"] = G.fmt_money(r["c"], M, unsigned=True) if r["dir"] == "out" else None
                    P["credit"] = G.fmt_money(r["c"], M, unsigned=True) if r["dir"] == "in" else None
                if "amount" in cols:
                    P["amount"] = card_fmt(r["sa"], S) if S["card"] else G.fmt_money(r["sa"], M)
                if self.has_bal:
                    P["balance"] = G.fmt_money(r["bal"], MB)
                r["P"] = P
        allrows = [r for a in D["accounts"] for r in a["rows"]]
        extra_bal = []
        for a in D["accounts"]:
            extra_bal += [G.fmt_money(a["opening"], MB), G.fmt_money(a["closing"], MB)]
        PW = S["PW"]
        ml, mr = S["ml"], S["mr"]
        CW = PW - ml - mr
        gap = max(8.0, self.size * 1.3)
        fixed = 0.0
        self.w, self.align = {}, {}
        for k in cols:
            if k in ("type", "details", "ref"):
                continue
            if k == "date":
                ss = [r["P"]["date"] for r in allrows] + [DATES[S["dfmt"]](D["start"]),
                                                          DATES[S["dfmt"]](D["end"])]
                al = "left"
            else:
                ss = [r["P"].get(k) or "" for r in allrows]
                if k == "balance":
                    ss += extra_bal
                if k in ("debit", "credit"):
                    ss += [G.fmt_money(a["tout"] if k == "debit" else a["tin"], M, unsigned=True)
                           for a in D["accounts"]]
                al = "right"
            wv = max(stringWidth(s, self.reg, self.size) for s in ss)
            wh = stringWidth(heads[k], self.bold, self.size)
            self.w[k] = max(wv, wh) + 3
            self.align[k] = al
            fixed += self.w[k]
        tcols = [k for k in cols if k in ("type", "details", "ref")]
        weights = dict(type=0.0, details=3.0, ref=1.0)
        avail = CW - fixed - gap * (len(cols) - 1)
        for k in tcols:
            self.align[k] = "left"
        nat = {k: max(stringWidth(r["texts"].get(k, ""), self.reg, self.size) for r in allrows) + 4 for k in tcols}
        lw = max(stringWidth(lab[z], self.bold, self.size) for z in lab)
        mins = {k: max([stringWidth(w, self.bold, self.size) for r in allrows for w in r["texts"].get(k, "").split()]
                       + [stringWidth(heads[k], self.bold, self.size), lw if k == "details" else 0]) + 3
                for k in tcols}
        for k in tcols:
            if weights[k] == 0.0:
                self.w[k] = nat[k]
        rest = avail - sum(self.w[k] for k in tcols if weights[k] == 0.0)
        flex = [k for k in tcols if weights[k] > 0]
        tw = sum(weights[k] for k in flex)
        for k in flex:
            self.w[k] = max(mins[k], min(nat[k], rest * weights[k] / tw))
        if sum(self.w[k] for k in tcols) > avail + 0.5:
            raise GenError("%s: main table does not fit" % S["name"])
        spare = avail - sum(self.w[k] for k in tcols)
        if "details" in flex:
            self.w["details"] += spare * rng.uniform(0.3, 0.9)
            spare = avail - sum(self.w[k] for k in tcols)
        gap += spare / max(1, len(cols) - 1)
        self.x = {}
        x = ml
        for k in cols:
            self.x[k] = x
            x += self.w[k] + gap
        self.ml, self.CW = ml, CW
        self.lead = self.size * 1.22
        self.rowgap = self.size * 0.35
        self.line_h = self.lead + self.rowgap
        self.first_text = "details"
        for r in allrows:
            r["lines"] = {k: G.wrap(r["texts"].get(k, ""), self.w[k], self.reg, self.size) or [""] for k in tcols}
            r["nl"] = max(len(v) for v in r["lines"].values())
            r["h"] = r["nl"] * self.lead + self.rowgap
        self.head_h = self.size * 1.18 + 7

    def anchor(self, k):
        return self.x[k] if self.align[k] == "left" else self.x[k] + self.w[k]

    def heading(self):
        S, sz = self.S, self.size

        def draw(sh, y):
            if S["mstyle"] == "band":
                sh.rect(self.ml - 2, y, self.CW + 4, self.head_h - 1, fill=GREY)
            elif S["mstyle"] == "rule":
                sh.line(self.ml, y + self.head_h - 1.5, self.ml + self.CW, y + self.head_h - 1.5, width=0.6)
            for k in self.cols:
                sh.text(self.anchor(k), y + sz * 1.05 + 1.5, self.heads[k], sz, bold=True,
                        align=self.align[k])
        return Block(self.head_h + 1, draw)

    def label(self, which, value, date=None):
        lab = self.lab[which]
        sz = self.size

        def draw(sh, y):
            base = y + sz * 0.95
            sh.text(self.x[self.first_text], base, lab, sz, bold=True)
            if value is not None and self.has_bal:
                sh.text(self.anchor("balance"), base, G.fmt_money(value, self.S["MB"]), sz, bold=True,
                        align="right")
            if date is not None:
                sh.text(self.x["date"], base, DATES[self.S["dfmt"]](date), sz)
        return Block(self.line_h, draw)

    def totals(self, a):
        sz = self.size
        M = self.S["M"]

        def draw(sh, y):
            sh.line(self.ml, y + 1, self.ml + self.CW, y + 1, width=0.4)
            base = y + 4 + sz * 0.95
            sh.text(self.x[self.first_text], base, self.lab["tot"], sz, bold=True)
            sh.text(self.anchor("debit"), base, G.fmt_money(a["tout"], M, unsigned=True), sz, bold=True,
                    align="right")
            sh.text(self.anchor("credit"), base, G.fmt_money(a["tin"], M, unsigned=True), sz, bold=True,
                    align="right")
        return Block(self.line_h + 5, draw)

    def row(self, r, show_date, zebra):
        S, sz, lead = self.S, self.size, self.lead

        def draw(sh, y):
            if S["rules"] == "zebra" and zebra:
                sh.rect(self.ml - 2, y - 0.5, self.CW + 4, r["h"] - 0.5, fill=PALE)
            base = y + sz * 0.95
            for k in self.cols:
                if k in r["lines"]:
                    for j, ln in enumerate(r["lines"][k]):
                        if ln:
                            sh.text(self.x[k], base + j * lead, ln, sz)
                    continue
                s = show_date if k == "date" else r["P"].get(k)
                if s:
                    sh.text(self.anchor(k), base, s, sz, align=self.align[k])
            if S["rules"] == "rows":
                sh.line(self.ml, y + r["h"] - 1, self.ml + self.CW, y + r["h"] - 1, width=0.25,
                        color=(0.7, 0.7, 0.7))
        return Block(r["h"], draw)


# ---------------------------------------------------------------------------
# Page flow
# ---------------------------------------------------------------------------

class Doc:
    def __init__(self, S):
        self.S = S
        self.pages = []
        self.bottom = S["PH"] - 42

    @property
    def pno(self):
        return len(self.pages)

    def new_page(self, role, header):
        self.pages.append(dict(role=role, items=[], y=0.0, kinds=[]))
        if header is not None:
            self.put(header)

    def fits(self, h):
        return self.pages[-1]["y"] + h <= self.bottom

    def room(self):
        return self.bottom - self.pages[-1]["y"]

    def put(self, b, gap=0.0):
        pg = self.pages[-1]
        if pg["y"] + b.h > self.bottom + 0.01:
            raise GenError("%s: block %s overflows page %d" % (self.S["name"], b.kind, self.pno))
        pg["items"].append((pg["y"], b))
        pg["y"] += b.h + gap
        b.pages.append(self.pno)
        if b.meta:
            pg["kinds"].append(b.meta["kind"])


def masthead(S, C, title):
    B = S["B"]
    PW, ml, mr = S["PW"], S["ml"], S["mr"]

    def draw(sh, y):
        if S["bankpos"] == "right":
            sh.text(PW - mr, y + 34, B["mast"], 11, bold=True, align="right")
        else:
            sh.text(ml, y + 34, B["mast"], 11, bold=True)
        sh.line(ml, y + 41, PW - mr, y + 41, width=0.5, color=MIDG)
        if title:
            sh.text(ml, y + 60, title, 12.5, bold=True)
    return Block(72 if title else 52, draw)


PAGE_TITLES = ["Important information about your accounts", "Interest rate update", "Your account information",
               "Notices", "Information for you", "Your banking at a glance"]
BACK_TITLES = ["Important information", "Terms, fees and other information", "More information",
               "Things you should know", None]


def stmt_header(S, C, D, Mn, compact):
    B, PW, ml, mr = S["B"], S["PW"], S["ml"], S["mr"]
    rng = random.Random(zlib.crc32((S["name"] + ":hdr").encode()))
    p1, p2 = ML.period_text(rng.choice(["long", "short", "upper"]), D["start"], D["end"])
    title = rng.choice(["Account statement", "Statement of account", "%s statement" % S["product"],
                        "Your statement"])
    A = D["accounts"][0]
    right = [("Account", S["product"]), ("Account number" if not S["card"] else "Card number", S["acct"]),
             ("Statement period", "%s to %s" % (p1, p2)), ("Statement number", "%d" % rng.randint(2, 180))]
    box = None
    if S["card"]:
        owed0, owed1 = -A["opening"], -A["closing"]
        pairs = [("Previous balance", G.fmt_money(owed0, S["M"], unsigned=True)),
                 ("Purchases and charges", G.fmt_money(A["tout"], S["M"], unsigned=True)),
                 ("Payments and credits", G.fmt_money(A["tin"], S["M"], unsigned=True)),
                 ("Closing balance", G.fmt_money(owed1, S["M"], unsigned=True)),
                 ("Minimum payment due", G.fmt_money(max(1000, owed1 // 33), S["M"], unsigned=True)),
                 ("Payment due date", ML.period_text("long", D["end"] + dt.timedelta(days=25), D["end"])[0])]
        box = kv_block(C, 0, 250, "Account summary", pairs, size=8)
    elif not Mn.has_bal:
        pairs = [("Opening balance", G.fmt_money(A["opening"], S["MB"])),
                 ("Total withdrawals", G.fmt_money(A["tout"], S["M"], unsigned=True)),
                 ("Total deposits", G.fmt_money(A["tin"], S["M"], unsigned=True)),
                 ("Closing balance", G.fmt_money(A["closing"], S["MB"]))]
        box = kv_block(C, 0, 250, "Balance summary", pairs, size=8)
    left = [S["holder"]] + S["addr"]
    if compact:
        top = 8
        h = top + 26 + 11 * max(len(right), 1) + 6
    else:
        top = 34
        h = top + 52 + 11 * max(len(left), len(right)) + 6
    bh = box.h + 6 if box else 0

    def draw(sh, y):
        if compact:
            sh.line(ml, y + top - 2, PW - mr, y + top - 2, width=0.8)
            sh.text(ml, y + top + 13, title.upper() if rng.random() < 0.5 else title, 12, bold=True)
            yy = y + top + 26
            sh.text(ml, yy + 8, S["holder"], 8, bold=True)
        else:
            if S["bankpos"] == "right":
                sh.text(PW - mr, y + top + 6, B["mast"], 16, bold=True, align="right")
                sh.text(ml, y + top + 6, title, 12, bold=True)
            else:
                sh.text(ml, y + top + 6, B["mast"], 16, bold=True)
                sh.text(PW - mr, y + top + 6, title, 12, bold=True, align="right")
            yy = y + top + 30
            for k, t in enumerate(left):
                sh.text(ml, yy + 8 + 11 * k, t, 8, bold=(k == 0))
        for k, (lab, v) in enumerate(right):
            by = yy + 8 + 11 * k
            vx0 = sh.text(PW - mr, by, v, 8, align="right")[0]
            sh.text(vx0 - 5, by, lab + ":", 8, align="right", bold=True)
        if box:
            bx = PW - mr - box.w
            box2 = kv_block(C, bx, 250, box_title, pairs_box, size=8)
            box2.draw(sh, y + h)
    if box:
        box_title = "Account summary" if S["card"] else "Balance summary"
        pairs_box = pairs
    return Block(h + bh, draw)


def cont_header(S, D):
    PW, ml, mr = S["PW"], S["ml"], S["mr"]
    t = "%s  -  %s  %s  -  statement continued" % (S["B"]["mast"], S["product"], S["acct"])

    def draw(sh, y):
        sh.text(ml, y + 30, t, 7.5)
        sh.line(ml, y + 36, PW - mr, y + 36, width=0.4, color=MIDG)
    return Block(46, draw)


def section_title(S, Mn, a):
    sz = Mn.size

    def draw(sh, y):
        sh.text(Mn.ml, y + sz + 6, "%s   %s" % (a["product"], a["acct"]), sz + 1.5, bold=True)
    return Block(sz * 2.4 + 6, draw)


def build_decoy(kind, S, C, k, pos, x=None, W=None, avail=None):
    seed = zlib.crc32(("%s:%d:decoy%d:%s" % (S["name"], S["attempt"], k, kind)).encode())
    r = random.Random(seed)
    x = S["ml"] if x is None else x
    W = C.CW if W is None else W
    fn = BUILDERS[kind]
    b = fn(C, r, x, W, avail=avail) if kind in NEEDS_AVAIL else fn(C, r, x, W)
    b.kind = kind
    if kind in FULLPAGE:
        b.newpage = True
    b.meta["position"] = pos
    return b


def flow(S, D, C, Mn):
    doc = Doc(S)
    rng = random.Random(zlib.crc32(("%s:%d:flow" % (S["name"], S["attempt"])).encode()))
    placed = []
    k = [0]

    def mk(kind, pos, **kw):
        k[0] += 1
        b = build_decoy(kind, S, C, k[0], pos, **kw)
        placed.append(b)
        return b

    page_avail = doc.bottom - 72
    by_pos = {}
    for kind, pos in S["decoys"]:
        by_pos.setdefault(pos, []).append(kind)

    # ---- front matter: recipe decoys first (letters lead), then filler to the target
    front = sorted(by_pos.get("front", []), key=lambda z: 0 if z == "cover_letter" else 1)
    fill_pool = list(CARD_FRONT_FILL if S["card"] else FRONT_FILL)
    rng.shuffle(fill_pool)
    used = set(front)
    fill_pool = [f for f in fill_pool if f not in used] + [f for f in fill_pool if f in used]
    queue = [(f, False) for f in front]
    fp = 0
    while True:
        if not queue:
            if fp >= len(fill_pool) * 2:
                break
            queue.append((fill_pool[fp % len(fill_pool)], True))
            fp += 1
        kind, filler = queue.pop(0)
        b = mk(kind, "front", avail=page_avail)
        needs_new = (not doc.pages) or b.newpage or not doc.fits(b.h + 10)
        if needs_new and doc.pno >= S["front_target"] and filler:
            placed.pop()
            break
        if needs_new:
            if kind == "cover_letter":
                doc.new_page("front", None)
                doc.pages[-1]["y"] = 40
            else:
                doc.new_page("front", masthead(S, C, rng.choice(PAGE_TITLES)))
        doc.put(b, gap=14)
        if b.newpage and kind in FULLPAGE and kind != "new_design":
            doc.pages[-1]["y"] = doc.bottom   # a full-page insert owns its page
        if doc.pno >= S["front_target"] and not queue and doc.room() < 140:
            break
    # ---- the statement (on a fresh page, or partway down a page that opens with front matter)
    tops = by_pos.get("top", [])
    top_blocks = [mk(t, "top") for t in tops]
    need_top = sum(b.h + 12 for b in top_blocks)
    partway = False
    if S["partway"]:
        doc.new_page("statement", masthead(S, C, rng.choice(PAGE_TITLES)))
        small = [f for f in fill_pool if f not in FULLPAGE]
        for j in range(rng.randint(1, 2)):
            b = mk(small[(fp + j) % len(small)], "front")
            if doc.room() - b.h - 14 >= 400 + need_top:
                doc.put(b, gap=14)
                partway = True
            else:
                placed.pop()
        hdr = stmt_header(S, C, D, Mn, True)
        doc.pages[-1]["y"] += 6
        doc.put(hdr, gap=8)
    else:
        doc.new_page("statement", stmt_header(S, C, D, Mn, False))
    start_page = doc.pno
    front_full = start_page - 1
    for b in top_blocks:
        if not doc.fits(b.h + 12):
            raise Retry("top decoy does not fit")
        doc.put(b, gap=12)
    mids = by_pos.get("mid", [])
    multi = len(D["accounts"]) > 1
    zebra = False
    for ai, A in enumerate(D["accounts"]):
        rows = A["rows"]
        first_h = (Mn.size * 2.4 + 6 if multi else 0) + Mn.head_h + 2 * Mn.line_h + sum(r["h"] for r in rows[:2])
        if not doc.fits(first_h):
            doc.new_page("statement", cont_header(S, D))
        if multi:
            doc.put(section_title(S, Mn, A))
        doc.put(Mn.heading())
        if Mn.has_bal:
            doc.put(Mn.label("open", A["opening"], D["start"] if S["dated_open"] else None))
        bal = A["opening"]
        prev, first_on_page = None, True
        mid_at = max(2, int(len(rows) * rng.uniform(0.4, 0.7))) if (ai == 0 and mids) else -1
        reserve = Mn.line_h if (S["bf"] and Mn.has_bal) else 0.0
        for i, r in enumerate(rows):
            def show(fop):
                if not S["dgroup"]:
                    return r["P"]["date"]
                return r["P"]["date"] if (r["date"] != prev or fop) else None
            brk = i == mid_at
            if brk or not doc.fits(r["h"] + reserve):
                if reserve:
                    doc.put(Mn.label("cf", bal))
                if brk:
                    mb = [mk(m, "mid", avail=page_avail) for m in mids]
                    doc.new_page("mid", masthead(S, C, rng.choice(PAGE_TITLES)))
                    for b in mb:
                        if b.newpage and doc.pages[-1]["items"][1:]:
                            doc.new_page("mid", masthead(S, C, None))
                        if not doc.fits(b.h):
                            doc.new_page("mid", masthead(S, C, None))
                        doc.put(b, gap=14)
                doc.new_page("statement", cont_header(S, D))
                doc.put(Mn.heading())
                if reserve:
                    doc.put(Mn.label("bf", bal))
                first_on_page = True
            sd = show(first_on_page)
            r["shown_date"] = sd
            doc.put(Mn.row(r, sd, zebra))
            r["page"] = doc.pno
            zebra = not zebra
            first_on_page = False
            prev = r["date"]
            bal = r["bal"]
        tail = (Mn.line_h if Mn.has_bal else 0) + (Mn.line_h + 5 if (S["totline"] and "debit" in Mn.cols) else 0)
        if not doc.fits(tail + 4):
            doc.new_page("statement", cont_header(S, D))
            doc.put(Mn.heading())
        if Mn.has_bal:
            doc.put(Mn.label("close", A["closing"], D["end"] if S["dated_open"] else None))
        if S["totline"] and "debit" in Mn.cols:
            doc.put(Mn.totals(A))
        doc.pages[-1]["y"] += 14
    end_page = doc.pno
    for kind in by_pos.get("below", []):
        b = mk(kind, "below", avail=page_avail)
        if not doc.fits(b.h):
            doc.new_page("below", masthead(S, C, None))
        doc.put(b, gap=14)
    # ---- back matter: recipe decoys, then filler to the target number of pages
    back = by_pos.get("back", [])
    pool = list(BACK_FILL)
    rng.shuffle(pool)
    pool = [p for p in pool if p not in back] + [p for p in pool if p in back]
    target = max(end_page + S["back_target"], doc.pno + 1)
    queue = [(b, False) for b in sorted(back, key=lambda z: 1 if z == "terms" else 0)]
    first = True
    fp = 0
    while True:
        if not queue:
            if doc.pno >= target and not first:
                break
            queue.append((pool[fp % len(pool)], True))
            fp += 1
            if fp > 40:
                raise GenError("back matter never filled")
        kind, filler = queue.pop(0)
        b = mk(kind, "back", avail=page_avail)
        needs_new = first or b.newpage or not doc.fits(b.h + 10)
        if needs_new and filler and doc.pno >= target:
            placed.pop()
            break
        if needs_new:
            doc.new_page("back", masthead(S, C, rng.choice(BACK_TITLES)))
        first = False
        doc.put(b, gap=14)
        if b.newpage and kind in FULLPAGE and kind != "new_design":
            doc.pages[-1]["y"] = doc.bottom
    info = dict(front_full=front_full, start_page=start_page, partway=partway, end_page=end_page,
                n_pages=doc.pno)
    return doc, placed, info


def render(S, doc, path):
    sh = Sheet(path, PAGES[S["page"]], S["font"])
    n = len(doc.pages)
    PW, PH = S["PW"], S["PH"]
    lab = ["Page %d of %d", "Page %d / %d", "%d of %d"][zlib.crc32(S["name"].encode()) % 3]
    for pno, pg in enumerate(doc.pages):
        sh.begin_page()
        for y, b in pg["items"]:
            b.draw(sh, y)
        sh.text(PW / 2, PH - 14, FOOTER, 6.5, align="center")
        sh.text(PW - 30, PH - 26, lab % (pno + 1, n), 7, align="right")
        sh.text(S["ml"], PH - 26, S["B"]["legal"], 6.5)
        sh.end_page()
    sh.save()


# ---------------------------------------------------------------------------
# Truth and checks
# ---------------------------------------------------------------------------

def truth_rows(S, D, Mn):
    out = []
    multi = len(D["accounts"]) > 1
    tsrc = [k for k in Mn.cols if k in ("type", "details", "ref")]
    for ai, a in enumerate(D["accounts"]):
        for r in a["rows"]:
            text = {Mn.heads[k]: r["texts"].get(k, "") for k in tsrc}
            desc = " ".join(r["texts"].get(k, "") for k in tsrc if r["texts"].get(k, ""))
            amt = r["P"].get("debit") or r["P"].get("credit") or r["P"].get("amount")
            row = {"date": r["date"].isoformat(), "description": desc,
                   "debit": r["c"] / 100.0 if r["dir"] == "out" else None,
                   "credit": r["c"] / 100.0 if r["dir"] == "in" else None,
                   "balance": r["bal"] / 100.0 if Mn.has_bal else None,
                   "text": text, "page": r["page"], "redacted": [], "overlay_redacted": [],
                   "printed": {"date": r["shown_date"], "date2": None, "amount": amt, "indicator": None,
                               "balance": r["P"].get("balance")}}
            if multi:
                row["account_index"] = ai
            out.append(row)
    return out


def check_chain(t):
    accts = t.get("accounts") or [dict(account_index=None, opening_balance=t["opening_balance"],
                                       closing_balance=t["closing_balance"])]
    for a in accts:
        rows = [r for r in t["rows"] if r.get("account_index") == a["account_index"]]
        b = round(a["opening_balance"] * 100)
        for r in rows:
            b += round((r["credit"] or 0) * 100) - round((r["debit"] or 0) * 100)
            if r["balance"] is not None and b != round(r["balance"] * 100):
                raise GenError("%s: chain broken at %s %r" % (t["case"], r["date"], r["description"]))
        if b != round(a["closing_balance"] * 100):
            raise GenError("%s: closing %s != chain %s" % (t["case"], a["closing_balance"], b))


def check_parse(t, S=None):
    M, MB = t["money_format"], t["balance_format"]
    for r in t["rows"]:
        p = r["printed"]
        want = round((r["credit"] or 0) * 100) - round((r["debit"] or 0) * 100)
        lay = t["amount_layout"]
        if lay == "card":
            v = card_parse(p["amount"], dict(M=M, card_style=t["card_style"]))
        elif lay == "signed":
            v = G.parse_money(p["amount"], M)
        else:
            v = G.parse_money(p["amount"], M, unsigned=True)
            v = v if r["credit"] is not None else -v
        if v != want:
            raise GenError("%s: printed %r parses to %d, truth %d" % (t["case"], p["amount"], v, want))
        if p["balance"] is not None and G.parse_money(p["balance"], MB) != round(r["balance"] * 100):
            raise GenError("%s: printed balance %r wrong" % (t["case"], p["balance"]))


def check_collisions(t):
    real = {}
    for r in t["rows"]:
        real.setdefault((r["date"], round((r["debit"] or r["credit"] or 0) * 100)), r)
    n = 0
    for d in t["decoys"]:
        for z in d["dated_rows"]:
            if z["mirror"]:
                n += 1
                continue
            for a in z["amounts"]:
                if (z["date"], abs(a)) in real:
                    raise Retry("%s: decoy %s row %s %d equals a real row" % (t["case"], d["kind"], z["date"], a))
    return n


def features_of(S, D, Mn, info, decoys):
    f = ["page:" + S["page"], "font:%s/%gpt" % (S["font"], S["size"]), "layout:" + S["layout"],
         "date_format:" + S["dfmt"], "decoy_date_format:" + S["ddfmt"],
         "money:%s%s/%s" % (S["M"]["cur"] or "-", S["M"]["thou"] or "nothou", S["M"]["neg"]),
         "balance_neg:%s%s" % (S["MB"]["neg"], ":" + "/".join(S["MB"]["tok"]) if S["MB"]["neg"] == "suf" else ""),
         "main_heading_style:" + S["mstyle"], "decoy_table_style:" + S["dstyle"],
         "front_pages:%d" % info["front_full"], "back_pages:%d" % info["back_pages"],
         "statement_pages:%d" % (info["end_page"] - info["start_page"] + 1)]
    if info["partway"]:
        f.append("statement_starts_partway_down_page")
    pos = {d["position"] for d in decoys}
    for p in ("top", "mid", "below"):
        if p in pos:
            f.append("decoys_" + {"top": "above_table_same_page", "mid": "between_statement_pages",
                                  "below": "below_table_same_page"}[p])
    if sum(1 for d in decoys if d["rows"]) >= 5:
        f.append("heavy_decoys")
    if any(d["chains"] for d in decoys):
        f.append("chaining_decoy_balance")
    if any(d.get("mirrors") for d in decoys):
        f.append("mirrored_transfer_in_linked_account")
    if len(D["accounts"]) > 1:
        f.append("multi_account")
    if S["card"]:
        f.append("credit_card:" + S["card_style"])
    if not Mn.has_bal:
        f.append("no_balance_column")
    if S["dgroup"]:
        f.append("date_once_per_day")
    if S["bf"] and Mn.has_bal:
        f.append("brought_carried_forward")
    if S["dated_open"] and Mn.has_bal:
        f.append("dated_opening_closing_lines")
    if S["totline"] and "debit" in Mn.cols:
        f.append("column_totals_line")
    if min(r["bal"] for r in D["accounts"][0]["rows"]) < 0 and not S["card"]:
        f.append("overdrawn")
    if S["rules"] != "none":
        f.append("row_rules:" + S["rules"])
    return f


def build_once(spec, out_dir, attempt, shrink):
    S = resolve(spec, attempt, shrink)
    D = gen_data(S)
    C = Ctx(S, D)
    Mn = Main(S, D, C)
    doc, placed, info = flow(S, D, C, Mn)
    if doc.pno > MAX_PAGES:
        raise Retry("%d pages" % doc.pno)
    info["back_pages"] = doc.pno - info["end_page"]
    if info["front_full"] < 2 or not 1 <= info["back_pages"] <= 5:
        raise Retry("front %d / back %d pages" % (info["front_full"], info["back_pages"]))
    decoys = []
    for b in placed:
        m = dict(b.meta)
        m["pages"] = sorted(set(b.pages))
        if not m["pages"]:
            raise GenError("%s: decoy %s never placed" % (S["name"], m["kind"]))
        decoys.append(m)
    multi = len(D["accounts"]) > 1
    A = D["accounts"][0]
    lay = "card" if S["card"] else ("separate" if "debit" in S["cols"] else "signed")
    cues = []
    if Mn.has_bal:
        cues.append("running_balance")
    cues.append("printed_totals")
    if lay in ("card", "signed"):
        cues.append("sign_markers")
    kinds = sorted({d["kind"] for d in decoys})
    note = ("Real %s statement (%s) surrounded by %d decoy blocks: %s. Front matter %d page(s), back matter %d "
            "page(s)." % ("credit card" if S["card"] else "bank", S["layout"], len(decoys), ", ".join(kinds),
                          info["front_full"], info["back_pages"]))
    truth = {
        "case": S["name"], "generator": GENERATOR, "note": note, "bank": S["B"]["name"],
        "source_format": "pdf",
        "opening_balance": None if multi else A["opening"] / 100.0,
        "closing_balance": None if multi else A["closing"] / 100.0,
        "opening_printed": True, "closing_printed": True,
        "period": {"start": D["start"].isoformat(), "end": D["end"].isoformat()},
        "rows": truth_rows(S, D, Mn), "decidable": True, "decidable_by": cues, "undecidable_reason": None,
        "newest_first": False,
        "columns": [{"heading": Mn.heads[k], "kind": G.KIND_TRUTH.get(k, "text") if k != "amount" else
                     "amount_signed", "source": k if k in ("type", "details", "ref") else None} for k in Mn.cols],
        "money_format": S["M"], "balance_format": S["MB"], "amount_layout": lay,
        "card_side": False, "card_style": S["card_style"] if S["card"] else None, "date_format": S["dfmt"],
        "indicator_tokens": None, "legend": None, "page_count": doc.pno, "page_size": S["page"],
        "font": S["font"], "font_size": S["size"], "account_number": S["acct"], "product": S["product"],
        "decoys": decoys, "decoy_kinds": kinds, "front_pages": info["front_full"],
        "back_pages": info["back_pages"],
        "statement_pages": sorted({r["page"] for a in D["accounts"] for r in a["rows"]}),
        "statement_first_page": info["start_page"], "statement_last_page": info["end_page"],
        "statement_starts_partway": info["partway"],
        "page_roles": [pg["role"] for pg in doc.pages],
    }
    if multi:
        truth["accounts"] = [{"account_index": i, "product": a["product"], "account_number": a["acct"],
                              "opening_balance": a["opening"] / 100.0, "closing_balance": a["closing"] / 100.0}
                             for i, a in enumerate(D["accounts"])]
    truth["row_count"] = len(truth["rows"])
    truth["features"] = features_of(S, D, Mn, info, decoys)
    check_chain(truth)
    check_parse(truth)
    truth["mirrored_decoy_rows"] = check_collisions(truth)
    if S["dfmt"] in G.DM_NUMERIC and not any(r["date"].day > 12 for a in D["accounts"] for r in a["rows"]):
        raise GenError("numeric dates never show a day > 12")
    path = os.path.join(out_dir, S["name"] + ".pdf")
    render(S, doc, path)
    G.write_json(os.path.join(out_dir, S["name"] + ".truth.json"), truth)
    return truth


def build_case(spec, out_dir):
    shrink = 1.0
    last = None
    for attempt in range(12):
        try:
            return build_once(spec, out_dir, attempt, shrink)
        except Retry as e:
            last = e
            if "pages" in str(e):
                shrink *= 0.8
    raise GenError("%s: gave up after retries (%s)" % (spec["name"], last))


# ---------------------------------------------------------------------------
# The pdftotext pass
# ---------------------------------------------------------------------------

def norm(s):
    return re.sub(r"\s+", " ", s)


def check_dir(d, only=None):
    names = sorted(f[:-len(".truth.json")] for f in os.listdir(d) if f.endswith(".truth.json"))
    if only:
        names = [n for n in names if only in n]
    bad = []
    st = dict(cases=0, pages=0, strings=0, probes=0, decoys=0, mirrors=0)
    for name in names:
        t = json.load(open(os.path.join(d, name + ".truth.json")))
        pdf = os.path.join(d, name + ".pdf")
        if not os.path.exists(pdf):
            bad.append("%s: no pdf" % name)
            continue
        st["cases"] += 1
        txt = subprocess.run(["pdftotext", "-layout", pdf, "-"], capture_output=True, text=True,
                             check=True).stdout
        pages = [norm(p) for p in txt.split("\f")]
        if pages and not pages[-1].strip():
            pages = pages[:-1]
        if len(pages) != t["page_count"]:
            bad.append("%s: %d pages, truth %d" % (name, len(pages), t["page_count"]))
        for k, p in enumerate(pages):
            st["pages"] += 1
            if FOOTER not in p:
                bad.append("%s: footer missing on page %d" % (name, k + 1))
        if t["front_pages"] < 2 or not 1 <= t["back_pages"] <= 5:
            bad.append("%s: front %d back %d" % (name, t["front_pages"], t["back_pages"]))
        try:
            check_chain(t)
            check_parse(t)
            st["mirrors"] += check_collisions(t)
        except GenError as e:
            bad.append(str(e))
        if t["row_count"] != len(t["rows"]):
            bad.append("%s: row_count" % name)
        for r in t["rows"]:
            page = pages[r["page"] - 1] if r["page"] - 1 < len(pages) else ""
            if r["page"] in range(1, t["statement_first_page"]) or r["page"] > t["statement_last_page"]:
                bad.append("%s: row on page %d outside the statement" % (name, r["page"]))
            for key, s in r["printed"].items():
                if s is None:
                    continue
                st["strings"] += 1
                if norm(s) not in page:
                    bad.append("%s: %s %r not on page %d" % (name, key, s, r["page"]))
            for w in r["description"].split():
                if w not in page:
                    bad.append("%s: word %r not on page %d" % (name, w, r["page"]))
                    break
        for dcy in t["decoys"]:
            st["decoys"] += 1
            txt_pages = " ".join(pages[p - 1] for p in dcy["pages"] if p - 1 < len(pages))
            for pr in dcy["probes"]:
                st["probes"] += 1
                if norm(pr) not in txt_pages:
                    bad.append("%s: decoy %s probe %r not on pages %s" % (name, dcy["kind"], pr, dcy["pages"]))
    print("check: %d PDFs, %d pages, %d truth strings, %d decoys (%d probe strings), %d mirrored decoy rows"
          % (st["cases"], st["pages"], st["strings"], st["decoys"], st["probes"], st["mirrors"]))
    for b in bad[:40]:
        print("  FAIL", b)
    if bad:
        print("check: %d problem(s)" % len(bad))
        return 1
    print("check: all good")
    return 0


def main(argv=None):
    ap = argparse.ArgumentParser(description="Build the decoy-table statement set.")
    ap.add_argument("--out", help="directory to write PDFs and truth files into")
    ap.add_argument("--only", help="only cases whose name contains this")
    ap.add_argument("--check", help="re-check a built directory against pdftotext")
    a = ap.parse_args(argv)
    if not a.out and not a.check:
        ap.error("give --out DIR and/or --check DIR")
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
            print("%-40s %3d rows %2d pg (front %d, stmt %s, back %d) %2d decoys" % (
                t["case"], t["row_count"], t["page_count"], t["front_pages"],
                ",".join(str(p) for p in t["statement_pages"]), t["back_pages"], len(t["decoys"])))
        print("built %d case(s) into %s" % (n, a.out))
    if a.check:
        return check_dir(a.check, a.only)
    return 0


if __name__ == "__main__":
    sys.exit(main())
