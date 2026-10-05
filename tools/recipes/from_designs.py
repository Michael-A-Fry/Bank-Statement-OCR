#!/usr/bin/env python3
"""Turn STATEMENT DESIGN blocks (docs/context/recipe-intake-prompt.md, answered by
Copilot on real statements) into draft recipes for R/recipes.R.

  python3 tools/recipes/from_designs.py docs/context/RECIPE_STATEMENTS --out recipes/drafts

Statements of the same design (same bank, same table heading, same date format) are
grouped into ONE recipe: a "must appear" phrase is kept only if EVERY statement of the
design printed it. Placeholders the privacy rules put in (PERSON A, MERCHANT B, XXXX,
REF 1001 ...) and anything with digits are never used as words to look for.
Blocks the converter cannot map exactly are listed with the reason, never guessed.
"""
import argparse, collections, json, os, re, sys

TITLES = [
    ("bank", r"BANK \[bank\]"), ("title", r"PRODUCT \[title\]"), ("kind", r"FILE KIND"),
    ("all", r"MUST APPEAR"), ("none", r"MUST NOT APPEAR"), ("starts", r"EACH STATEMENT STARTS WITH"),
    ("period", r"STATEMENT PERIOD"), ("opening", r"OPENING BALANCE\b(?! \$)"), ("closing", r"CLOSING BALANCE\b(?! \$)"),
    ("totals", r"PRINTED TOTALS"), ("header", r"TABLE HEADING LINE"), ("columns", r"COLUMNS \[table: columns\]"),
    ("dates", r"DATE FORMAT \[dates"), ("money", r"MONEY IN AND OUT"), ("running", r"RUNNING BALANCE"),
    ("order", r"ROW ORDER"), ("skip", r"LINES INSIDE THE TABLE"), ("ends", r"LINES THAT END THE TABLE"),
    ("norows", r"NO TRANSACTIONS \[table"), ("multiline", r"ROWS ON MORE THAN ONE LINE"),
    ("sections", r"\bSECTIONS\b"), ("sheets", r"SPREADSHEETS ONLY"), ("else", r"ANYTHING ELSE"), ("sample", r"\nSAMPLE")]
KNOWN_DATES = ["%d/%m/%Y", "%d/%m/%y", "%Y-%m-%d", "%d-%m-%Y", "%d-%m-%y", "%d.%m.%Y", "%d.%m.%y", "%Y/%m/%d",
               "%m/%d/%Y", "%d %b %Y", "%d %B %Y", "%d %b %y", "%d-%b-%Y", "%b %d, %Y", "%B %d, %Y", "%d %b",
               "%d %B", "%b %d", "%B %d", "%d/%m", "%d-%b", "%d-%b-%y"]
PLACEHOLDER = re.compile(r"\b(PERSON|MERCHANT|BUSINESS|REF|PART|CODE) [A-Z0-9]+\b|XXXX|\d")
ROLE = {"date": "date", "second-date": "date2", "description": "description", "debit": "debit",
        "credit": "credit", "amount": "amount", "balance": "balance", "type": "type",
        "other-text": "text", "other-money": "text"}

def sections(b):
    pos = []
    for key, rx in TITLES:
        m = re.search(rx, b)
        if m:
            # step past the rest of a "[key: ...]" tag and any " -- ..." on the title
            tail = re.match(r"[^\n\]]*\]", b[m.end():])
            pos.append((m.start(), m.end() + (tail.end() if tail and "[" in b[m.start():m.end() + tail.end()] else 0), key))
    pos.sort()
    out = {}
    for i, (s, e, k) in enumerate(pos):
        nxt = pos[i + 1][0] if i + 1 < len(pos) else len(b)
        body = b[e:nxt]
        body = re.sub(r"\n\s*\d+\.\s*$", "", body.rstrip())          # the next item's number
        out[k] = body.strip()
    return out

def quoted(s): return [q.strip() for q in re.findall(r'"([^"]+)"', s or "")]
def clean(ph): return [p for p in ph if p and not PLACEHOLDER.search(p) and len(p) >= 3]
def first_word(s): return (s or "").replace("[kind]", "").split()[0].strip().lower() if (s or "").split() else ""
# A phrase as the reader matches it (R/recipes.R .rc_flat): lower case, words only.
def flat(s): return " %s " % re.sub(r"[^a-z0-9%]+", " ", (s or "").lower()).strip()
def printed_in(p, phrases): return any(flat(p) in flat(x) for x in phrases if x)
def slug(s): return re.sub(r"[^a-z0-9]+", "_", s.lower()).strip("_")

VARIANT = re.compile(r'^(\s*[a-z]\)\s*)?(Older|Newer)[^:"]*:\s*(.*)$')

def variants(b):
    """A block that describes an older and a newer design of the same statement
    ("Older: ...", "Newer variant: ...", '"A" / "B" -> role', '"A" OR "B"') becomes
    two blocks, one per design; every unmarked line belongs to both. A marked line
    followed by bare quoted lines (a heading listed one word per line) carries its
    mark down. A block without such marks is returned as it is."""
    hdr = sections(b).get("header", "")
    if not re.search(r"(?m)^\s*(Older|Newer)\b", hdr): return [b]
    out = {"Older": [], "Newer": []}; mode = None
    for ln in b.split("\n"):
        m = VARIANT.match(ln)
        if m:
            mode = m.group(2); out[mode].append((m.group(1) or "") + m.group(3)); continue
        if mode and re.match(r'^\s*"[^"]*"\s*$', ln): out[mode].append(ln); continue
        mode = None
        two = re.match(r'^(\s*)"([^"]+)"\s*(?:/|OR)\s*"([^"]+)"(.*)$', ln)
        if two:
            out["Older"].append('%s"%s"%s' % (two.group(1), two.group(2), two.group(4)))
            out["Newer"].append('%s"%s"%s' % (two.group(1), two.group(3), two.group(4))); continue
        out["Older"].append(ln); out["Newer"].append(ln)
    return ["\n".join(out["Older"]), "\n".join(out["Newer"])]

def parse(b):
    S = sections(b)
    d = {"problems": []}
    bank = (S.get("bank") or "").strip()
    bank = re.sub(r"^other:\s*", "", bank).strip().strip('"')
    d["bank"] = slug(bank.split("\n")[0]) if bank else ""
    t = quoted(S.get("title", ""))
    d["title"] = t[0] if t else (S.get("title", "").split("\n")[0].strip())
    d["kind"] = first_word(S.get("kind", ""))
    d["all"] = clean(quoted(S.get("all")))
    d["none"] = clean(quoted(S.get("none")))
    st = clean(quoted(S.get("starts")))
    d["starts"] = st[0] if st else None
    per = S.get("period", "")
    pa = re.search(r'a\)\s*"([^"]+)"', per)
    d["period_label"] = pa.group(1).strip() if pa else None
    po = re.search(r'd\)\s*"([^"]+)"', per)
    d["open_start"] = po.group(1).strip() if po else None
    hdr = S.get("header", "")
    if re.search(r"(?m)^\s*(Older|Newer)\b", hdr): d["problems"].append("several design variants in one block")
    hdr = re.split(r"Is the heading", hdr)[0]
    d["header"] = quoted(hdr)
    cols = []
    for m in re.finditer(r'"([^"]+)"\s*(?:=>|->)\s*([a-z-]+)', S.get("columns", "")):
        cols.append((m.group(1).strip(), ROLE.get(m.group(2), None), m.group(2)))
    # columns are listed in any order; the heading line sets it
    hl = [h.lower() for h in d["header"]]
    if cols and all(c[0].lower() in hl for c in cols): cols.sort(key=lambda c: hl.index(c[0].lower()))
    d["columns"] = cols
    dt = (S.get("dates") or "").split("\n")[0]
    fm = re.match(r"\s*(.+?)\s+(yes|no)\b", dt)
    d["date_format"] = (fm.group(1).strip() if fm else dt.strip())
    # "OTHER: 17th June" -- an ordinal day is read as the day number (normalise.R)
    if re.match(r"OTHER:\s*\d{1,2}(st|nd|rd|th) [A-Z][a-z]{3,}", d["date_format"]): d["date_format"] = "%d %B"
    if re.match(r"OTHER:\s*\d{2}-\d{2}-\d{2}$", d["date_format"]): d["date_format"] = "%m-%d-%y"
    d["date_year"] = (fm.group(2) if fm else None)
    mo = S.get("money", "")
    a = re.search(r"a\)\s*(separate columns|one signed column|type column)", mo)
    d["style"] = {"separate columns": "debit_credit_cols", "one signed column": "signed",
                  "type column": "signed"}.get(a.group(1)) if a else None
    neg = re.search(r"d\)\s*(.*?)\s*e\)", mo, re.S); pos = re.search(r"e\)\s*(.*?)\s*f\)", mo, re.S)
    d["negative"] = [x for x in quoted(neg.group(1) if neg else "") if x in ("OD", "DR")]
    d["positive"] = [x for x in quoted(pos.group(1) if pos else "") if x == "CR"]
    # g) the type codes a description starts with: "DD" = "Direct Debit"
    g = re.search(r"g\)\s*(.*)$", mo, re.S)
    d["types"] = [(c, m) for c, m in re.findall(r'"([A-Za-z0-9]{1,6})"\s*=\s*"([^"]+)"', g.group(1) if g else "")]
    o = re.search(r"(oldest_first|newest_first)", S.get("order", ""))
    d["order"] = o.group(1) if o else "oldest_first"
    # a heading word ("Credit") printed again inside the table is the heading, not a
    # line to skip -- as a skip it would swallow a row whose description starts with it
    d["skip"] = [x for x in clean(quoted(S.get("skip"))) if not any(flat(h).startswith(flat(x)) for h in d["header"])]
    d["ends"] = clean(quoted(S.get("ends")))
    nr = S.get("norows", "")
    d["norows"] = [] if re.search(r"UNSURE|NONE", nr) else clean(quoted(nr))
    return d

def check(d):
    p = list(d["problems"])
    if d["kind"] not in ("pdf-text",): p.append("kind %s (recipes read text PDFs for now)" % d["kind"])
    if d["date_format"] not in KNOWN_DATES and not re.match(r"^(%[dmyYbB]|[ ./-])+$", d["date_format"] or ""):
        p.append("date format %r is not one the reader knows" % d["date_format"])
    if not d["header"]: p.append("no table heading")
    heads = [c[0] for c in d["columns"]]
    if [h for h in heads if h in d["header"]] != [h for h in d["header"] if h in heads]:
        p.append("columns are not in the heading's order")
    roles = [c[1] for c in d["columns"]]
    if None in roles: p.append("a column role is not one of the choices")
    if "date" not in roles or "description" not in roles: p.append("needs a date and a description column")
    if not d["style"]: p.append("money style not given")
    return p

def recipe_yaml(rid, ds, status="draft", extra_none=(), sib_seen=()):
    d0 = ds[0]
    common = set(d0["all"])
    for d in ds[1:]: common &= set(d["all"])
    alls = [p for p in d0["all"] if p in common] or d0["header"][:2]
    # When a sibling design prints every one of these too, the heading words only
    # this design prints tell them apart ("Card Used" against "Credit amount").
    if sib_seen and all(printed_in(p, sib_seen) for p in alls):
        alls += [h for h in d0["header"] if not printed_in(h, sib_seen) and h not in alls][:2]
    # One start phrase only when every statement of the design prints the same one.
    st = set(d["starts"] for d in ds)
    starts = [(st.pop(), len(ds))] if len(st) == 1 and None not in st else []
    # A "must not appear" phrase can never be one any statement of the design printed.
    seen = set(x for d in ds for x in d["all"] + d["skip"] + d["ends"] + d["header"] + [d["title"], d["starts"] or ""])
    # In a recipe merged from several statements, one statement's "must not appear"
    # may be printed by another (CashBack lists "Interest - Purchases"; Low Rate
    # prints it): only phrases EVERY statement of the design lists are kept.
    own_none = set(ds[0]["none"])
    for d in ds[1:]: own_none &= set(d["none"])
    # The reader ignores capitals and matches whole words, so "Date of transaction"
    # would also block a statement printing "Date of Transaction".
    nones = sorted(x for x in (own_none | set(extra_none)) - set(alls) if not printed_in(x, seen | set(alls)))
    nones = list({flat(x): x for x in reversed(nones)}.values())[::-1]
    uniq = lambda xs: list(dict.fromkeys(xs))
    skip = uniq(x for d in ds for x in d["skip"])
    # A section heading ("Sundry Account Transactions") is skipped and reading carries
    # on: rows follow it. It is never also a line that ends the table.
    ends = uniq(x for d in ds for x in d["ends"] if x not in skip)
    norows = uniq(x for d in ds for x in d["norows"])
    cols, n_text, seen = [], 0, set()
    for head, role, raw in d0["columns"]:
        if head not in d0["header"] or role is None: continue
        if role == "text":
            n_text += 1; role = "text%d" % n_text
        if role in seen: continue
        seen.add(role); cols.append((role, head))
    year = "printed" if re.search(r"%[yY]", d0["date_format"]) else "period"
    q = lambda s: json.dumps(s, ensure_ascii=True)
    L = ["# Recipe written by tools/recipes/from_designs.py from %d real statement design(s)" % len(ds),
         "# described by the owner (docs/context/RECIPE_STATEMENTS). Every reading with it must still",
         "# prove itself by the statement's own arithmetic; one that does not is not used.",
         "recipe: %s" % rid, "format: 1", "version: 1", "bank: %s" % d0["bank"],
         "title: %s" % q(d0["title"]), "kind: pdf", "status: %s" % status, "recognise:",
         "  all: [%s]" % ", ".join(q(x) for x in alls)]
    if nones: L.append("  none: [%s]" % ", ".join(q(x) for x in nones))
    if starts: L.append("statement_starts: %s" % q(starts[0][0]))
    if d0["period_label"] or year == "period":
        L.append("period:"); L.append("  label: %s" % q(d0["period_label"] or ""))
        if d0["open_start"]: L.append("  open_start: %s" % q(d0["open_start"]))
    L.append("table:")
    L.append("  header: [%s]" % ", ".join(q(h) for h in d0["header"]))
    L.append("  columns:")
    for role, head in cols: L.append("    %s: {under: %s}" % (role, q(head)))
    if ends: L.append("  ends_at: [%s]" % ", ".join(q(x) for x in ends))
    if skip: L.append("  skip: [%s]" % ", ".join(q(x) for x in skip))
    if norows: L.append("  no_rows: [%s]" % ", ".join(q(x) for x in norows))
    L += ["dates:", "  format: %s" % q(d0["date_format"]), "  year: %s" % year,
          "money:", "  style: %s" % d0["style"]]
    if d0["negative"]: L.append("  negative: [%s]" % ", ".join(q(x) for x in d0["negative"]))
    if d0["positive"]: L.append("  positive: [%s]" % ", ".join(q(x) for x in d0["positive"]))
    L.append("order: %s" % d0["order"])
    types = {}
    for d in ds:
        for c, m in d.get("types", []): types.setdefault(c, m)
    if types:
        L.append("types:")
        for c in sorted(types): L.append("  %s: %s" % (json.dumps(c), json.dumps(types[c])))
    return "\n".join(L) + "\n"

def main():
    ap = argparse.ArgumentParser(); ap.add_argument("src"); ap.add_argument("--out", required=True)
    ap.add_argument("--status", default="draft", choices=["draft", "proven"])
    a = ap.parse_args()
    text = open(a.src, encoding="utf-8").read()
    blocks = [b for b in re.split(r"\n(?=STATEMENT DESIGN)", text) if b.startswith("STATEMENT DESIGN")]
    groups, skipped = collections.OrderedDict(), []
    for i, blk in enumerate(blocks, 1):
      for b in variants(blk):
        d = parse(b); p = check(d)
        if p: skipped.append((i, d["bank"], d["title"], p)); continue
        key = (d["bank"], tuple(h.lower() for h in d["header"]), d["date_format"])
        groups.setdefault(key, []).append(d)
    os.makedirs(a.out, exist_ok=True)
    # What tells a design from its same-bank siblings: each sibling's must-appear
    # phrases that this design's statements never print become this design's
    # must-not-appear phrases.
    def alls_of(ds):
        c = set(ds[0]["all"])
        for d in ds[1:]: c &= set(d["all"])
        return c
    def seen_of(ds):
        return set(x for d in ds for x in d["all"] + d["skip"] + d["ends"] + d["header"] + [d["title"], d["starts"] or ""])
    extra, sibs = {}, {}
    for k, ds in groups.items():
        sib = [alls_of(o) for k2, o in groups.items() if k2 != k and k2[0] == k[0]]
        extra[k] = sorted(set().union(*sib) - seen_of(ds)) if sib else []
        sibs[k] = set().union(*[seen_of(o) for k2, o in groups.items() if k2 != k and k2[0] == k[0]])
    ids = collections.Counter()
    for (bank, hdr, fmt), ds in groups.items():
        base = "%s_%s" % (bank, slug(ds[0]["title"])[:30]); ids[base] += 1
        rid = base if ids[base] == 1 else "%s_%d" % (base, ids[base])
        open(os.path.join(a.out, rid + ".yaml"), "w").write(recipe_yaml(rid, ds, a.status, extra[(bank, hdr, fmt)], sibs[(bank, hdr, fmt)]))
        print("recipe %-45s from %d statement(s)" % (rid, len(ds)))
    print("\n%d blocks -> %d draft recipes; %d blocks not converted:" % (len(blocks), len(groups), len(skipped)))
    for i, bank, title, p in skipped: print("  block %d %s %s: %s" % (i, bank, title[:35], "; ".join(p)))

if __name__ == "__main__": main()
