# QVF lookalike findings (5 Oct) -- the fix list for recipes / the reader

Baseline (cold, new tool, 0 AUTO_WRONG everywhere; per-statement CSVs in baseline/):
ANZ 5/9, ANZ Loan 3/6 (end-to-end); Kiwibank PDF 4/8; Westpac CC 0/7; BNZ Visa 0/9;
Kiwibank CC 6/7; ANZ+ASB Visa 3/17; BNZ Excel 2/7 (.xls was refused before 36d2209);
Kiwibank Excel 6/7; Westpac Excel 5/7.

Extract the QVF load script (it is in the repo root .qvf; do NOT commit the output --
it carries staff names and server paths):
  python3 -c "import zlib,json;b=open('Statement Converter 300925.qvf','rb').read();o=zlib.decompressobj().decompress(b[30752:]).decode('utf-8','replace');print(json.JSONDecoder().raw_decode(o)[0]['qScript'])" > /tmp/script.qvs
Tabs: ANZ 2576-3375, ANZ Loan 3376-4119, ANZ Visa 4120-5048, ASB Visa 5049-5471,
BNZ Visa 5472-5994, Kiwibank CC 5995-6496, Kiwibank PDF 6497-6965, Westpac CC 6966-7478,
Excel BNZ 2033-2195, Kiwibank 2196-2413, Westpac 2414-2519, shared 107-1786, output 7479-7642.

## General rules found (each confirmed by an in-memory patch unless marked)
1. Open period "START - 31 Mar 2022": read as an open range; its end settles years. (extract_metadata.R:66-84)
2. "No transactions for this period" + opening == closing (+ zero totals): proven empty statement. (auto_read.R:607)
3. A dated line with no money figure (rate change) is an event to note, not a lost row. (auto_read.R:1065, 1374-1393)
4. Top table line printing only a balance figure is a candidate opening balance even if dated. (auto_read_pdf.R:1093)
5. Pages before the first "Page 1 of N" belong to the first statement. (split.R:48)
6. Split also on a repeated labelled header block (account + period), with an independent count agreeing. (split.R:44-48)
7. Section title ("General Payments & Charges") is not a column heading: pass over a one-cell words line crossing column boundaries. (auto_read_pdf.R:639-643) -- unblocks 4/7 Westpac CC.
8. Compare two date columns with each date in the year the period gives it, never a fixed year. (auto_read_pdf.R:580-585)
9. Day-month date takes the year that puts it nearest the period (Dec purchases on Jan statement). (parse_pdf_table.R:738)
10. Summary checks: compare money-out with the SUM of all money-out summary lines. (auto_read.R:1838-1880)
11. Labels line with figures directly underneath: pair each figure with the label above. (BNZ Visa; auto_read_summ.R:28-50)
12. On a card/loan, "current balance" names the closing balance. (auto_read_prove.R:29-31)
13. A totals line becomes a closing balance only if its column holds a figure on most rows; a reading with no balance step never outranks the headings' Debit/Credit words. (auto_read_prove.R:210; auto_read.R:629)
14. A printed section subtotal ("Total for card ...") ends a date run. (auto_read.R:1593)
15. Newest-first zero: judge 0.00 against the row before it in time. (parse_pdf_table.R:1211-1236)
16. Newest-first statement without printed ends: opening = oldest balance - its amount; closing = newest balance (bundle joins). (convert.R:513)
17. Excel: OD/DR/CR/brackets carried in the cell's number format is the sign. (read_input.R:30)
18. Excel: pending/unstatemented block after the table is set aside. (auto_read_tabular.R:197, 358)
19. Excel: lone "." or "-" in a figures column is zero. (auto_read_tabular.R:18, 76)
20. Excel: a column of account numbers with >1 value -> split by it, prove each. (auto_read.R:1528)
21. Heading words split at capitals/_/- ("ThisPartyCode" -> code). (auto_read_tabular.R:527)
22. Feed: account_number and account_name empty for automatic readings (parse_pdf_table.R:1358, feed.R:292).
23. ANZ "Transaction type and details" spans 5 physical columns; join them into the description (type code first) -- the new tool keeps only the widest.
Measuring gap: score_auto.R does not split bundles -- use score_convert.R.

## Regenerating the lookalikes
`python3 docs/context/qvf/gen/make_<type>.py` writes to docs/context/qvf/sets/<area>/ (git-ignored).
They import tools/synth/make_layouts.py (path found relative to the file). The BNZ .xls
writer needs xlwt: `pip install --target docs/context/qvf/.pylib xlwt` (dev-time only; never shipped).
Score: `Rscript tools/synth/score_convert.R docs/context/qvf/sets/<area> --mode cold`.
