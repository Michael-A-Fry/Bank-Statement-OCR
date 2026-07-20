# Sample statement download checklist

This web environment's network policy blocks outbound downloads (only GitHub +
package registries are reachable), so most items below were **found** (searched,
vetted for authenticity/PII) but must be **downloaded manually** and added to
the repo. A few CSV/OFX fixtures were reachable via GitHub and are already in.

## How to add the manual ones to the repo (simplest path)
1. Download each PDF below in your browser, using the **suggested filename**.
2. On GitHub → this repo → branch **`bank-statement-ocr-platform`** → open
   `samples/raw/<bank>/` → **Add file → Upload files** → drag them in → commit.
3. Tell me when they're up; I'll `git pull`, verify each, read the layouts, and
   start building templates.

Legend: ✅ high-value statement layout · ⚪ lower priority (guide/disclosure)
· ⚠️ third-party host — eyeball for real customer data first · 🟢 already captured.

---

## ANZ → `samples/raw/anz/`  (manual)
- ✅ `anz_creditcard_summary_sample_01.pdf` — credit-card summary, labelled *Sample*
  https://www.anz.co.nz/content/dam/anzconz/documents/rates-fees-agreements/credit-cards/ANZ-Card-summary-sample.pdf
- ✅ `anz_investment_statement_guide_01.pdf` — annotated synthetic account statement ("Mr AB Sample")
  https://www.anz.co.nz/content/dam/anzconz/documents/personal/investments-kiwisaver/UT-Account-Statement-Guide_f.pdf
- ✅ `anz_kiwisaver_statement_guide_01.pdf` — annotated synthetic KiwiSaver statement
  https://www.anz.co.nz/content/dam/anzconz/documents/personal/investments-kiwisaver/KS-Account-Statement-Guide_f.pdf
- ⚪ `anz_internetbanking_howto_guide_01.pdf` — guide with statement/transaction screenshots
  https://www.anz.co.nz/content/dam/anzconz/documents/guides/How-to-Guides-Internet-Banking.pdf

## Westpac → `samples/raw/westpac/`  (manual)
- ✅ `westpac_creditcard_howtoread_01.pdf` — annotated credit-card statement
  https://www.westpac.co.nz/assets/Personal/credit-cards/documents/Westpac-how-to-read-your-credit-card-statement-guide-PDF.pdf
- ✅ `westpac_business_export_formats_01.pdf` — CSV/Excel import-export field-layout spec
  https://assets.dam.westpac.co.nz/is/content/wnzl/dist/ways-to-bank/digital/business-online/Business-Online_Transaction-Import-and-Export-File-Formats_guide.pdf
- ✅ `westpac_choices_everyday_homeloan_sample_01.pdf` — everyday/home-loan summary, *SAMPLE*
  https://www.westpac.co.nz/assets/Personal/home-loans/documents/Choices-Everyday-Home-Loan-Summary-sample-v2.0.pdf
- ✅ `westpac_choices_homeloan_summary_sample_02.pdf` — home-loan summary, *Sample v2.17*
  https://www.westpac.co.nz/assets/Personal/home-loans/documents/Choices-Home-Loan-Summary-Sample-v2.17.pdf

## BNZ → `samples/raw/bnz/`  (manual)
- ✅ `bnz_view_statements_ib_guide_01.pdf` — official guide, embeds redacted sample statement
  https://www.bnz.co.nz/assets/personal-banking-help-support/Internet-Banking/PDFs/qrg-View-Statements-IB.pdf
- ✅ `bnz_app_view_statement_guide_01.pdf` — official app guide, annotated statement screens
  https://www.bnz.co.nz/assets/bnz/personal-banking/help-and-support/BNZ2325_App-View-Statement.pdf
- ✅ `bnz_business_datafile_format_guide_01.pdf` — GIFTS CSV/data-file format spec (BAL + TRN records)
  https://www.bnz.co.nz/assets/bnz/business-banking/help-and-support/file-download-format-guide.pdf

## Kiwibank → `samples/raw/kiwibank/`  🟢 partly captured
- 🟢 `kiwibank_savings_01.ofx` — OFX savings export (already in repo)
- 🟢 `kiwibank_transaction_01.csv` — 15-column CSV transaction export (already in repo)
- Optional extras (manual): Kiwibank's own annotated statement help pages/PDFs at
  `kiwibank.co.nz` (statement & credit-card-statement help). Grab if easy.

## ASB → `samples/raw/asb/`  🟢 partly captured
- 🟢 `asb_transaction_export_01.csv`, `_02.tdv`, `_03.csv` — FastNet export layouts (already in repo)
- ASB's own annotated "how to read your statement" pages live on `asb.co.nz` (bot-protected;
  grab from a browser if you want the PDF layout).

## SBS → `samples/raw/sbs/`  (manual)
- ✅ `sbs_view_download_statements_01.pdf` — synthetic e-statement ("MR AB SAMPLE", balance summary)
  https://www.sbsbank.co.nz/assets/PDFs/How-to-guides/How-to-View-and-download-statements.pdf
- ✅ `sbs_view_account_balance_01.pdf` — synthetic account list + balances/transactions
  https://www.sbsbank.co.nz/assets/PDFs/How-to-guides/How-to-View-Account-balance.pdf
- ⚪ `sbs_make_payment_01.pdf` — UI/account-list screenshots
  https://www.sbsbank.co.nz/assets/PDFs/How-to-guides/How-to-Make-a-payment.pdf

## TSB → `samples/raw/tsb/`  (manual)
- ⚪ `tsb_mastercard_card_summary_sample_01.pdf` — Mastercard *Card Summary* (disclosure, not a txn statement)
  https://www.tsb.co.nz/sites/default/files/2023-04/TSB-credit-mastercard-card-summary_sample_v1-effective-29-november-2022.pdf

## No public specimen exists
- **The Co-operative Bank, HSBC NZ, Heartland, Rabobank NZ, CCB NZ** — none publish a
  downloadable sample statement; only prohibited third-party generators / out-of-scope
  disclosure reports were found. (HSBC NZ retail is winding down.)

---

## Third-party sources (⚠️ check for real customer data before use)
- BNZ statement screenshots (mortgage-broker guides):
  - https://mymortgage.co.nz/site_files/8755/upload_files/blog/BNZstatements.pdf
  - https://www.mymortgage.co.nz/site_files/8755/upload_files/blog/BNZStatementsusingOnlineBankingMobileApp1.pdf
  - https://www.savemybacon.co.nz/media/0f0146b1-d637-4aa7-8a26-1c211a135eb6/B4ruGw/General/Bank%20Statements/BNZ-Android.pdf
- SBS export screenshots:
  - https://www.savemybacon.co.nz/media/2c395168-cca6-4aa4-96ce-63ca4e856b92/l9BkYg/General/Bank%20Statements/SBS_Desktop.pdf

## Real-PII files — deliberately NOT committed (your call, redact first if used)
Genuine ASB CSV exports on GitHub containing real names/transactions:
- https://github.com/CuriousCraftsman/Conscious-Spending-App/blob/master/asb_txs_copy.csv
- https://github.com/NickTooley/PPL-Labelling/blob/master/Web%20Crawler/Export20181024133953.csv
- https://github.com/NickTooley/Oh-Sugar/blob/master/WebCrawler/WebCrawler/Export20181024133953.csv

---

## Note on coverage
Public NZ bank *transaction-statement* specimens are scarce (real statements sit
behind login). The PDFs above are strong for **layout/template design**; the
GitHub CSV/OFX fixtures give real **Excel/CSV-path** structure. For true
validation, a few **real redacted statements** per bank — everyday/transaction
and Visa/Mastercard, in PDF and Excel — would materially strengthen the golden
test set.
