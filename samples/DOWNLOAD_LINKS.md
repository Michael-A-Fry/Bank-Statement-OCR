# Sample statement download checklist

This web environment's network policy blocks outbound downloads, so these were
**found** (via search, vetted for authenticity/PII) but must be **downloaded
manually** and added to the repo.

## How to add them to the repo (simplest path)
1. Download each PDF below in your browser, saving it with the **suggested
   filename**.
2. On GitHub, open this repo → branch **`bank-statement-ocr-platform`** →
   navigate into `samples/raw/<bank>/` → **Add file → Upload files** → drag the
   PDFs in → commit. (Commits land as you automatically.)
3. Tell me when they're up; I'll `git pull`, verify each is a real PDF, read
   the layouts, and start building templates from them.

Legend: ✅ statement-layout (high value) · ⚪ lower priority (guide/disclosure)
· ⚠️ third-party host — check for real customer data before use.

---

## ANZ → `samples/raw/anz/`
- ✅ `anz_creditcard_summary_sample_01.pdf` — credit card summary, labelled *Sample*
  https://www.anz.co.nz/content/dam/anzconz/documents/rates-fees-agreements/credit-cards/ANZ-Card-summary-sample.pdf
- ✅ `anz_investment_statement_guide_01.pdf` — annotated synthetic account statement ("Mr AB Sample")
  https://www.anz.co.nz/content/dam/anzconz/documents/personal/investments-kiwisaver/UT-Account-Statement-Guide_f.pdf
- ✅ `anz_kiwisaver_statement_guide_01.pdf` — annotated synthetic KiwiSaver statement
  https://www.anz.co.nz/content/dam/anzconz/documents/personal/investments-kiwisaver/KS-Account-Statement-Guide_f.pdf
- ⚪ `anz_internetbanking_howto_guide_01.pdf` — guide with statement/transaction screenshots
  https://www.anz.co.nz/content/dam/anzconz/documents/guides/How-to-Guides-Internet-Banking.pdf

## Westpac → `samples/raw/westpac/`
- ✅ `westpac_creditcard_howtoread_01.pdf` — annotated credit-card statement
  https://www.westpac.co.nz/assets/Personal/credit-cards/documents/Westpac-how-to-read-your-credit-card-statement-guide-PDF.pdf
- ✅ `westpac_business_export_formats_01.pdf` — CSV/Excel import-export field layout spec
  https://assets.dam.westpac.co.nz/is/content/wnzl/dist/ways-to-bank/digital/business-online/Business-Online_Transaction-Import-and-Export-File-Formats_guide.pdf
- ✅ `westpac_choices_everyday_homeloan_sample_01.pdf` — everyday/home-loan summary, *SAMPLE*
  https://www.westpac.co.nz/assets/Personal/home-loans/documents/Choices-Everyday-Home-Loan-Summary-sample-v2.0.pdf
- ✅ `westpac_choices_homeloan_summary_sample_02.pdf` — home-loan summary, *Sample v2.17*
  https://www.westpac.co.nz/assets/Personal/home-loans/documents/Choices-Home-Loan-Summary-Sample-v2.17.pdf

## BNZ → `samples/raw/bnz/`
- ✅ `bnz_view_statements_ib_guide_01.pdf` — official guide, embeds redacted sample statement
  https://www.bnz.co.nz/assets/personal-banking-help-support/Internet-Banking/PDFs/qrg-View-Statements-IB.pdf
- ✅ `bnz_app_view_statement_guide_01.pdf` — official app guide, annotated statement screens
  https://www.bnz.co.nz/assets/bnz/personal-banking/help-and-support/BNZ2325_App-View-Statement.pdf
- ✅ `bnz_business_datafile_format_guide_01.pdf` — GIFTS CSV/data-file format spec (BAL + TRN records)
  https://www.bnz.co.nz/assets/bnz/business-banking/help-and-support/file-download-format-guide.pdf

## SBS → `samples/raw/sbs/`
- ✅ `sbs_view_download_statements_01.pdf` — synthetic e-statement ("MR AB SAMPLE", balance summary)
  https://www.sbsbank.co.nz/assets/PDFs/How-to-guides/How-to-View-and-download-statements.pdf
- ✅ `sbs_view_account_balance_01.pdf` — synthetic account list + balances/transactions
  https://www.sbsbank.co.nz/assets/PDFs/How-to-guides/How-to-View-Account-balance.pdf
- ⚪ `sbs_make_payment_01.pdf` — UI/account-list screenshots
  https://www.sbsbank.co.nz/assets/PDFs/How-to-guides/How-to-Make-a-payment.pdf

## TSB → `samples/raw/tsb/`
- ⚪ `tsb_mastercard_card_summary_sample_01.pdf` — Mastercard *Card Summary* (disclosure, not a txn statement)
  https://www.tsb.co.nz/sites/default/files/2023-04/TSB-credit-mastercard-card-summary_sample_v1-effective-29-november-2022.pdf

## The Co-operative Bank → `samples/raw/cooperative/`
- No authentic public specimen exists (bank publishes none; only prohibited third-party/fake generators found).

---

## Third-party sources (⚠️ check for real customer data before use)
Per "if it's online, use it" — usable, but eyeball for real PII first:
- BNZ statement screenshots (broker guides):
  - https://mymortgage.co.nz/site_files/8755/upload_files/blog/BNZstatements.pdf
  - https://www.mymortgage.co.nz/site_files/8755/upload_files/blog/BNZStatementsusingOnlineBankingMobileApp1.pdf
  - https://www.savemybacon.co.nz/media/0f0146b1-d637-4aa7-8a26-1c211a135eb6/B4ruGw/General/Bank%20Statements/BNZ-Android.pdf
- SBS export screenshots:
  - https://www.savemybacon.co.nz/media/2c395168-cca6-4aa4-96ce-63ca4e856b92/l9BkYg/General/Bank%20Statements/SBS_Desktop.pdf

---

## Pending (hunters still finishing)
- Kiwibank, ASB, HSBC (+ Heartland, Rabobank, CCB) — links appended when their runs complete.

## Note on coverage
Public NZ bank *transaction-statement* specimens are scarce (real statements
sit behind login). The above are strong for **layout/template design**. For
**validation on real data**, a few **real redacted statements** per bank —
especially everyday/transaction and Visa/Mastercard, in both PDF and Excel —
would materially strengthen the golden-file test set.
