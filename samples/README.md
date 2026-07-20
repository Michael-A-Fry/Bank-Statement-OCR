# Sample statement corpus

Publicly available **specimen / sample** statements used to design templates and
build the golden-file test set. This is real-format data for validation — it is
**not** the customer data the platform will process in production.

## Provenance & handling rules
- **Public specimens only.** Prefer statements explicitly labelled *sample*,
  *specimen*, *example*, or clearly synthetic/mock.
- **No real customer PII.** Do not store statements that appear to be a real
  individual's private document with genuine personal details. If such a file
  is the only example of a format, record the URL in the manifest and flag it —
  do not download it.
- Every stored file has an entry in the manifest recording its **source URL,
  bank, statement type, and format**, so provenance is always traceable.

## Layout
```
samples/
  raw/
    <bank-slug>/            # anz, bnz, westpac, kiwibank, asb, cooperative, sbs, tsb, hsbc, ...
      <files>              # e.g. anz_everyday_sample_01.pdf
      _manifest.csv        # per-bank provenance manifest
  catalogue.csv            # master catalogue across all banks
```

## Manifest columns
`filename,bank,statement_type,format,source_url,authenticity,notes`
- `statement_type`: everyday/transaction, savings, credit-card/visa/mastercard, other
- `format`: pdf | xlsx | xls | csv
- `authenticity`: specimen | synthetic | redacted | unknown
