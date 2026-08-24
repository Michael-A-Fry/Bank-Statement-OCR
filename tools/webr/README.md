# Running the engine without R installed

**You do not need this.** The offline server has R 4.6.0 and `tests/run_tests.R`
is the test runner for everyone. This folder exists for one situation: a machine
that has to work on this codebase and has no R and no way to install one.

That is not hypothetical — it is how the report extractor
(`R/tables.R` / `R/tables_detect.R` / `R/doc_extract.R`) was verified. It is also
where nine of the defects recorded as N152–N160 in
[`../../docs/context/findings-register.md`](../../docs/context/findings-register.md)
were found: they were **measured**, against
`../../tests/testthat/helper-doc-hard.R`, not reasoned about.

## What it is

[WebR](https://docs.r-wasm.org/webr/latest/) is R compiled to WebAssembly. The
npm package carries the entire R runtime as a `.wasm` file, so nothing is
downloaded at boot and it runs under Node with the repository mounted as a
filesystem.

```
npm install webr                      # ~20 MB, all of R
node run.mjs some-script.R            # runs it with this repo mounted at /repo
```

## What works and what does not

| | |
|---|---|
| R version | **4.6.0** — the same as the server |
| Available | `base`, `stats`, `utils`, `graphics`, `grDevices`, `methods`, `tools`, `grid`, `parallel`, … |
| **Not** available | `yaml`, `testthat`, `jsonlite`, `openxlsx`, `pdftools`, `magick`, `shiny`, `DT` |

WebR installs binary packages from `repo.r-wasm.org`, which an air-gapped or
proxied box cannot reach — so treat the built-in set as the whole set.

That rules out most of this suite: without `yaml` no template loads, so every
statement test fails for want of a template rather than for want of correctness.
What it **does** cover completely is the report extractor, which is deliberately
base-R only, and every pure helper in the engine.

`testthat-shim.R` supplies just enough of testthat (`test_that`, the `expect_*`
family, `skip_if_not`) for the repository's own test files to run unmodified. A
test that fails for a missing package is reported as a **skip**, not a failure,
so the report is about the code rather than about the sandbox.

## The measurement that matters

Run the suite against the commit you are changing **and** against the commit
before it, and compare. A large absolute failure count means nothing here; a
failure count that went **up** means something. On the round that added the
report extractor:

```
BASE 57fcbf6   pass 1905   fail 346   skip 311   over 72 files
HEAD           pass 2157   fail 347   skip 327   over 76 files
```

No existing file's failure count moved. That is the fact worth having, and the
only kind of assurance this harness can honestly give.
