# Engine parameters — the tuning numbers, in one visible place

The engine makes a handful of **numeric judgement calls**: how far apart two
money figures can be and still "reconcile", which years are plausible for a
statement date, when a page is too image-like to read without OCR, when a word is
"drawn over". These are neither vocabulary nor deployment switches — they're
algorithmic tuning a maintainer changes deliberately, with the test suite as the
guard. To keep every such decision *visible* (rather than a bare `0.005` or
`1990` buried mid-function and repeated in five places), they all live named and
commented in [`R/params.R`](../../R/params.R).

## Why a file, not the lexicon or config

This is the third lane, distinct from the two customisation tiers:

| | Holds | Who changes it | Where |
|---|---|---|---|
| **Lexicon** | recognition *vocabulary* (markers, synonyms, regexes) | admin, at runtime, hot-applied | `dictionaries/lexicon.yaml` |
| **Config** | *deployment* switches (feed gate, paths, metadata level) | admin/installer | `config/config.yaml` |
| **Parameters** | algorithmic *tuning* (tolerances, thresholds, bounds) | maintainer, with tests | `R/params.R` |

Parameters are deliberately **not** admin-editable and **not** in config: a
wrong money tolerance or year bound could make the engine silently-wrong, which
is exactly what the charter forbids a non-maintainer from being able to do by
turning a knob. They change in code, behind the suite. The value of pulling them
into one file isn't runtime editability — it's that a reviewer can read every
tuning decision the engine makes on a single screen, and each is defined once.

## The catalogue

Every constant, what it decides, and the effect of moving it. Values shown are
the shipped defaults.

### Dates — the trusted-year window
| Parameter | Default | Decides |
|---|---|---|
| `PARAM_YEAR_MIN` | `1990` | A parsed year below this is treated as a mis-parse (a 2-digit year read as 4-digit, OCR noise, a footer/copyright year) and the date is rejected as untrustworthy rather than emitted wrong. |
| `PARAM_YEAR_MAX` | `2100` | The symmetric upper bound. |

This window is the one the date parser (`.date_strict`), the reconciliation period
check, the diagnostics effective-range check, and the year-from-text fallback all
share, via the `.plausible_year()` helper — so they can never disagree about what
year is believable. **To support genuinely older archives**, widen `PARAM_YEAR_MIN`
here once and every site follows. Narrow the window and you reject more as
mis-parses (safer, but may refuse a real edge date); widen it and you trust more
(riskier). Real statements sit well inside `[1990, 2100]`.

### Money — the reconciliation tolerance
| Parameter | Default | Decides |
|---|---|---|
| `PARAM_MONEY_TOL` | `0.005` | Two money figures are "equal" when they differ by less than half a cent. Used by `balance_reconciliation` and `running_balance_continuity` (never `==`, which is unsafe on floats). |

Tighten it (e.g. `0.0005`) and legitimate half-cent rounding in a source could
fail reconciliation; loosen it (e.g. `0.05`) and a real 4-cent error could pass.
Half a cent is the sweet spot for statements quoted to the cent.

### OCR — routing and confidence
| Parameter | Default | Decides |
|---|---|---|
| `PARAM_OCR_MIN_CHARS` | `20` | Below this many non-space characters, a page's text layer is "effectively empty" → OCR it (unless it has real word boxes, i.e. a digital PDF). |
| `PARAM_OCR_MIN_WORDS` | `3` | Fewer real word boxes than this → a scanned page whose only digital text is an incidental stamp/footer → OCR it. |
| `PARAM_OCR_MAX_BAD_RATIO` | `0.30` | More than this fraction of garbage characters (broken CID font) → OCR it. |
| `PARAM_OCR_CELL_MIN_CONF` | `60` | A per-cell OCR confidence (0–100) below this in a date/amount/balance cell earns an `ocr_low_conf` flag on that row. |
| `PARAM_OCR_PAGE_MIN_CONF` | `70` | A page-mean OCR confidence below this raises a high-severity "OCR is unsure" diagnostic. |

The two confidence floors are conservative on purpose: only clearly-doubtful
reads are flagged, so the signal stays meaningful. The routing three decide
*whether a page is read by OCR at all* — a digital PDF (word boxes present) is
never OCR'd regardless.

### Oversized-input advisories (not hard limits)
| Parameter | Default | Decides |
|---|---|---|
| `PARAM_MAX_PAGES` | `100` | PDFs longer than this get a "may hit tool limits" caution in diagnostics. |
| `PARAM_MAX_PAGE_PT` | `2880` | A page dimension over this (40 inches) gets a "can break rendering/OCR" caution. |

The engine still *tries* on oversized input — these only ensure a stall has a
visible explanation rather than looking like a silent hang.

### Redaction — the occlusion scan
| Parameter | Default | Decides |
|---|---|---|
| `PARAM_REDACT_DARK_LEVEL` | `60` | A rendered greyscale pixel (0 black … 255 white) below this counts as "dark". |
| `PARAM_REDACT_OCC_THRESH` | `0.70` | A word whose box is this fraction-or-more dark pixels is treated as drawn-over (redacted). |
| `PARAM_REDACT_VECTOR_DPI` | `150` | The render resolution for the occlusion scan. |

`DARK_LEVEL` and `OCC_THRESH` together answer "is this word hidden under a box?".
Over-detection (a genuinely very dark word) merely over-redacts — the contract's
declared safe failure — so the defaults lean toward catching redactions.

## Changing one

1. Edit the value in `R/params.R`.
2. Run `Rscript tests/run_tests.R` — the suite is the guard; several tests pin
   exact reconciliation and date-parse outcomes that these numbers drive.
3. If a test now fails, that's the parameter's blast radius made visible — decide
   whether the new behaviour is intended before updating the expectation.
