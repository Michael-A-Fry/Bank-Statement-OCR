# Customisation & scaling — how to teach the engine new things

Once the engine logic is stable, almost everything you'd want to change is **data,
not code**. There are three tiers, with a clear precedence, and a human-approved
loop that lets the tool *learn* new vocabulary without ever giving up determinism.

## The three tiers (precedence: template > lexicon > built-in)

| Tier | Holds | Where | Who edits it |
|---|---|---|---|
| **1. Templates** | per-bank FACTS — this bank's columns, date format, fingerprint, and its debit token (`type_debit_value: cow`) | `templates/*.yaml` | analyst, via the builder / Advanced YAML |
| **2. Lexicon** | the engine's generic RECOGNITION vocabularies — the synonyms/markers/shapes it *tries* when it auto-detects, drafts and parses | `dictionaries/lexicon.yaml` | admin, via **Admin → Data capture → Recognition vocabulary** |
| **2b. Label dictionary** | label WORDINGS ("Opening Balance" synonyms) | `dictionaries/labels.yaml` | admin, via the dictionary editor |
| **3. Built-in defaults** | the values shipped in code; the lexicon falls back to these | `R/lexicon.R` | maintainer (bug fixes only) |

**Config is NOT a customisation tier** — `config/config.yaml` holds deployment
switches (feed gate, metadata level, paths, admin password), never vocabulary.

### Worked example — a bank writes `cow`/`horse` for debit/credit

- **If it's just this one bank:** the drafter now infers it automatically (it reads
  the debit/credit markers from the lexicon). If you ever need to pin it by hand,
  the template carries `type_debit_value: cow` / `type_credit_value: horse`.
- **If you want the engine to recognise `cow`/`horse` everywhere, forever:** add to
  the lexicon —
  ```yaml
  debit_markers:  [cow]
  credit_markers: [horse]
  ```
  That one edit plumbs through **detection** (the column is recognised as an
  indicator), **drafting** (`type_debit_value` is inferred), and **parsing** (rows
  sign correctly). No code change. Word lists ADD to the built-ins (you keep
  `D`/`DR`/…); a regex REPLACES (and is rejected if it won't compile); date formats
  append; field patterns override per field.

Everything the engine "checks for" reads from the lexicon: `debit_markers`,
`credit_markers`, `amount_style_debit_headers`, `amount_style_credit_headers`,
`dr_cr_suffix_debit`, `dr_cr_suffix_credit`, `overdrawn_markers`,
`period_connectives`, `header_keywords`, `layout_stopwords`, `redaction_markers`,
`redaction_block_glyphs`, `money_regex`, `date_regex`, `account_regex`,
`card_regex`, `date_formats`, `field_name_patterns`.

## The learning loop (deterministic, human-approved)

This is how the tool gets smarter over time **without** the model ever changing a
figure:

1. **Capture.** Every conversion records what it did NOT recognise — indicator
   tokens matching neither declared value, columns no template mapped (see
   [metadata-capture.md](metadata-capture.md), `novelty.*`). Local, forever, PII-safe.
2. **Aggregate.** `lexicon_suggestions()` ranks those across the whole
   `logs/metadata` corpus by frequency ("`COW` seen 40× as an unrecognised
   indicator").
3. **Propose.** Admin → Data capture → **Suggestions from your data** shows the
   ranked list. *(Today the ranking is plain frequency; a local model can later
   slot in here to rank/classify better — the rest of the loop is unchanged.)*
4. **Approve.** A human picks a token and the direction it means and clicks
   Approve; it is appended to the lexicon.
5. **Act.** The deterministic engine reads the approved vocabulary on the next
   conversion.

**The invariant:** a model may only ever *propose* additions to human-approved,
externalised, deterministic vocabularies (lexicon or templates). It never changes
engine behaviour directly. That is what keeps the charter's "never silently wrong"
intact while the system learns — the approval gate is the boundary between
probabilistic suggestion and deterministic behaviour.

## Best practice as you scale

- **New per-bank layout →** a template (builder). **New recognised word/marker →**
  the lexicon. **New behaviour/logic →** code (a maintainer, with tests). Keep those
  three lanes distinct and the engine stays stable.
- The lexicon ships **empty** (all categories fall back to built-ins), so a fresh
  install behaves exactly like the tested engine; you extend only what you need.
- Every override is validated on save (a bad regex is rejected, not silently
  applied), backed up (`*.bak`), and hot-applied (the cache clears) — so editing
  vocabulary is safe and reversible.
- `BSO_LEXICON` (env) can point the engine at an alternate lexicon file per
  deployment.
