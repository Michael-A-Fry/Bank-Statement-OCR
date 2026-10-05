QVF MINING BRIEF (shared by five readers)

The team's current tool is a Qlik Sense app, "Statement Converter". Its load script has been extracted to:
  /tmp/claude-0/-home-user-Bank-Statement-OCR/2deb355e-d37d-57e7-92d4-15560b8cd423/scratchpad/qvf/script.qvs   (7,642 lines)

How it works:
- A person picks the statement type on a form.
- A PDF connector ("Mole") hands the script every word with its position (WordPosition).
- One hand-written section per type then walks those words using fixed landmarks, for example "Account at a glance" or "Statement period".
- Shared subroutines and maps are at lines 107-1786. The output is assembled at lines 7479-7642.
It "works on a lot of statements" because it knows these types exactly.

The NEW tool, which you are measuring, is the R repo at /home/user/Bank-Statement-OCR. Read its README.md and docs/design.md first.
- It reads any statement by position and PROVES each reading with the statement's own arithmetic: running balance, opening + rows = closing, printed totals.
- Outcomes: proven / layout_match (automatic); check / unread (a person looks).
- A wrong figure in an automatic outcome is the worst failure (AUTO_WRONG). It must be 0.

YOUR JOB, for each statement type in your area:
1. FORMAT CARD -> scratchpad/qvf/cards/<type_key>.md. In plain words, describe:
   - what the statement looks like as the QVF expects it: the landmarks it searches for, the page furniture, the table columns in order, how money in/out is told apart (separate columns, CR/DR, OD, sign), how the year is found, how several statements in one file are split, and which lines it drops (totals, interest lines, "No transactions for this period", carried forward and so on);
   - every special case the script handles: quote the line numbers, and say WHY each one exists (a special case is a real-world quirk);
   - which output columns the QVF fills for this type (Transaction Type, Code Description, Other Party Account Name/Number, Transaction Time, Sort Number and so on) and how.
   Do NOT copy people's names, server paths or anything personal from the script into your files.
2. LOOKALIKE STATEMENTS -> a Python generator at scratchpad/qvf/gen/make_<type_key>.py that writes into scratchpad/qvf/sets/<area_key>/.
   - At least 6 statements per type: text PDFs for PDF types, and the real file shape (xlsx/csv) for Excel types. Each must have an answer key <name>.truth.json in EXACTLY the format tools/synth/make_layouts.py writes (read it, and reuse its drawing helpers by importing it if practical; read tools/synth/README.md).
   - Make them faithful to what the script expects: the same landmark words, column order, markers and quirks.
   - Include the type's edge cases as separate statements, for example a statement with no transactions, an OD balance, a period crossing a new year, two or three statements in one file, a "START - date" period, wrapped descriptions, and interest lines.
   - All data invented: no real names, accounts or merchants that identify anyone.
3. MEASURE: run `cd /home/user/Bank-Statement-OCR && Rscript tools/synth/score_auto.R <your set dir> --mode cold --out <your set dir>/cold.csv`, then again with `--mode trained`. Cold is "first go, nothing learned", which is what the owner asked about.
4. DIAGNOSE every statement that is not auto_right:
   - find the root cause in the new tool's code, with file:line;
   - propose a fix as ONE plain sentence stating a GENERAL rule, never "if ANZ then ...". The owner's policy: every fix is a general rule; park low-realism cases;
   - flag any AUTO_WRONG loudly.
5. OUTPUT FIELDS: compare what the QVF outputs for this type with what the new tool writes (R/outputs.R and R/feed.R). List any field the QVF fills that the new tool does not, or fills differently. Qlik dashboards may depend on them.

RULES
- Do NOT edit anything in /home/user/Bank-Statement-OCR (read-only). Write only under scratchpad/qvf/.
- Never read /root/.claude/uploads or /tmp/priv* (private files).
- Do not run the whole test suite.

REPORT (your final message is data, not a chat reply):
(a) per type: the card path, the number of statements, cold and trained counts per cell (auto_right / AUTO_WRONG / check_right / check_wrong / unread);
(b) the failure list: statement, cell, root cause file:line, one-sentence general rule fix, severity (S1 = AUTO_WRONG, S2 = should have been automatic, S3 = other);
(c) QVF output fields the new tool lacks;
(d) anything in the QVF that is clever and that the new tool should copy as a general rule.
