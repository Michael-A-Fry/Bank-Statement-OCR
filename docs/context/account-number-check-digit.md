# The account-number check digit — researched, deliberately not switched on

A New Zealand bank account number carries its own check digit. That makes it the
second thing on a bank statement that can be verified without a second source — the
first being the running balance, which the engine already foots and cross-foots.

**It is not implemented.** `.kpi_account_number` in `R/reconcile.R` checks the
*shape* (2-4-7-2) and stops there. This page records the algorithm so the work is not
lost, and records honestly why finishing it would currently make the tool worse.

---

## Why it is not switched on

**It cannot be validated here.** Every account number in this repo's fixtures and
synthetic corpus is deliberately fake — which is the right call for test data in a
police tool — so there is not one known-good number to prove an implementation
against. Worse, several of the fake ones sit in *real* branch ranges
(`01-0234-…`, `02-1234-…`, `06-0193-…`), so switching the check on would brand the
test corpus invalid, and from then on the suite could not tell a true positive from
our own made-up data.

**An unvalidated check that can call a genuine statement invalid is worse than no
check.** A failing KPI takes trust to `low` and holds the run for review. Getting the
weightings or the branch ranges subtly wrong therefore means a real statement, read
perfectly, is held back — and the reason given is an accusation about the evidence.

**The tables go stale, and this machine is air-gapped.** Bank and branch ranges move
as banks merge and as new ranges are issued. A stale table fails in the direction of
rejecting valid new accounts, silently, on a server with no way to notice.

### What it would take to finish it

1. **One real statement per bank** whose account number is known good — enough to
   prove each algorithm path. Four or five banks covers almost all NZ volume
   (01 ANZ, 02 BNZ, 03 Westpac, 06 ANZ, 12 ASB, 38 Kiwibank).
2. The fixtures' fake numbers changed to values that **fail** the check digit while
   keeping a valid bank/branch, so the corpus proves the check fires — or moved to
   branch ranges that are not issued, so the check reports `na` and stays silent.
3. A dated note recording which version of the IRD table was used, because a figure
   in a forensic tool has to be attributable to the rule that produced it.

Until (1) exists, the shape check is the honest half: it needs no table, cannot go
stale, and catches the damage that actually happens to a scanned statement — a lost
or misread digit.

---

## The format

```
BB - BBBB - AAAAAAA - SS
bank  branch   base    suffix
 2      4        7      2 or 3     <- as PRINTED, the national convention
 2      4        8      4          <- the MAXIMA in the IRD spec, for the
                                      16-digit internal normalisation
```

For the algorithm the number is normalised to 16 digits: bank (2), branch (4), base
right-justified and zero-filled to 8, suffix right-justified and zero-filled to 4.

## The algorithm

Multiply each of the 16 digits by its weight, sum, and the total must be divisible by
the modulus. `A` in a weight column means **10**.

| Part | A | B | C | D | E | F | G | X |
|---|---|---|---|---|---|---|---|---|
| bank (2) | `00` | `00` | `37` | `00` | `00` | `00` | `00` | `00` |
| branch (4) | `6379` | `0000` | `0000` | `0000` | `0000` | `0000` | `0000` | `0000` |
| base (8) | `00A58421` | `00A58421` | `91A53421` | `07654321` | `00005432` | `01731731` | `01371371` | `00000000` |
| suffix (4) | `0000` | `0000` | `0000` | `0000` | `0001` | `0000` | `0371` | `0000` |
| modulus | 11 | 11 | 11 | 11 | 11 | 10 | 10 | 1 |

Three details that are easy to get wrong, and each of which turns the check into a
machine for rejecting valid accounts:

- **Algorithms E and G sum the digits of each product** before adding it to the
  total — twice if the result is still two digits. **F does not**, despite also being
  modulo 10. A widely-repeated summary of this spec says "F and G"; it is E and G.
- **A versus B is chosen by the base number, not the bank**: base below `00990000`
  uses A, otherwise B. So one bank uses both.
- **X always passes** (modulo 1 leaves no remainder). It is not a bug and must not be
  "fixed" into a rejection. C is unused.

## Bank ID to algorithm

| Bank | Branch ranges | Algorithm |
|---|---|---|
| 01 | 0001–0999, 1100–1199, 1800–1899 | A / B |
| 02 | 0001–0999, 1200–1299 | A / B |
| 03 | 0001–0999, 1300–1399, 1500–1599, 1700–1799, 1900–1999 | A / B |
| 06 | 0001–0999, 1400–1499 | A / B |
| 08 | 6500–6599 | D |
| 09 | 0000 | E |
| 11 | 5000–6499, 6600–8999 | A / B |
| 12 | 3000–3299, 3400–3499, 3600–3699 | A / B |
| 13–24, 27, 30, 35, 38 | various | A / B |
| 25 | 2500–2599 | F |
| 26 | 2600–2699 | G |
| 28, 29 | 2100–2299 | G |
| 31 | 2800–2849 | X |
| 33 | 6700–6799 | F |

**A bank or branch outside the table must report `na`, never `fail`.** An account the
table does not know is an account the table cannot judge, and the two are only the
same thing if you assume the table is complete and current. On an offline machine it
is neither.

## Sources

The authoritative document is Inland Revenue's *Bank Account Number Check Digit
Validation*. The table above was cross-read from two independent open-source
implementations of it, which is how the E/G digit-summing rule and the A/B selection
rule were pinned down:

- <https://github.com/td512/ird-bank>
- <https://en.wikipedia.org/wiki/New_Zealand_bank_account_number>

Both are secondary. Before this is switched on, check the table against the IRD
document itself and record its version here.

## Related

- `R/reconcile.R` — `.kpi_account_number`, the shape check that *is* implemented
- [charter.md](charter.md) — the rule this follows: the tool may refuse and it may
  explain, but it does not guess
