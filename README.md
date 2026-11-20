# statematch

Turn messy US state names into clean abbreviations, names and FIPS codes — and
tolerate small misspellings while doing it. Base R only, no dependencies.

```r
library(statematch)

state_abb(c("Arizona", "tx", "Tenessee", "N. Dakota", "West Virgina"))
#> "AZ" "TX" "TN" "ND" "WV"

state_match(c("Pennsylvnaia", "Mis", "Narnia"))   # shows how each was matched
abbr2state(c("AZ", "tx"))                              # strict reverse lookup
state_name("wi"); state_fips("DC")
```

## How matching works
1. Postal abbreviation (`"az"`).
2. Exact full name or alias after normalising case, punctuation and whitespace
   (`"Washington D.C."`, `"N. Dakota"`).
   Traditional abbreviations are aliases (`"Calif"`, `"Ariz"`, `"Penn"`,
   `"Tenn"`, `"Wash"`, `"Miss"`, ...).
3. Unique prefix, 3+ letters (`"cal"`, `"tex"`, `"N Car"`).
4. Fuzzy match with a transposition-aware edit distance. Allowed edits scale
   with name length (`max_dist`, default 0.25).

Guard rails: if a prefix fits several states (`"Mis"`, `"New"`) the result is
`NA` with `method = "ambiguous"`; inputs under 4 characters are never fuzzy
matched; two-letter codes are never "corrected". Use `state_match()` to audit
`method` and `distance`, and review anything that is not `"exact"` or
`"abbreviation"`.

## Improvements over `usdata::abbr2state` / `state2abbr`
- No placeholder-digit hacks for Dakota/Carolina/Arkansas/Alaska; ambiguity is
  handled explicitly.
- Catches transposition typos, reports how each match was made.
- Also returns names and FIPS; optional territories; NA-safe; factors OK.

## Install (local / GitHub)
```r
install.packages("path/to/statematch", repos = NULL, type = "source")
# after pushing to GitHub:
# remotes::install_github("bjjefferies/statematch")
```
