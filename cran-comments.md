## R CMD check results

0 errors | 0 warnings | 0 notes

* This is release 1.0.2 to CRAN replacing ticks with single quotes in 
DESCRIPTION. (Please excuse: I had single quote, but devtools::spell_check() 
complained about a variable name, and so I tried ticks instead. That change
did not change the result with devtools::spell_check(), but it still passed 
all other checks, so I left it.

Checked fine on 5 standard GitHub Actions plus check_win_devel and locally with 
"R CMD check --as-cran". 