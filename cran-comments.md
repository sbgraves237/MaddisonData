## R CMD check results

* This is release 1.1.0 to CRAN.

0 errors | 0 warnings | 0 notes
On local computer including with "--as-cran" and 5 standard GitHub actions. 

devtools::check_win_devel() flagged several "(possibly) invalid URLs", e.g.: 
URL: https://en.wikipedia.org/wiki/Maddison_Project"

They were all Wikipedia, which I suspect probably has code that identifies bots 
and refuses to respond. 
urlchecker::url_check() locally said "All URLs are correct!"