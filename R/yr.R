#' year with fraction`
#'
#' `yr` converts a `Date` to a year and fraction. For example, 2025-01-01 
#' becomes 2025.00000, while 2025-01-02 becomes 2025.00234, because (2-1)/365 
#' is 0.00234 to 5 significant digits. However, 2024-01-02 becomes 2024.0233, 
#' because (2-1)/366 is only 0.00233 to 5 significant digits. 
#' 
#' @param x quantity that can be converted to a `Date` object using 
#' `as.Date(x)`. 
#' @param \dots arguments passed to [lubridate::ymd()].
#' 
#' @returns a number (numeric vector). 
#' 
#' @export
#'
#' @examples
#' Jan2_24_25 <- c('2024-01-02', '2025-01-02')
#' J2yr <- yr(Jan2_24_25)
#' J2y <- yr(as.POSIXct(Jan2_24_25))
#' all.equal(J2yr, J2y)
#' 
#' @seealso [lubridate::decimal_date()], [lubridate::ymd()]
#' @keywords manip
yr <- function(x, ...)lubridate::decimal_date(lubridate::ymd(x, ...))