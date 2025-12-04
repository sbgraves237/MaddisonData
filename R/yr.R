#' year with fraction`
#'
#' `yr` converts a `Date` to a year and fraction. For example, 2025-01-01 
#' becomes 2025.00000, while 2025-01-02 becomes 2025.00234, because (2-1)/365 
#' is 0.00234 to 5 significant digits. However, 2024-01-02 becomes 2024.0233, 
#' because (2-1)/366 is only 0.00233 to 5 significant digits. 
#' 
#' @param x quantity that can be converted to a `Date` object using 
#' `as.Date(x)`. 
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
#' @keywords manip
yr <- function(x){
  # date as year and fraction. 
  X <- as.Date(x)
  Yr <- lubridate::year(X) # year 
  yd <- lubridate::yday(X) # days since last year 
  x1 <- (X-yd+1)
  x2 <- x1
  lubridate::year(x2) <- Yr+1
  daysInYr <- (x2-x1)
  Yr_ <- Yr + ((yd-1)/as.numeric(daysInYr))
}