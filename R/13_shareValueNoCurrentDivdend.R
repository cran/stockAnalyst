#'Calculates value of a share of a Non-Dividend-Paying Company.
#'@description
#'The fact that a stock is currently paying no dividends does not mean that the principles of the dividend discount model do not apply. Even though D0 (current dividend) and/or D1(dividend in one year) may be zero, and the company may not begin paying dividends for some time (say five years), the present value of future dividends may still capture the value of the company. Assume that a company is currently paying no dividend and will not pay one for several years. If the company begins paying a dividend of $1.00 five years from now, and the dividend is expected to grow at 5 percent thereafter, this future dividend stream can be discounted back to find the value of the company share at given discount rate. Of course, if a company never ever pays any dividends and as the result will never be able to distribute cash to shareholders, in that case the stock is worthless (Jerald E. Pinto, 2020).
#'@details
#' According to information provided by Jerald E. Pinto (2020), the method \code{shareValueNoCurrentDivdend} is developed for computing  value of a share of a Non-Dividend-Paying Company for the values passed to its four arguments. Here, \code{divN} is the dollar value of the dividend beginning in n years (say 5 years), \code{t} is number of years at which company is expected to start paying dividends, for example, 5 years, \code{g} is the rate at which the dividend is expected to grow, and \code{r} is the discount rate (or required rate of return on equity).
#'@param divN A number.
#'@param t A number.
#'@param g A number.
#'@param r A number.
#'@return Input values to four arguments  \code{divN} , \code{t}, \code{g} and \code{r}.
#'@author MaheshP Kumar, \email{maheshparamjitkumar@@gmail.com}
#'@references
#'Pinto, J. E. (2020). Equity Asset Valuation (4th ed.). Wiley Professional Development (P&T). https://bookshelf.vitalsource.com/books/9781119628194
#'@examples
#'shareValueNoCurrentDivdend(divN=1.00,t=5, g=0.05,r=0.11)
#'shareValueNoCurrentDivdend(divN=1.20,t=3, g=0.07,r=0.15)
#'@export
shareValueNoCurrentDivdend <-function(divN,t, g, r){
  shareValue <- divN/(r - g)
  discShareValue <- shareValue/((1+r)^(t-1))
  (discShareValue  = round(discShareValue , digits=2))
}
