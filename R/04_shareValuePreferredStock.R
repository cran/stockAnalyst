#'Calculates value of non-callable fixed-rate Perpetual Preferred Stock.
#'@description
#'The Gordon growth model can also be used to value the non-callable form of a traditional type of preferred stock, fixed-rate perpetual preferred stock (stock with a specified dividend rate that has a claim on earnings senior to the claim of common stock, and no maturity date). Perpetual preferred stock has been used particularly by financial institutions such as banks to obtain permanent equity capital while diluting the interests of common equity (Jerald E. Pinto, 2020).
#'@details
#'If the dividend on such preferred stock is D, it is because payments extend into the indefinite future a perpetuity (a stream of level payments extending to infinity) exists in the constant amount of D. With g = 0, which is true because dividends are fixed for such preferred stock, the Gordon growth model becomes Share value is equal to amount of dividend, divided by required rate of return. In light of this information provided by Jerald E. Pinto (2020), the method \code{shareValuePreferredStock} is developed to compute the value of non-callable fixed-rate Perpetual Preferred Stock for the values passed to its two arguments. Here,\code{dividend} is fixed amount of dividend and \code{r} is required rate of return.
#'@param dividend A number.
#'@param r A number.
#'@return Input values to two arguments  \code{dividend} and \code{r}.
#'@author MaheshP Kumar, \email{maheshparamjitkumar@@gmail.com}
#'@references
#'Pinto, J. E. (2020). Equity Asset Valuation (4th ed.). Wiley Professional Development (P&T). https://bookshelf.vitalsource.com/books/9781119628194
#'@examples
#'shareValuePreferredStock(dividend=1.00,r=0.09)
#'@export
shareValuePreferredStock <-function(dividend,r){
   share_value <- (dividend/r)
  (share_value = round(share_value, digits=2))
}

