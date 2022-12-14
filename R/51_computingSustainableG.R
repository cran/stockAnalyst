#'Calculates Sustainable Growth Rate.
#'@description
#'Sustainable growth rate as the rate of dividend (and earnings) growth that can be sustained for a given level of return on equity, assuming that the capital structure is constant through time and that additional common stock is not issued. The reason for studying this concept is that it can help in estimating the stable growth rate in a Gordon growth model valuation.Sustainable growth rate(g) is equal to  earnings retention rate , represented by \code{b} (that is equal to 1 minus dividend payout ratio) multiplied with return on equity (Jerald E. Pinto, 2020).
#'@details
#'According to information provided in Jerald E. Pinto (2020), the method \code{computingSustainableG} is developed for computing Sustainable Growth Rate for the values passed to its two arguments.Here, \code{retentionRate} is retention rate (that is equal to 1 minus dividend payout ratio), and \code{ROE} is return on equity.
#'@param retentionRate A number.
#'@param ROE A number.
#'@return Input values to two arguments  \code{retentionRate} and \code{ROE}.
#'@author MaheshP Kumar, \email{maheshparamjitkumar@@gmail.com}
#'@references
#'Pinto, J. E. (2020). Equity Asset Valuation (4th ed.). Wiley Professional Development (P&T). https://bookshelf.vitalsource.com/books/9781119628194
#'@examples
#'computingSustainableG(retentionRate=0.60,ROE=0.25)
#'@export
computingSustainableG <-function(retentionRate,ROE){
  g <- retentionRate * ROE
  (g = round(g, digits=4))
}

