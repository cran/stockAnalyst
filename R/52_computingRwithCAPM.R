#'Calculates CAPM based required rate of return.
#'@description
#'The CAPM is an equation for required return that should hold in equilibrium (the condition in which supply equals demand) if the assumptions of the model are met; among the key assumptions are that investors are risk averse and that they make investment decisions based on the mean return and variance of returns of their total portfolio. The chief insight of the model is that investors evaluate the risk of an asset in terms of contribution of the asset to the systematic risk of their total portfolio (systematic risk is risk that cannot be shed by portfolio diversification). Because the CAPM provides an economically grounded and relatively objective procedure for required return estimation, it has been widely used in valuation (Jerald E. Pinto, 2020).
#'@details
#'The CAPM based Required return on share  is equal to currently expected Risk Free Return (RFR)  plus market beta that is multiplied with Equity Risk Premium (ERP).For example, if the current expected risk-free return is 3 percent, the market beta of the asset is 1.20, and the equity risk premium is 4.5 percent, then the required return is  0.030 + 1.20*(0.045) = 0.084 or 8.4 percent.Based on this information provided by Jerald E. Pinto (2020), the method \code{computingRwithCAPM} is developed for computing CAPM based required rate of return for the values passed to its three arguments.Here, \code{RFR} is currently expected Risk Free Return, \code{marketBeta} the market beta of the asset and, \code{ERP} represents Equity Risk Premium.
#'@param RFR A number.
#'@param marketBeta A number.
#'@param ERP A number.
#'@return Input values to three arguments  \code{RFR} , \code{marketBeta} and \code{ERP}.
#'@author MaheshP Kumar, \email{maheshparamjitkumar@@gmail.com}
#'@references
#'Pinto, J. E. (2020). Equity Asset Valuation (4th ed.). Wiley Professional Development (P&T). https://bookshelf.vitalsource.com/books/9781119628194
#'@examples
#'computingRwithCAPM(RFR=.049,marketBeta=0.74,ERP=0.045)
#'computingRwithCAPM(RFR=.05,marketBeta=1.00,ERP=0.041)
#'@export
computingRwithCAPM <-function(RFR,marketBeta,ERP){
  r <- RFR + marketBeta*ERP
  (r = round(r, digits=4))
}
