#'Calculates required rate of return on equity based on Fama French Model.
#'@description
#'By the end of the 1980s, empirical evidence had accumulated that, at least over certain long time periods, in the US and several other equity markets, investment strategies biased toward small-market capitalization securities and/or value might generate higher returns over the long run than the CAPM predicts. In 1993, researchers Eugene Fama and Kenneth French addressed these perceived weaknesses of the CAPM in a model with three factors, known as the Fama–French model (FFM). The FFM is among the most widely known non-proprietary multi-factor models. The factors are RMRF, standing for RM minus RF, the return on a market value(RM)-weighted equity index in excess of the one-month T-bill rate based on face value (RF); this is one way the equity risk premium(ERP) can be represented and ERP is the factor that FFM shares with the CAPM. The second factor is SMB (small minus big), which is a size (market capitalization) factor. SMB is the average return on three small-cap portfolios minus the average return on three large-cap portfolios. Thus SMB represents a small-cap return premium.Third factor is HML (high minus low), the average return on two high book-to-market portfolios minus the average return on two low book-to-market portfolios. With high book-to-market (equivalently, low price-to-book) shares representing a value bias and low book-to-market representing a growth bias, in general, HML represents a value return premium (Jerald E. Pinto, 2020).
#'@details
#'Based on the information provided by Jerald E. Pinto (2020), the method \code{computingRwithFFM} is developed for computing required rate of return on equity based on Fama–French Model for the values passed to its seven arguments. Here, \code{RFR} is risk free return, \code{marketBeta} is market beta, \code{sizeBeta} is size beta,\code{valBeta} is value beta, \code{RMRF} represents equity risk premium, \code{SMB} represents small cap risk premium and \code{HML} represents value premium.
#'@param RFR A number.
#'@param marketBeta A number.
#'@param sizeBeta A number.
#'@param valBeta A number.
#'@param RMRF A number.
#'@param SMB A number.
#'@param HML A number.
#'@return Input values to seven arguments  \code{RFR} , \code{marketBeta} , \code{sizeBeta} , \code{valBeta} , \code{RMRF} , \code{SMB} and \code{HML}.
#'@author MaheshP Kumar, \email{maheshparamjitkumar@@gmail.com}
#'@references
#'Pinto, J. E. (2020). Equity Asset Valuation (4th ed.). Wiley Professional Development (P&T). https://bookshelf.vitalsource.com/books/9781119628194
#'@examples
#'computingRwithFFM(RFR=0.041,marketBeta=1.2,sizeBeta=0.5,valBeta=0.8,RMRF=0.055,SMB=0.02,HML=0.043)
#'@export
computingRwithFFM <-function(RFR,marketBeta,sizeBeta,valBeta,RMRF,SMB,HML){
  r <- RFR + marketBeta*RMRF + sizeBeta*SMB + valBeta*HML
  (r = round(r, digits=3))
}
