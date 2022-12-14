#'Calculates value of a share based on return on equity (ROE) growth under the Multistage Residual Income Valuation.
#'@description
#'In many applications, a drawback to the single-stage model is that it assumes the excess ROE above the cost of equity will persist indefinitely. More likely, a  ROE of the company will revert to a mean value of ROE over time, and at some point, the residual income will be zero. If a company or industry has an abnormally high ROE, other companies will enter the marketplace thus increasing competition and lowering returns for all companies. Similarly, if an industry has a low ROE, companies will exit the industry (through bankruptcy or otherwise) and ROE will tend to rise over time. As with the single-stage DDM, the single-stage residual income model also assumes a constant growth rate through time. In light of these considerations, the residual income model has been adapted in practice to handle declining residual income. For example, Lee, Myers, and Swaminathan (as cited in Jerald E. Pinto, 2020) used a residual income model to value the Dow by assuming that ROE fades (reverts) to the industry mean over time. Lee and Swaminathan found that the residual income model had more ability than traditional price multiples to predict future returns. Fortunately, other models are available that enable analysts to relax the assumption of indefinite persistence of excess returns.
#'@details
#'According to information provided by Jerald E. Pinto (2020), the method \code{shareValueRImultiStg} is developed to compute share value based on ROE growth under the Multistage Residual Income Valuation for the values passed to its six arguments. Here, \code{bgnBV} is beginning Book Value Per Share, \code{ROE} is Return on Equity, \code{r} is required rate of return on equity, \code{tm} is a vector of number of years ranging from 1 to any specified number of years Residual Income Values are to be computed, \code{premium} certain premium over book value, \code{n} in one finite-horizon model of residual income valuation assumes that at the end of time horizon \code{n}, a certain premium over book value exists for the company.
#'@param bgnBV A number.
#'@param ROE A vector.
#'@param r A number.
#'@param tm A vector.
#'@param pr A number.
#'@param n A number.
#'@return Input values to six arguments  \code{bgnBV} \code{ROE}, \code{r}, \code{tm} , \code{pr} and \code{n}.
#'@author MaheshP Kumar, \email{maheshparamjitkumar@@gmail.com}
#'@references
#'Pinto, J. E. (2020). Equity Asset Valuation (4th ed.). Wiley Professional Development (P&T). https://bookshelf.vitalsource.com/books/9781119628194
#'@examples
#'shareValueRImultiStg(ROE=c(0.3333,0.3571,0.4848),bgnBV=c(6,7,8.25),r=0.10,tm=c(1,2,3),pr=1.1,n=3)
#'@export
shareValueRImultiStg<-function (ROE,bgnBV,r,tm,pr,n){
  RI <- ((ROE-r)*bgnBV)
  pv_RI=sum(RI/(1 + r)^tm)
  share_Valued <- (bgnBV[1]+pv_RI+ (pr/(1 + r)^n))
  (share_Valued= round(share_Valued, digits=2))
}
