#'Calculates share value using Residual Income plus present value of terminal value (PVTV).
#'@description
#'As with other valuation approaches, such as dividend discount model (DDM) and free cash flow, a multistage residual income approach can be used to forecast residual income for a certain time horizon and then estimate a terminal value based on continuing residual income at the end of that time horizon. Continuing residual income is residual income after the forecast horizon. As with other valuation models, the forecast horizon for the initial stage should be based on the ability to explicitly forecast inputs in the model. Because ROE has been found to revert to mean levels over time and it may decline to the cost of equity in a competitive environment, residual income approaches often model ROE fading toward the cost of equity. As ROE approaches the cost of equity, residual income approaches zero. An ROE equal to the cost of equity would result in residual income of zero. The PVTV incorporates the impact of \code{pf}, the persistence factor (Jerald E. Pinto, 2020).
#'@details
#'According to information provided by Jerald E. Pinto (2020), the method \code{shareValueRIplusPVTV} is developed to compute share value based on ROE growth under the Multistage Residual Income Valuation for the values passed to its six arguments. Here, \code{bgnBVPS} is the beginning Book Value Per Share, \code{EPS} is Earnings Per Share, \code{r} is the required rate of return on equity, \code{times} is a vector of number of years ranging from 1 to any specified number of years Residual Income Values are to be computed, \code{pf} is the persistence factor, \code{n} in one finite-horizon model of residual income valuation assumes that at the end of time horizon \code{n}, a certain premium over book value exists for the company.
#'@param bgnBVPS A number vector.
#'@param EPS A number vector.
#'@param r A number.
#'@param times A vector.
#'@param pf A number.
#'@param n A number.
#'@return Input values to six arguments  \code{bgnBVPS} \code{EPS}, \code{r}, \code{times},\code{pf},\code{n}.
#'@author MaheshP Kumar, \email{maheshparamjitkumar@@gmail.com}
#'@references
#'Pinto, J. E. (2020). Equity Asset Valuation (4th ed.). Wiley Professional Development (P&T). https://bookshelf.vitalsource.com/books/9781119628194
#'@examples
#'shareValueRIplusPVTV(bgnBVPS=c(6,7,8.25),EPS=c(2,2.5,4),r=0.10,times=c(1,2,3),pf=0.6,n=3)
#'@export
shareValueRIplusPVTV<-function (bgnBVPS,EPS,r,times,pf,n)
{
  RI <- (EPS-r*bgnBVPS)
  pv_RI  <-sum(RI/(1 + r)^times)
  share_Value <- bgnBVPS[1] + pv_RI + ((EPS[n]-r*bgnBVPS[n])/((1+r-pf)*(1+r)^(n)))
  (share_Value= round( share_Value, digits=3))
}
