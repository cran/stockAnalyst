#'Calculates Predicted Price to Earning Multiple based on Cross-Sectional Regression.
#'@description
#'A predicted PE, which is conceptually similar to a justified PE, can be estimated from cross-sectional regressions of PE on the fundamentals believed to drive security valuation. This approach is pioneered by experts Kisor and Whitbeck 1963 and Malkiel and Cragg in 1970 (as cited in Jerald E. Pinto, 2020). The studies measured PEs for a group of stocks and the characteristics which determine PE such as: growth rate in earnings, payout ratio, and a measure of volatility, such as standard deviation of earnings changes or beta. An analyst can conduct such cross-sectional regressions by using any set of explanatory variables considered to determine investment value. The analyst must bear in mind; however, the potential distortions that can be introduced by multi-collinearity among independent variables (Jerald E. Pinto, 2020).
#'@details
#'According to information provided by Jerald E. Pinto (2020), the method \code{predictedPEonCSR} is developed for computing Cross-Sectional Regression for values passed to its seven arguments. Here, \code{b0} is intercept, \code{b1} is given coefficient of x1DRP,  \code{b2} is given coefficient of x2Beta, \code{b3} is given coefficient of x3EGR, \code{x1DRP} is Dividend Payout Ratio that is taken as first variable X1, \code{x2Beta} is company beta that is taken as  variable X2, and \code{x3EGR} is five-year earnings growth rate that is taken as variable X3 of the regression equation.
#'@param  b0 number.
#'@param  b1 number.
#'@param  b2 number.
#'@param  b3 number.
#'@param  x1DRP number.
#'@param  x2Beta number.
#'@param  x3EGR number.
#'@return Input values to seven arguments  \code{b0}, \code{b1}, \code{b2}, \code{b3},\code{x1DRP},\code{x2Beta}, and  \code{x3EGR}.
#'@author MaheshP Kumar, \email{maheshparamjitkumar@@gmail.com}
#'@references
#'Pinto, J. E. (2020). Equity Asset Valuation (4th ed.). Wiley Professional Development (P&T). https://bookshelf.vitalsource.com/books/9781119628194
#'@examples
#'predictedPEonCSR(b0=12.12, b1=2.25, b2= -0.20, b3=14.43, x1DRP=0.45, x2Beta=0.9, x3EGR=0.08)
#'@export
predictedPEonCSR<-function (b0,b1,b2,b3,x1DRP,x2Beta,x3EGR){
  disp_val<- b0 + b1*x1DRP + b2*x2Beta + b3*x3EGR
  (disp_val = round(disp_val, digits=1))
}
