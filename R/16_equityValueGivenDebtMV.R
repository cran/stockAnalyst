#'Calculates the amount of estimated total equity value by deducting the given Market Value of Debt from  Value of firm based on Discounted FCFF.
#'@description
#'The FCFF valuation approach estimates the value of the firm as the present value of future FCFF discounted at the weighted average cost of capital. Because FCFF is the cash flow available to all suppliers of capital, using WACC to discount FCFF gives the total value of all of the firm’s capital. The value of equity is the value of the firm minus the market value of its debt (Jerald E. Pinto, 2020).
#'@details
#'According to information provided by Jerald E. Pinto (2020), the method \code{equityValueGivenDebtMV} is developed to compute estimated total equity value by deducting the given Market Value of Debt from Discounted Value of FCFF for the values passed to its four arguments. Here, \code{FCFF} is given amount of future Free Cash Flow to the Firm (FCFF) in millions of dollars. For example, a value of 0.04 means 0.4 millions or 400,000 dollars , \code{t} is a vector of number of years ranging from 1 to any specified number of years for which FCFF is to be discounted,  \code{WACC} is Weighted Average Cost of Capital and \code{debtMV} is Market Value of the debt. Values used for FCFF, Market Value of Debt and the output obtained are in millions of dollars. An output of 1.00494 means 1,004,940 dollars.
#'@param FCFF A vector.
#'@param t A vector.
#'@param debtMV A number
#'@param WACC A number.
#'@return Input values to three arguments  \code{FCFF}, \code{t},\code{debtMV}, and  \code{WACC}.
#'@author MaheshP Kumar, \email{maheshparamjitkumar@@gmail.com}
#'@references
#'Pinto, J. E. (2020). Equity Asset Valuation (4th ed.). Wiley Professional Development (P&T). https://bookshelf.vitalsource.com/books/9781119628194
#'@examples
#'equityValueGivenDebtMV(FCFF=c(0.4,0.4,0.4,0.4),t=c(1,2,3,4),WACC=0.12,debtMV= 0.21)
#'@export
equityValueGivenDebtMV <-function (FCFF,t,WACC,debtMV)
{
  firmValue <- sum(FCFF/(1 + WACC)^t)
  equityVal <- firmValue - debtMV
  (equityVal = round(equityVal, digits=6))
}
