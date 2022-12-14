#'Calculates the amount of estimated total equity value by deducting the given Market Value of Debt from Value of firm based on single stage constant growth of FCFF.
#'@description
#'Amount of estimated total equity value is obtained by deducting the given Market Value of Debt from Value of firm based on single stage constant growth of FCFF. Consider that FCFF grows at a constant rate, \code{g}, such that FCFF in any period is equal to FCFF in the previous period multiplied by (1 + g). This means that this method is based on single stage constant growth model. So, FCFFt = FCFF(t–1) times (1 + g). If FCFF grows at a constant rate, firm value(FCFF1) is FCFF0 times (1+g) divided by (WACC-g).
#'@details
#'According to information provided by Jerald E. Pinto (2020), the method \code{equityValueConstantG} is developed to compute estimated value of the firm if FCFF is growing at a constant rate for the values passed to its three arguments. Here, \code{FCFF0} is given amount of future Free Cash Flow to the Firm in millions of dollars, \code{g} is constant rate of growth under single stage constant growth model, and  \code{WACC} is Weighted Average Cost of Capital.
#'@param FCFF0 A number.
#'@param g A number.
#'@param debtVal A number.
#'@param WACC A number.
#'@return Input values to tfour arguments  \code{FCFF0} \code{g}, and \code{WACC} .
#'@author MaheshP Kumar, \email{maheshparamjitkumar@@gmail.com}
#'@references
#'Pinto, J. E. (2020). Equity Asset Valuation (4th ed.). Wiley Professional Development (P&T). https://bookshelf.vitalsource.com/books/9781119628194
#'@examples
#'equityValueConstantG(FCFF0=1.8,g=0.08,WACC=0.12,debtVal= 18 )
#'equityValueConstantG(FCFF0=700,g=0.05,WACC=0.102,debtVal=2200)
#'@export
equityValueConstantG<-function (FCFF0,g,WACC,debtVal)
{
  firmVal <- FCFF0*(1+g)/(WACC-g)
  equityVal <- firmVal - debtVal
  ( equityVal = round( equityVal, digits=2))
}


