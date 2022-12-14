#'Calculate value of a share using three stage Dividend Discount Model (DDM).
#'@description
#'In general three-stage version of DDM model, the company is assumed to have three distinct stages of growth and the growth rate of the second stage is typically constant. For example, Stage 1 could assume 20 percent growth for three years, Stage 2 could have 10 percent growth for four years, and Stage 3 could have 5 percent growth thereafter.
#'@details
#'According to information provided Jerald E. Pinto (2020), the method \code{shareValUsingThreeStageDDMl} is developed to compute value of a share using three stage Dividend Discount Model for the values passed to its six arguments. Here, \code{divNot} is dollar value of the current dividend, \code{r} is required rate of return on equity, \code{n1} is number of years in Stage 1, \code{n2} is number of years in Stage 2, \code{g1} is expected growth rates for the first stage, \code{g2} is expected growth rates for the stage two, and \code{g3} is expected growth rates for the continuing third stage.
#'@param divNot A number.
#'@param r A number.
#'@param n1 A number.
#'@param n2 A number.
#'@param g1 A number.
#'@param g2 A number.
#'@param g3 A number.
#'@return Input values to seven arguments  \code{divNot}  , \code{r}, \code{n1}, \code{n2}, \code{g1}, \code{g2}and \code{g3}.
#'@author MaheshP Kumar, \email{maheshparamjitkumar@@gmail.com}
#'@references
#'Pinto, J. E. (2020). Equity Asset Valuation (4th ed.). Wiley Professional Development (P&T). https://bookshelf.vitalsource.com/books/9781119628194
#'@examples
#'shareValUsingThreeStageDDM(divNot=1.60,r=0.12,n1=2,n2=5,g1=0.14,g2=0.12,g3=0.102)
#'shareValUsingThreeStageDDM(divNot=3.30,r=0.09,n1=2,n2=5,g1=0.14,g2=0.12,g3=0.0675)
#'@export
shareValUsingThreeStageDDM <-function(divNot,r,n1,n2,g1,g2,g3){
  shareVal<- (divNot*((1+g1)^n1)*((1+g2)^n2)*(1+g3))/(r-g3)
  (shareVal = round(shareVal, digits=4))
}
