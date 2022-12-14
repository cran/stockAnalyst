#'Calculates Leading PE Multiple based on average of expected EPS for the next four quarters.
#'@description
#'The Leading PE, also know as forward PE is a major and logical alternative to the trailing PE because valuation is naturally forward looking. In the definition of forward PE, analysts have interpreted, “expected earnings of next year” as expected EPS for the next four quarters or the next 12 months or the next fiscal year (Jerald E. Pinto, 2020). In this method, first definition of Leading PE (i.e., the next four quarters) is used.
#'@details
#'In the given example, forecasts of EPS are $0.15 for the quarter ending 31 March 2019, $0.18 for the quarter ending 30 June 2019, $0.18 for the quarter ending 30 September 2019, and $0.24 for the quarter ending 31 December 2019. The sum of the forecasts for the next four quarters is $0.15 + $0.18 + $0.18 + $0.24 = $0.75, and the leading PE for this stock is $15/$0.75 = 20.0.
#'@param  currentShPr number.
#'@param  Q1EPS number.
#'@param  Q2EPS number.
#'@param  Q3EPS number.
#'@param  Q4EPS number.
#'@return Input values to five arguments  \code{currentShPr}, \code{Q1EPS}, \code{Q2EPS},\code{Q3EPS}, and \code{Q4EPS} .
#'@author MaheshP Kumar, \email{maheshparamjitkumar@@gmail.com}
#'@references
#'Pinto, J. E. (2020). Equity Asset Valuation (4th ed.). Wiley Professional Development (P&T). https://bookshelf.vitalsource.com/books/9781119628194
#'@examples
#'leadingPEnext4Qs(currentShPr=15,Q1EPS=0.15,Q2EPS=0.18,Q3EPS=0.18,Q4EPS=0.24)
#'@export
leadingPEnext4Qs<-function (currentShPr,Q1EPS, Q2EPS, Q3EPS,Q4EPS){
  disp_val<- currentShPr/(Q1EPS + Q2EPS + Q3EPS + Q4EPS)
  (disp_val = round(disp_val, digits=1))
}
