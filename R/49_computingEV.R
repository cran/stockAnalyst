#'Calculates absolute amount of Enterprise Value.
#'@description
#'Analysts commonly define that enterprise value is equal to Market value of common equity (Number of shares outstanding multiplied with Price per share) plus, the Market value of preferred stock (if any) plus, the Market value of debt less, the cash and investments (specifically: cash, cash equivalents, and short- term investments. Cash and investments (sometimes termed non-earning assets) are subtracted because EV is designed to measure the net price an acquirer would pay for the company as a whole. The acquirer must buy out current equity and debt providers but then receives access to the cash and investments, which lower the net cost of the acquisition. (For example, cash and investments can be used to pay off debt or loans used to finance the purchase.) The same logic explains the use of market values: In repurchasing debt, an acquirer has to pay market prices. Some debt, however, may be private and it does not trade; some debt may be publicly traded but trade infrequently. When analysts do not have market values, they often use book values obtained from the balance sheet (Jerald E. Pinto, 2020).
#'@details
#'According to information provided by Jerald E. Pinto (2020), the method \code{computingEVdollarVal} is developed for computing absolute amount of Enterprise Value for the values passed to its four arguments. Here, \code{commonEquityMV} is  market value of Common Equity, \code{prefStockMV} is market value of Preference Stock,\code{debtMV} is market value of the Debt , and \code{cashNequi} is amount of Cash and cash equivalents.
#'@param  commonEquityMV number.
#'@param  prefStockMV number.
#'@param  debtMV number.
#'@param  cashNequi number.
#'@return Input values to four arguments  \code{commonEquityMV}, \code{prefStockMV}, \code{debtMV}, , \code{cashNequi}.
#'@author MaheshP Kumar, \email{maheshparamjitkumar@@gmail.com}
#'@references
#'Pinto, J. E. (2020). Equity Asset Valuation (4th ed.). Wiley Professional Development (P&T). https://bookshelf.vitalsource.com/books/9781119628194
#'@examples
#'computingEVdollarVal(commonEquityMV=15008,prefStockMV=0,debtMV=2013,cashNequi=4060)
#'@export
computingEVdollarVal<-function (commonEquityMV,prefStockMV,debtMV,cashNequi){
  Disp_Val<- (commonEquityMV + prefStockMV + debtMV - cashNequi)
  (Disp_Val = round(Disp_Val, digits=2))
}
