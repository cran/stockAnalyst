% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/53_computingWACC.R
\name{computingWACC}
\alias{computingWACC}
\title{Calculates Weighted Average Cost of Capital(WACC).}
\usage{
computingWACC(dollarValDebt, dollarValCEquity, rDebt, rCEquity, taxRate)
}
\arguments{
\item{dollarValDebt}{A number.}

\item{dollarValCEquity}{A number.}

\item{rDebt}{A number.}

\item{rCEquity}{A number.}

\item{taxRate}{A number.}
}
\value{
Input values to five arguments  \code{dollarValDebt} , \code{dollarValCEquity}, \code{rDebt}, \code{rCEquity}, and \code{taxRate}.
}
\description{
The overall required rate of return of a suppliers of capital is usually referred to as cost of capital. The cost of capital is most commonly estimated using the after-tax weighted average cost of capital, or weighted average cost of capital (WACC) for short; a weighted average of required rates of return for the component sources of capital.It is interesting fact to note that in many jurisdictions, corporations may deduct net interest expense from income in calculating taxes owed, but they cannot deduct payments to shareholders, such as dividends. Because capital structure (the proportions of debt and equity financing) can change over time, WACC may also change over time. In addition, the company’s current capital structure may also differ substantially from what it will be in future years. For these reasons, analysts often use target weights instead of the current market-value weights when calculating WACC (Jerald E. Pinto, 2020)
}
\details{
Based on the information provided by Jerald E. Pinto (2020), the method \code{computingWACC} is developed for computing Weighted Average Cost of Capital(WACC) for the values passed to its five arguments. Here, \code{dollarValDebt} is dollar value of the debt, \code{dollarValCEquity} is dollar value of the common equity, \code{rDebt} before-tax required return on debt,\code{rCEquity} is required return on equity, and \code{taxRate} is corporate tax rate.
}
\examples{
computingWACC(dollarValDebt=35,dollarValCEquity=65,rDebt=0.056,rCEquity=0.127,taxRate=0.29)
}
\references{
Pinto, J. E. (2020). Equity Asset Valuation (4th ed.). Wiley Professional Development (P&T). https://bookshelf.vitalsource.com/books/9781119628194
}
\author{
MaheshP Kumar, \email{maheshparamjitkumar@gmail.com}
}
