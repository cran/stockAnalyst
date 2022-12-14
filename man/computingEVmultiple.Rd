% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/50_computingEVmultiple.R
\name{computingEVmultiple}
\alias{computingEVmultiple}
\title{Calculates Enterprise Value Multiple as EV to EBITDA or EV to sales.}
\usage{
computingEVmultiple(basis = c("sales", "EBITDA"), EV, EBITDA, sales)
}
\arguments{
\item{basis}{character vector.}

\item{EV}{number.}

\item{EBITDA}{number.}

\item{sales}{number.}
}
\value{
Input values to four arguments  \code{basis}, \code{EV}, \code{EBITDA},and \code{sales}.
}
\description{
Enterprise value to EBITDA is by far the most widely used enterprise value multiple.Analysts use EV/EBITDA is usually more appropriate than PE alone for comparing companies with different financial leverage (debt), because EBITDA is a pre- interest earnings figure, in contrast to EPS, which is post-interest.Enterprise value to sales is a major alternative to the price- to- sales ratio. The PS multiple has the conceptual weakness that it fails to recognize that for a debt- financed company, not all sales belong to a company’s equity investors. Some of the proceeds from the company’s sales will be used to pay interest and principal to the providers of the company’s debt capital. For example, a PS for a company with little or no debt would not be comparable to a PS for a company that is largely financed with debt. \code{EV/S} would be the basis for a valid comparison in such a case. So, EV/S is an alternative sales- based ratio that is particularly useful when comparing companies with diverse capital structures (Jerald E. Pinto, 2020).
}
\details{
According to information provided by Jerald E. Pinto (2020), the method \code{computingEVmultiple} is developed for computing Enterprise Value Multiple as EV to EBITDA or EV to sales for the values passed to its four arguments. Here, \code{basis} is character string, either "sales" or "EBITDA" , \code{EV} is absolute amount of Enterprise Value (in millions of dollars),\code{EBITDA} is absolute amount of Earnings Before Interest,Taxes, Depreciation, and Amortization (in millions of dollars), and \code{sales} is absolute amount of sales (in millions of dollars).
}
\examples{
computingEVmultiple("sales",EV=14411,EBITDA=3320,sales=18962)
computingEVmultiple("EBITDA",EV=14411,EBITDA=3320,sales=18962)
}
\references{
Pinto, J. E. (2020). Equity Asset Valuation (4th ed.). Wiley Professional Development (P&T). https://bookshelf.vitalsource.com/books/9781119628194
}
\author{
MaheshP Kumar, \email{maheshparamjitkumar@gmail.com}
}
