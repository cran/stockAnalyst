% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/19_firmValueConstantG.R
\name{firmValueConstantG}
\alias{firmValueConstantG}
\title{Calculates the estimated value of the firm when FCFF is growing at a constant rate.}
\usage{
firmValueConstantG(FCFF0, g, WACC)
}
\arguments{
\item{FCFF0}{A number.}

\item{g}{A number.}

\item{WACC}{A number.}
}
\value{
Input values to three arguments  \code{FCFF0} \code{g}, and \code{WACC} .
}
\description{
Assume that free cash flow to the firm (FCFF) grows at a constant rate, \code{g}, in such a way that FCFF in any period is equal to FCFF of the previous period multiplied by (1 + g). This means this method is based on single stage constant growth model. So, FCFFt is equal to FCFF of period (t–1) multiplied with (1 + g). If FCFF grows at a constant rate, firm value (FCFF1) is equal to FCFF0*(1+g)/(WACC-g).
}
\details{
According to information provided by Jerald E. Pinto (2020), the method \code{firmValueConstantG} is developed to compute estimated value of the firm when FCFF is growing at a constant rate for the values passed to its three arguments. Here, \code{FCFF0} is given amount of future Free Cash Flow to the Firm in millions of dollars, \code{g} is constant rate of growth under single stage constant growth model, and \code{WACC} is Weighted Average Cost of Capital.
}
\examples{
firmValueConstantG(FCFF0=1.8,g=0.08,WACC=0.12)
firmValueConstantG(FCFF0=700,g=0.05,WACC=0.102)
}
\references{
Pinto, J. E. (2020). Equity Asset Valuation (4th ed.). Wiley Professional Development (P&T). https://bookshelf.vitalsource.com/books/9781119628194
}
\author{
MaheshP Kumar, \email{maheshparamjitkumar@gmail.com}
}
