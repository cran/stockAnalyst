% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/55_computingRwithHmodel.R
\name{computingRwithHmodel}
\alias{computingRwithHmodel}
\title{Calculates the required rate of return on equity using two stage H-Model.}
\usage{
computingRwithHmodel(divNot, spNot, n, H, gS, gL)
}
\arguments{
\item{divNot}{A number.}

\item{spNot}{A number.}

\item{n}{A number.}

\item{H}{A number.}

\item{gS}{A number.}

\item{gL}{A number.}
}
\value{
Input values to six arguments  \code{divNot} , \code{spNot} , \code{n}, \code{H}, \code{gS} and \code{gL}.
}
\description{
Calculates the required rate of return on equity using two stage H-Model.
}
\details{
According to information provided Jerald E. Pinto (2020), the method \code{computingRwithHmodel} is developed to compute the required rate of return on equity using two stage H-Model for the values passed to its six arguments.Here, \code{divNot} is dollar value of the current dividend , \code{spNot} is current share price, \code{n} is number of years of super-normal growth period, \code{H} is which is one-half of n (that is the length of the super-normal growth period), \code{gS} is initial short-term dividend growth rate, and \code{gL} is normal long-term dividend growth rate after Year 2H (that is \code{n}).
}
\examples{
computingRwithHmodel(divNot=1,spNot=20,n=10,H=10/2,gS=0.10,gL=0.06)
}
\references{
Pinto, J. E. (2020). Equity Asset Valuation (4th ed.). Wiley Professional Development (P&T). https://bookshelf.vitalsource.com/books/9781119628194
}
\author{
MaheshP Kumar, \email{maheshparamjitkumar@gmail.com}
}
