% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/45_terminalValueUsingPE.R
\name{terminalValueUsingPE}
\alias{terminalValueUsingPE}
\title{Calculates  Terminal Value (TV) of the stock using PEs.}
\usage{
terminalValueUsingPE(
  avg = c("comparable", "GGM"),
  benchmarkPE,
  En,
  payout,
  g,
  r
)
}
\arguments{
\item{avg}{character vector.}

\item{benchmarkPE}{number.}

\item{En}{number.}

\item{payout}{number.}

\item{g}{number.}

\item{r}{number.}
}
\value{
Input values to six arguments  \code{avg}, \code{benchmarkPE},\code{En},\code{payout},\code{g}, and \code{r}.
}
\description{
Terminal Value at time \code{n} is calculated by taking benchmark value of trailing PE that is multiplied by EPS of the stock at time \code{n} where the final growth stage begins . This also means that Terminal Value of the stock is obtained by using comparable benchmark PE without considering growth or using multistage GGM and thereby incorporating the impact of growth (Jerald E. Pinto, 2020).
}
\details{
According to information obtained from Jerald E. Pinto (2020), the method \code{terminalValueUsingPE} is developed for computing Terminal Value (TV) of the stock using PEs  for the values passed to its six arguments. Here, \code{avg} is character string, either comparable or GGM , \code{benchmarkPE} is benchmark PE Multiple,\code{En} is EPS of the stock at time \code{n} where the final growth stage begins, \code{payout} is payout ratio, \code{g} is sustainable growth rate from GGM, and \code{r} is required rate of return on the equity.
}
\examples{
terminalValueUsingPE("comparable",benchmarkPE=14.3,En=3,payout=0.45,g=0.0715,r=0.10)
terminalValueUsingPE("GGM", benchmarkPE=14.3,En=3,payout=0.45,g=0.0715,r=0.10)
}
\author{
MaheshP Kumar, \email{maheshparamjitkumar@gmail.com}
}
