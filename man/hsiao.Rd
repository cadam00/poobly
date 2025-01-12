\name{hsiao}
\alias{hsiao}
\title{Hsiao test}
\usage{
hsiao(formula, data, index = NULL, ...)
}
\description{
Hsiao poolability test, as described by Hsiao (1986;2022).
}
\arguments{
\item{formula}{
\code{formula} object for \code{plm::plm} function.
}
\item{data}{
\code{plm::pdata.frame} or \code{data.frame} object for input. Note that for the
\code{data.frame} an additional \code{index} should be given.
}
\item{index}{
An \code{index} input for \code{plm::pdata.frame} in case that \code{data.frame}
is given instead of \code{plm::pdata.frame}.
}
\item{...}{
Rest arguments passed to \code{plm::plm}. Note that \code{'model'} and
\code{'effect'} can not be used in the current \code{hsiao} function and in
such a case an error will be triggered.
}
}
\details{
Hsiao (1986;2022) poolability/homogeneity test consists of three consecutive
tests. One for testing if the slope and constant coefficients are same across
the panel. If it is not the case, then a second test is conducted, with the
heterogeneity of both slope and constant coefficients as alternative hypothesis.
If this second hypothesis is not rejected, then the final third hypothesis is
tested, where the null of the same slope and constant against the alternative of
same slopes but different constants is tested.
}

\value{
\code{hsiao} and \code{list} object with hypotheses with their corresponding
F-statistics and p-values.
}

\references{
Hsiao, C. (1986) \emph{Analysis of Panel Data}. 1st edn. Cambridge: Cambridge
University Press (Econometric Society Monographs).

Hsiao, C. (2022) \emph{Analysis of Panel Data}. 4th edn. Cambridge: Cambridge
University Press (Econometric Society Monographs), pp. 43-49.

}

\examples{
library(plm)
data("Gasoline", package = "plm")

x <- hsiao(lgaspcar ~ lincomep + lrpmg + lcarpcap, Gasoline)
print(x)
##
##                   Hsiao Homogeneity Test
##
## Hypothesis| Null |                Alternative
## ----------+------+-------------------------------------------
##     H1    |Pooled|                    H2
##     H2    |  H3  |      Heterogeneous intercept & slope
##     H3    |Pooled|Heterogeneous intercept & Homogeneous slope
## =============================================================
##
## formula: lgaspcar ~ lincomep + lrpmg + lcarpcap
##
##     Hypothesis  F-statistic   p-value
##   1     H1       129.3166     < 0.001
##   2     H2        27.3352     < 0.001
##   3     H3        83.9608     < 0.001
}
