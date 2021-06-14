\name{pReplicate}
\alias{pReplicate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Probability of replicating an effect of Killeen (2005)}
%%  ~~function to do ... ~~
\description{
  Computes the probability that a replication study yields an effect
  estimate in the same direction as in the original study. 
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
pReplicate(po, zo = p2z(p = po, alternative = alternative), c = 1,
           alternative = "two.sided")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{po}{
  A vector of p-values from the original study, default is \code{NULL}
}
  \item{zo}{
  A vector of z-values from the original study. 
  Is calculated from \code{po}, if necessary. 
}
  \item{c}{
    The ratio of the variances of the original and replication effect
    estimates. This is usually the ratio of the sample size of the
    replication study to the sample size of the original study. 
}
  \item{alternative}{Either \code{"one.sided"} or \code{"two.sided"}. 
  Defaults to \code{"two.sided"}. 
  Specifies if the p-value is one-sided or two-sided.}
}
\details{
  This extends the statistic p_rep
  ("the probability of replicating an effect") by Killeen (2005) to
  the case of possibly unequal sample sizes, see also Senn (2002). 

%%  ~~ If necessary, more details than the description above ~~
}
\value{
The probability that a replication study yields an effect
  estimate in the same direction as in the original study. 
}
\references{
Killeen, P. R. (2005). An alternative to null-hypothesis significance
tests. \emph{Psychological Science}, \bold{16}(5), 345--353. \url{https://doi.org/10.1111/j.0956-7976.2005.01538.x}

Senn, S. (2002). Letter to the Editor, \emph{Statistics in Medicine},
\bold{21}, 2437--2444.

Held, L. (2019). The assessment of intrinsic
credibility and a new argument for p < 0.005. \emph{Royal Society Open
Science}, 6:181534. \url{https://dx.doi.org/10.1098/rsos.181534}
}
\author{
Leonhard Held
}
\examples{
pReplicate(po = c(0.05, 0.01, 0.001))
pReplicate(po = c(0.05, 0.01, 0.001), c = 2)
pReplicate(po = c(0.05, 0.01, 0.001), c = 2, alternative = "one.sided")
pReplicate(zo = c(2, 3, 4))
}