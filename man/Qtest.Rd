\name{Qtest}
\alias{Qtest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Q-test to assess compatibility between original and replication effect estimate}
%%  ~~function to do ... ~~
\description{
  Computes p-value from meta-analytic Q-test to assess compatibility between
  original and replication effect estimate
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Qtest(thetao, thetar, seo, ser)
}
\arguments{
  \item{thetao}{
    A vector of effect estimates from original studies. 
  }
  \item{thetar}{
    A vector of effect estimates from replication studies. 
  }
  \item{seo}{
    A vector of standard errors of the original effect estimates.
  }
  \item{ser}{
    A vector of standard errors of the replication effect estimates.
  }
}
\details{
  This function computes the p-value from a meta-analytic Q-test assessing 
  compatibility between original and replication effect estimate. Rejecting
  compatibility when the p-value is smaller than alpha is equivalent with 
  rejecting compatibility based on a (1 - alpha) prediction interval.
  See Hedges and Schauer (2019) for more details.
    %%  ~~ If necessary, more details than the description above ~~
  }
\value{
  p-value from Q-test
}
\references{

  Hedges, L. V., Schauer, J. M. (2019). More Than One Replication Study Is 
  Needed for Unambiguous Tests of Replication. Journal of Educational and 
  Behavioral Statistics, 44(5), 543-570. 
  \url{https://doi.org/10.3102/1076998619852953}

}
\seealso{
  \code{\link{predictionInterval}}
}
\author{
Samuel Pawel
}
\examples{
Qtest(thetao = 2, thetar = 0.5, seo = 1, ser = 0.5)
}