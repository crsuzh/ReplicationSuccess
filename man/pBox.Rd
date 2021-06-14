\name{pBox}
\alias{pBox}
\title{Computes Box's tail probability}
\description{
  Box's tail probability is computed based on the z-values of the
  original and the replication study, the corresponding variance ratio
  and the significance level. 
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
pBox(zo, zr, c, level = 0.05, alternative = "two.sided")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{zo}{
    A vector of z-values from original studies. 
  }
  \item{zr}{
    A vector of z-values from replication studies. 
  }
  \item{c}{A vector of variance ratios of the original and replication
          effect estimates. This is usually the ratio of the sample
          size of the replication study to the sample size of the
          original study.}
  \item{level}{Significance level. Default is 0.05.}
  \item{alternative}{either \code{"one.sided"} or \code{"two.sided"}. Defaults to \code{"two.sided"}. Specifies if one-sided or two-sided Box's tail probability is computed.}
}
\value{
Box's tail probability. 
}
\references{
Box, G.E.P. (1980). Sampling and Bayes' inference in scientific
modelling and robustness (with discussion). \emph{Journal of the Royal Statistical
  Society, Series A}, \bold{143}, 383-430. 


Held, L. (2020). A new standard for the analysis and design of replication studies (with discussion). 
  \emph{Journal of the Royal Statistical Society: Series A (Statistics in Society)}. 
  \url{https://doi.org/10.1111/rssa.12493}

}
\author{
Leonhard Held
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
pBox(zo = p2z(0.01), zr = p2z(0.02), c = 2)
pBox(zo = p2z(0.02), zr = p2z(0.01), c = 1/2)
pBox(zo = p2z(0.02, alternative = "one.sided"), zr = p2z(0.01, alternative = "one.sided"), 
     c = 1/2, alternative = "one.sided")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
