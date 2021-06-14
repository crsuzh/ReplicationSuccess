\name{pvalueBound}
\alias{pvalueBound}
\title{Computes bound for the p-values entering the harmonic mean
  chi-squared test}
\description{
  Necessary or sufficient bounds for significance of the harmonic mean chi-squared test
  are computed for n one-sided p-values.
  %%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
pvalueBound(alpha, n, type = "necessary")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{alpha}{
    The significance level. 
  }
  \item{n}{
    The number of p-values. 
  }
  \item{type}{either \code{"necessary"} or \code{"sufficient"}.  Defaults to \code{"necessary"}. If \code{"necessary"} then the
    necessary bounds are computed. If \code{"sufficient"} then the
    sufficient bounds are computed. 
  }
}
\value{
The bound for the p-values. 
}
\references{

Held, L. (2020). The harmonic mean chi-squared test to substantiate scientific findings. 
    \emph{Journal of the Royal Statistical Society, Series C (Applied Statistics)}, to appear.
    \url{https://arxiv.org/abs/1911.10633}
}
\author{
Leonhard Held
}
\seealso{
  \code{\link{hMeanChiSq}}
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
pvalueBound(0.025^2, n=2, "necessary")
pvalueBound(0.025^2, n=2, "sufficient")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

