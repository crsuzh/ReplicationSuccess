\name{p2z}
\alias{p2z}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
    Transforms p-values to z-values
%%  ~~function to do ... ~~
}
\description{
    Transforms one- or two-sided p-values to z-values.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
p2z(p, alternative = "two.sided")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p}{
   a vector of p-values
%%     ~~Describe \code{bf} here~~
}
 \item{alternative}{
  Specifies direction of the alternative of the p-value. 
  Either \code{"less"}, \code{"greater"}, \code{"one.sided"}, or \code{"two.sided"}. Defaults to \code{"two.sided"}. 
  If \code{"one.sided"} or \code{"two.sided"}, it is assumed that the z-value is positive.
%%     ~~Describe \code{prior.odds} here~~
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{A numeric vector of z-values
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
%\references{
%% ~put references to the literature/web site here ~
%}
%\author{
%%  ~~who you are~~
%}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{z2p}}
}

\examples{
p2z(p = c(0.005, 0.01, 0.05))
p2z(p = c(0.005, 0.01, 0.05), alternative = "greater")
p2z(p = c(0.005, 0.01, 0.05), alternative = "less")

p <- seq(0.001, 0.05, 0.0001)
plot(p, p2z(p), type = "l", ylim = c(0, 3.5), 
ylab = "z")
lines(p, p2z(p, alternative = "greater"), lty = 2)
legend("bottomleft", c("two-sided", "greater"), lty = c(1, 2), bty = "n")
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
