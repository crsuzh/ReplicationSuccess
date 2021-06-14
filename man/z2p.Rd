\name{z2p}
\alias{z2p}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
    Transforms z-values to p-values
%%  ~~function to do ... ~~
}
\description{
  Transforms z-values to the corresponding one- or two-sided p-values.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
z2p(z, alternative = "two.sided")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{z}{
   a vector of z-values
%%     ~~Describe \code{bf} here~~
}
   \item{alternative}{
    Specifies direction of the alternative of p-value. 
    Either \code{"less"}, \code{"greater"}, \code{"one.sided"}, \code{"two.sided"}.  
    Defaults to \code{"two.sided"}.  
%%     ~~Describe \code{prior.odds} here~~
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
  A numeric vector of p-values
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
  \code{\link{p2z}}
}

\examples{
z2p(z = c(1, 2, 5))
z2p(z = c(1, 2, 5), alternative = "less")
z2p(z = c(1, 2, 5), alternative = "greater")

z <- seq(-3, 3, by = 0.01)
plot(z, z2p(z), type = "l", 
      xlab = "z", ylab = "p", 
      ylim = c(0, 1))
lines(z, z2p(z, alternative = "greater"), lty = 2)
legend("topright", c("two-sided", "greater"), lty = c(1, 2), bty = "n")
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
