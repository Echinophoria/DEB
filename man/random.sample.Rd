\name{random.sample}
\alias{random.sample}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Radon sample of n elements
}
\description{
create a random sample of n elements for a normal distribution determined by the mean, sd, lower and upper bounds.
}
\usage{
random.sample(n, mean, sd, lowerBound, upperBound)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{
Number of elements to calculate
}
  \item{mean}{
Mean of the distribution
}
  \item{sd}{
Standard deviation of the distribution
}
  \item{lowerBound}{
minimum value of the distribution
}
  \item{upperBound}{
maximum value of the distribution
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
{n}{a vector containing n values with the mean and sd of the given distribution and bounded by the upper and lower bounds given}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Not me...
}
\note{
Distribution is assumed to be normal
Useful when need to generate data for simulations following the descriptors of a given population.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
#considering a length distribution of the observed population
#determined by a mean of 5, and sd 0.5, and with a minimum
#value of 3 and maximum of 6.5
L = random.sample(20,5,0.5,3,6.5)
}
