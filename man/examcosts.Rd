\name{examcosts}
\alias{examcosts}
\title{Cost of exam}
\usage{
examcosts(t,S,a,mu,sigma1,rho1)
}
\arguments{
  \item{t}{duration}
  \item{S}{cost factor}
  \item{a}{cutoff}
  \item{mu}{mean}
  \item{sigma1}{original SD}
  \item{rho1}{original reliability}
}
\value{Costs of exam}
\description{
Used in the paper.
}
\examples{
examcosts(1, 20, 5.5,5.8,2, 0.5)
}
