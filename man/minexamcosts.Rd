\name{minexamcosts}
\alias{minexamcosts}
\title{Minimized cost of exam}
\usage{
minexamcosts(S,a,mu,sigma1,rho1)
}
\arguments{
  \item{S}{cost factor}
  \item{a}{cutoff}
  \item{mu}{mean}
  \item{sigma1}{original SD}
  \item{rho1}{original reliability}
}
\value{List (topt, ropt, copt), where topt = value of t that minimizes the costs,
ropt = value of the reliability if t = topt, copt = costs if t = topt}
\description{
Used in the paper.
}
\examples{
minexamcosts(20, 5.5,5.8,2, 0.5)
}

