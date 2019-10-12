\name{examcosts}
\alias{examcosts}
\title{Cost of exam}
\usage{
examcosts(t,S,a,mu,sigma1,rho1)
}
\arguments{
  \item{t}{duration}
  \item{S}{costs quotient}
  \item{a}{cutoff}
  \item{mu}{mean}
  \item{sigma1}{original SD}
  \item{rho1}{original reliability}
}
\value{Costs of exam}
\description{
Can be used to computed the normed costs as defined in the paper, S*F(t) + t,
where S is the costs quotient, and F(t) is the probability if incorrect failing in an exam with duration t, mean mu,
standard deviaton sigma1 at t=1, reliability rho1 at t=1, and cutoff a.
}
\examples{
examcosts(1, 20, 5.5,5.8,2, 0.5)
}
