\name{pif}
\alias{pif}
\title{Probability of incorrect fail}
\usage{
pif(a,mu, tau, rhot)
}
\arguments{
  \item{a}{cutoff}
  \item{mu}{mean}
  \item{tau}{true score SD}
  \item{rhot}{correlation}
}
\value{Probability of incorrect fail}
\description{
Used in Examples of the paper.
}
\examples{
pif(5.5, 5.8, 1, 0.5)
}
