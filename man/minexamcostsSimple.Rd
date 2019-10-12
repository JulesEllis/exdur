\name{minexamcostsSimple}
\alias{minexamcostsSimple}
\title{Minimized cost of exam}
\usage{
minexamcostsSimple(min, max, step)
}
\arguments{
  \item{min}{minimum of independent variable (V)}
  \item{max}{maximum of independent variable (V)}
  \item{step}{length of a single step}
}
\value{A matrix with in the first column the costs quotients S (numbers between min and max),
and in the second column the corresponding value of the reliability with minimized costs if
the simple model with D = 0 is being used.}
\description{
Used in the paper to produce Figure 7.
}
\examples{
plot(minexamcostsSimple(1,500,1))
}

