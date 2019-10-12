\name{tabminexamcosts}
\alias{tabminexamcosts}
\title{Table with minimized exam costs}
\usage{
tabminexamcosts()
}
\value{A dataframe with these variables: S,D, rho1,topt, ropt,copt,stopt, sropt,scopt.
S = costs quotient, D = standardized cutoff, rho1 = reliability with t = 1, topt = value of t that minimizes the costs,
ropt = value of the reliability if t = topt, copt = costs if t = topt, stopt = value of t that minimizes the costs with D = 0,
sropt = value of the reliability if t = stopt, scopt = costs if t = topt}
\description{
Used to produce plots 2-5 in the paper.
}
\examples{
tabminexamcosts()
}

