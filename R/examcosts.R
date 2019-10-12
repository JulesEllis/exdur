pif<-function(a,mu,tau,rhot){
  z1=(a-mu)/tau
  z2=z1*sqrt(rhot)
  pnorm(z2)-pbivnorm(z2,z1,sqrt(rhot))}




examcosts<-function(t,S,a,mu,sigma1,rho1){
  tau = sigma1 * sqrt(rho1)
  rhot = t*rho1 / (1 + (t-1) * rho1)
  S*pif(a,mu,tau,rhot) + t}


minexamcosts<-function(S,a,mu,sigma1,rho1){
  a <- optim(1,examcosts,NULL,S,a,mu,sigma1,rho1,method="L-BFGS-B",lower=0)
  t=a$par
  rhot = t*rho1 / (1 + (t-1) * rho1)
  c(t, rhot, a$value)}

# Used to create table 1 of paper:
table1 <- function(){
  N=100
  a=5.5
  mu=5.8
  sigma1 = 1.93
  rho1 = 0.8
  tau = sigma1 * sqrt(rho1)
  h = 3
  L = 112
  Q = 1/3
  S = L*Q/h
  t = c(5/3, 4/3, 1, 2/3, 1/3)
  D = t*h
  rhot = t*rho1 / (1 + (t-1) * rho1)
  F=pif(a,mu,tau,rhot)
  C = examcosts(t,S,a,mu,sigma1,rho1)
  C = N * h * C
  a<-c(D, t, rhot, N*F, N*L*Q*F, N*h*t,C)
  dim(a)<-c(5,7)
  a}

#Not used in paper
table2 <-function(){
  rho1 = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
  S = 100*(1-rho1)/rho1
  a<-c()
  for (i in 1:9) {
    a<-rbind(a, minexamcosts(S[i],0,0,1,rho1[i]))}
  cbind(rho1,S, a)}


#table3 <-function(){
#  n=10
#  a<-c()
#  S=2.5
#  for (i in 1:n) {
#    S = S*2
#    rho1=0.5
#    a<-c(a, S, minexamcosts(S,0,0,1,rho1)[2])}
#  dim(a)<-c(2,n)
#  t(a)}

#Not used in paper
table3 <-function(){
  #Not used in paper
  n=10
  a<-c()
  for (i in 1:n) {
    S = 10*i
    for (j in 1:9){
    r=j/10
#    rho1=r/(1+r)
    rho1=r
    a<-c(a, minexamcosts(S,0,0,1,rho1)[2])}}
  dim(a)<-c(9,n)
  t(a)}

#Used to obtain outcomes for Figures 2-5 of paper:
tabminexamcosts<-function(){
  Slist = seq(10,100,10)
  Dlist = seq(-1,1,0.2)
  Rlist = seq(0.1,0.9,0.1)
  Olist=c()
  for (S in Slist){
    for (D in Dlist){
      for (rho1 in Rlist){
        a <- minexamcosts(S,D,0,1,rho1)
        b <- minexamcosts(S,0,0,1,rho1)
        Olist = c(Olist,S,D,rho1, a,b)
      }
    }
  }
  n = length(Slist) * length(Dlist) * length(Rlist)
  dim(Olist)<-c(9, n)
  Olist<-as.data.frame(t(Olist))
  colnames(Olist)<-c("S","D", "rho1","topt", "ropt","copt","stopt", "sropt","scopt")
  Olist
}

#Used to make Figure 7 of paper:
minexamcostsSimple<-function(min,max,step){
  costfactor <- seq(min,max,step)
  S = 10
  rlist=c()
  for (c in costfactor){
    cS = c/S
    rho1 = cS/(1+cS)
    a <- minexamcosts(S,0,0,1,rho1)
    rhot = a[2]
    rlist = c(rlist,c, rhot)}
  n = 1+(max-min)/step
  dim(rlist)<-c(2, n)
  t(rlist)}

# Example:
# plot(minexamcostsSimple(1,500,1))
