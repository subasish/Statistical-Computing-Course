# Assignment 6.5.2

GPQ_CV_CI<- function(x,iter, cl){

xbar <- mean(x)
s <- sd(x)
n <- length(x)

df = n-1.0
const1 = s*sqrt(df/n)
const2 = 0.5*s*s*df


z = rnorm(iter)
v = rchisq(iter,df)
gv= (xbar*sqrt(v/df)/s -z/sqrt(n))^(-2) # I corrected here. GV should be >= 0


ll<- quantile(sqrt(gv), cl) # I corrected here
ul<- quantile(sqrt(gv), (1-cl))



cat(" Lower=",ll,"\n","Upper=",ul,"\n")  
}

x<- c(3.19, 3.73, 5.64, 5.30, 6.95, 3.25, 6.80, 5.39, 7.31, 7.11,
4.44, 7.15, 5.64, 3.43, 6.00, 6.04, 5.36, 4.64, 6.80, 4.30)

GPQ_CV_CI(x, 100000, 0.025)