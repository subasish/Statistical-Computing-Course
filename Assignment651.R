# Assignment 6.5.1 OK

generalized_ci<- function(x, iter, cl){

y <- log(x)
xbar <- mean(y)
s <- sd(y)
n <- length(y)

df = n-1.0;
const1 = s*sqrt(df/n);
const2 = 0.5*s*s*df;


z = rnorm(iter)
v = rchisq(iter,df)
U= sqrt(v)
gv= xbar + z*const1/sqrt(U)+const2/U


ll_log <- quantile(gv, cl)
ul_log <- quantile(gv, (1-cl))
ll <- exp(ll_log)
ul <- exp(ul_log)


cat(" Lower(log)=",ll_log,"\n","Upper(log)=",ul_log,"\n", "Lower=",ll,"\n","Upper=",ul,"\n" )
}
  
x <- c(45, 30, 38, 42, 63,43, 102, 86, 99, 63, 58, 34, 37, 55, 
58, 153, 75, 58, 36, 59, 43, 102, 52, 30, 21, 40, 141, 85, 161, 
86, 161, 86, 71)

generalized_ci(x, 100000, 0.025)