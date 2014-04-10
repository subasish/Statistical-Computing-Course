#OK

NR_widow <- function(xi,lm, maxiter=100){
iter <- 0 
repeat{
n0=3062
N=4075 
nmn0=4075-3062 
sum = 1628 
ll<- n0*log (xi + (1-xi)*exp(-lm)) + nmn0*(log (1-xi)-lm) + log (lm)*sum;
F1<-(n0*(1-exp(-lm))) / (xi + (1-xi)*exp(-lm)) - nmn0 / (1-xi);
F2<- -(n0*(1-xi)*exp(-lm)) / ((1-xi)*exp(-lm) + xi) + sum / lm - nmn0;
d=matrix(nrow=2,ncol=2)
d[1,1]=  -(n0*(1-exp(-lm))**2) / ((1-xi)*exp(-lm) + xi)**2 - nmn0 / (1-xi)**2;
d[1,2]= (n0*exp(-lm)) / ((1-xi)*exp(-lm) + xi) +
(n0*(1-xi)*(1-exp(-lm))*exp(-lm)) / ((1-xi)*exp(-lm) + xi)**2;
d[2,1]= (n0*exp(-lm)) / ((1-xi)*exp(-lm) + xi) +
(n0*(1-xi)*(1-exp(-lm))*exp(-lm)) / ((1-xi)*exp(-lm) + xi)**2;
d[2,2]= (n0*(1-xi)*exp(-lm)) / ((1-xi)*exp(-lm) + xi) -
(n0*(1-xi)**2*exp(-lm)**2) / ((1-xi)*exp(-lm) + xi)**2 - sum / lm**2;

dd <- det(d)

xi <- xi-1/dd*(d[2,2]*F1-d[1,2]*F2)
lm <- lm-1/dd*(-d[2,1]*F1+d[1,1]*F2)
if (abs(ll)<= 1e-7 ||iter >maxiter) {break}
iter <- iter+1
}
return(c(xi,lm))
}

NR_widow(0.75,0.4)





