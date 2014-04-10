# OK, it works
# Assignment 4.6.1

mles.gamma <- function(x){
n <- length(x)
y <- log(x)
xb <- mean(x)
yb <- mean(y)
s <- log(xb)-yb
a0 <- (3-s+sqrt((s-3)**2+24*s))/12/s
iter <- 1
repeat{
a1 <- a0-(log(a0)-digamma(a0)-s)/(1/a0-trigamma(a0))
if(abs(a1-a0) <= 1.0e-7 || iter >= 30){break}
a0 <- a1
iter <- iter + 1
}
b <- xb/a1
return(c(a1,b))
}



bias<- function (nr, n, a,b, maxiter=10000)
	{
	iter<- 0
	b1all<- 0
	b2all<- 0
		repeat{
			iter<- iter+1
 			if(iter>10000){break}
			x<- b*rgamma(n,a)
                  mles<- mles.gamma(x)
			ahat<- mles[1]
			bhat<- mles[2]
			b1<- ahat - a
			b2<- bhat - b
			b1all<- b1all + b1
			b2all<- b2all + b2
			}
		b1mean<- b1all/nr
		b2mean<- b2all/nr
            cbind(n, a, b, bias=b1mean, bias=b2mean) 
}

bias(10000, 200, 2, 3)
bias(10000, 20, 2, 3)