# Problem 4.4.3.

# 1
# 95% CI for the ratio lamda1/lamda2 using 
# (a) the exact CI for p, and (b) using the score CI for p.

# (a)
# Exact CI for p

exact.os.ci.bin <- function(k, n, ind, cl)
{
	al <- (1.0-cl)
	phat <- k/n
	if(ind == "upp")
		{
		x <- phat; y <- .999999
		f <- function(p,k,n,al)
			{
			return(pbinom(k,n,p)-al)
			}
		}
	else
	{
	x <- phat
	y <- .000001
	f <- function(p,k,n,al)
		{
		return(1.-pbinom(k,n,p)+dbinom(k,n,p)-al)
		}
	}
	fx <- f(x,k,n,al)
	fy <- f(y,k,n,al)
	if(fx*fy > 0)
	{
	return('the signs of the function are the same')}
	c0 <- (x*fy-y*fx)/(fy-fx)
	l <- 1
	repeat
		{
		c1 <- (x*fy-y*fx)/(fy-fx)
		if(f(c1,k,n,al)*fx < 0)
			{
			y <- c1
			fy <- f(y,k,n,al)
			if(f(c0,k,n,al)*f(c1,k,n,al) > 0){fx <- fx/2}
			}
		else
		{
		x <- c1
		fx <- f(x,k,n,al)
		if(f(c0,k,n,al)*f(c1,k,n,al) > 0){fy <- fy/2}
		}
		c0 <- c1
		if(l > 100 || abs(f(c1,k,n,al)) <= 1.0e-7){break}
		l <- l + 1
		}
return(c1)
}


exact.ts.ci.bin <- function(k, n, cl)
{
	if(k == 0)
	{
	low <- 0
	upp <- exact.os.ci.bin(k, n, "upp", cl)
	}
	else if(k == n)
	{
	upp <- 1
	low <- exact.os.ci.bin(k, n, "low", cl)
	}
	else
	{
	low <- exact.os.ci.bin(k, n, "low", (1.0+cl)/2)
	upp <- exact.os.ci.bin(k, n, "upp", (1.0+cl)/2)
	}
return(c(low,upp))
}



exact.ratio<- function(n1,n2,k1,k2,cl)
{
	m=k1+k2
      exact<- exact.ts.ci.bin(k1,m,cl)
	pl <- exact[1]
	pu <- exact[2]
	rlow <- (n2*pl)/(n1*(1-pl))
	rupp <- (n2*pu)/(n1*(1-pu))
	cat(" Lower=",rlow,"\n","Upper=",rupp,"\n")
}

exact.ratio(20,30,40,22,0.95)



# (b)

# Score CI for p


score.ci.two.means <- function(k1, k2, n1, n2, cl)
{
	al <- 1-cl
      t<- al/2
	crt <- qnorm(1-t)
	ph1<-k1/n1
	ph2<- k2/n2
	p<- (n1*ph1)/(n1*ph1+n2*ph2)
	m<- k1+k2
	pl<- p-crt*sqrt(p*(1-p)/m)
	pu<- p+crt*sqrt(p*(1-p)/m)
	lower<-(n2*pl)/(n1*(1-pl))
	upper<- (n2*pu)/(n1*(1-pu))
	cat(" Lower=",lower,"\n","Upper=",upper,"\n")
}


score.ci.two.means (40, 22, 20,30, 0.95)

# Interpretation:
# The output of Exact methods shows tighter values than the Score methods.


# 2

# lamda1-lamda2:

poisson.score.ci <- function(x, n,cl)
{
	al <- 1-cl
      t<- al/2
	m<- x/n
	crt <- qnorm(1-t)
	zsq <- crt**2
	cent <- m+(zsq/(2*n))
	me=sqrt(cent^2-m^2)
	lower <- cent-me
	upper <- cent+me
	return(c(lower,upper))
}


poisson.mover.dif.ci <- function(k1, k2, n1, n2, cl)
{
	fci <- poisson.score.ci(k1, n1, cl) 
	sci <- poisson.score.ci(k2, n2, cl) 
	p1h <- k1*(1/n1)
	p2h <- k2*(1/n2)
	dif <- p1h-p2h
	low <- dif-sqrt((p1h-fci[1])**2+(p2h-sci[2])**2)
	upp <- dif+sqrt((p1h-fci[2])**2+(p2h-sci[1])**2)
	cat(" Lower=",low,"\n","Upper=",upp,"\n")
}


poisson.mover.dif.ci(40, 22, 20, 30,0.95)








