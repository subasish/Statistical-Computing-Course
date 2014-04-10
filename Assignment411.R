# Both seems to work, but the second one is not that accurate
# try n =30, k=26, cl = .95, and compare the results with the statcalc
# Problem 4.1.1.
# Find the 95% score CIs and the corresponding exact CIs for n = 30,
# k = 5, 18 and 26.

# Score CI:
bin.score.ci <- function(n,x,cl)
{
	al <- 1-cl
	crt <- qnorm(1-al/2)
	zsq <- crt**2
	ph <- x/n
	dr <- 1+zsq/n
	cent <- (ph+(1/2)*zsq/n)/dr
	me <- crt*sqrt(ph*(1-ph)+(1/4)*zsq/n)/sqrt(n)/dr
	low <- cent-me
	upp <- cent+me
	cat(" Lower=",low,"\n","Upper=",upp,"\n")
}


bin.score.ci(30, 5, 0.95)
bin.score.ci(30, 18, 0.95)
bin.score.ci(30, 26, 0.95)




# Exact CI:
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
	x <- phat; y <- .000001
	f <- function(p,k,n,al)
		{
		return(1.-pbinom(k,n,p)+dbinom(k,n,p)-al)
		}
	}
	fx <- f(x,k,n,al)
	fy <- f(y,k,n,al)
	if(fx*fy > 0)
		{
		return('the signs of the function are the same')
		}
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
cat(" Lower=",low,"\n","Upper=",upp,"\n")
}


exact.ts.ci.bin(5,30,.95)
exact.ts.ci.bin(18,30,.95)
exact.ts.ci.bin(26,30,.95)
