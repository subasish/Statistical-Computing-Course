# Works good.

# Problem 4.4.1.
# Find 95% CIs using the exact method and the score method

#Exact CI:
exact.method.ci <- function(x, py, cl) 
     	{
       Z <- qnorm(0.5*(1+cl)) 
       t<- x+0.5
       Zinsert <- (Z/3)*sqrt(1/t)
       L<- 1-1/(9*t)-Zinsert
       U<- 1-1/(9*t)+Zinsert
       r <- x/py
       LL <- ((x+0.5)*L^3)/py 
       UL <- ((x+0.5)*U^3)/py 
       cbind(x, py, lower=LL, upper=UL) 
       }

exact.method.ci(63, 75, 0.95)



#Score CI:
poisson.score.ci <- function(x, py, cl)
{
	al <- 1-cl
      t<- al/2
	m<- x/py
	crt <- qnorm(1-t)
	zsq <- crt**2
	cent <- m+(zsq/(2*py))
	me=sqrt(cent^2-m^2)
	lower <- cent-me
	upper <- cent+me
	cat(" Lower=",lower,"\n","Upper=",upper,"\n")
}

poisson.score.ci(63, 75,0.95)

# Interpretation:
# The output of Exact methods shows tighter values than the Score methods.

