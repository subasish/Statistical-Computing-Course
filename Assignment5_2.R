# Problem 4.4.2.
# Compute the p-value


poisson.p <- function(k1, k2, n1, n2, cvalue=2)
{
	p1 <- (n1*cvalue/n2)/(1+n1*cvalue/n2)
	pvalue <- 0
	for(i in k1:(k1+k2))
		{
  		pvalue <- pvalue + dbinom(i,k1+k2,p1)
  		}
cat(" p-value=",pvalue, "\n")
}

poisson.p(40, 22, 20, 30)

# Interpretation:
# There’s not enough evidence to indicate that lamda1 is larger than 2*lamda2 .



