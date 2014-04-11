# 4.5.1.
# Compute 95% CI for µ1 -µ2 based on the Welch approximate degrees of
# degrees of freedom method.

welch.approx<- function(x1,x2,cl)
{
	n1=length(x1)
	n2=length(x2)
	x1hat=mean(x1)
	x2hat=mean(x2)
	s1=sd(x1)
	s2=sd(x2)

 
	num=(((s1**2)/n1)+((s2**2)/n2))^2
	denom=(s1^4)/((n1^2)*(n1-1)) + (s2^4)/((n2^2)*(n2-1))
	F=num/denom
	mul=sqrt((s1^2)/n1 + (s2^2)/n2)

	al=1-cl 
	q=qt(1-al/2, F)
 
	lower=(x1hat-x2hat)-q*mul
	upper=(x1hat-x2hat)+q*mul

	cat(" Lower=",lower,"\n","Upper=",upper,"\n")
}

x1=c(2.64,2.89,1.89,1.46,2.47,1.75,2.39,0.69,1.05,0.51)
x2=c(3.04,2.89,5.04,2.74,5.16,2.96,1.06,2.54,3.57,3.39,3.28,4.29)

welch.approx(x1,x2,0.95)
