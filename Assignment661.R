# Assignment 6.6.1 OK
# Subasish Das

cov.prob.bf <- function(nr, n1, n2, sig1, sig2, cl){
al = (1+cl)/2
ab=(1-cl)/2
u1 = 2; u2 = 3
dif = u1-u2
df1 = n1-1; df2 = n2-1
zl1 = -qnorm(ab)
zl2 = -qnorm(ab)
se1 = sig1/sqrt(n1)
se2 = sig2/sqrt(n2)
xb1 = u1 + rnorm(nr)*se1
xb2 = u2 + rnorm(nr)*se2
s1sq = rchisq(nr,df1)*sig1**2/df1
s2sq = rchisq(nr,df2)*sig2**2/df2
s1 = sqrt(s1sq)
s2 = sqrt(s2sq)
el1 = xb1-zl1*s1/sqrt(n1)
ul1 = xb1+zl1*s1/sqrt(n1)
el2 = xb2-zl2*s2/sqrt(n2)
ul2 = xb2+zl2*s2/sqrt(n2)
elo = xb1-xb2-sqrt((xb1-el1)**2+(xb2-ul2)**2)
ulo = xb1-xb2+sqrt((xb1-ul1)**2+(xb2-el2)**2)
ind = (elo <= dif & dif <= ulo)
cov1 = sum(ind)/nr
f <- (s1sq/n1+s2sq/n2)**2/(s1sq**2/n1/n1/df1+s2sq**2/n2/n2/df2)
crt <- qt(al,f)
se <- crt*sqrt(s1sq/n1+s2sq/n2)
el <- xb1-xb2-se
ul <- xb1-xb2+se
ind = (el <= dif & dif <= ul)
cov2 = sum(ind)/nr
return(c(cov1,cov2))
}

cov.prob.bf(100000,10,5,1,.1,.95)
cov.prob.bf(100000,10,5,1,.1,.95)
cov.prob.bf(100000,10,5,1,.1,.95)
cov.prob.bf(100000,5,10,1,.1,.95)
cov.prob.bf(100000,10,10,1,.1,.95)
cov.prob.bf(100000,20,10,1,.1,.95)
