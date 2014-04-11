
# Check yourself:
#> nrmethod(20,.05) 
#df= 20 
#Percentile value= 10.85081 
# illinois_method(20,.05)
# df= 20 
# Percentile value= 10.84879 

#Why this difference? 








# Assignment 2.7.1
# Find 90th and 95th percentiles of
# chi-square distributions with df= 10, 15 and 20.

# Newton-Raphson method

nrmethod <- function(df, per, maxiter=100){
iter <- 1
zp=qnorm(per)
t= 2/(9*df)
x0=df*(1-t+zp*sqrt(t))^3
repeat{
ans <- pchisq(x0,df) - per
x0 <- x0-ans/(dchisq(x0,df))
res <- pchisq(x0,df) - 0.9
if(abs(res) <= 1.0e-7 || iter > maxiter) {break}
iter <- iter + 1
}
cat(" df=",df, "\n","Percentile value=",x0,"\n")
}

nrmethod(10, 0.9)
nrmethod(15, 0.9)
nrmethod(20, 0.9)
nrmethod(10, 0.95)
nrmethod(15, 0.95)
nrmethod(20, 0.95)



# Find 90th and 95th percentiles of
# chi-square distributions with df= 10, 15 and 20.

# Illionois method

illinois_method <- function(df, per){
l <- 1
zp=round(qnorm(per),3)
zp1=round(qnorm((1+per)/2),3)
t= 2/(9*df)
xl=df*(1-t+zp*sqrt(t))^3
xu=df*(1-t+zp1*sqrt(t))^3
f <- function(xu){
return(pchisq(xu,df)-per)}
fxl <- f(xl)
fxu <- f(xu)
if(fxl*fxu > 0){
return('The criterion is not met')}
c0 <- (xl*fxu-xu*fxl)/(fxu-fxl)
repeat{
c1 <- (xl*fxu-xu*fxl)/(fxu-fxl)
if(f(c1)*fxl < 0){
y <- c1
fxu <- f(xu)
if(f(c0)*f(c1) > 0){fxl <- fxl/2}
}
else{
x <- c1
fxl <- f(xl)
if(f(c0)*f(c1) > 0){fxu <- fxu/2}
}
c0 <- c1
if(l > 100 || abs(f(c1)) <= 1.0e-7){break}
l <- l + 1
}
cat(" df=",df, "\n","Percentile value=",c1,"\n")
}


illinois_method(10, 0.9)
illinois_method(15, 0.9)
illinois_method(20, 0.9)
illinois_method(10, 0.95)
illinois_method(15, 0.95)
illinois_method(20, 0.95)


 

