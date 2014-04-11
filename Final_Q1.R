# FINAL PROJECT
# 1(a).

fit_weibull <- function(x)
{
    xbar <- mean(x)
    varx <- var(x)
    f <- function(b){
    return(gamma(1+2/b)/gamma(1+1/b)^2 - 1 - varx/xbar^2)
			  }
    bhat <- uniroot(f,c(0.02,50))$root
    ahat <- xbar/gamma(1+1/bhat)
    return(c(ahat,bhat))
}

x<- c(5.1, 2.4, 0.4, 0.5, 2.5, 0.1, 6.8, 1.2, 0.5, 0.6, 
5.3, 2.3, 1.8, 1.2, 1.3, 1.1, 0.9, 3.2, 1.0, 0.9, 0.4, 
0.6, 8.0, 0.4, 2.7, 0.2, 2.0, 0.2, 0.5, 0.8, 2.0, 2.9, 0.1, 4.0)
fit_weibull(x)

y <- rweibull(34, 1.84, 0.96)
p = ppoints(sort(x), a=0.3)
p1 = ppoints(sort(y), a=0.3)
par(mfrow=c(1,2))
plot(sort(x), -log(1-p), log="xy", type="o", col="blue",
     xlab="vinyl chloride data", 
     main = "Weibull Q-Q Plot (Vinyl Chloride data)")
plot(sort(y), -log(1-p1), log="xy", type="o", col="red",
     xlab="Random Weibull number", 
     main = "Weibull Q-Q Plot (Random Weibull number)")

# The data fits a Weibull distribution.



# Real_data vs. R function
x_inbuilt <- rweibull (34, 0.9, 1.888)
qqplot (x, x_inbuilt, main="qqplot")
abline(0,1)



# 1(b).
weib.mles <- function(x){
n <- length(x)
y <- log(x)
s1 <- mean(y)
c0 <- 1/(sqrt(var(y))*sqrt(6)/pi)
l <- 1
repeat{
z <- x**c0
s2 <- sum(z)
s3 <- sum(z*y)
s4 <- sum(z*y*y)
fn <- 1/c0+s1-s3/s2
if(abs(fn) <= 1.0e-6 | l > 30){break}
c1 <- c0+fn/(1/c0**2+(s2*s4-s3**2)/s2**2)
c0 <- c1
l <- l+1
}
ch <- c0
bh <- (s2/n)**(1/ch)
return(c(ch,bh))
}



ci.weib.para <- function(nr,x,cl){
al <- (1-cl)/2; ll <- nr*al; lu <- nr-ll
gpqc <- seq(1:nr)
gpqb <- seq(1:nr)
n <- length(x)
ml <- weib.mles(x)
ch0 <- ml[1]; bh0 <- ml[2]
for(i in 1:nr){
x <- rweibull(n,1,1)
ml <- weib.mles(x)
chs <- ml[1]; bhs <- ml[2]
gpqc[i] <- ch0/chs
gpqb[i] <- bh0*(1/bhs)**(chs/ch0)
}
gpqc <- sort(gpqc)
gpqb <- sort(gpqb)
blow <- gpqb[ll]; bupp <- gpqb[lu]
clow <- gpqc[ll]; cupp <- gpqc[lu]
return(c(clow,cupp,blow,bupp))
}


CIvalues <- function(nr, x, cl){
ci.weib <- ci.weib.para(nr, x, 0.95)
clow<- ci.weib[1]
cupp<- ci.weib[2]
blow<- ci.weib[3]
bupp<- ci.weib[4]
lower<- blow*gamma(1+1/clow)
upper<- bupp*gamma(1+1/cupp)
cat(" Lower =", lower, "\n", "Upper =", upper,"\n")
}


x<- c(5.1, 2.4, .4, .5, 2.5, .1, 6.8, 1.2, .5, .6, 5.3, 2.3, 1.8, 1.2, 1.3, 1.1, .9, 3.2, 1.0, .9,.4, 
.6, 8.0, .4, 2.7, .2, 2.0, .2, .5, .8,2.0, 2.9, .1, 4.0)
CIvalues(100000, x, 0.95)


# 1 (c).

st.weib <- function(nr, x, t, cl){
n <- length(x)
sl <- seq(1:nr)
alpha <- 1-cl
ll <- nr*alpha; lu <- nr-ll
ml <- weib.mles(x)
c0h <- ml[1]; b0h <- ml[2]
s0t <- exp(-(t/b0h)**c0h)
for(i in 1:nr){
x <- rweibull(n,1,1)
ml <- weib.mles(x)
chs <- ml[1]; bhs <- ml[2]
sl[i] <- log(-log(s0t))/chs+log(bhs)
}
sl <- sort(sl)
low <- exp(-exp(sl[lu]))
cat("The lower CL for S(t) \n")
return(low)
}

x<- c(5.1, 2.4, 0.4, 0.5, 2.5, 0.1, 6.8, 1.2, 0.5, 0.6, 5.3, 2.3, 1.8, 1.2, 1.3, 1.1, 0.9, 3.2, 1.0, 0.9, 0.4, 
0.6, 8.0, 0.4, 2.7, 0.2, 2.0, 0.2, 0.5, 0.8, 2.0, 2.9, 0.1, 4.0)

t <- sort(x)
st.weib(100000, t, 2, 0.95)

# At least 24% of data of Vinyl Chloride concentration is above 2.
