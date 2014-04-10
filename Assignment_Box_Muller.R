# Assignment 6.2.1. 
# Write R function for generating normal random numbers
# using Box-Muller transformation.


normal_boxmuller <- function(nr, myu, sigma) {
	urandom1 <- runif(nr)
	urandom2 <- runif(nr)
	Rsq <- (-2 * log(urandom1))
	theta <- 2 * pi * urandom2
	x <- sqrt(Rsq) * cos(theta)
	y <- sqrt(Rsq)* sin(theta)
	z <- (x + y) / sqrt(2)
	y <- myu + (sigma * z)
	return (y)
}

normal_boxmuller (1000, 0, 1)


out <- normal_boxmuller(1000000, 0, 1)
out[1:10]
summary(out)
sd(out)
hist(out,br=50,xlab=" ",ylab=" ",main="1,000,,000 simulations from 
Normal using Box-Muller Transformation",freq=F)
curve(dnorm(x),from=-4,to=4,col="red",add=T)