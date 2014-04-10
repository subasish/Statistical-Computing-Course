# FINAL PROJECT
# Subasish Das
# CLID: sxd1684


# 4(a).
X_val <- function(p){
  unif <- runif(1)
  n <- length(face)
  for (i in 1:n) {
    if (unif < p[i])
      return(i)
  } 
}


my_sample <- function(iter,p){
  init <- NULL
  for(i in 1:iter){
    init <- c(init,X_val(p))
  }
  return(init)
}

face <- seq(1:6)
p <- c(1, 2, 3, 4, 5, 6)/6
data.frame(face, p) 
t <- my_sample(10000, p)


# Vector programming

my_sample1 = function(n) 
{
X=runif(n)
t1 <- (X<=1/6)
t2 <- (X<=2/6 & X>1/6)*2
t3 <- (X<=3/6 & X>2/6)*3
t4 <- (X<=4/6 & X>3/6)*4
t5 <- (X<=5/6 & X>4/6)*5
t6 <- (X<=1 & X>5/6)*6

X <- t1 + t2 + t3 + t4 + t5 + t6
return (X)
}

face_num <- my_sample1(1000000)


# 4(b).

# Sample mean
i <- c(1, 2, 3, 4, 5, 6)
p1 <- rep(1/6,6)
mean <- sum(i*p1)
mean

# From simulation
my_mean <- function (nr, z){
z <- my_sample(nr, p)
mymean <- mean(z)
return (mymean)
}
p <- c(1, 2, 3, 4, 5, 6)/6
my_mean(100000, z)


# Sample variance
i <- c(1, 2, 3, 4, 5, 6)
p1 <- rep(1/6,6)
var <- sum(i^2*p1) - (mean)^2
var

# From simulation
my_variance <- function (nr, z){
for(i in 1:nr){
var(my_sample(10000, p))
myvar <- var(my_sample(10000, p))
return (myvar)
}
}
p <- c(1, 2, 3, 4, 5, 6)/6
my_variance(100000, p)



# 4(c).
prob <- function(nr){
t <- my_sample(nr, p)
cond <- t > 3 
p <- 0
for(i in 1:nr){
if(cond[i]) p <- p + 1}
p1 <- p/nr
return(p1)
}

p <- c(1, 2, 3, 4, 5, 6)/6
prob(100000)


# 4(d).
p <- c(1, 2, 3, 4, 5, 6)/6
results <- my_sample(100000,p)
count <- table(results)
hist(results, prob=T, breaks=seq(-0.5, 6.5, by=1), ylim=c(0,1))

# Inspection of hisrogram  supports our common sense intuition that a bigger sample will 
# more likely  reflect the whole population. In particular, as the size of our sample goes 
# up, our estimated mean is more likely to be closer to the parent population mean. This idea 
# is known as the law of large numbers.


# 4(e).
p <- c(1, 2, 3, 4, 5, 6)/6
results <- my_sample(100000,p)
boxplot(results, main= "Boxplot")

# The distribution is nearly symmetric.

