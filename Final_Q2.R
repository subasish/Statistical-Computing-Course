# FINAL PROJECT
# Subasish Das
# CLID: sxd1684


# 2(a).
my_rlogistic<- function (x, loc, scale){
y = loc + scale * log(x / (1-x))
return(y)
}

par(mfrow=c(1,2))
x <- runif(1000000, 0, 1)
function_rlogistic<- my_rlogistic(x,0,1)
hist(function_rlogistic, col="blue")
inbuilt_function <- rlogis(1000000, 0, 1)
hist(inbuilt_function, col= "red")



# 2(b).

probab <- function(nr, loc, scale){
c<- rlogis(nr, loc, scale)
cond <- c > 3 
p <- 0
for(i in 1:nr){
if(cond[i]) p <- p + 1}
p1 <- (1-p/nr)
return(p1)
}

probab(100000, 1, 2)


# 2(c).
logistic_m_v<- function (x, loc, scale){
var <- (scale^2*pi^2)/3
mean <- loc
return(c(var,mean))
}

x <- runif(1000000, 0, 3)
logistic_m_v(x, 2, 4)

c<- rlogis(1000000, 2, 4)
var(c)
median(c)
