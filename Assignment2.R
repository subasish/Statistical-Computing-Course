# Assignment 2.8.1.
# Solve equations using the N-R method.

solve_NR <- function(x1,x2, tol=1e-7, maxiter=100){
iter <- 0 
repeat{
F1 <- 3*x1^2+2*x1*x2+5*x2^2-69
F2 <- 4*x1^2-3*x1*x2+x2^2-7
d=matrix(nrow=2,ncol=2) 
d[1,1]= 6*x1+2*x2
d[1,2]= 2*x1+10*x2
d[2,1]= 8*x1-3*x2
d[2,2]= -3*x1+2*x2
dd <- det(d)
x1 <- x1-1/dd*((2*x2-3*x1)*F1-2*(x1+5*x2)*F2)
x2 <- x2-1/dd*((-8*x1+3*x2)*F1+2*(x2+3*x1)*F2)
if (abs(F1^2+F2^2) <= tol ||iter >maxiter) {break}
iter <- iter+1
}
return(c(x1,x2))
}

solve_NR(1,1)
solve_NR(2,1)


