# FINAL PROJECT
# Subasish Das
# CLID: sxd1684

# 5
# Illinois method
solve_d <- function(m, c, q, brac1, brac2) {
repeat {
d <- 0.5*(brac1 + brac2)
fn <- qt(q,m,d)
fna <- qt(q, m, brac1)
fnb <- qt(q, m, brac2)
fn1 <- fn - c
fna1 <- fna - c
fnb1 <- fnb - c
if(fna1*fn1>0) {
brac1 <- d
}
if(fnb1*fn1>0) {
brac2 <- d
}
if(abs(fn1) < 1e-6) {
return(d)
break
      }
d <- 0.5*(brac1 + brac2)
}
}

solve_d(20, 3.72896, 0.90, 2, 2.5)
solve_d(42, 4.01663, 0.95, 2, 2.5)




# Simple Interpolation
ncp <- function(m, c, q, x, y){
s <- qt(q, m, x)
r <- qt(q, m, y)
d <- (c-s)*(y-x)/(r-s)+ x
print(d)
}
ncp(20, 3.72896, 0.9, 2, 2.5)
ncp(42, 4.01663, 0.95, 2, 2.5)