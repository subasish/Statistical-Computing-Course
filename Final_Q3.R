# FINAL PROJECT
# Subasish Das
# CLID: sxd1684


# 3(a).
rela_risk <- function(x1, n1, x2, n2, n, cl){
initial <- rep(0, n)
rr <- (x1/n1)/(x2/n2)
for(i in 1:n){
x1boot_st <- rbinom(1, n1, x1/n1)
if(x1boot_st==0){
      x1boot_st <- 0.5
    }
    if(x1boot_st==n1){
      x1boot_st <- n1-0.5
    }
x2boot_st <- rbinom(1, n2, x2/n2)
if(x2boot_st==0){
      x2boot_st <- 0.5
    }
    if(x2boot_st==n2){
      x2boot_st <- n2-0.5
    }
initial[i] <- ((x1boot_st-0.5)/n1)/((x2boot_st-0.5)/n2)
		}
ord <- sort(initial)
al <- 1-cl
place <- (al/2)*n 
lower <- ord[place] 
upper <- ord[n - place]
cat(" Relative risk =", rr, "\n", "Lower =", lower, "\n", "Upper =", upper,"\n")
}

rela_risk(16, 32, 10, 40, 100000, 0.95)

# install.packages("gsDesign")
library(gsDesign)
ciBinomial(16, 10, 32, 40, alpha=0.05, adj= 1, scale= "rr")


# 3(b).
odds_ratio <- function(x1, n1, x2, n2, n, cl){
initial <- rep(0, n)
oddsratio <- ((x1/n1)/(1-x1/n1))/((x2/n2)/(1-x2/n2))
for(i in 1:n){
x1boot_st <- rbinom(1, n1, x1/n1)
if(x1boot_st==0){
      x1boot_st <- 0.5
    }
    if(x1boot_st==n1){
      x1boot_st <- n1-0.5
    }
x2boot_st <- rbinom(1, n2, x2/n2)
if(x2boot_st==0){
      x2boot_st <- 0.5
    }
    if(x2boot_st==n2){
      x2boot_st <- n2-0.5
    }
initial[i] <- (((x1boot_st-0.5)/n1)/(1-(x1boot_st-0.5)/n1))/(((x2boot_st-0.5)/n2)/(1-(x2boot_st-0.5)/n2));
		}
ord <- sort(initial)
al <- 1-cl
place <- (al/2)*n
lower <- ord[place] 
upper <- ord[n - place]
cat(" Odds ratio =", oddsratio, "\n", "Lower =", lower, "\n", "Upper =", upper,"\n")
}

odds_ratio(16, 32, 10, 40, 100000, 0.95)

# install.packages("gsDesign")
library(gsDesign)
ciBinomial(16, 10, 32, 40, alpha=0.05, adj= 1, scale= "or")

