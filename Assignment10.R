# PG 139 
# Example 5.5.2 (E_step) OK

em_alg<-function(initial,num,iteration){
 n<-length(num)
 zhi<-initial[1]
 lamda<-initial[2]
 y0<-num[1]
 y<-num[2:n]
 xA<-0
 xB<-0
 for (i in 1:iteration){
   xA<-c(xA,y0*zhi[i]/(zhi[i]+(1-zhi[i])*exp(-lamda[i])))
   xB<-c(xB,y0-xA[i+1])
   zhi<-c(zhi,xA[i+1]/(y0+sum(y)))
   lamda<-c(lamda,sum((1:(n-1))*y)/(xB[i+1]+sum(y)))
 }
 cbind(xA,xB,zhi,lamda)
} 
 
y<-c(3062,587,284,103,33,4,2)
initial<- c(0.75, 0.4)

em_alg(initial,y,iteration=30)