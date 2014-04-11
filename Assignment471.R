# Assignment 4.7.1

MLE.of.b.and.c<- function(x,y, maxiter=1000)
{
iter<- 0
yhat<- mean(y)
num<- (yhat-y)**2
num1<- sum(num)
n<- length(x)
cu<- (sqrt(6)/pi)*sqrt(num1/(n-1))
	repeat{
	s1<- (1/n)*sum(y)
	z<- x^cu
	s2<- sum(z)
	s3<- sum(z*y)
	s4<- sum(z*y^2)
	f<- 1/cu + s1 - s3/s2
	if (f<- 0 || iter>maxiter) {break}
		iter<- iter+1}
	cu<- cu + f/ (1 + cu**2 + (s2*s4 -s3**2) /s2**2)
	bu<- (1/n*s2)^(1/cu)

cat(" MLE of c=",cu,"\n","MLE of b=",bu,"\n")
}


x<- c(17.88,28.92, 33.00, 41.52, 42.12, 45.60, 48.40, 51.84,
	51.96, 54.12, 55.56, 67.80, 68.64, 68.64, 68.88, 84.12,
	93.12, 98.64, 105.12, 105.84, 127.92, 128.04, 173.40)
y<- log(x) # it should be within the function

MLE.of.b.and.c(x,y)
