library(MASS)
attach(Boston)
lm.fit = lm(medv ~ crim + dis + indus + lstat + tax + rm, data = Boston)
summary(lm.fit)
df = data.frame(Boston$crim, Boston$dis, Boston$indus, Boston$lstat, Boston$tax, Bost
                on$rm)
X= cbind(rep(1, 506), as.matrix(df))
Y= as.vector(Boston$medv)
XTX=t(X)%*%X
XTX_inv = solve(XTX)
beta_hat = XTX_inv%*%t(X)%*%Y
beta_hat
y_hat = X%*%beta_hat
y_bar = mean(Y)
RSS = sum((Y-y_hat)^2)
RSS
## Calculating beta hat
backsub = function (x, y){
  l=dim(x)
  n=l[1] ## assuming x is traingular
  p=l[2]
  for (j in seq(p,1,-1))
  {
    y[j,1]=y[j,1]/x[j,j]
    if((j-1)>0)
      y[1:(j-1),1]=y[1:(j-1),1]-(y[j,1]*x[1:(j-1),j])
  }
  return(y)
}
qrq=qr.Q(qr(X)) ## Q--not traingular
qrr=qr.R(qr(X)) ## R--traingular
yadj=ginv(qrq) %*% Y ##inverse of qr_q by MASS function
betahatqr = backsub(qrr, yadj) ##apply backward sub to R upper traingular
betahatqr
## Calculating RSS:
y_hat = X%*%betahatqr
y_bar = mean(Y)
RSS = sum((Y-y_hat)^2)
RSS
a=.001
n=100
beta = as.matrix(rep(2, 7))
norm = sqrt(sum((t(X) %*% X %*% beta)**2))
for (i in 1:n){
  beta = beta - 2 * a *(t(X) %*% X %*% beta - t(X)%*%Y)
  norm = t(X) %*% X %*% beta
  i=i+1
}
beta