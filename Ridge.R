

setwd("~/Desktop/Data")
# Data
BF <- read.table("Bodyfat.txt")
attach(BF)

# X_1 Triceps Skinfold Thickness 
# X_2 Thigh Circumference 
# X_3 Mid Arm Circumference
# Y Body fat

#EDA
plot(BF)

# Strange things 
lm(Y~X1+X2+X3)


# Standardize X variables
n <- length(Y)
Z1 <- (X1-mean(X1))/(sqrt(n-1)*sd(X1))
Z2 <- (X2-mean(X2))/(sqrt(n-1)*sd(X2))
Z3 <- (X3-mean(X3))/(sqrt(n-1)*sd(X3))
Y.star <- (Y-mean(Y))/(sqrt(n-1)*sd(Y)) 

# Standardized design matrix 
X.star <- cbind(Z1,Z2,Z3)

# Correlation matrix
t(X.star)%*%X.star

# Note
cor(X1,Y)
cor(X2,Y)
cor(X3,Y)
t(X.star)%*%Y.star


# Standardized Ridge Estimates 
c <- .02
beta.R.hat <- solve(t(X.star)%*%X.star+diag(rep(c,3)))%*%t(X.star)%*%Y.star

# reverse transform the model 
beta.1 <- beta.R.hat[1]*(sd(Y)/sd(X1))
beta.1
beta.2 <- beta.R.hat[2]*(sd(Y)/sd(X2))
beta.2
beta.3 <- beta.R.hat[3]*(sd(Y)/sd(X3))
beta.3
beta.0 <- mean(Y)-(beta.1*mean(X1)+beta.2*mean(X2)+beta.3*mean(X3))
beta.0



final <- matrix(0,ncol=3,nrow=13)
c.list <- c(0,.002,.004,.006,.008,.01,.02,.03,.04,.05,.1,.5,1)
for (i in 1:13) {
  
  c <- c.list[i]
  final[i,] <- solve(t(X.star)%*%X.star+diag(rep(c,3)))%*%t(X.star)%*%Y.star
  
  
}

  
# lm.ridge function (I am not sure how this funciton works)
library("MASS")  
lm(Y~X1+X2+X3)
lm.ridge(Y~X1+X2+X3,lambda=.02)

# Bootstrap

boot.fun <- function(dataset,c=0) {
  
  X1 <- dataset[,1]
  X2 <- dataset[,2]
  X3 <- dataset[,3]
  Y <- dataset[,4]
  
  #standardize
  n=length(Y)
  Z1 <- (X1-mean(X1))/(sqrt(n-1)*sd(X1))
  Z2 <- (X2-mean(X2))/(sqrt(n-1)*sd(X2))
  Z3 <- (X3-mean(X3))/(sqrt(n-1)*sd(X3))
  Y.star <- (Y-mean(Y))/(sqrt(n-1)*sd(Y)) 
  
  # Standardized Ridge Estimates 
  beta.R.hat <- solve(t(X.star)%*%X.star+diag(rep(c,3)))%*%t(X.star)%*%Y.star
  
  # reverse transform the model 
  beta.1 <- beta.R.hat[1]*(sd(Y)/sd(X1))
  beta.2 <- beta.R.hat[2]*(sd(Y)/sd(X2))
  beta.3 <- beta.R.hat[3]*(sd(Y)/sd(X3))
  beta.0 <- mean(Y)-(beta.1*mean(X1)+beta.2*mean(X2)+beta.3*mean(X3))
  return(c(beta.0,beta.1,beta.2,beta.3))
  
}

boot.fun(dataset=BF,c=0.02)  
  
  
n <- length(Y)
B <- 1000
estimates <- NULL

for (b in 1:B) {
  new_sample <- sample(1:n, size = n, replace = TRUE)
  estimates[b] <- boot.fun(BF[new_sample,])[2]
}

hist(estimates)






