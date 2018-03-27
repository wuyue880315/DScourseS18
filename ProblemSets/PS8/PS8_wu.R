library(nloptr)

#Random number generator 
set.seed(100)

#Creat maxtrix
N <- 100000
K <- 10
sigma <- 0.5
X <- matrix(rnorm( N*K, mean = 0, sd=sigma),N,K)

#First column
X[,1] <- 1

#Set the error term. (eps)
eps <- rnorm(N, mean = 0, sd=0.5)

#set true beta values
beta <- c(1.5, -1, -0.25, 0.75, 3.5, -2, 0.5, 1, 1.25, 2)
beta <- as.vector(beta)

#Generate the Y euqal to X * beta + eps
Y <- X %*% beta + eps

###########################################
#Compute OLS by using closed-form solution#
###########################################

beta_CF <- solve(crossprod(X)) %*% crossprod(X,Y) 
beta_CF #The different between the result and true beta is very small.

#######################################
#Compute OLS by using gradient descent#
#######################################

alpha <- 0.0000003
maxiter <- 500000
#Define the gradient
gradient <- function(beta,X,Y){
  return(as.vector(-2*t(X)%*%(Y-X%*%beta)))
}

#Set the initial values of beta.
beta <- runif(dim(X)[2])

#Vector contain all beta for all steps.
beta.all <- matrix("numeric", length(beta), maxiter)

#Gradient decent method to find the minimum
i <- 1
beta0 <- 0*beta
while(norm(as.matrix(beta0)-as.matrix(beta))>1e-8){
  beta0 <- beta
  beta <- beta0 - alpha * gradient(beta0,Y,X)
  beta.all[,i]<-beta
  if (i%%10000==0){
    print(beta)
  }
  i <- i +1
}

beta

#############################
#Compute OLS by using L-BFGS#
#############################

objfun<-function(beta,Y,X){
  return(crossprod(Y-X%*%beta))
}
beta0<-runif(dim(X)[2])
options <- list("algorithm"="NLOPT_LD_LBFGS","xtol_rel"=1.0e-8,"maxeval"=1e6)
result <- nloptr(x0=beta0,eval_f = objfun,eval_grad_f = gradient, opts = options,Y=Y,X=X)
result

##################################
#Compute OLS by using Nelder-Mead#
##################################

options <- list("algorithm"="NLOPT_LD_LBFGS","xtol_rel"=1.0e-8,"maxeval"=1e6)
result <- nloptr(x0=beta0,eval_f = objfun,eval_grad_f = gradient, opts = options,Y=Y,X=X)
result

#The result is same as the result in L-BFGS method.

gradient <- function (theta ,Y,X) {
  grad <- as.vector ( rep (0, length ( theta )))
  beta <- theta [1:( length ( theta ) -1)]
  sig <- theta [ length ( theta )]
  grad [1:( length ( theta ) -1)] <- -t(X)%*%(Y - X%*%beta )/( sig ^2)
  grad [ length ( theta )] <- dim (X) [1] /sig - crossprod (Y-X%*%beta )/( sig^3)
  return ( grad )
}
beta0<-runif(dim(X)[2])
options <- list("algorithm"="NLOPT_LD_LBFGS","xtol_rel"=1.0e-8,"maxeval"=1e6)
result <- nloptr(x0=beta0,eval_f = objfun,eval_grad_f = gradient, opts = options,Y=Y,X=X)
result

############################
#Compute beta by using lm()#
############################

summary(lm(Y~X -1))
