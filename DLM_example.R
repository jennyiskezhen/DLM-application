
DLM <- function(X,Y,delta1,delta2,sq){

  # Dynamic linear Model
  # Based on flow and the constituent
  
  # ARGUMENTS
  # X = log(flow data) 
  # Y = log(sediment data)
  # delta = dynamicity
  
  # RESULTS
  # m = mean of the estimated parameters (1X2)
  # error = error between observation data and one-setp ahead forecast 
  # f = one-step ahead forecast

  ######### start dlm
  sq <- sq + 1
  N <- length(X)
  
  ######initial condition
  m.in <- matrix(c(0,0),ncol = 1)
  fit <- lm(Y ~ X)
  stderr <- sqrt(deviance(fit)/df.residual(fit))
  S.in <- stderr^2
  
  #### X
  if (sq == 2) {
    X <- cbind(rep(1,N),X)
    m.in <- matrix(c(0,0),ncol = 1)
  } else if (sq == 3) {
    X <- cbind(rep(1,N),X,X^2)
    m.in <- matrix(c(0,0,0),ncol = 1)
  }
  
  ####### variance-covariance matrix
  alpha <- 1
  if (sq == 2) {
    C.in <- diag(x = c(5,alpha), nrow = sq)
  } else if (sq == 3) {
    C.in <- diag(x = c(5,alpha,alpha), nrow = sq)
  }

  ################ start loop 
  m <- array(NA,c(N,sq)) # NX2 matrix
  C <- array(0,c(N,sq,sq)) # 2 NX2 matrix
  R <- array(0,c(N,sq,sq))
  W <- array(0,c(N,sq,sq))
  A <- array(NA,c(N,sq))
  f <- array(NA,N)
  e <- array(NA,N)
  e.orig <- array(NA,N)
  S <- array(NA,N)
  Q <- array(NA,N)
  nn <- array(NA,N)

  mp <- m.in
  Cp <- C.in
  Sp <- S.in
  nnp <- 0
  
  for (t in 1:N) {
    
    W[t,1,1] <- (1-delta1)/delta1*Cp[1,1]
    W[t,2,2] <- (1-delta2)/delta2*Cp[2,2]
    
    if (sq == 3) {
      W[t,3,3] <- (1-delta2)/delta2*Cp[3,3]
    }
    
    R[t,,] <- Cp + W[t,,] # 3X3
    
    f[t] <- t(X[t,]) %*% mp 
    Q[t] <- t(X[t,])%*%R[t,,]%*%X[t,] + Sp 
    A[t,] <- R[t,,]%*%X[t,]/Q[t] # 3X1
    
    if (!is.na(Y[t]) & !is.na(X[t,2])) {    
      nn[t] <- nnp + 1
      e[t] <- Y[t] - f[t]
      e.orig[t] <- exp(Y[t]) - exp(f[t])
      S[t] <- Sp + Sp/nn[t]*(e[t]^2/Q[t]-1)
      m[t,] <- mp + A[t,]*e[t]
      C[t,,] <- S[t]/Sp*(R[t,,] - A[t,]%*%t(A[t,])*Q[t]) # 3X3
    } else {
      nn[t] <- nnp
      e[t] <- NA
      e.orig[t] <- NA
      S[t] <- Sp
      m[t,] <- mp
      C[t,,] <- R[t,,]
    }  
    
    mp <- t(t(m[t,]))
    Sp <- S[t]
    Cp <- C[t,,]
    nnp <- nn[t]
  }

  df <- list(m,e,e.orig,f)
  
  names(df) <- c("m","e","e.orig","f")
  
  return(df)
}

