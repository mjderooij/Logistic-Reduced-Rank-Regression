rm(list=ls())
drugdat <- read.table('https://archive.ics.uci.edu/ml/machine-learning-databases/00373/drug_consumption.data', sep = ",")
for (v in 14:32){
  drugdat[,v] = ifelse(drugdat[,v] == "CL3", 1, ifelse(drugdat[,v] == "CL4", 1, ifelse(drugdat[, v] == "CL5",1, ifelse(drugdat[,v] == "CL6", 1, 0))))
}

# add variable names
colnames(drugdat) = c(
  "id",
  "age",
  "gender",
  "educ",
  "country",
  "ethnic",
  "N",
  "E",
  "O",
  "A",
  "C",
  "impulse",
  "SS",
  "Alcohol",
  "Am",
  "Amyl",
  "Be",
  "Caff",
  "Ca",
  "Choc",
  "Co",
  "Crack",
  "Ex",
  "Heroin",
  "Ke",
  "Le",
  "LSD",
  "Me",
  "Mu",
  "Ni",
  "Semer",
  "VSA"
)

X = as.matrix(drugdat[,c(2,3,7:13)])
Y = as.matrix(drugdat[,14:32])  
idx = which(colMeans(Y) > 0.1 & colMeans(Y) < 0.9)
Y = Y[, idx]
rm(list= ls()[!(ls() %in% c('X','Y'))])

################################################################################################
################################################################################################
# FUNCTIONS
################################################################################################
################################################################################################

#'  The function lpca performs logistic pca with or without predictors to obtain
#'  a unsupervised or supervised mapping of binary response variables.
#'
#' This function runs:
#' logistic principal component analysis (if X = NULL)
#' logistic reduced rank regression (if X != NULL)
#'
#' @param Y An N times R binary matrix  .
#' @param X An N by P matrix with predictor variables
#' @param S Positive number indicating the dimensionality of teh solution
#' @param dim.indic An R by S matrix indicating which response variable pertains to which dimension
#' @param eq Only applicable when dim.indic not NULL; equality restriction on regression weighhts per dimension
#' @param lambda if TRUE does lambda scaling (see Understanding Biplots, p24)
#' @param maxiter maximum number of iterations
#' @param dcrit convergence criterion
#'
#' @return This function returns an object of the class \code{lpca} with components:
#' \item{Y}{Matrix Y from input}
#' \item{Xoriginal}{Matrix X from input}
#' \item{X}{Scaled X matrix}
#' \item{mx}{Mean values of X}
#' \item{sdx}{Standard deviations of X}
#' \item{ynames}{Variable names of responses}
#' \item{xnames}{Variable names of predictors}
#' \item{probabilities}{Estimated values of Y}
#' \item{m}{main effects}
#' \item{U}{matrix with coordinates for row-objects}
#' \item{B}{matrix with regression weight (U = XB)}
#' \item{V}{matrix with vectors for items/responses}
#' \item{iter}{number of main iterations from the MM algorithm}
#' \item{deviance}{value of the deviance at convergence}
#'
#' @examples
#' data(dataExample_lpca)
#' Y = as.matrix(dataExample_lpca[1:20 , 1:8])
#' X = as.matrix(dataExample_lpca[1:20 , 9:13])
#' # unsupervised
#' output = lpca(Y = Y, S = 2)
#'
#'
#'
#' @import tidyverse
#' @importFrom stats plogis
#'
#' @export
lpca <- function(Y, X = NULL, S = 2, dim.indic = NULL, eq = FALSE, lambda = FALSE, maxiter = 65536, dcrit = 1e-5){
  
  
  # checks
  if ( is.null( Y ) ) stop( "missing response variable matrix Y")
  if( ! is.null(X)) if ( nrow(Y) != nrow(X) ) stop( "number of rows in X and Y should match")
  if( S < 1) stop("dimensionality (S) should be larger than 1")
  if( maxiter < 1) stop("maximum number of iterations should be a positive number")
  if( dcrit < 0) stop("converence criterion should be a positive number")
  if( is.null(dim.indic) && eq == TRUE) stop("equality restriction can only be imposed when items pertian to dimensions")
  
  ynames = colnames(Y)
  
  # Data Coding
  Q = 2 * as.matrix(Y) - 1
  Q[is.na(Q)] <- 0 # passive treatment of missing responses
  N = nrow(Q)
  R = ncol(Q)
  
  ###############################################################
  # unsupervised analysis
  ###############################################################
  if( is.null(X) ){
    mx = NULL
    sdx = NULL
    B = NULL
    Xoriginal = NULL
    xnames = NULL
    
    # starting values
    m = colMeans(4 * Q)
    U = matrix(0, N, S)
    V = matrix(0, R, S)
    
    # compute deviance
    theta = outer(rep(1, N), m)
    dev.old <- -2 * sum(log(plogis(Q * theta)))
    
    for (iter in 1:maxiter) {
      # compute working response
      Z = as.matrix(theta + 4 * Q * (1 - plogis(Q * theta)))
      
      # update main effects
      m = as.numeric(colMeans(Z))
      
      # update U and V
      udv = svd(scale(Z, center = m, scale = FALSE))
      U = matrix(udv$u[, 1:S], N, S) %*% diag(udv$d[1:S], nrow = S, ncol = S)
      V = matrix(udv$v[, 1:S], R, S)
      
      # compute deviance
      theta = outer(rep(1, N), m) + U %*% t(V)
      dev.new <- -2 * sum(log(plogis(Q * theta)))
      
      if ( ((dev.old - dev.new)/dev.old) < dcrit ) break
      if (iter == maxiter) warning("Maximum number of iterations reached - not converged (yet)")
      dev.old = dev.new
    }
    if(lambda){
      # lambda-scaling - see Gower/Le Roux & Lubbe, page 24
      ssqu = sum(U^2)
      ssqv = sum(V^2)
      lambda = (N/R) * ssqv/ssqu
      U = lambda*U
      V = V/lambda
    }
  }
  ###############################################################
  # supervised analysis
  ###############################################################
  if( !is.null(X) ){
    
    xnames = colnames(X)
    
    # items pertain to specified dimensions
    if( !is.null(dim.indic) ){
      if(eq){
        V = dim.indic
        iVVV= t(solve(t(V) %*% V) %*% t(V))
      }
      dim.indic = matrix(as.logical(dim.indic), R, S)
    }
    
    Xoriginal = X
    # center X
    X = as.matrix(scale(X, center = TRUE, scale = TRUE))
    mx = attr(X, "scaled:center")
    sdx = attr(X, "scaled:scale")
    # 
    # mx = rep(0, ncol(X)) 
    # sdx = rep(1, ncol(X))
    P = ncol(X)
    
    #
    eig.out = eigen(t(X) %*% X)
    Rx = eig.out$vectors %*% diag(sqrt(eig.out$values)) %*% t(eig.out$vectors)
    iXXX = solve(t(X) %*% X) %*% t(X)
    iRx = solve(Rx)
    iRxX = iRx %*% t(X)
    
    
    # starting values
    m = colMeans(4 * Q)
    B = matrix(0, P, S)
    V = matrix(0, R, S)
    
    # compute deviance
    theta = outer(rep(1, N), m)
    dev.old <- -2 * sum(log(plogis(Q * theta)))
    
    for (iter in 1:maxiter) {
      # compute working response
      Z = as.matrix(theta + 4 * Q * (1 - plogis(Q * theta)))
      
      # update main effects
      # m = as.numeric(colMeans(Z))
      m = as.numeric(colMeans(Z - X %*% B %*% t(V))) # in case X is not centered. 
      
      # update B and V
      if( is.null(dim.indic) ){
        udv = svd(iRxX %*% scale(Z, center = m, scale = FALSE))
        B = iRx %*% matrix(udv$u[, 1:S], P, S) * sqrt(N)
        V = matrix(udv$v[, 1:S], R, S) %*% diag(udv$d[1:S], nrow = S, ncol = S) / sqrt(N)
      }
      
      if( !is.null(dim.indic) ){
        if(!eq){
          for(s in 1:S){
            Ztilde = scale(Z, center = m, scale = FALSE) - X%*%B[ , -s] %*% t(V[ , -s])
            udv = svd(iRxX %*% Ztilde[, dim.indic[ ,s]])
            B[ , s] = iRx %*% matrix(udv$u[, 1], P, 1) * sqrt(N)
            V[dim.indic[, s], s] = (udv$v[, 1] * udv$d[1]) / sqrt(N)
          }
        }
        if(eq) B = iXXX %*% scale(Z, center = m, scale = FALSE) %*% iVVV #old: if(eq) B = iXXX %*% scale(Z, center = mu, scale = FALSE) %*% iVVV
      }
      
      # compute deviance
      theta = outer(rep(1, N), m) + X %*% B %*% t(V)
      dev.new <- -2 * sum(log(plogis(Q * theta)))
      
      if (((dev.old - dev.new)/dev.old) < dcrit) break
      if (iter == maxiter) warning("Maximum number of iterations reached - not converged (yet)")
      dev.old = dev.new
    }
    U = X %*% B
  }
  
  # create output object of class "l.pca"
  output = list(
    Y = Y,
    Q = Q,
    Xoriginal = Xoriginal,
    X = X,
    mx = mx,
    sdx = sdx,
    ynames = ynames,
    xnames = xnames,
    theta = theta,
    probabilities = plogis(theta),
    m = m,
    U = U,
    B = B,
    V = V,
    iter = iter,
    deviance = dev.new
  )
  class(output) = "lpca"
  return(output)
}
q.lpca = function(object){
  # quality of representation 
  # input: lpca object
  # return: Quality of Representation
  Y = object$Y
  X = object$Xoriginal
  R = ncol(Y)
  devs.lr = devs.null = rep(NA,R)
  for(r in 1:R){
    v = glm(Y[,r] ~ X, family = "binomial")
    devs.null[r] = v$null.deviance
    devs.lr[r] = v$deviance
  }
  devs.lpca = -2 * colSums(log(plogis(object$Q * object$theta)))
  # quality of representation
  QOR = (devs.null - devs.lpca) /(devs.null - devs.lr)
  QoR = matrix(QOR, R, 1)
  rownames(QoR) = colnames(Y)
  return(QoR)
}

#' plots the results of a
#' logistic principal component analysis (X = NULL)
#' logistic reduced rank regression (X != NULL)
#'
#' @param x an object of type lpca
#' @param dims which dimensions to visualize
#' @param type either pca or dist
#' @param ycol colour for representation of response variables
#' @param xcol colour for representation of predictor variables
#' @param ocol colour for representation of row objects
#' @param \dots additional arguments to be passed.
#' @return Plot of the results obtained from lpca
#'
#' @examples
#' data(dataExample_lpca)
#' Y = as.matrix(dataExample_lpca[1:20 , 1:8])
#' X = as.matrix(dataExample_lpca[1:20 , 9:13])
#' # unsupervised
#' output = lpca(Y = Y, S = 2)
#' plot(output)
#'
#'
#' @import ggforce
#' @import ggplot2
#' @import ggrepel
#'
#' @export
plot.lpca <- function(x, dims = c(1,2), type = "pca", ycol = "darkgreen", xcol = "lightskyblue", ocol = "grey",...){
  
  object = x
  
  if(type == "pca"){
    plt = plot.lpca1(object, dims = c(1,2), ycol = ycol, xcol = xcol, ocol = ocol) # aangepast
  }
  if(type == "dist"){
    plt = plot.lpca2(object, dims = c(1,2), ycol = ycol, xcol = xcol, ocol = ocol) # aangepast
  }
  suppressWarnings(print(plt))
  
  return(plt)
}
plot.lpca1 <- function(object, dims = c(1,2), ycol = "darkgreen", xcol = "lightskyblue", ocol = "grey"){
  # plots the results of a
  # logistic principal component analysis (X = NULL)
  # logistic reduced rank regression (X != NULL)
  #
  # @param object an object of type lpca
  # @param dims which dimensions to visualize
  # @param ycol colour for representation of response variables
  # @param xcol colour for representation of predictor variables
  # @param ocol colour for representation of row objects
  #
  
  ######################################################
  # retrieve information from object
  ######################################################
  Y = object$Y
  
  U = as.data.frame(object$U[ , dims])
  N = nrow(U)
  colnames(U) = c("dim1", "dim2")
  
  V = object$V[ , dims]
  VV = as.data.frame(V)
  R = nrow(V)
  colnames(VV) = c("dim1", "dim2")
  rownames(VV) = object$ynames
  
  ######################################################
  # retrieve information for response variables variable axes
  ######################################################
  
  MCy <- data.frame(labs=character(),
                    vary = integer(),
                    dim1 = double(),
                    dim2 = double(), stringsAsFactors=FALSE)
  ll = 0
  lo = log(seq(from = 0.1, to = 0.9, by = 0.1)/(1 - seq(from = 0.1, to = 0.9, by = 0.1)))
  l.m = length(lo)
  markerlabs = paste(seq(0.1, 0.9, by = 0.1))
  
  for(r in 1:R){
    markers = matrix(lo - object$m[r], length(lo), 1)
    v = matrix(V[r, dims], nrow = 2, ncol = 1)
    markerscoord = markers %*% t(v %*% solve(t(v) %*% v))
    MCy[(ll + 1): (ll + l.m), 1] = markerlabs
    MCy[(ll + 1): (ll + l.m), 2] = r
    MCy[(ll + 1): (ll + l.m), 3:4] = markerscoord
    ll = ll + l.m
  }
  
  ######################################################
  # retrieve information for predictor variables variable axes
  ######################################################
  X = object$X
  isx = !is.null(X)
  if(isx){
    P = ncol(X)
    B = object$B[ , dims]
    Xo = object$Xoriginal
    
    # for solid line
    MCx1 <- data.frame(labs=character(),
                       varx = integer(),
                       dim1 = double(),
                       dim2 = double(), stringsAsFactors=FALSE)
    # for markers
    MCx2 <- data.frame(labs=character(),
                       varx = integer(),
                       dim1 = double(),
                       dim2 = double(), stringsAsFactors=FALSE)
    
    ll = 0
    lll = 0
    for(p in 1:P){
      b = matrix(B[p , ], 2, 1)
      # solid line
      minx = min(Xo[, p])
      maxx = max(Xo[, p])
      m.x1 = c(minx,maxx)
      markers1 = matrix((m.x1 - object$mx[p])/object$sdx[p], 2, 1)
      markerscoord1 = outer(markers1, b) # markers1 %*% t(b %*% solve(t(b) %*% b))
      MCx1[(ll + 1): (ll + 2), 1] = paste0(c("min", "max"), p)
      MCx1[(ll + 1): (ll + 2), 2] = p
      MCx1[(ll + 1): (ll + 2), 3:4] = markerscoord1
      ll = ll + 2
      # markers
      m.x2 = pretty(Xo[, p])
      m.x2 = m.x2[which(m.x2 > minx & m.x2 < maxx)]
      l.m = length(m.x2)
      markers2 = matrix((m.x2 - object$mx[p])/object$sdx[p], l.m, 1)
      markerscoord2 = outer(markers2, b) # markers2 %*% t(b %*% solve(t(b) %*% b))
      MCx2[(lll + 1): (lll + l.m), 1] = paste(m.x2)
      MCx2[(lll + 1): (lll + l.m), 2] = p
      MCx2[(lll + 1): (lll + l.m), 3:4] = markerscoord2
      lll = lll + l.m  
    } # loop p
  } #isx
  
  ######################################################
  # plotting - objects
  ######################################################
  plt = ggplot() +
    geom_point(data = U, aes_string(x = 'dim1', y = 'dim2'), colour = ocol, alpha = 0.5, size = 0.5) +
    xlab(paste("Dimension", dims[1])) +
    ylab(paste("Dimension", dims[2])) + 
    labs(title = "Type I triplot")
  
  a = max(abs(c(ggplot_build(plt)$layout$panel_scales_x[[1]]$range$range, ggplot_build(plt)$layout$panel_scales_y[[1]]$range$range))) + 0.1
  
  ######################################################
  # variable axes with ticks and markers for predictors
  ######################################################
  if(isx){
    plt = plt + geom_abline(intercept = 0, slope = B[,2]/B[,1], colour = xcol, linetype = 3) +
      geom_line(data = MCx1, aes_string(x = 'dim1', y = 'dim2', group = 'varx'), col = xcol, size = 1.5) +
      geom_point(data = MCx2, aes_string(x = 'dim1', y = 'dim2'), col = xcol, size = 2) +
      geom_text(data = MCx2, aes_string(x = 'dim1', y = 'dim2', label = 'labs'), nudge_x = 0.08, nudge_y = -0.08, size = 2.5)
  }
  
  ######################################################
  # variable axes with ticks and markers for responses
  ######################################################
  plt = plt + geom_abline(intercept = 0, slope = V[,2]/V[,1], colour = ycol, linetype = 3) +
    geom_point(data = MCy, aes_string(x = 'dim1', y = 'dim2'), shape = 15, colour = ycol) +
    geom_text(data = MCy, aes_string(x = 'dim1', y = 'dim2', label = 'labs'), nudge_y = -0.08, size = 1.5)
  
  ######################################################
  # variable labels
  ######################################################

  idx2 = apply(abs(V), 1, which.max)
  t2 = s2 = rep(NA,R)
  for(rr in 1:R){
    t2[rr] = (a * 1.1)/(abs(V[rr,idx2[rr]])) * V[rr,-idx2[rr]]
    s2[rr] = sign(V[rr,idx2[rr]])
  }
  CC2 = cbind(idx2, t2, s2)
  
  if(isx){
    idx1 = apply(abs(B), 1, which.max)
    t1 = s1 = rep(NA,P)
    for(pp in 1:P){
      t1[pp] = (a * 1.1)/(abs(B[pp,idx1[pp]])) * B[pp,-idx1[pp]]
      s1[pp] = sign(B[pp,idx1[pp]])
    }
    CC1 = cbind(idx1, t1, s1)
  }
  
  if(isx) CC = rbind(CC1, CC2) else CC = CC2
  if(isx) rownames(CC) = c(object$xnames, object$ynames) else rownames(CC) = object$ynames
  colnames(CC) = c("idx", "t", "s")
  
  bottom = which(CC[, "idx"] == 2 & CC[, "s"] == -1)
  top =  which(CC[, "idx"] == 2 & CC[, "s"] == 1)
  right = which(CC[, "idx"] == 1 & CC[, "s"] == 1)
  left = which(CC[, "idx"] == 1 & CC[, "s"] == -1)
  
  
  if(length(CC[top, "t"])==0){
    plt <- plt + scale_x_continuous(limits = c(-a,a), breaks = CC[bottom, "t"], labels = rownames(CC)[bottom],
                                    sec.axis = sec_axis(trans ~ ., breaks = 0, labels = ""))
  }else{
    plt <- plt + scale_x_continuous(limits = c(-a,a), breaks = CC[bottom, "t"], labels = rownames(CC)[bottom],
                                    sec.axis = sec_axis(trans ~ ., breaks = CC[top, "t"], labels = rownames(CC)[top]))
  }
  
  
  if(length(CC[right, "t"])==0){
    plt = plt + scale_y_continuous(limits = c(-a,a), breaks = CC[left, "t"], labels = rownames(CC)[left],
                                   sec.axis = sec_axis(trans ~ ., breaks = 0, labels = ""))
  }else{
    plt = plt + scale_y_continuous(limits = c(-a,a), breaks = CC[left, "t"], labels = rownames(CC)[left],
                                   sec.axis = sec_axis(trans ~ ., breaks = CC[right, "t"], labels = rownames(CC)[right]))
  }
  
  
  plt = plt + theme(axis.line = element_line(colour = "black"),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.border = element_blank(),
                    panel.background = element_blank())
  plt = plt + coord_fixed()
  
  #suppressWarnings(print(plt))
  
  return(plt)
}
plot.lpca2 <- function(object, dims = c(1,2), ycol = "darkgreen", xcol = "lightskyblue", ocol = "grey"){
  # plots the results of a
  # logistic principal component analysis (X = NULL)
  # logistic reduced rank regression (X != NULL)
  # distance representation
  #
  # @param object an object of type lpca
  # @param dims which dimensions to visualize
  # @param ycol colour for representation of response variables
  # @param xcol colour for representation of predictor variables
  # @param ocol colour for representation of row objects
  #
  
  object = lpca2dist(object)
  
  ######################################################
  # retrieve information from object
  ######################################################
  Y = object$Y
  
  U = as.data.frame(object$U[ , dims])
  N = nrow(U)
  colnames(U) = c("dim1", "dim2")
  
  V = object$V[ , dims]
  VV = as.data.frame(V)
  R = nrow(V)
  colnames(VV) = c("dim1", "dim2")
  rownames(VV) = object$ynames
  
  ######################################################
  # retrieve information for response variables variable axes
  ######################################################
  
  # MCy <- data.frame(labs=character(),
  #                   vary = integer(),
  #                   dim1 = double(),
  #                   dim2 = double(), stringsAsFactors=FALSE)
  # ll = 0
  # lo = log(seq(from = 0.1, to = 0.9, by = 0.1)/(1 - seq(from = 0.1, to = 0.9, by = 0.1)))
  # l.m = length(lo)
  # markerlabs = paste(seq(0.1, 0.9, by = 0.1))
  #
  # for(r in 1:R){
  #   markers = matrix(lo - object$m[r], length(lo), 1)
  #   v = matrix(V[r, dims], nrow = 2, ncol = 1)
  #   markerscoord = markers %*% t(v %*% solve(t(v) %*% v))
  #   MCy[(ll + 1): (ll + l.m), 1] = markerlabs
  #   MCy[(ll + 1): (ll + l.m), 2] = r
  #   MCy[(ll + 1): (ll + l.m), 3:4] = markerscoord
  #   ll = ll + l.m
  # }
  #
  ######################################################
  # retrieve information for predictor variables variable axes
  ######################################################
  X = object$X
  isx = !is.null(X)
  if(isx){
    P = ncol(X)
    B = object$B[ , dims]
    Xo = object$Xoriginal
    
    # for solid line
    MCx1 <- data.frame(labs=character(),
                       varx = integer(),
                       dim1 = double(),
                       dim2 = double(), stringsAsFactors=FALSE)
    # for markers
    MCx2 <- data.frame(labs=character(),
                       varx = integer(),
                       dim1 = double(),
                       dim2 = double(), stringsAsFactors=FALSE)
    
    ll = 0
    lll = 0
    for(p in 1:P){
      b = matrix(B[p , ], 2, 1)
      # solid line
      minx = min(Xo[, p])
      maxx = max(Xo[, p])
      m.x1 = c(minx,maxx)
      markers1 = matrix((m.x1 - object$mx[p])/object$sdx[p], 2, 1)
      markerscoord1 = outer(markers1, b) # markers1 %*% t(b %*% solve(t(b) %*% b))
      MCx1[(ll + 1): (ll + 2), 1] = paste0(c("min", "max"), p)
      MCx1[(ll + 1): (ll + 2), 2] = p
      MCx1[(ll + 1): (ll + 2), 3:4] = markerscoord1
      ll = ll + 2
      # markers
      m.x2 = pretty(Xo[, p])
      m.x2 = m.x2[which(m.x2 > minx & m.x2 < maxx)]
      l.m = length(m.x2)
      markers2 = matrix((m.x2 - object$mx[p])/object$sdx[p], l.m, 1)
      markerscoord2 = outer(markers2, b) # markers2 %*% t(b %*% solve(t(b) %*% b))
      MCx2[(lll + 1): (lll + l.m), 1] = paste(m.x2)
      MCx2[(lll + 1): (lll + l.m), 2] = p
      MCx2[(lll + 1): (lll + l.m), 3:4] = markerscoord2
      lll = lll + l.m
    } # loop p
  } #isx
  
  ######################################################
  # plotting - objects
  ######################################################
  plt = ggplot() +
    geom_point(data = U, aes_string(x = 'dim1', y = 'dim2'), colour = ocol, alpha = 0.5, size = 0.5) +
    geom_point(data = VV, aes_string(x = 'dim1', y = 'dim2'), colour = ycol) +
    geom_text_repel(data = VV, aes_string(x = 'dim1', y = 'dim2', label = 'rownames(VV)'), family = "mono") +
    xlab(paste("Dimension", dims[1])) +
    ylab(paste("Dimension", dims[2])) + 
    labs(title = "Type D triplot")

  ######################################################
  # variable axes with ticks and markers for predictors
  ######################################################
  # if(isx){
  #   plt = plt + geom_abline(intercept = 0, slope = B[,2]/B[,1], colour = xcol, linetype = 3) +
  #     geom_line(data = MCx1, aes_string(x = 'dim1', y = 'dim2', group = 'varx'), col = xcol) +
  #     geom_point(data = MCx2, aes_string(x = 'dim1', y = 'dim2'), col = xcol) +
  #     geom_text(data = MCx2, aes_string(x = 'dim1', y = 'dim2', label = 'labs'), nudge_y = -0.08, size = 1.5)
  # }
  if(isx){
    plt = plt + geom_abline(intercept = 0, slope = B[,2]/B[,1], colour = xcol, linetype = 3) +
      geom_line(data = MCx1, aes_string(x = 'dim1', y = 'dim2', group = 'varx'), col = xcol, size = 1.5) +
      geom_point(data = MCx2, aes_string(x = 'dim1', y = 'dim2'), col = xcol, size = 2) +
      geom_text(data = MCx2, aes_string(x = 'dim1', y = 'dim2', label = 'labs'), nudge_x = 0.08, nudge_y = -0.08, size = 2.5)
  }  
  ######################################################
  # variable axes with ticks and markers for responses
  ######################################################
  # plt = plt + geom_abline(intercept = 0, slope = V[,2]/V[,1], colour = ycol) +
  #   geom_point(data = MCy, aes(x = dim1, y = dim2), shape = 15, colour = ycol) +
  #   geom_text(data = MCy, aes(x = dim1, y = dim2, label = labs), nudge_y = -0.08, size = 1.5)
  
  ######################################################
  # variable labels
  ######################################################
  a = ceiling(max(abs(c(ggplot_build(plt)$layout$panel_scales_x[[1]]$range$range, ggplot_build(plt)$layout$panel_scales_y[[1]]$range$range))))
  
  # idx2 = apply(abs(V), 1, which.max)
  # t2 = s2 = rep(NA,R)
  # for(rr in 1:R){
  #   t2[rr] = (a * 1.1)/(abs(V[rr,idx2[rr]])) * V[rr,-idx2[rr]]
  #   s2[rr] = sign(V[rr,idx2[rr]])
  # }
  # CC2 = cbind(idx2, t2, s2)
  
  if(isx){
    idx1 = apply(abs(B), 1, which.max)
    t1 = s1 = rep(NA,P)
    for(pp in 1:P){
      t1[pp] = (a * 1.1)/(abs(B[pp,idx1[pp]])) * B[pp,-idx1[pp]]
      s1[pp] = sign(B[pp,idx1[pp]])
    }
    CC = cbind(idx1, t1, s1)
    rownames(CC) = object$xnames
    colnames(CC) = c("idx", "t", "s")
    
    bottom = which(CC[, "idx"] == 2 & CC[, "s"] == -1)
    top =  which(CC[, "idx"] == 2 & CC[, "s"] == 1)
    right = which(CC[, "idx"] == 1 & CC[, "s"] == 1)
    left = which(CC[, "idx"] == 1 & CC[, "s"] == -1)
    
    if(length(CC[top, "t"])==0){
      plt = plt + scale_x_continuous(limits = c(-a,a), breaks = CC[bottom, "t"], labels = rownames(CC)[bottom],
                                     sec.axis = sec_axis(trans ~ ., breaks = 0, labels = ""))
    }else{
      plt = plt + scale_x_continuous(limits = c(-a,a), breaks = CC[bottom, "t"], labels = rownames(CC)[bottom],
                                     sec.axis = sec_axis(trans ~ ., breaks = CC[top, "t"], labels = rownames(CC)[top]))
    }
    
    
    if(length(CC[right, "t"])==0){
      plt = plt + scale_y_continuous(limits = c(-a,a), breaks = CC[left, "t"], labels = rownames(CC)[left],
                                     sec.axis = sec_axis(trans ~ ., breaks = 0, labels = ""))
    }else{
      plt = plt + scale_y_continuous(limits = c(-a,a), breaks = CC[left, "t"], labels = rownames(CC)[left],
                                     sec.axis = sec_axis(trans ~ ., breaks = CC[right, "t"], labels = rownames(CC)[right]))
    }
    
    
  }
  
  plt = plt + theme(axis.line = element_line(colour = "black"),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.border = element_blank(),
                    panel.background = element_blank())
  plt = plt + coord_fixed()
  
  #suppressWarnings(print(plt))
  
  return(plt)
}
source("~/surfdrive/LogitMDA/lrrr/plot.lpcah.R")
lpca2dist = function(object){
  # transformation of lpca object to distance representations
  #
  K = -object$V/2 # divide by 2??
  B = object$B
  U = object$U
  a = object$m
  
  R = nrow(K)
  M = ncol(K)
  
  Ak = kronecker(diag(R), matrix(c(1,-1),2,1)) # for K - response variable discrimination
  Al = kronecker(diag(R), matrix(c(1,1),2,1)) # for L - response variable position
  
  L = matrix(0, R, M)
  
  # copy from melodic.R
  if(M == 1){L = a/K}
  if(M > 1){
    for(r in 1:R){
      L[r, ] = a[r] * K[r, ]/(2 *sum(K[r, ]^2))
    }
  }
  # L = L/2
  Vl = Al %*% L  # midpoints
  
  Vk = Ak %*% K # deviation from midpoints/discrimination
  Vl = Al %*% L  # midpoints/location
  V = Vl + Vk
  
  ynames = paste0(rep(object$ynames, each = 2), c(0,1))
  
  mldm = list(
    Y = object$Y,
    Xoriginal = object$Xoriginal,
    X = object$X,
    mx = object$mx,
    sdx = object$sdx,
    ynames = ynames,
    xnames = object$xnames,
    U = U,
    B = B,
    V = V,
    K = K,
    L = L,
    iter = object$iter,
    deviance = object$deviance
  )
  return(mldm)
}


################################################################################################
################################################################################################
# ANALYSES
################################################################################################
################################################################################################

out = lpca(Y = Y, X = X[ , -8], dcrit = 1e-8)

# check probabilities
library(Rfast)
out2 = lpca2dist(out)
W = as.data.frame(out2$V); rownames(W) = out2$ynames; colnames(W) = c("dim1", "dim2")
U = out2$U
D = 0.5 * dista(U, W)^2 
Ghat = matrix(NA, 1885, 22)

for(r in seq(1, 21, by = 2)){
  Ghat[ , r] = exp(-D[ , r])/(exp(-D[ , r]) + exp(-D[ , (r+1)]))
  Ghat[, r+1] = 1- Ghat[, r]
}
head(out$probabilities)
colnames(Ghat) = out2$ynames; head(Ghat[, seq(2, 22, by = 2)])


plt1 = plot.lpca1(out)
ggsave("~/surfdrive/logitMDA/lrrr/paper/drugs_i.pdf", plt1, width = 15, height = 15, units = "cm")

plt2 = plot.lpca2(out)
ggsave("~/surfdrive/logitMDA/lrrr/paper/drugs_d.pdf", plt2, width = 15, height = 15, units = "cm")

# response = rep(c("no", "yes"), 11)
# plt1 + geom_point(data = W, aes(x = dim1, y = dim2, shape = response), size = 4, col = "darkgreen", show.legend = FALSE)
# WW = cbind(W[seq(1,21,by = 2), ], W[seq(2, 22,by = 2), ])
# colnames(WW) = c("dim1a", "dim2a", "dim1b", "dim2b")
# plt1 + geom_segment(aes(x = dim1a, y = dim2a, xend = dim1b, yend = dim2b), 
#                     #arrow = arrow(length = unit(0.02, "npc")), 
#                     size = 1,
#                     colour = "darkgreen",
#                     data = WW)

plt3 = plot.lpcah(out)
ggsave("~/surfdrive/logitMDA/lrrr/paper/drugs_h.pdf", plt3, width = 15, height = 15, units = "cm")
ggsave("~/surfdrive/logitMDA/lrrr/paper/drugs_h.pdf", plt3, limitsize = FALSE)

pdf("~/surfdrive/logitMDA/lrrr/paper/drugs_h.pdf")
print(plt3)
dev.off()

OR = matrix(NA, 11,11)
for(t in 1:10){
  for(s in (t+1):11){
    ct  = table(Y[, t], Y[, s])
    OR[s, t] = OR[t, s] = (ct[1,1] * ct[2,2])/(ct[1,2] * ct[2,1])
  }
}
COS = matrix(NA, 11, 11)
for(t in 1:10){
  for(s in (t+1):11){
    COS[s, t] = COS[t, s] = (out$V[t, ] %*% out$V[s, ])/(sqrt(out$V[t, ] %*% out$V[t, ]) * sqrt(out$V[s, ] %*% out$V[s, ]))
  }
}
rownames(OR) = colnames(OR) = colnames(Y)
rownames(COS) = colnames(COS) = colnames(Y)
OR
COS
plot(OR, COS, type = "p")
plot(log(OR), log(COS), type = "p")


library(VGAM)
df = as.data.frame(cbind(Y, X))
vgam.out = rrvglm(cbind(Am, Be, Ca, Co, Ex, Ke, Le, LSD, Me, Mu, Ni) ~ 
                    age + gender + N + E + O + A + C  + SS,
                  binomialff(multiple.responses = TRUE), data = df, Rank = 2)

################################################################################################
library(microbenchmark)
mb <- microbenchmark(
  "rrvglm" = rrvglm(cbind(Am, Be, Ca, Co, Ex, Ke, Le, LSD, Me, Mu, Ni) ~ 
                      age + gender + N + E + O + A + C + SS,
                    binomialff(multiple.responses = TRUE), data = df, Rank = 2),
  "lpca" = lpca(Y = Y, X = X[ , -8],  dcrit = 1e-7),
  unit="s",
  times=10); mb
