################################################################################################
################################################################################################
# DATA
################################################################################################
################################################################################################
rm(list=ls())
library(foreign)
library(dplyr)
setwd("~/surfdrive/Shared/Melodic - friso/SHARE data and analyses/SHARE spss files")

ph1 = read.spss("sharew1_rel7-1-0_ALL_datasets_spss/sharew1_rel7-1-0_ph.sav", to.data.frame = TRUE, use.value.labels = F)
mh1 = read.spss("sharew1_rel7-1-0_ALL_datasets_spss/sharew1_rel7-1-0_mh.sav", to.data.frame = TRUE, use.value.labels = F)
dn1 = read.spss("sharew1_rel7-1-0_ALL_datasets_spss/sharew1_rel7-1-0_dn.sav", to.data.frame = TRUE, use.value.labels = F)
br1 = read.spss("sharew1_rel7-1-0_ALL_datasets_spss/sharew1_rel7-1-0_br.sav", to.data.frame = TRUE, use.value.labels = F)
cf1 = read.spss("sharew1_rel7-1-0_ALL_datasets_spss/sharew1_rel7-1-0_cf.sav", to.data.frame = TRUE, use.value.labels = F)
gv_health1 = read.spss("sharew1_rel7-1-0_ALL_datasets_spss/sharew1_rel7-1-0_gv_health.sav", to.data.frame = TRUE, use.value.labels = F)
gv_isced1 = read.spss("sharew1_rel7-1-0_ALL_datasets_spss/sharew1_rel7-1-0_gv_isced.sav", to.data.frame = TRUE, use.value.labels = F)
W1 <- data.frame(cbind(ph1$mergeid, ph1$country, dn1$dn042_, dn1$dn003_, dn1$dn014_, gv_isced1$isced1997_r,
                       br1$br001_, br1$br015_, gv_health1$sphus, cf1$cf010_, cf1$cf016tot, cf1$cf008tot,
                       ph1$ph049d6, ph1$ph049d3, ph1$ph049d1, ph1$ph049d4, ph1$ph049d2, ph1$ph049d5,
                       ph1$ph006d5, ph1$ph006d2, ph1$ph006d7, ph1$ph006d6, ph1$ph006d8, ph1$ph006d1,
                       ph1$ph006d4, mh1$mh002_))
colnames(W1) <- c("ID", "Country", "Sex", "Age", "Marital_status", "Highest_edu",
                  "Tobacco", "Phys_act", "SR_health", "Fluency", "Del_recall", "Imm_recall",
                  "Toilet", "Bathing_washing", "Dressing", "Eating", "Walking", "Get_out_bed",
                  "Diabetes", "Hypertension", "Asthma", "Chron_Lung_D", "Joint_disorders",
                  "Angina", "Stroke", "Depression")
W1[W1==-1] <- NA
W1[W1==-2] <- NA
W1$Highest_edu[W1$Highest_edu>6] <- NA
W1$Depression[W1$Depression==5] <- 0
W1$Tobacco[W1$Tobacco==5] <- 0
### Assigning to the variables to the right classes.
W1[,c(2,3,5:7,13:26)] <- lapply(W1[,c(2,3,5:7,13:26)], as.factor)
W1[,c(4,8:12)] <- lapply(W1[,c(4,8:12)], as.numeric)
### Converting the 'year of birth variable' to an 'Age' variable.
W1$Age <- 2004 - W1$Age
mis_col <- matrix(-9999, ncol(W1), 2)
for (i in 1:ncol(W1)){
  mis_col[i,1] <- sum(is.na(W1[,i]))
  mis_col[i,2] <- sum(is.na(W1[,i]))/nrow(W1)
}
### Creating the same matrix as above, only now the missing variable 'Loneliness'
### included, and at the bottom the total amount of missingness for the W1.
### This matrix is mergeable with the other waves.
vec <- c(13) ; na_col <- matrix(0, 27, 2) ; na_col[-vec,] <- mis_col
rownames(mis_col[c(1:9),]) <- colnames(W1[,c(1:9)])
colnames(mis_col) <- colnames(na_col) <- c("Amount_NA", "Prop_NA")
mis <- sum(is.na(W1))
prop_mis <- mis/(nrow(W1)*ncol(W1))
na_col <- rbind(na_col, c(mis, prop_mis))
na_col[,2] <- round(na_col[,2], digits = 3)

### Determining how many NA will be deleted
before <- nrow(W1)
W1 <- W1[complete.cases(W1),]
after <- nrow(W1)

### Creating a matrix in which is summarized how many cases are removed (and proprotion)
exc <- matrix(-9999, 1, 4)
exc[1,1] <- before; exc[1,2] <- after
exc[1,3]<- before-after; exc[1,4] <- round((before-after) / before, 3)
colnames(exc) <- c("n_total", "n_remained", "n_excluded", "prop_excluded")
rownames(exc) <- "W1"
X <- W1[,2:18]
X$Country <- relevel(X$Country, ref="14")
X[,-c(1,4,5)] <- lapply(X[,-c(1,4,5)], as.numeric)
X <- model.matrix(~., data=X)
X <- X[,-1]
Y = W1[,19:26]
Y[,1:8] <- lapply(Y[,1:8], as.numeric)
Y <- as.matrix(Y)
Y <- Y-1
Y = Y[, -3] # asthma eriut
country = W1$Country

# give new names to variables

# country = recode(country, 
#        "11" = "Austria", 
#        "12" = "Germany",
#        "13" = "Sweden",
#        "14" = "Netherlands",
#        "15" = "Spain",
#        "16" = "Italy",
#        "17" = "France",
#        "18" = "Denmark",
#        "19" = "Greece",
#        "20" = "Switzerland",
#        "23" = "Belgium",
#        "25" = "Israel")

country = recode(country, 
                 "11" = "AT", 
                 "12" = "DE",
                 "13" = "SE",
                 "14" = "NL",
                 "15" = "ES",
                 "16" = "IT",
                 "17" = "FR",
                 "18" = "DK",
                 "19" = "EL",
                 "20" = "CH",
                 "23" = "BE",
                 "25" = "IL")

colnames(X) = c(
  "Austria",
  "Germany",
  "Sweden",
  "Spain",
  "Italy",
  "France",
  "Denmark",
  "Greece",
  "Switzerland",
  "Belgium",
  "Israel",
  "Female",
  "Age",
  "MS2",
  "MS3",
  "MS4",
  "MS5",
  "MS6",
  "HE1",
  "HE2",
  "HE3",
  "HE4",
  "HE5",
  "HE6",
  "Tobacco",
  "PA",
  "SRH",
  "Fluency",
  "Drecall",
  "Irecall",
  "Toilet",
  "Bathing",
  "Dressing",
  "Eating",
  "Walking",
  "Gettingup"
)
X[ , c(12, 25, 31, 32, 33, 34, 35, 36)] = X[ , c(12, 25, 31, 32, 33, 34, 35, 36)] - 1  
colnames(Y) = c("Di", "H", "CL", "JD", "An", "S", "De")

XX = X[, -c(19:24, 29:31, 34:36)] # friso's selection
XX = X[ , -c(19:24, 28:30, 31:36)] # 
XX[ , c(13, 20, 21)] = scale(XX[ , c(13, 20, 21)]) # scale the continuous variables
X = XX[, 1:13] # only country, gender, and age 
gender = X[, 12]
gender = recode(gender, 
                 "0" = "M", 
                 "1" = "F")
rm(list= ls()[!(ls() %in% c('X','Y','country', 'gender'))])

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
    # X = as.matrix(scale(X, center = TRUE, scale = TRUE))
    # mx = attr(X, "scaled:center")
    # sdx = attr(X, "scaled:scale")
    # 
    mx = rep(0, ncol(X)) 
    sdx = rep(1, ncol(X))
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
predict.lpca = function(object, newX){
  B = object$B
  V = object$V
  X = scale(newX, center = object$mx, scale = object$sdx)
  logform = matrix(out$m, nrow(X), ncol(object$Y), byrow = TRUE) + X %*% B %*% t(V)
  P = 1/(1 + exp(-logform))
  return(P)
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
plot.lpca1 <- function(object, dims = c(1,2), ycol = "darkgreen", xcol = "lightskyblue", ocol = "grey", oshape = NULL){
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
      # m.x2 = pretty(Xo[, p])
      if(p %in% c(13, 20, 21)){
        m.x2 = pretty(Xo[, p])
        m.x2 = m.x2[which(m.x2 > minx & m.x2 < maxx)]
        l.m = length(m.x2)
        markers2 = matrix((m.x2 - object$mx[p])/object$sdx[p], l.m, 1)
        markerscoord2 = outer(markers2, b) # markers2 %*% t(b %*% solve(t(b) %*% b))
        MCx2[(lll + 1): (lll + l.m), 1] = paste(m.x2)
        MCx2[(lll + 1): (lll + l.m), 2] = p
        MCx2[(lll + 1): (lll + l.m), 3:4] = markerscoord2
        lll = lll + l.m  
      } 
    } # loop p
  } #isx
  
  ######################################################
  # plotting - objects
  ######################################################
  plt = ggplot() +
    geom_point(data = U, aes_string(x = 'dim1', y = 'dim2', colour = factor(ocol), shape = factor(oshape)), 
               show.legend = T, alpha = 0.5, size = 0.5) +
    xlab(paste("Dimension", dims[1])) +
    ylab(paste("Dimension", dims[2])) + 
    labs(title = "Type I triplot")
  
  a = max(abs(c(ggplot_build(plt)$layout$panel_scales_x[[1]]$range$range, ggplot_build(plt)$layout$panel_scales_y[[1]]$range$range))) + 0.1
  ax = ggplot_build(plt)$layout$panel_scales_x[[1]]$range$range
  ay = ggplot_build(plt)$layout$panel_scales_y[[1]]$range$range  
  ######################################################
  # variable axes with ticks and markers for predictors
  ######################################################
  if(isx){
    plt = plt + geom_abline(intercept = 0, slope = B[,2]/B[,1], colour = xcol, linetype = 3) +
      geom_line(data = MCx1, aes_string(x = 'dim1', y = 'dim2', group = 'varx'), col = xcol, size = 1.5) +
      geom_point(data = MCx2, aes_string(x = 'dim1', y = 'dim2'), col = xcol) +
      geom_text(data = MCx2, aes_string(x = 'dim1', y = 'dim2', label = 'labs'), nudge_y = -0.10, size = 3.5)
  }
  
  ######################################################
  # variable axes with ticks and markers for responses
  ######################################################
  plt = plt + geom_abline(intercept = 0, slope = V[,2]/V[,1], colour = ycol) +
    geom_point(data = MCy, aes_string(x = 'dim1', y = 'dim2'), shape = 15, colour = ycol) +
    geom_text(data = MCy, aes_string(x = 'dim1', y = 'dim2', label = 'labs'), nudge_y = -0.10, size = 2.5)
  
  ######################################################
  # variable labels
  ######################################################
  # a = ceiling(max(abs(c(ggplot_build(plt)$layout$panel_scales_x[[1]]$range$range, ggplot_build(plt)$layout$panel_scales_y[[1]]$range$range))))
  # a = max(abs(c(ggplot_build(plt)$layout$panel_scales_x[[1]]$range$range, ggplot_build(plt)$layout$panel_scales_y[[1]]$range$range))) + 0.1
  
  idx2 = apply(abs(V), 1, which.max)
  t2 = s2 = rep(NA,R)
  for(rr in 1:R){
    # define a based on ax and ay / see line 531 532
    t2[rr] = (a * 1.1)/(abs(V[rr,idx2[rr]])) * V[rr,-idx2[rr]]
    s2[rr] = sign(V[rr,idx2[rr]])
  }
  CC2 = cbind(idx2, t2, s2)
  
  if(isx){
    idx1 = apply(abs(B), 1, which.max)
    t1 = s1 = rep(NA,P)
    for(pp in 1:P){
      # define a based on ax and ay / see line 531 532
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
plot.lpca2 <- function(object, dims = c(1,2), ycol = "darkgreen", xcol = "lightskyblue", ocol = "grey", oshape = NULL){
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
  # plt = ggplot() +
  #   geom_point(data = U, aes_string(x = 'dim1', y = 'dim2'), colour = ocol) +
  #   # geom_point(data = U, aes(x = dim1, y = dim2), colour = ocol, shape = ".") +
  #   geom_point(data = VV, aes_string(x = 'dim1', y = 'dim2'), colour = ycol) +
  #   geom_text_repel(data = VV, aes_string(x = 'dim1', y = 'dim2', label = 'rownames(VV)'), family = "mono") +
  #   #geom_text(data = VV, aes(x = dim1, y = dim2, label = rownames(VV)), family = "mono") +
  #   xlab(paste("Dimension", dims[1])) +
  #   ylab(paste("Dimension", dims[2]))

  plt = ggplot() +
    geom_point(data = U, aes_string(x = 'dim1', y = 'dim2', colour = factor(ocol), shape = factor(oshape)), 
               show.legend = T, alpha = 0.5, size = 0.5) +
    geom_point(data = VV, aes_string(x = 'dim1', y = 'dim2'), colour = ycol) +
    geom_text_repel(data = VV, aes_string(x = 'dim1', y = 'dim2', label = 'rownames(VV)'), family = "mono") +
    xlab(paste("Dimension", dims[1])) +
    ylab(paste("Dimension", dims[2]))  + 
    labs(title = "Type D triplot")
  
  ######################################################
  # variable axes with ticks and markers for predictors
  ######################################################
  if(isx){
    plt = plt + geom_abline(intercept = 0, slope = B[,2]/B[,1], colour = xcol, linetype = 3) +
      geom_line(data = MCx1, aes_string(x = 'dim1', y = 'dim2', group = 'varx'), col = xcol, size = 1.5) +
      geom_point(data = MCx2, aes_string(x = 'dim1', y = 'dim2'), col = xcol) +
      geom_text(data = MCx2, aes_string(x = 'dim1', y = 'dim2', label = 'labs'), nudge_y = -0.08, size = 1.5)
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

plot.lpcah <- function(object, dims = c(1,2), ycol = "darkgreen", xcol = "lightskyblue", ocol = "grey", oshape = NULL){
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
  object2 = lpca2dist(object)
  Y = object$Y
  
  U = as.data.frame(object$U[ , dims])
  N = nrow(U)
  colnames(U) = c("dim1", "dim2")
  
  V = object$V[ , dims]
  VV = as.data.frame(V)
  R = nrow(V)
  colnames(VV) = c("dim1", "dim2")
  rownames(VV) = object$ynames
  
  W = object2$V
  WW = cbind(W[seq(1, 13, by = 2), ], W[seq(2, 14, by = 2), ])
  WW = as.data.frame(WW)
  rownames(WW) = object$ynames
  colnames(WW) = c("dim1a", "dim2a", "dim1b", "dim2b")
  
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
      # m.x2 = pretty(Xo[, p])
      if(p %in% c(13, 20, 21)){
        m.x2 = pretty(Xo[, p])
        m.x2 = m.x2[which(m.x2 > minx & m.x2 < maxx)]
        l.m = length(m.x2)
        markers2 = matrix((m.x2 - object$mx[p])/object$sdx[p], l.m, 1)
        markerscoord2 = outer(markers2, b) # markers2 %*% t(b %*% solve(t(b) %*% b))
        MCx2[(lll + 1): (lll + l.m), 1] = paste(m.x2)
        MCx2[(lll + 1): (lll + l.m), 2] = p
        MCx2[(lll + 1): (lll + l.m), 3:4] = markerscoord2
        lll = lll + l.m  
      } 
    } # loop p
  } #isx
  
  ######################################################
  # plotting - objects
  ######################################################
  plt = ggplot() +
    geom_point(data = U, aes_string(x = 'dim1', y = 'dim2', colour = factor(ocol), shape = factor(oshape)),  
               show.legend = T, alpha = 0.5, size = 0.5) +
    xlab(paste("Dimension", dims[1])) +
    ylab(paste("Dimension", dims[2])) + 
    labs(title = "Hybrid triplot")
  
  a = max(abs(c(ggplot_build(plt)$layout$panel_scales_x[[1]]$range$range, ggplot_build(plt)$layout$panel_scales_y[[1]]$range$range))) + 0.1
  ax = ggplot_build(plt)$layout$panel_scales_x[[1]]$range$range
  ay = ggplot_build(plt)$layout$panel_scales_y[[1]]$range$range  
  ######################################################
  # variable axes with ticks and markers for predictors
  ######################################################
  if(isx){
    plt = plt + geom_abline(intercept = 0, slope = B[,2]/B[,1], colour = xcol, linetype = 3) +
      geom_line(data = MCx1, aes_string(x = 'dim1', y = 'dim2', group = 'varx'), col = xcol) +
      geom_point(data = MCx2, aes_string(x = 'dim1', y = 'dim2'), col = xcol) +
      geom_text(data = MCx2, aes_string(x = 'dim1', y = 'dim2', label = 'labs'), nudge_y = -0.10, size = 3.5)
  }
  
  ######################################################
  # variable axes with ticks and markers for responses
  ######################################################
  plt = plt + geom_abline(intercept = 0, slope = V[,2]/V[,1], colour = ycol, linetype = 3) +
    geom_point(data = MCy, aes_string(x = 'dim1', y = 'dim2'), shape = 15, colour = ycol) +
    geom_text(data = MCy, aes_string(x = 'dim1', y = 'dim2', label = 'labs'), nudge_y = -0.10, size = 2.5)
  
  plt = plt + geom_segment(aes(x = dim1a, y = dim2a, xend = dim1b, yend = dim2b), colour = ycol, data = WW)
  
  ######################################################
  # variable labels
  ######################################################
  # a = ceiling(max(abs(c(ggplot_build(plt)$layout$panel_scales_x[[1]]$range$range, ggplot_build(plt)$layout$panel_scales_y[[1]]$range$range))))
  # a = max(abs(c(ggplot_build(plt)$layout$panel_scales_x[[1]]$range$range, ggplot_build(plt)$layout$panel_scales_y[[1]]$range$range))) + 0.1
  
  idx2 = apply(abs(V), 1, which.max)
  t2 = s2 = rep(NA,R)
  for(rr in 1:R){
    # define a based on ax and ay / see line 531 532
    t2[rr] = (a * 1.1)/(abs(V[rr,idx2[rr]])) * V[rr,-idx2[rr]]
    s2[rr] = sign(V[rr,idx2[rr]])
  }
  CC2 = cbind(idx2, t2, s2)
  
  if(isx){
    idx1 = apply(abs(B), 1, which.max)
    t1 = s1 = rep(NA,P)
    for(pp in 1:P){
      # define a based on ax and ay / see line 531 532
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

################################################################################################
################################################################################################
# ANALYSES
################################################################################################
################################################################################################

# exploratory
colMeans(Y)


out = lpca(Y, X, dcrit = 1e-8) # same convergence criterion as rrvglm

q.lpca(out)


plt1 = plot.lpca1(out, ocol = country, oshape = gender); 
plt1 = plot.lpca1(out, oshape = gender); 
# plt = plt  + theme(legend.position = "bottom")
# plt1 = plt1  + theme(legend.position = "bottom", 
#                    legend.direction = "horizontal", 
#                    legend.box = "vertical")
plt1 = plt1  + theme(legend.position = "none") 
plt1 = plt1 + labs(colour = "country", shape = "gender")
suppressWarnings(print(plt1))
ggsave("~/surfdrive/logitMDA/lrrr/paper/share_i.pdf", plt1, width = 15, height = 15, units = "cm")

plt2 = plot.lpca2(out, ocol = country, oshape = gender)
plt2 = plt2  + theme(legend.position = "none")
plt2 = plt2 + labs(colour = "country", shape = "gender")
suppressWarnings(print(plt2))

ggsave("~/surfdrive/logitMDA/lrrr/paper/share_d.pdf", plt2, width = 15, height = 15, units = "cm")

plt3 = plot.lpcah(out, ocol = country, oshape = gender)
plt3 = plt3  + theme(legend.position = "none")
plt3 = plt3 + labs(colour = "country", shape = "gender")
suppressWarnings(print(plt3))
ggsave("~/surfdrive/logitMDA/lrrr/paper/share_h.pdf", plt3, width = 15, height = 15, units = "cm")


#Some proportions in the data
Austria = colMeans(Y[X[, 1]== 1, ])
Germany = colMeans(Y[X[, 2]== 1, ])
Sweden = colMeans(Y[X[, 3]== 1, ])
Spain = colMeans(Y[X[, 4]== 1, ])
Italy = colMeans(Y[X[, 5]== 1, ])
France = colMeans(Y[X[, 6]== 1, ])
Denmark = colMeans(Y[X[, 7]== 1, ])
Greece = colMeans(Y[X[, 8]== 1, ])
Switzerland = colMeans(Y[X[, 9]== 1, ])
Belgium = colMeans(Y[X[, 10]== 1, ])
Israel = colMeans(Y[X[, 11]== 1, ])
Nederland = colMeans(Y[rowSums(X[, 1: 11]) == 0, ])
rbind(Austria, Germany, Sweden, Spain, Italy, France, Denmark, Greece, Switzerland, Belgium, Israel, Nederland)
FEMALE = colMeans(Y[X[, 12]== 1, ])
MALE = colMeans(Y[X[, 12]== 0, ])
rbind(MALE, FEMALE)
# 
# plt2 + xlim(c(-10,10)) + ylim(c(-10,10))
# plt2 + xlim(c(-5,5)) + ylim(c(-5,5))

################################################################################################
library(VGAM)
df = as.data.frame(cbind(Y, X[, 1:13]))
vgam.out = rrvglm(cbind(Di, H, As, CL, JD, An, S, De) ~ 
                  Austria + Germany + Sweden + Spain + Italy + France + Denmark + Greece + Switzerland + Belgium + Israel + Female + Age,
                  binomialff(multiple.responses = TRUE), data = df, Rank = 2)
#,control= rrvglm.control(trace = TRUE))

################################################################################################
library(microbenchmark)
mb <- microbenchmark(
  "rrvglm" = rrvglm(cbind(Di, H, CL, JD, An, S, De) ~ 
                      Austria + Germany + Sweden + Spain + Italy + France + Denmark + Greece + Switzerland + Belgium + Israel + Female + Age,
                    binomialff(multiple.responses = TRUE), data = df, Rank = 2),
  "lpca" = lpca(Y = Y, X = X,  dcrit = 1e-7),
  unit="s",
  times=10); mb



