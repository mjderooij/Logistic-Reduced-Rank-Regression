plot.lpcah <- function(object, dims = c(1,2), ycol = "darkgreen", xcol = "lightskyblue", ocol = "grey"){
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
  WW = cbind(W[seq(1,21,by = 2), ], W[seq(2, 22,by = 2), ])
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
    geom_point(data = U, aes(x = dim1, y = dim2), colour = ocol, alpha = 0.5, size = 0.5) +
    xlab(paste("Dimension", dims[1])) +
    ylab(paste("Dimension", dims[2])) + 
    labs(title = "Hybrid triplot")
  
  # a = max(abs(c(ggplot_build(plt)$layout$panel_scales_x[[1]]$range$range, ggplot_build(plt)$layout$panel_scales_y[[1]]$range$range))) + 0.1
  margins <- c("l" = ggplot_build(plt)$layout$panel_scales_x[[1]]$range$range[1] - .1,
               "r" = ggplot_build(plt)$layout$panel_scales_x[[1]]$range$range[2] + .1,
               "b" = ggplot_build(plt)$layout$panel_scales_y[[1]]$range$range[1] - .1,
               "t" = ggplot_build(plt)$layout$panel_scales_y[[1]]$range$range[2] + .1)
  
  ######################################################
  # variable axes with ticks and markers for predictors
  ######################################################
  if(isx){
    plt = plt + geom_abline(intercept = 0, slope = B[,2]/B[,1], colour = xcol, linetype = 3) +
      geom_line(data = MCx1, aes(x = dim1, y = dim2, group = varx), col = xcol) +
      geom_point(data = MCx2, aes(x = dim1, y = dim2), col = xcol, size = 1) +
      geom_text(data = MCx2, aes(x = dim1, y = dim2, label = labs), nudge_x = 0.08, nudge_y = -0.08, size = 2.5)
  }
  
  ######################################################
  # variable axes with ticks and markers for responses
  ######################################################
  plt = plt + geom_abline(intercept = 0, slope = V[,2]/V[,1], colour = ycol, linetype = 3) +
    geom_point(data = MCy, aes(x = dim1, y = dim2), size = 1, shape = 15, colour = ycol) +
    geom_text(data = MCy, aes(x = dim1, y = dim2, label = labs), nudge_y = -0.08, size = 1.5)
  
  plt = plt + geom_segment(aes(x = dim1a, y = dim2a, xend = dim1b, yend = dim2b), colour = ycol, data = WW)
  
  ######################################################
  # variable labels
  ######################################################
  
  if(isx){
    BV = rbind(B, V)
    names = c(object$xnames, object$ynames)
  } 
  else{
    BV = V
    names = object$ynames
  } 
  
  beta <- BV[,2]/BV[,1]
  
  lab <- data.frame("xname" = names, 
                    "b" = beta, 
                    "Yleft" = beta*margins["l"],
                    "Yright" = beta*margins["r"])
  
  orientation = sign(BV[,1]) #sign of dim1 defines direction l-r
  lab$side =  c("left","right")[ as.numeric(BV[,1] > 0)+1] 
  lab$side[lab$Yleft < margins["b"] & orientation<0 ] = "bottom"
  lab$side[lab$Yleft > margins["t"] & orientation<0 ] = "top"
  lab$side[lab$Yright < margins["b"]& orientation>0] = "bottom"
  lab$side[lab$Yright > margins["t"]& orientation>0] = "top"
  
  lab$X <- lab$Y <- NA
  lab$X[lab$side == "bottom"] <- (margins["b"]/beta[lab$side == "bottom"])
  lab$X[lab$side == "top"] <- (margins["t"]/beta[lab$side == "top"])
  lab$Y[lab$side == "left"] <- margins["l"]*beta[lab$side == "left"]
  lab$Y[lab$side == "right"] <-margins["r"]*beta[lab$side == "right"]
  
  lab <- split(lab, lab$side)
  
  plt = plt + 
    scale_x_continuous(breaks = lab$bottom$X, labels = lab$bottom$xname, sec.axis = sec_axis(trans ~ ., breaks = lab$top$X, labels = lab$top$xname)) +
    scale_y_continuous(breaks = lab$left$Y, labels = lab$left$xname, sec.axis = sec_axis(trans ~ ., breaks = lab$right$Y, labels = lab$right$xname))
  
  
  # ######################################################
  # # variable labels
  # ######################################################
  # 
  # idx2 = apply(abs(V), 1, which.max)
  # t2 = s2 = rep(NA,R)
  # for(rr in 1:R){
  #   t2[rr] = (a * 1.1)/(abs(V[rr,idx2[rr]])) * V[rr,-idx2[rr]]
  #   s2[rr] = sign(V[rr,idx2[rr]])
  # }
  # CC2 = cbind(idx2, t2, s2)
  # 
  # if(isx){
  #   idx1 = apply(abs(B), 1, which.max)
  #   t1 = s1 = rep(NA,P)
  #   for(pp in 1:P){
  #     t1[pp] = (a * 1.1)/(abs(B[pp,idx1[pp]])) * B[pp,-idx1[pp]]
  #     s1[pp] = sign(B[pp,idx1[pp]])
  #   }
  #   CC1 = cbind(idx1, t1, s1)
  # }
  # 
  # if(isx) CC = rbind(CC1, CC2) else CC = CC2
  # if(isx) rownames(CC) = c(object$xnames, object$ynames) else rownames(CC) = object$ynames
  # colnames(CC) = c("idx", "t", "s")
  # 
  # bottom = which(CC[, "idx"] == 2 & CC[, "s"] == -1)
  # top =  which(CC[, "idx"] == 2 & CC[, "s"] == 1)
  # right = which(CC[, "idx"] == 1 & CC[, "s"] == 1)
  # left = which(CC[, "idx"] == 1 & CC[, "s"] == -1)
  # 
  # 
  # if(length(CC[top, "t"])==0){
  #   plt <- plt + scale_x_continuous(limits = c(-a,a), breaks = CC[bottom, "t"], labels = rownames(CC)[bottom],
  #                                   sec.axis = sec_axis(trans ~ ., breaks = 0, labels = ""))
  # }else{
  #   plt <- plt + scale_x_continuous(limits = c(-a,a), breaks = CC[bottom, "t"], labels = rownames(CC)[bottom],
  #                                   sec.axis = sec_axis(trans ~ ., breaks = CC[top, "t"], labels = rownames(CC)[top]))
  # }
  # 
  # 
  # if(length(CC[right, "t"])==0){
  #   plt = plt + scale_y_continuous(limits = c(-a,a), breaks = CC[left, "t"], labels = rownames(CC)[left],
  #                                  sec.axis = sec_axis(trans ~ ., breaks = 0, labels = ""))
  # }else{
  #   plt = plt + scale_y_continuous(limits = c(-a,a), breaks = CC[left, "t"], labels = rownames(CC)[left],
  #                                  sec.axis = sec_axis(trans ~ ., breaks = CC[right, "t"], labels = rownames(CC)[right]))
  # }
  # 
  
  plt = plt + 
    coord_fixed(xlim = margins[c("l","r")], ylim = margins[c("b","t")], expand = F) +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank())
  
  suppressWarnings(print(plt))
  
  return(plt)
}