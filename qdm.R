qdm <- function(o, p, s, precip, pr.threshold, n.quantiles, jitter.factor=0.01){
  
  # tau.s = F.s(x.s)
  # delta = x.s {/,-} F.p^-1(tau.s)
  # yout = F.o^-1(tau.s) {*,+} delta
  
  if (all(is.na(o)) | all(is.na(p)) | all(is.na(s))) {
    return(yout=rep(NA, length(s)))
  } else{
    
    # Apply a small amount of jitter to accomodate ties due to limited measurement precision
    o <- jitter(o, jitter.factor)
    p <- jitter(p, jitter.factor)
    s <- jitter(s, jitter.factor)
    
    # For ratio data, treat exact zeros as left censored values less than pr.threshold
    if(precip){
      epsilon <- .Machine$double.eps
      o[o < pr.threshold & !is.na(o)] <- runif(sum(o < pr.threshold, na.rm=TRUE), min=epsilon, max=pr.threshold)
      p[p < pr.threshold & !is.na(p)] <- runif(sum(p < pr.threshold, na.rm=TRUE), min=epsilon, max=pr.threshold)
      s[s < pr.threshold & !is.na(s)] <- runif(sum(s < pr.threshold, na.rm=TRUE), min=epsilon, max=pr.threshold)
    }
    
    # Calculate empirical quantiles using Weibull plotting position
    n <- max(length(o), length(p), length(s))
    if(is.null(n.quantiles)) n.quantiles <- n
    tau <- seq(1/(n+1), n/(n+1), length=n.quantiles)
    quant.o <- quantile(o, tau, type=6, na.rm=TRUE)
    quant.p <- quantile(p, tau, type=6, na.rm=TRUE)
    quant.s <- quantile(s, tau, type=6, na.rm=TRUE)
    
    # Apply QDM bias correction
    tau.s <- approx(quant.s, tau, s, rule=2)$y    
    if(precip){
      delta <- s/approx(tau, quant.p, tau.s, rule=2)$y # if rule= 2, the value at the closest data extreme is used
      yout <- approx(tau, quant.o, tau.s, rule=2)$y*delta
    } else{
      delta <- s - approx(tau, quant.p, tau.s, rule=2)$y
      yout <- approx(tau, quant.o, tau.s, rule=2)$y + delta
    }
    #yout.h <- approx(quant.p, quant.o, p, rule=2)$y
    
    # For precip data, set values less than threshold to zero
    if(precip){
      #yout.h[yout.h < pr.threshold] <- 0
      yout[yout < pr.threshold] <- 0
    }
    
    return(yout)
  }
}
