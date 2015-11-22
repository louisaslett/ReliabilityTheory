nonParBayesSystemInferencePriorSets <- function(at.times, survival.signature, test.data, nLower=2, nUpper=2, yLower=0.5, yUpper=0.5) {
  # Sanity checks
  K <- ncol(survival.signature)-1 # number of types of component
  if( any(at.times<0) ) {
    stop("all at.times must be non-negative")
  }
  if( !any(prob.col <- (names(survival.signature)=="Probability")) ) {
    stop("survival signature must contain a variable named 'Probability'")
  }
  if( !is.list(test.data) || length(test.data) != K ) {
    stop("test.data must be a named list containing the same number of components as specified by the survival signature")
  }
  if( !all(sort(names(test.data)) == sort(names(survival.signature)[!prob.col])) ) {
    stop("component names in survival.signature and test.data must match exactly")
  }
  if( is.vector(nLower) && is.vector(nUpper) && is.vector(yLower) && is.vector(yUpper) ) {
    # If the prior is identical for all components it will be a vector
    # If constant over time too, a one element vector
    if( !all.equal(length(nLower), length(nUpper), length(yLower), length(yUpper)) ) {
      stop("nLower, nUpper, yLower and yUpper prior parameter vectors must be the same length")
    }
    if( length(nLower)!=1 && length(nLower)!=length(at.times) ) {
      stop("prior parameter vectors, nLower, nUpper, yLower and yUpper, must be either length 1 (for time homogeneous prior) or of the same length as at.times argument (where the prior parameters at time at.times[i] are now nLower[i], nUpper[i], yLower[i] and yUpper[i])")
    }
    # Now reform the one or more element vector into the data frame format
    # Repetition is somewhat wasteful of memory, but unless ludicrously high
    # time resolution probably not noticably so and will make the code much
    # simpler
    nLower <- as.data.frame(matrix(rep(nLower, K), nrow=length(at.times), ncol=K, byrow=FALSE))
    names(nLower) <- names(survival.signature[!prob.col])
    nUpper <- as.data.frame(matrix(rep(nUpper, K), nrow=length(at.times), ncol=K, byrow=FALSE))
    names(nUpper) <- names(survival.signature[!prob.col])
    yLower <- as.data.frame(matrix(rep(yLower, K), nrow=length(at.times), ncol=K, byrow=FALSE))
    names(yLower) <- names(survival.signature[!prob.col])
    yUpper <- as.data.frame(matrix(rep(yUpper, K), nrow=length(at.times), ncol=K, byrow=FALSE))
    names(yUpper) <- names(survival.signature[!prob.col])
  } else if( is.data.frame(nLower) && is.data.frame(nUpper) && is.data.frame(yLower) && is.data.frame(yUpper) ) {
    # If the prior is (possibly) different for all components it will be a data frame
    # If constant over time, a one row data frame
    if( ncol(nLower) != K || ncol(nUpper) != K || ncol(yLower) != K || ncol(yUpper) != K ) {
      stop("nLower, nUpper, yLower and yUpper must have priors for the same number of components as specified by the survival signature")
    }
    if( !all.equal(nrow(nLower), nrow(nUpper), nrow(yLower), nrow(yUpper)) ) {
      stop("nLower, nUpper, yLower and yUpper must have matching size (they differ in number of rows)")
    }
    if( !all(sort(names(nLower)) == sort(names(survival.signature)[!prob.col])) ||
        !all(sort(names(nUpper)) == sort(names(survival.signature)[!prob.col])) ||
        !all(sort(names(yLower)) == sort(names(survival.signature)[!prob.col])) ||
        !all(sort(names(yUpper)) == sort(names(survival.signature)[!prob.col])) ) {
      stop("component names in survival.signature and nLower, nUpper, yLower and yUpper prior lists must match exactly")
    }
    if( nrow(nLower)!=1 && nrow(nLower)!=length(at.times) ) {
      stop("prior parameter vectors, nLower, nUpper, yLower and yUpper, must be either length 1 (for time homogeneous prior) or of the same length as at.times argument (where the prior parameters at time at.times[i] are now nLower[i,j], nUpper[i,j], yLower[i,j] and yUpper[i,j]).")
    }
    # Now reform format.  Here all we need to do is:
    # i) rearrange the columns to match the survival signature ordering
    nLower <- nLower[,names(survival.signature)[!prob.col]]
    nUpper <- nUpper[,names(survival.signature)[!prob.col]]
    yLower <- yLower[,names(survival.signature)[!prob.col]]
    yUpper <- yUpper[,names(survival.signature)[!prob.col]]
    # and ii) grow the row dimension if it is 1
    if( nrow(nLower)==1 ) {
      nLower <- as.data.frame(matrix(nLower, nrow=length(at.times), ncol=K, byrow=TRUE))
      names(nLower) <- names(survival.signature)[!prob.col]
      nUpper <- as.data.frame(matrix(nUpper, nrow=length(at.times), ncol=K, byrow=TRUE))
      names(nUpper) <- names(survival.signature)[!prob.col]
      yLower <- as.data.frame(matrix(yLower, nrow=length(at.times), ncol=K, byrow=TRUE))
      names(yLower) <- names(survival.signature)[!prob.col]
      yUpper <- as.data.frame(matrix(yUpper, nrow=length(at.times), ncol=K, byrow=TRUE))
      names(yUpper) <- names(survival.signature)[!prob.col]
    }
  } else {
    stop("nLower, nUpper, yLower and yUpper arguments must be either a vector or data frame and must match in type")
  }

  # Go through the times from smallest to biggest so that we have the best
  # possible ordering for priors
  nCur <- nUpper[order(at.times)[1],,drop=TRUE]
  yCur <- yLower[order(at.times)[1],,drop=TRUE]
  pLower <- sapply(order(at.times), function(i, at.times, nLower, nUpper, yLower, yUpper, sig, prob.col, test.data, m, N, K) {
    t <- at.times[i]
    nLower <- unlist(nLower[i,])
    nUpper <- unlist(nUpper[i,])
    nCur <- pmax(pmin(nCur, nUpper), nLower)
    yLower <- unlist(yLower[i,])
    yUpper <- unlist(yUpper[i,])
    yCur <- pmax(pmin(yCur, yUpper), yLower)
    s <- sapply(test.data, function(t_i, t) { sum(t_i>t) }, t=t)
    res <- optim(c(nCur, yCur), function(pri, sig, prob.col, s, m, N, K) {
      n <- pri[1:K]
      y <- pri[-(1:K)]
      sum(apply(sig, 1, function(sigvec, prob.col, s, m, N, n, y) {
        l <- sigvec[!prob.col]
        sig <- sigvec[prob.col]

        sig * prod(choose(m,l) * beta(l+n*y+s, m-l+n*(1-y)+N-s) / beta(n*y+s, n*(1-y)+N-s))
      }, prob.col=prob.col, s=s, m=m, N=N, n=n, y=y))
    }, method="L-BFGS-B", lower=c(nLower, yLower), upper=c(nUpper, yUpper), sig=sig, prob.col=prob.col, s=s, m=m, N=N, K=K)
    nCur <<- res$par[1:K]
    yCur <<- res$par[-(1:K)]
    res$value
  }, at.times=at.times, nLower=nLower, nUpper=nUpper, yLower=yLower, yUpper=yUpper, sig=survival.signature, prob.col=prob.col, test.data=test.data, m=apply(survival.signature[,-length(survival.signature),drop=FALSE], 2, max), N=sapply(test.data, length), K=K)[rank(at.times)]

  nCur <- nLower[order(at.times)[1],,drop=TRUE]
  yCur <- yUpper[order(at.times)[1],,drop=TRUE]
  pUpper <- sapply(order(at.times), function(i, at.times, nLower, nUpper, yLower, yUpper, sig, prob.col, test.data, m, N, K) {
    t <- at.times[i]
    nLower <- unlist(nLower[i,])
    nUpper <- unlist(nUpper[i,])
    nCur <- pmax(pmin(nCur, nUpper), nLower)
    yLower <- unlist(yLower[i,])
    yUpper <- unlist(yUpper[i,])
    yCur <- pmax(pmin(yCur, yUpper), yLower)
    s <- sapply(test.data, function(t_i, t) { sum(t_i>t) }, t=t)
    res <- optim(c(nCur, yCur), function(pri, sig, prob.col, s, m, N, K) {
      n <- pri[1:K]
      y <- pri[-(1:K)]
      -sum(apply(sig, 1, function(sigvec, prob.col, s, m, N, n, y) {
        l <- sigvec[!prob.col]
        sig <- sigvec[prob.col]

        sig * prod(choose(m,l) * beta(l+n*y+s, m-l+n*(1-y)+N-s) / beta(n*y+s, n*(1-y)+N-s))
      }, prob.col=prob.col, s=s, m=m, N=N, n=n, y=y))
    }, method="L-BFGS-B", lower=c(nLower, yLower), upper=c(nUpper, yUpper), sig=sig, prob.col=prob.col, s=s, m=m, N=N, K=K)
    nCur <<- res$par[1:K]
    yCur <<- res$par[-(1:K)]
    -res$value
  }, at.times=at.times, nLower=nLower, nUpper=nUpper, yLower=yLower, yUpper=yUpper, sig=survival.signature, prob.col=prob.col, test.data=test.data, m=apply(survival.signature[,-length(survival.signature),drop=FALSE], 2, max), N=sapply(test.data, length), K=K)[rank(at.times)]
  list(lower=pLower, upper=pUpper)
}
