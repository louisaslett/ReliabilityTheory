nonParBayesSystemInference <- function(at.times, survival.signature, test.data, alpha=1, beta=1) {
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
  if( is.vector(alpha) && is.vector(beta) ) {
    # If the prior is identical for all components it will be a vector
    # If constant over time too, a one element vector
    if( length(alpha)!=length(beta) ) {
      stop("alpha and beta prior parameter vectors must be the same length")
    }
    if( length(alpha)!=1 && length(alpha)!=length(at.times) ) {
      stop("prior parameter vectors, alpha and beta, must be either length 1 (for time homogeneous prior) or of the same length as at.times argument (where the prior parameters at time at.times[i] are now alpha[i] and beta[i])")
    }
    # Now reform the one or more element vector into the data frame format
    # Repetition is somewhat wasteful of memory, but unless ludicrously high
    # time resolution probably not noticably so and will make the code much
    # simpler
    alpha <- as.data.frame(matrix(rep(alpha, K), nrow=length(at.times), ncol=K, byrow=FALSE))
    names(alpha) <- names(survival.signature[!prob.col])
    beta <- as.data.frame(matrix(rep(beta, K), nrow=length(at.times), ncol=K, byrow=FALSE))
    names(beta) <- names(survival.signature[!prob.col])
  } else if( is.data.frame(alpha) && is.data.frame(beta) ) {
    # If the prior is (possibly) different for all components it will be a data frame
    # If constant over time, a one row data frame
    if( ncol(alpha) != K || ncol(beta) != K ) {
      stop("alpha and beta must have priors for the same number of components as specified by the survival signature")
    }
    if( nrow(alpha) != nrow(beta) ) {
      stop("alpha and beta must have matching size (they differ in number of rows)")
    }
    if( !all(sort(names(alpha)) == sort(names(survival.signature)[!prob.col])) || !all(sort(names(beta)) == sort(names(survival.signature)[!prob.col])) ) {
      stop("component names in survival.signature and alpha/beta prior lists must match exactly")
    }
    if( nrow(alpha)!=1 && nrow(alpha)!=length(at.times) ) {
      stop("prior parameter vectors, alpha and beta, must be either length 1 (for time homogeneous prior) or of the same length as at.times argument (where the prior parameters at time at.times[i] are now alpha[i,j] and beta[i,j]).")
    }
    if( nrow(beta)!=1 && nrow(beta)!=length(at.times) ) {
      stop("prior parameter vectors, alpha and beta, must be either length 1 (for time homogeneous prior) or of the same length as at.times argument (where the prior parameters at time at.times[i] are now alpha[i,j] and beta[i,j]).")
    }
    # Now reform format.  Here all we need to do is:
    # i) rearrange the columns to match the survival signature ordering
    alpha <- alpha[,names(survival.signature)[!prob.col]]
    beta <- beta[,names(survival.signature)[!prob.col]]
    # and ii) grow the row dimension if it is 1
    if( nrow(alpha)==1 ) {
      alpha <- as.data.frame(matrix(alpha, nrow=length(at.times), ncol=K, byrow=TRUE))
      names(alpha) <- names(survival.signature)[!prob.col]
      beta <- as.data.frame(matrix(beta, nrow=length(at.times), ncol=K, byrow=TRUE))
      names(beta) <- names(survival.signature)[!prob.col]
    }
  } else {
    stop("alpha and beta arguments must be either a vector or data frame and must match in type")
  }

  sapply(1:length(at.times), function(i, at.times, alpha, beta, sig, prob.col, test.data, m, n) {
      t <- at.times[i]
      alpha <- unlist(alpha[i,])
      beta <- unlist(beta[i,])
      s <- sapply(test.data, function(t_i, t) { sum(t_i>t) }, t=t)
      sum(apply(sig, 1, function(sigvec, prob.col, s, m, alpha, beta) {
          l <- sigvec[!prob.col]
          sig <- sigvec[prob.col]

          sig * prod(choose(m,l) * beta(l+alpha+s, m-l+beta+n-s) / beta(alpha+s, beta+n-s))
        }, prob.col=prob.col, s=s, m=m, alpha=alpha, beta=beta))
    }, at.times=at.times, alpha=alpha, beta=beta, sig=survival.signature, prob.col=prob.col, test.data=test.data, m=apply(survival.signature[,-length(survival.signature),drop=FALSE], 2, max), n=sapply(test.data, length))
}
