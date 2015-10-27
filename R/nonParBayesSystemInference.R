nonParBayesSystemInference <- function(at.times, survival.signature, test.data, alpha=1, beta=1) {
  # Sanity checks
  if( any(at.times<0) ) {
    stop("all at.times must be non-negative")
  }
  if( !any(prob.col <- (names(survival.signature)=="Probability")) ) {
    stop("survival signature must contain a variable named 'Probability'")
  }
  if( !all(sort(names(test.data)) == sort(names(survival.signature)[!prob.col])) ) {
    stop("component names in survival.signature and test.data must match exactly")
  }
  if( length(alpha)!=length(beta) ) {
    stop("alpha and beta prior parameter vectors must be the same length")
  }
  if( length(alpha)!=1 && length(alpha)!=length(at.times) ) {
    stop("prior parameter vectors, alpha and beta, must be either length 1 (for time homogeneous prior) or of the same length as at.times argument (where the prior parameters at time at.times[i] are now alpha[i] and beta[i])")
  }

  apply(cbind(at.times, alpha, beta), 1, function(t.alpha.beta, sig, td, m, n) {
      t <- t.alpha.beta[1]
      alpha <- t.alpha.beta[2]
      beta <- t.alpha.beta[3]
      s <- sapply(td, function(ti, t) { sum(ti>t) }, t=t)
      sum(apply(sig, 1, function(sigvec, s, m, alpha, beta) {
          l <- sigvec[1:(length(sigvec)-1)]
          sig <- sigvec[length(sigvec)]

          sig * prod(choose(m,l) * beta(l+alpha+s, m-l+beta+n-s) / beta(alpha+s, beta+n-s))
        }, s=s, m=m, alpha=alpha, beta=beta))
    }, sig=survival.signature, td=test.data, m=apply(survival.signature[,-length(survival.signature)], 2, max), n=sapply(test.data, length))
}
