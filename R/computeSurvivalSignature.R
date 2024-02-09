# Function to compute the survival signature of an arbitrary system
computeSystemSurvivalSignature <- function(sys, cutsets = NULL, frac = FALSE) {
  # Check we have a system
  assertSystem(sys, require_types = TRUE)

  # Define the component type of s,t nodes as unknown
  V(sys)$compType[match(c("s","t"), V(sys)$name)] <- NA
  # And check that otherwise the list is complete
  if(length(V(sys)$name[-match(c("s","t"), V(sys)$name)]) != length(na.omit(V(sys)$compType[-match(c("s","t"), V(sys)$name)]))) {
    stop("Some component types are missing in 'compType' vertex attribute.")
  }

  # Build component type to vertex mapping list
  CbyType <- list()
  for(type in unique(na.omit(V(sys)$compType))) {
    CbyType[[type]] <- as.integer(V(sys)[which(V(sys)$compType==type)])
  }
  if(length(names(CbyType)) > 1)
    CbyType <- CbyType[order(names(CbyType))]

  # Handy stuff to know
  numC <- length(na.omit(unique(V(sys)$compType))) # number of types of component
  Cnums <- table(na.omit(V(sys)$compType)) # number of each type of component
  totalC <- sum(Cnums) # total number of components

  # Construct the survival signature table
  survSig <- matrix(0, nrow=prod(Cnums+1), ncol=numC+1)
  tmpT <- 1
  tmpE <- prod(Cnums+1)
  for(i in 1:numC) {
    tmpE <- tmpE/(Cnums[i]+1)
    survSig[,i] <- rep(0:Cnums[i], times=tmpT, each=tmpE)
    tmpT <- tmpT*(Cnums[i]+1)
  }
  #survSig <- as.data.frame(survSig)

  # Now start finding the survival signature
  if(is.null(cutsets)) {
    cutsets <- minimalVertexPairCutSets(sys, "s", "t")
  }

  # Check all states
  comp <- as.integer(V(sys))[-match(c("s","t"), V(sys)$name)]
  for(x in 0:{2^totalC-1}) {
    state <- digitsBase(x, base=2, ndigits=totalC)
    failed <- comp[state==0]
    working <- comp[state==1]
    if( sum(vapply(cutsets, function(x) { prod(x %in% failed) == 1 }, TRUE)) == 0 ) { # TRUE only if the system is working
      #numWorkingByType <- sapply(CbyType, function(x) { sum(working%in%x) })
      #print(which(apply(survSig[,1:numC], 1, identical, y=as.double(vapply(CbyType, function(x) { sum(working%in%x) }, 1)))))
      survSig[which(apply(survSig[,1:numC,drop=FALSE], 1, identical, y=as.double(vapply(CbyType, function(x) { sum(working%in%x) }, 1)))),numC+1] <- survSig[which(apply(survSig[,1:numC,drop=FALSE], 1, identical, y=as.double(vapply(CbyType, function(x) { sum(working%in%x) }, 1)))),numC+1]+1
    }
  }

  # Normalise
  survSig <- as.data.frame(survSig)
  if(is.null(names(CbyType))) {
    names(survSig) <- c(1:numC, "Probability")
  } else {
    names(survSig) <- c(names(CbyType), "Probability")
  }
  if(!frac) {
    survSig[,numC+1] <- survSig[,numC+1]/apply(survSig[,-(numC+1),drop=FALSE], 1, function(x) { prod(choose(Cnums, x)) })
  } else {
    survSig[,numC+1] <- apply(survSig, 1, function(x) {
      cd <- gcd(x[numC+1], prod(choose(Cnums, x[-(numC+1)])))
      num <- x[numC+1]/cd
      denom <- prod(choose(Cnums, x[-(numC+1)]))/cd
      if(num == 0) { return("0") }
      if(num == denom) { return("1") }
      paste(x[numC+1]/cd, "/", prod(choose(Cnums, x[-(numC+1)]))/cd, sep="") })
  }
  survSig
}
