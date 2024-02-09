setCompTypes <- function(sys, types) {
  # Check we have a system
  assertSystem(sys)
  # Confirm types is a list
  if(typeof(types)!="list") {
    stop("Types must be specified as a named list matching types to component numbers, eg list(T1 = c(1,3,5), T2 = c(2,4))")
  }
  # Check every component appears only once
  if(length(unlist(types)) != length(unique(unlist(types)))) {
    stop("a component cannot have two types")
  }

  V(sys)$compType <- NA # This just creates the attribute compType
  for(i in 1:length(types)) {
    V(sys)$compType[match(types[[i]], V(sys)$name)] <- names(types)[i]
  }
  if(sum(is.na(V(sys)$compType)) != 2) {
    stop("some components have not been given a type")
  }
  sys
}
