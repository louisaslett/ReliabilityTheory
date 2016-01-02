setCompTypes <- function(sys, types) {
  # Check we have a graph
  if(typeof(sys)!="list" || class(sys)!="igraph") {
    stop("the first argument must be a graph representing a system")
  }
  # Check that we have s,t nodes as a minimum
  if(sum(is.na(match(c("s","t"), V(sys)$name)))) {
    stop("The graph object must contain vertices named 's' and 't' to indicate the start and terminal points of the system being specified.")
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
