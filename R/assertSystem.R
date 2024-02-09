assertSystem <- function(sys, require_types = FALSE) {
  # Check we have a system
  if(typeof(sys)!="list" || !inherits(sys, "system")) {
    stop("the first argument must be a system (see function createSystem)")
  }
  # Check that we have s,t nodes as a minimum
  if(sum(is.na(match(c("s","t"), V(sys)$name)))) {
    stop("the system object must contain vertices named 's' and 't' to indicate the start and terminal points of the system being specified")
  }
  # Check types are set if needed
  if(require_types && is.na(match("compType", list.vertex.attributes(sys)))) {
    stop("the type of each component must be set for this function (see setCompTypes)")
  }
  return(TRUE)
}
