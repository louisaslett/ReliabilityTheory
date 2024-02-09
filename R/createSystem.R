# Create a system from a graph, optionally specifying component types at
# creation time
createSystem <- function(..., types = NULL) {
  args <- as.list(match.call())[-1]
  if(length(which(names(args) == "types")) > 0) {
    args <- args[-which(names(args) == "types")]
  }
  res <- do.call("graph_from_literal", args)
  class(res) <- c("system", class(res))

  if(sum(is.na(match(c("s","t"), V(res)$name)))) {
    stop("The system design must contain vertices named 's' and 't' to indicate the start and terminal points of the system being specified.")
  }

  if(!is.null(types)) {
    res <- setCompTypes(res, types)
  } else {
    res <- setCompTypes(res, list("T1" = setdiff(V(res)$name, c("s", "t"))))
  }

  return(res)
}
