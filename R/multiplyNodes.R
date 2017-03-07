##############################################################################
#' multiplyNodes
#' Works out the cumulative effect of linear scalars on branches per iteration
#' @param scales A vector of scalars for a node
#' @param name The name of the node
#' @param tree The time tree
#' @param Node_effects A list, one element per node, to fill with the cumulative scalars
#' @name multiplyNodes
#' @keywords internal
multiplyNodes <- function(scales, name, tree, Node_effects) {
  # get descendents
  descs <- c(getDescs(tree, name), as.numeric(name))
  .tmp <- lapply(Node_effects[as.character(descs)], function(x) x * scales)
  return(.tmp)
}
