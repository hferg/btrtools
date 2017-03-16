#' plotPhylo
#'
#' A silly little function that makes plotting a tree without tip labels and with
#' an axis easier and quicker.
#' @param tree An object of class phylo
#' @param tips Logical - show tip labels or not?
#' @param nodes TRUE shows all node labels, FALSE supresses node labels (default), or a vecotr of which nodes to highlight.
#' @param nodecex Scaling factor for node labels.
#' @param scale Logical - show the scale bar, or not?
#' @param ... Generic plot arguments (edge.width, edge.cols etc.)
#' @name plotPhylo
#' @export

plotPhylo <- function(tree, tips = FALSE, nodes = NULL, nodecex = NULL, scale = TRUE, ...) {
  plot(tree, show.tip.label = tips, ...)
  if (!is.null(nodes)) {
    if (is.numeric(nodes)) {
      nodelabels(node = nodes, cex = cex)
    } else if (nodes == FALSE) {}
    else {
      nodelabels(cex = nodecex)
    }
  }
  if (scale) {
    axisPhylo()
  }
}
