##############################################################################
#' 
#' scalarSearch
#' Searches through the posterior of an RJ continuous model for scalars and 
#' returns them.
#' @param rj_output partially processed RJ output.
#' @param counts The counts table.
#' @name scalarSearch
scalarSearch <- function(rj_output, counts, fullmrcas) {
  alltypes <- vector(mode = "list", length = nrow(rj_output))
  allmrcas <- vector(mode = "list", length = nrow(rj_output))

  rates <- matrix(rep(1, nrow(counts) * nrow(rj_output)), ncol = nrow(rj_output))
  rownames(rates) <- counts[ , "descNode"]

  # make lists for the origins of deltas etc.
  .tmp <- rep(1, nrow(rj_output))
  Node <- replicate(nrow(counts), as.numeric(paste(.tmp)), simplify = FALSE)
  Branch <- replicate(nrow(counts), as.numeric(paste(.tmp)), simplify = FALSE)
  Delta <- replicate(nrow(counts), as.numeric(paste(.tmp)), simplify = FALSE)
  Lambda <- replicate(nrow(counts), as.numeric(paste(.tmp)), simplify = FALSE)
  Kappa <- replicate(nrow(counts), as.numeric(paste(.tmp)), simplify = FALSE)
  Node_effects <- replicate(nrow(counts), as.numeric(paste(.tmp)), simplify = FALSE)
  names(Node) <- counts[ , "descNode"]
  names(Branch) <- counts[ , "descNode"]
  names(Delta) <- counts[ , "descNode"]
  names(Lambda) <- counts[ , "descNode"]
  names(Kappa) <- counts[ , "descNode"]
  names(Node_effects) <- counts[ , "descNode"]

  print("Searching for scalars...")
    pb <- txtProgressBar(min = 0, max = nrow(rj_output), style = 3)
    for (i in 1:nrow(rj_output)) {
      lastrates <- rj_output[i, !is.na(rj_output[i, ])]
      
      # If the number of columns is seven, there are no scalars applied this generation.
      if (ncol(lastrates) == 7) {
        nodes <- NA
        scales <- NA
        types <- NA
      } else {
        
        int <- lastrates[8:length(lastrates)]
   
        nodes <- unlist(c(int[grep("NodeID*", names(int))]))
        scales <- unlist(c(int[grep("Scale*", names(int))]))
        types <- unlist(c(int[grep("NodeBranch*", names(int))]))
        mrcas <- sapply(nodes, function(x) fullmrcas[fullmrcas$node %in% x, "mrca"])
        alltypes[[i]] <- types
        allmrcas[[i]] <- mrcas

        # Is this for-loop filling the scalar objects? Do I need to make them 
        # within this function?
        for (j in 1:length(mrcas)) {
          nm <- paste0(types[j], "[[\"", as.character(mrcas[j]), "\"]]", "[", i, "]")
          eval(parse(text = paste0(nm, "<-", scales[j])))
        }
      }
      setTxtProgressBar(pb, i)    
    }

    close(pb)
    res <- list(alltypes = alltypes,
                allmrcas = allmrcas,
                rates = rates,
                Node = Node,
                Branch = Branch,
                Delta = Delta,
                Lambda = Lambda,
                Kappa = Kappa,
                Node_effects = Node_effects)
  return(res)
}
