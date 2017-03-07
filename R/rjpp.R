






##############################################################################
#' rjpp
#'
#' A function that takes the output of a kappa, lambda, delta, VRates etc. RJ bayesTraits run and runs post-processing on it.
#' @param rjlog The RJ output of the run - typically suffixed with .VarRates.txt
#' @param tree The time tree the analysis was run on as an object of class "phylo", or the filename of the timetree.
#' @param burnin The burnin (if required) for the mcmc (generally worked out from the other logfile)
#' @param thinning Thinning parameter for the MCMC output - again, worked out from the raw MCMC output logfile.
#' @param meanbranches If true, calculates mean, median and mode branch lengths and returns mean tree.
#' @param ratestable 
#' @import phytools
#' @export
#' @name rjpp
rjpp <- function(rjlog, rjtrees, tree, burnin = 0, thinning = 1, 
  meanbranches = TRUE, ratestable = TRUE) {

  pboptions(type = "txt", style = 3, char = "=")

  if (class(tree) == "phylo") {
    extree <- ladderize(tree)
  } else {
    extree <- ladderize(read.nexus(tree))
  }

  print("Loading log file.")
  rjout <- loadRJ(rjlog, burnin = burnin, thinning = thinning)

  print("Loading posterior trees.")
  posttrees <- read.nexus(rjtrees)
  posttrees <- posttrees[burnin:length(posttrees)]

  if (meanbranches) {
    print("Calculating mean branch lengths.")
    meanbl <- meanBranches(reftree = extree, trees = posttrees, burnin = burnin, 
      thinning = thinning, pbar = TRUE)
  } else {
    meanbl = FALSE
  }

  rj_output <- rjout$rj_output
  subtrees <- rjout$subtrees
  rjtaxa <- rjout$taxa
  niter <- nrow(rj_output)
  print("Finding taxa.")
  
  taxa <- pblapply(subtrees$node, function(x) getTaxa(x, subtrees = subtrees))
  
  print("Calculating MRCAs.")
  fullmrcas <- unlist(pblapply(taxa, function(x) getMRCAbtr(x , tree = extree, rjtaxa = rjtaxa)))
  fullmrcas <- data.frame(node = subtrees$node, mrca = fullmrcas)
 
  counts <- createCountsTable(extree, meanbl)

  # Find the scalars.
  all_scalars <- scalarSearch(rj_output, counts, fullmrcas)

  # Calculate cumulative node effects
  for (i in 1:length(all_scalars$Node)) {
    .tmp <- multiplyNodes(all_scalars$Node[[i]], 
      names(all_scalars$Node)[i], 
      extree, 
      all_scalars$Node_effects)
    all_scalars$Node_effects[names(.tmp)] <- .tmp
  }

  all_scalars$Node_effects <- lapply(1:length(all_scalars$Node_effects), 
    function(x) all_scalars$Node_effects[[x]] * all_scalars$Branch[[x]])
  names(all_scalars$Node_effects) <- counts[ , "descNode"]
  
  origins <- list(nodes = do.call(rbind, all_scalars$Node), 
                  branches = do.call(rbind, all_scalars$Branch), 
                  delta = do.call(rbind, all_scalars$Delta),
                  lambda = do.call(rbind, all_scalars$Lambda), 
                  kappa = do.call(rbind, all_scalars$Kappa), 
                  rates = do.call(rbind, all_scalars$Node_effects)
                  )

  alltypes <- unlist(all_scalars$alltypes)
  allmrcas <- unlist(all_scalars$allmrcas)

  bs <- table(unlist(allmrcas)[alltypes == "Branch"])
  ns <- table(unlist(allmrcas)[alltypes == "Node"])
  ds <- table(unlist(allmrcas)[alltypes == "Delta"])
  ks <- table(unlist(allmrcas)[alltypes == "Kappa"])
  ls <- table(unlist(allmrcas)[alltypes == "Lambda"])

  bstaxa <- counts$descNode %in% names(bs)
  nstaxa <- counts$descNode %in% names(ns)
  dstaxa <- counts$descNode %in% names(ds)
  kstaxa <- counts$descNode %in% names(ks)
  lstaxa <- counts$descNode %in% names(ls)

  counts$nOrgnBRate[bstaxa] <- bs[match(counts$descNode[bstaxa], names(bs))] 
  counts$nOrgnScalar[bstaxa] <- counts$nOrgnBRate[bstaxa] + bs[match(counts$descNode[bstaxa], names(bs))] 
  
  counts$nOrgnNRate[nstaxa] <- ns[match(counts$descNode[nstaxa], names(ns))]
  counts$nOrgnScalar[nstaxa] <- counts$nOrgnBRate[nstaxa] + ns[match(counts$descNode[nstaxa], names(ns))]
  
  counts$nOrgnDelta[dstaxa] <- ds[match(counts$descNode[dstaxa], names(ds))]
  counts$nOrgnScalar[dstaxa] <- counts$nOrgnBRate[dstaxa] + ds[match(counts$descNode[dstaxa], names(ds))]
  
  counts$nOrgnKappa[kstaxa] <- ks[match(counts$descNode[kstaxa], names(ks))]
  counts$nOrgnScalar[kstaxa] <- counts$nOrgnBRate[kstaxa] + ks[match(counts$descNode[kstaxa], names(ks))]
  
  counts$nOrgnLambda[lstaxa] <- ls[match(counts$descNode[lstaxa], names(ls))]
  counts$nOrgnScalar[lstaxa] <- counts$nOrgnBRate[lstaxa] + ls[match(counts$descNode[lstaxa], names(ls))]
  
  # Fill in transformation detail.

  counts[ , "meanDelta"] <- rowMeans(origins$delta)
  counts[ , "medianDelta"] <- apply(origins$delta, 1, median)
  counts[ , "modeDelta"] <-  apply(origins$delta, 1, modeStat)
  counts[ , "rangeDelta"] <- suppressWarnings(apply(origins$delta, 1, max) - apply(origins$delta, 1, min))

  counts[ , "meanKappa"] <- rowMeans(origins$kappa)
  counts[ , "medianKappa"] <- apply(origins$kappa, 1, median)
  counts[ , "modeKappa"] <- apply(origins$kappa, 1, modeStat)
  counts[ , "rangeKappa"] <- suppressWarnings(apply(origins$kappa, 1, max) - apply(origins$kappa, 1, min))

  counts[ , "meanLambda"] <- rowMeans(origins$lambda)
  counts[ , "medianLambda"] <- apply(origins$lambda, 1, median)
  counts[ , "modeLambda"] <- apply(origins$lambda, 1, modeStat)
  counts[ , "rangeLambda"] <- suppressWarnings(apply(origins$lambda, 1, max) - apply(origins$lambda, 1, min))

  counts[ , "meanRate"] <- rowMeans(origins$rates)
  counts[ , "medianRate"] <- apply(origins$rates, 1, median)
  counts[ , "modeRate"] <- apply(origins$rates, 1, modeStat)
  counts[ , "rangeRate"] <- suppressWarnings(apply(origins$rates, 1, max) - apply(origins$rates, 1, min))

  counts[ , "itersScaled"] <- 
  counts[ , "itersRatescaled"] <- apply(origins$rates, 1, function(x) sum(x != 1))
  counts[ , "itersDelta"] <- counts[ , "nOrgnDelta"]
  counts[ , "itersKappa"] <- counts[ , "nOrgnDelta"]
  counts[ , "itersLambda"] <- counts[ , "nOrgnDelta"]

  counts[ , "pScaled"] <- 
  counts[ , "pRate"] <- apply(origins$rates, 1, function(x) sum(x != 1)) / niter
  counts[ , "pDelta"] <- counts[ , "nOrgnDelta"] / niter
  counts[ , "pKappa"] <- counts[ , "nOrgnKappa"] / niter
  counts[ , "pLambda"] <- counts[ , "nOrgnLambda"] / niter

  counts[ , "nScalar"] <- 
  counts[ , "nRate"] <- counts[ , "nOrgnNRate"] + counts[ , "nOrgnBRate"]
  counts[ , "nDelta"] <- counts[ , "nOrgnDelta"]
  counts[ , "nKappa"] <- counts[ , "nOrgnDelta"]
  counts[ , "nLambda"] <- counts[ , "nOrgnDelta"]

  # Now just remove anything that is irrelevant (i.e. all values == 1) and then work out
  # what to return.

  counts <- counts[ , apply(counts, 2, function(x) all(x != 1))]
  counts <- counts[ , apply(counts, 2, function(x) all(x != 0))]

  if (meanbranches) {
    meantree <- extree
    meantree$edge.length <- counts[c(2:nrow(counts)) , "meanBL"]
  }

  if (all(sapply(origins$delta, function(x) all(x == 1)))) {
    origins$delta <- NULL
  }

  if (all(sapply(origins$kappa, function(x) all(x == 1)))) {
    origins$kappa <- NULL
  }

  if (all(sapply(origins$lambda, function(x) all(x == 1)))) {
    origins$lambda <- NULL
  }

  if (all(sapply(origins$nodes, function(x) all(x == 1)))) {
    origins$nodes <- NULL
  }  

  if (all(sapply(origins$branches, function(x) all(x == 1)))) {
    origins$branches <- NULL
  }

  if (all(sapply(origins$rates, function(x) all(x == 1)))) {
    origins$rates <- NULL
  }

  if (meanbranches) { 
    res <- list(data = counts, niter = niter, meantree = meantree)
  } else {
    res <- list(data = counts, niter = niter)
  }

  if (!is.null(origins$rates)) {
    scalars <- list(rates = origins$rates)
    origins$rates <- NULL
    res <- c(res, list(scalars = scalars))
  }

  res <- c(res, list(origins = origins))

  return(res)
}

