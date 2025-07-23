# Outer Functions ####
compare_clusterings <- function(data_reduced, clusterings, evals, subset_name = "six_vars") {
  best_results <- list()
  
  for (dr in names(data_reduced)) {
    df <- data_reduced[[dr]][[subset_name]]
    clusterings_for_dr <- clusterings[[dr]]
    
    result <- compare_clusterings_majority_vote(df, clusterings_for_dr, evals)
    
    best_results[[dr]] <- result
  }
  
  return(best_results)
}

# Inner Functions ####
compare_clusterings_majority_vote <- function(data, clusterings, indices) {
  
  # Summary functions for indices that return vectors
  index_summary_funs <- list(
    # Silhouette-based
    "clus.avg.silwidths" = mean,
    "min.clus.silwidth"  = min,
    "avg.silwidth"       = identity,
    
    # Classic cluster validity indices
    "ch"                 = identity,
    "dunn"               = identity,
    "dunn2"              = identity,
    "entropy"            = identity,
    "wb.ratio"           = identity,
    
    # Correlation-based indices
    "pearsongamma"       = identity,
    "g2"                 = identity,
    "g3"                 = identity,
    
    # Separation index
    "sindex"             = identity,
    
    # Within cluster sum of squares (k-means style)
    "within.cluster.ss"  = identity
  )
  
  message("Pre Eval")
  
  # Run cluster.stats for each clustering, store results
  evals <- lapply(clusterings, function(clust_assn) {
    cluster.stats(dist(data), clust_assn)
  })
  
  # Prepare matrix to hold summarized index values
  score_matrix <- matrix(NA_real_, nrow = length(indices), ncol = length(evals),
                         dimnames = list(indices, names(evals)))
  
  # Fill matrix with summarized values
  for (idx in indices) {
    #message("Processing index: ", idx)
    
    summary_fun <- index_summary_funs[[idx]]
    if (is.null(summary_fun)) {
      warning("No summary function for index '", idx, "', skipping.")
      next
    }
    
    for (m in seq_along(evals)) {
      # Special case: min.clus.silwidth uses min of clus.avg.silwidths vector
      if (idx == "min.clus.silwidth") {
        val_raw <- evals[[m]]$clus.avg.silwidths
        val <- if (!is.null(val_raw)) {
          min_val <- min(val_raw, na.rm = TRUE)
          # message(sprintf("Method %s, raw clus.avg.silwidths: %s, min: %f",
          #                 names(evals)[m],
          #                 paste(round(val_raw, 4), collapse = ", "),
          #                 min_val))
          min_val
        } else {
          NA_real_
        }
      } else if (idx == "clus.avg.silwidths") {
        val_raw <- evals[[m]]$clus.avg.silwidths
        if (is.null(val_raw)) {
          val <- NA_real_
        } else {
          val_num <- as.numeric(val_raw)
          # message(sprintf("Method %s, raw clus.avg.silwidths: %s, as.numeric: %s",
          #                 names(evals)[m],
          #                 paste(round(val_raw, 4), collapse = ", "),
          #                 paste(round(val_num, 4), collapse = ", ")))
          val <- summary_fun(val_num)  # usually mean
          #message(sprintf("Method %s, summarized clus.avg.silwidths: %f", names(evals)[m], val))
        }
      } else {
        # General case: extract raw index value
        val_raw <- evals[[m]][[idx]]
        #message(sprintf("Method %s, raw '%s': %s",
                        # names(evals)[m],
                        # idx,
                        # if (is.null(val_raw)) "NULL" else paste(round(val_raw, 4), collapse = ", ")))
        
        if (is.null(val_raw)) {
          val <- NA_real_
        } else if (is.vector(val_raw) && length(val_raw) > 1) {
          # Use summary function if vector (e.g. mean)
          val <- summary_fun(val_raw)
          #message(sprintf("Method %s, summarized '%s': %f", names(evals)[m], idx, val))
        } else {
          val <- val_raw
        }
      }
      
      # Safety check: ensure val is a numeric scalar before assignment
      if (length(val) != 1 || !is.numeric(val)) {
        warning(sprintf("Index '%s', method %s returned non-scalar or non-numeric value. Coercing to NA.",
                        idx, names(evals)[m]))
        val <- NA_real_
      }
      
      score_matrix[idx, m] <- val
    }
  }
  
  # Determine winner per index (depends on metric direction)
  # For these indices, higher is better (e.g. silhouette, CH, Dunn)
  higher_better <- c(
    "avg.silwidth",         # average silhouette width — higher is better
    "min.clus.silwidth",    # minimum cluster silhouette width — higher better
    "clus.avg.silwidths",   # average cluster silhouette widths — higher better
    "ch",                   # Calinski-Harabasz index — higher better
    "dunn",                 # Dunn index — higher better
    "dunn2",                # Dunn variant — higher better
    "sindex",               # separation index — higher better
    "wb.ratio",             # within-between ratio — higher better (interpret carefully)
    "pearsongamma",         # correlation measure — higher better
    "g2",                   # correlation measure — higher better
    "g3",                   # correlation measure — higher better
    "entropy"               # entropy — higher usually means better (depends on context)
  )
  
  # For within.cluster.ss and diameter lower is better
  lower_better <- c("within.cluster.ss")
  
  # For indices not listed, default higher is better
  winner_vec <- character(length(indices))
  names(winner_vec) <- indices
  
  for (idx in indices) {
    vals <- score_matrix[idx, ]
    if (all(is.na(vals))) {
      winner_vec[idx] <- NA_character_
      next
    }
    # Remove NA for comparison
    vals_valid <- vals[!is.na(vals)]
    if (length(vals_valid) == 0) {
      winner_vec[idx] <- NA_character_
      next
    }
    
    if (idx %in% lower_better) {
      best_val <- min(vals_valid)
    } else {
      best_val <- max(vals_valid)
    }
    # In case multiple tie, take first
    best_method <- names(vals)[which(vals == best_val)][1]
    winner_vec[idx] <- best_method
    
    # Print values and winner per index
    message(sprintf("Index '%s': ", idx),
            paste(sprintf("%s=%.4f", names(vals), vals), collapse = ", "),
            "; winner = ", best_method)
  }
  
  # Majority vote for overall best clustering
  vote_table <- table(winner_vec)
  vote_table <- vote_table[!is.na(names(vote_table))]
  if (length(vote_table) == 0) {
    stop("No valid winners found in any index.")
  }
  overall_best <- names(vote_table)[which.max(vote_table)]
  
  message("Overall best clustering method by majority vote: ", overall_best)
  
  # Return the cluster assignment vector of the best method
  list(method = overall_best,
       assn = clusterings[[overall_best]])
}
