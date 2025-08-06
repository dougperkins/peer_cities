# Outer Functions ####

evaluate_all_clusterings <- function(data_reduced, clusterings, indices, subset_name = "six_vars", summary_funs = list()) {
  common_methods <- intersect(names(data_reduced), names(clusterings))
  
  map(common_methods, function(method){
    message(glue("→ Evaluating: {method} {subset_name}"))
    evaluate_clusterings(data_reduced[[method]][[subset_name]], clusterings[[method]], indices, summary_funs)
  }) %>% set_names(common_methods)
}

normalize_all_eval_results <- function(eval_results) {
  map(eval_results, normalize_eval_df)
}

aggregate_normalized_eval_indices <- function(eval_normalized) {
  map(eval_normalized, aggregate_one_dr_normalized_eval_indices)
}

get_best_clusterings <- function(eval_aggregates, clusterings, use = c("median", "avg")) {
  use <- match.arg(use)  # ensure "median" or "avg"
  
  clust_best <- list()
  
  for (dr_method in names(eval_aggregates)) {
    eval_df <- eval_aggregates[[dr_method]]
    
    # Find best method based on selected column
    best_method <- eval_df$clustering[which.max(eval_df[[use]])]
    
    # Extract clustering assignments
    best_assn <- clusterings[[dr_method]][[best_method]]
    
    # Store in output list
    clust_best[[dr_method]] <- list(
      assn = best_assn,
      method = best_method
    )
  }
  
  return(clust_best)
}

# compare_clusterings <- function(data_reduced, clusterings, evals, subset_name = "six_vars") {
#   best_results <- list()
#   
#   for (dr in names(data_reduced)) {
#     df <- data_reduced[[dr]][[subset_name]]
#     clusterings_for_dr <- clusterings[[dr]]
#     
#     result <- compare_clusterings_majority_vote(df, clusterings_for_dr, evals)
#     
#     best_results[[dr]] <- result
#   }
#   
#   return(best_results)
# }


# Inner Functions ####


# compare_clusterings_majority_vote <- function(data, clusterings, indices) {
#   
#   # Summary functions for indices that return vectors
#   index_summary_funs <- list(
#     # Silhouette-based
#     "clus.avg.silwidths" = mean,
#     "min.clus.silwidth"  = min,
#     "avg.silwidth"       = identity,
#     
#     # Classic cluster validity indices
#     "ch"                 = identity,
#     "dunn"               = identity,
#     "dunn2"              = identity,
#     "entropy"            = identity,
#     "wb.ratio"           = identity,
#     
#     # Correlation-based indices
#     "pearsongamma"       = identity,
#     "g2"                 = identity,
#     "g3"                 = identity,
#     
#     # Separation index
#     "sindex"             = identity,
#     
#     # Within cluster sum of squares (k-means style)
#     "within.cluster.ss"  = identity
#   )
#   
#   message("Pre Eval")
#   
#   # Run cluster.stats for each clustering, store results
#   evals <- lapply(clusterings, function(clust_assn) {
#     cluster.stats(dist(data), clust_assn)
#   })
#   
#   # Prepare matrix to hold summarized index values
#   score_matrix <- matrix(NA_real_, nrow = length(indices), ncol = length(evals),
#                          dimnames = list(indices, names(evals)))
#   
#   # Fill matrix with summarized values
#   for (idx in indices) {
#     #message("Processing index: ", idx)
#     
#     summary_fun <- index_summary_funs[[idx]]
#     if (is.null(summary_fun)) {
#       warning("No summary function for index '", idx, "', skipping.")
#       next
#     }
#     
#     for (m in seq_along(evals)) {
#       # Special case: min.clus.silwidth uses min of clus.avg.silwidths vector
#       if (idx == "min.clus.silwidth") {
#         val_raw <- evals[[m]]$clus.avg.silwidths
#         val <- if (!is.null(val_raw)) {
#           min_val <- min(val_raw, na.rm = TRUE)
#           # message(sprintf("Method %s, raw clus.avg.silwidths: %s, min: %f",
#           #                 names(evals)[m],
#           #                 paste(round(val_raw, 4), collapse = ", "),
#           #                 min_val))
#           min_val
#         } else {
#           NA_real_
#         }
#       } else if (idx == "clus.avg.silwidths") {
#         val_raw <- evals[[m]]$clus.avg.silwidths
#         if (is.null(val_raw)) {
#           val <- NA_real_
#         } else {
#           val_num <- as.numeric(val_raw)
#           # message(sprintf("Method %s, raw clus.avg.silwidths: %s, as.numeric: %s",
#           #                 names(evals)[m],
#           #                 paste(round(val_raw, 4), collapse = ", "),
#           #                 paste(round(val_num, 4), collapse = ", ")))
#           val <- summary_fun(val_num)  # usually mean
#           #message(sprintf("Method %s, summarized clus.avg.silwidths: %f", names(evals)[m], val))
#         }
#       } else {
#         # General case: extract raw index value
#         val_raw <- evals[[m]][[idx]]
#         #message(sprintf("Method %s, raw '%s': %s",
#                         # names(evals)[m],
#                         # idx,
#                         # if (is.null(val_raw)) "NULL" else paste(round(val_raw, 4), collapse = ", ")))
#         
#         if (is.null(val_raw)) {
#           val <- NA_real_
#         } else if (is.vector(val_raw) && length(val_raw) > 1) {
#           # Use summary function if vector (e.g. mean)
#           val <- summary_fun(val_raw)
#           #message(sprintf("Method %s, summarized '%s': %f", names(evals)[m], idx, val))
#         } else {
#           val <- val_raw
#         }
#       }
#       
#       # Safety check: ensure val is a numeric scalar before assignment
#       if (length(val) != 1 || !is.numeric(val)) {
#         warning(sprintf("Index '%s', method %s returned non-scalar or non-numeric value. Coercing to NA.",
#                         idx, names(evals)[m]))
#         val <- NA_real_
#       }
#       
#       score_matrix[idx, m] <- val
#     }
#   }
#   
#   # Determine winner per index (depends on metric direction)
#   # For these indices, higher is better (e.g. silhouette, CH, Dunn)
#   higher_better <- c(
#     "avg.silwidth",         # average silhouette width — higher is better
#     "min.clus.silwidth",    # minimum cluster silhouette width — higher better
#     "clus.avg.silwidths",   # average cluster silhouette widths — higher better
#     "ch",                   # Calinski-Harabasz index — higher better
#     "dunn",                 # Dunn index — higher better
#     "dunn2",                # Dunn variant — higher better
#     "sindex",               # separation index — higher better
#     "wb.ratio",             # within-between ratio — higher better (interpret carefully)
#     "pearsongamma",         # correlation measure — higher better
#     "g2",                   # correlation measure — higher better
#     "g3",                   # correlation measure — higher better
#     "entropy"               # entropy — higher usually means better (depends on context)
#   )
#   
#   # For within.cluster.ss and diameter lower is better
#   lower_better <- c("within.cluster.ss")
#   
#   # For indices not listed, default higher is better
#   winner_vec <- character(length(indices))
#   names(winner_vec) <- indices
#   
#   for (idx in indices) {
#     vals <- score_matrix[idx, ]
#     if (all(is.na(vals))) {
#       winner_vec[idx] <- NA_character_
#       next
#     }
#     # Remove NA for comparison
#     vals_valid <- vals[!is.na(vals)]
#     if (length(vals_valid) == 0) {
#       winner_vec[idx] <- NA_character_
#       next
#     }
#     
#     if (idx %in% lower_better) {
#       best_val <- min(vals_valid)
#     } else {
#       best_val <- max(vals_valid)
#     }
#     # In case multiple tie, take first
#     best_method <- names(vals)[which(vals == best_val)][1]
#     winner_vec[idx] <- best_method
#     
#     # Print values and winner per index
#     message(sprintf("Index '%s': ", idx),
#             paste(sprintf("%s=%.4f", names(vals), vals), collapse = ", "),
#             "; winner = ", best_method)
#   }
#   
#   # Majority vote for overall best clustering
#   vote_table <- table(winner_vec)
#   vote_table <- vote_table[!is.na(names(vote_table))]
#   if (length(vote_table) == 0) {
#     stop("No valid winners found in any index.")
#   }
#   overall_best <- names(vote_table)[which.max(vote_table)]
#   
#   message("Overall best clustering method by majority vote: ", overall_best)
#   
#   # Return the cluster assignment vector of the best method
#   list(method = overall_best,
#        assn = clusterings[[overall_best]])
# }

normalize_eval_df <- function(df) {
  higher_is_better_vec <- c(
    avg_sil       = TRUE,   # average silhouette
    min_avg_sil   = TRUE,   # min average silhouette
    sil           = TRUE,   # silhouette (alias)
    s_index       = TRUE,   # usually silhouette-based
    ch            = TRUE,   # Calinski-Harabasz
    dunn          = TRUE,   # Dunn
    dunn2         = TRUE,   # fpc::dunn2 (GDI32)
    ball_hall     = FALSE,  # compactness (lower = better)
    br            = FALSE,  # Baker-Hubert Gamma Ratio (lower = better)
    wcss          = FALSE,  # Within-cluster sum of squares
    entropy       = FALSE,  # External index — lower is better
    c             = TRUE,   # Hubert’s C index — higher = better
    db            = FALSE,  # Davies-Bouldin — lower = better
    det_ratio     = TRUE,   # Determinant ratio (higher = better)
    gamma         = TRUE,   # Hubert’s Gamma — higher = better
    gplus         = FALSE,  # G+ index — lower = better
    ksq_detw      = TRUE,   # K²/det(W) — higher = better
    log_det_ratio = TRUE,   # Log(det(T)/det(W)) — higher = better
    log_ss_ratio  = TRUE,   # log(BSS/WSS) — higher = better
    mcr           = FALSE,  # Misclassification rate — lower = better
    pbm           = TRUE,   # PBM index — higher = better
    ptbi          = TRUE,   # Point-biserial — higher = better
    rt            = TRUE,   # Ratkowsky Lance — higher = better
    rl            = TRUE,   # R-squared like index — higher is better
    sd_scat       = FALSE,  # Scatter — lower is better
    sd_dis        = FALSE,  # Density separation — lower is better
    tau           = TRUE,   # Kendall's tau — higher = better
    trace_w       = FALSE,  # Trace of within matrix — lower = better
    trace_wib     = TRUE,   # Trace(W)/Trace(B) — lower is better → TRUE if inverted
    wb_ratio      = TRUE,   # W/B ratio — higher is better
    wg            = FALSE,  # Within/Global scatter — lower is better
    xb            = FALSE   # Xie-Beni — lower is better
  )
  
  df_clustering_id <- df %>% select(clustering)
  
  normed <- imap_dfc(df %>% select(-clustering), function(column, colname) {
    
    higher_is_better <- higher_is_better_vec[[colname]]
    if (is.null(higher_is_better)) stop(paste("Missing entry in higher_is_better for:", colname))
    
    norm_col <- if (higher_is_better) {
      (column - min(column, na.rm = TRUE)) / (max(column, na.rm = TRUE) - min(column, na.rm = TRUE))
    } else {
      (max(column, na.rm = TRUE) - column) / (max(column, na.rm = TRUE) - min(column, na.rm = TRUE))
    }
    
    tibble(!!paste0(colname, "_n") := norm_col)
  })
  
  bind_cols(df_clustering_id, normed)
}

evaluate_clusterings <- function(data, clusterings, indices, summary_funs = list()) {
  
  #data <- data[[subset_name]]
  data %<>% select(where(is.numeric))
  d <- dist(data)
  
  # Internal registry of clustering quality indices
  index_registry <- list(
    avg_sil = function(data, clust) {
      d <- dist(data)
      sil <- cluster::silhouette(clust, d)
      mean(sil[, 3])
    },
    min_avg_sil = function(data, clust) {
      stats <- fpc::cluster.stats(d, clust)
      min(stats$clus.avg.silwidths)
    },
    sil = function(data, clust){
      clusterCrit::intCriteria(as.matrix(data), as.integer(clust), "Silhouette")[[1]]
    },
    s_index = function(data, clust){
      stats <- fpc::cluster.stats(d, clust)
      stats$sindex
    },
    ch = function(data, clust) {
      clusterCrit::intCriteria(as.matrix(data), as.integer(clust), "Calinski_Harabasz")[[1]]
    },
    # Example: Dunn index using clusterCrit package
    dunn = function(data, clust) {
      clusterCrit::intCriteria(as.matrix(data), as.integer(clust), "Dunn")[[1]]
    },
    dunn2 = function(data, clust) {
      stats <- fpc::cluster.stats(d, clust)
      stats$dunn2
    },
    ball_hall = function(data, clust) {
      clusterCrit::intCriteria(as.matrix(data), as.integer(clust), "Ball_Hall")[[1]]
    },
    wcss = function(data, clust){
      stats <- fpc::cluster.stats(d, clust)
      stats$within.cluster.ss
    },
    sep = function(data, clust){
      stats <- fpc::cluster.stats(d, clust)
      stats$separation
    },
    wb_ratio = function(data, clust){
      stats <- fpc::cluster.stats(d, clust)
      stats$wb.ratio
    },
    br = function(data, clust) {
      clusterCrit::intCriteria(as.matrix(data), as.integer(clust), "Banfeld_Raftery")[[1]]
    },
    c = function(data, clust){
      clusterCrit::intCriteria(as.matrix(data), as.integer(clust), "C_index")[[1]]
    },
    db = function(data, clust){
      clusterCrit::intCriteria(as.matrix(data), as.integer(clust), "Davies_Bouldin")[[1]]
    },
    det_ratio = function(data, clust){
      clusterCrit::intCriteria(as.matrix(data), as.integer(clust), "Det_Ratio")[[1]]
    },
    gamma = function(data, clust){
      clusterCrit::intCriteria(as.matrix(data), as.integer(clust), "Gamma")[[1]]
    },
    g2 = function(data, clust){
      stats <- fpc::cluster.stats(d, clust)
      stats$g2
    },
    g3 = function(data, clust){
      stats <- fpc::cluster.stats(d, clust)
      stats$g3
    },
    entropy = function(data, clust){
      stats <- fpc::cluster.stats(d, clust)
      stats$entropy
    },
    gplus = function(data, clust){
      clusterCrit::intCriteria(as.matrix(data), as.integer(clust), "G_plus")[[1]]
    },
    ksq_detw = function(data, clust){
      clusterCrit::intCriteria(as.matrix(data), as.integer(clust), "Ksq_DetW")[[1]]
    },
    log_det_ratio = function(data, clust){
      clusterCrit::intCriteria(as.matrix(data), as.integer(clust), "Log_Det_Ratio")[[1]]
    },
    log_ss_ratio = function(data, clust){
      clusterCrit::intCriteria(as.matrix(data), as.integer(clust), "Log_SS_Ratio")[[1]]
    },
    mcr = function(data, clust){
      clusterCrit::intCriteria(as.matrix(data), as.integer(clust), "McClain_Rao")[[1]]
    },
    pbm = function(data, clust){
      clusterCrit::intCriteria(as.matrix(data), as.integer(clust), "PBM")[[1]]
    },
    ptbi = function(data, clust){
      clusterCrit::intCriteria(as.matrix(data), as.integer(clust), "Point_Biserial")[[1]]
    },
    rt = function(data, clust){
      clusterCrit::intCriteria(as.matrix(data), as.integer(clust), "Ray_Turi")[[1]]
    },
    rl = function(data, clust){
      clusterCrit::intCriteria(as.matrix(data), as.integer(clust), "Ratkowsky_Lance")[[1]]
    },
    sc_sy = function(data, clust){
      clusterCrit::intCriteria(as.matrix(data), as.integer(clust), "Scott_Symons")[[1]]
    },
    sd_scat = function(data, clust){
      clusterCrit::intCriteria(as.matrix(data), as.integer(clust), "SD_Scat")[[1]]
    },
    sd_dis = function(data, clust){
      clusterCrit::intCriteria(as.matrix(data), as.integer(clust), "SD_Dis")[[1]]
    },
    s_dbw = function(data, clust){
      clusterCrit::intCriteria(as.matrix(data), as.integer(clust), "S_Dbw")[[1]]
    },
    tau = function(data, clust){
      clusterCrit::intCriteria(as.matrix(data), as.integer(clust), "Tau")[[1]]
    },
    trace_w = function(data, clust){
      clusterCrit::intCriteria(as.matrix(data), as.integer(clust), "Trace_W")[[1]]
    },
    trace_wib = function(data, clust){
      clusterCrit::intCriteria(as.matrix(data), as.integer(clust), "Trace_WiB")[[1]]
    },
    wg = function(data, clust){
      clusterCrit::intCriteria(as.matrix(data), as.integer(clust), "Wemmert_Gancarski")[[1]]
    },
    xb = function(data, clust){
      clusterCrit::intCriteria(as.matrix(data), as.integer(clust), "Xie_Beni")[[1]]
    },
    gdi21 = function(data, clust){
      clusterCrit::intCriteria(as.matrix(data), as.integer(clust), "GDI21")[[1]]
    },
    gdi31 = function(data, clust){
      clusterCrit::intCriteria(as.matrix(data), as.integer(clust), "GDI31")[[1]]
    },
    gdi32 = function(data, clust){
      clusterCrit::intCriteria(as.matrix(data), as.integer(clust), "GDI32")[[1]]
    }
    # Add more indices here as needed...
  )
  
  # Check requested indices exist
  if (!all(indices %in% names(index_registry))) {
    missing <- setdiff(indices, names(index_registry))
    stop("Indices not found in registry: ", paste(missing, collapse = ", "))
  }
  
  method_names <- names(clusterings)
  score_matrix <- matrix(NA_real_, nrow = length(method_names), ncol = length(indices),
                         dimnames = list(method_names, indices))
  
  for (method in method_names) {
    clust <- clusterings[[method]]
    
    for (idx in indices) {
      index_fun <- index_registry[[idx]]
      summary_fun <- summary_funs[[idx]] %||% identity  # default identity
      
      result <- tryCatch({
        val <- index_fun(data, clust)
        if (is.vector(val) && length(val) > 1) {
          summary_fun(val)
        } else if (length(val) == 1 && is.numeric(val)) {
          val
        } else {
          warning(sprintf("Index '%s' for method '%s' returned unsupported value. Coercing to NA.", idx, method))
          NA_real_
        }
      }, error = function(e) {
        warning(sprintf("Error evaluating index '%s' for method '%s': %s", idx, method, e$message))
        NA_real_
      })
      
      score_matrix[method, idx] <- result
    }
  }
  
  as.data.frame(score_matrix) %>%
    mutate(clustering = method_names) %>%
    select(clustering, everything())
}

aggregate_one_dr_normalized_eval_indices <- function(results) {
  #results_clustering_id <- results %>% select(clustering)
  
  aggregated <- results %>%
    select(-clustering) %>%
    rowwise() %>%
    mutate(
      avg = mean(c_across(everything()), na.rm = TRUE),
      median = median(c_across(everything()), na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(clustering = results$clustering) %>%
    select(clustering, avg, median)
  
  aggregated
  #bind_cols(results_clustering_id, aggregated)
}

# normalize_metric <- function(df, column, higher_is_better = TRUE) {
#   col_sym <- enquo(column)
#   
#   df %>%
#     mutate(
#       !!paste0(as_label(col_sym), "_n") := {
#         x <- !!col_sym
#         if (higher_is_better) {
#           (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
#         } else {
#           (max(x, na.rm = TRUE) - x) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
#         }
#       }
#     )
# }