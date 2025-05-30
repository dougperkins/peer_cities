# Check feature correlation
feature_corr <- function(cities){
  cor_matrix <- cities %>%
    select(-NAME) %>%
    cor(use = "complete.obs")
  
  #print(cor_matrix)
  
  corrplot(cor_matrix,
           method = "color", type = "upper",
           addCoef.col = "black", tl.col = "black", tl.srt = 45,
           title = "Correlation Matrix", mar = c(0, 0, 2, 0)
  )
  
  corrplot(cor_matrix)
}

feature_pairs <- function(cities){
  # Make a pairs plot (scatterplot matrix) for the variables in acs_data_ests
  pair <- pairs(
    cities %>% select(-NAME),
    main = "Pairs Plot of ACS Variables",
    pch = 21,
    bg = "lightblue"
  )
}
