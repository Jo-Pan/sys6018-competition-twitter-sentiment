library(tidyverse)

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

knn_j <- function(x){
  tr_matrix <- rbind(comb_clean[comb_clean$dataset == 'train',sparse_99_col_index],x)
  dist_matrix <- as.matrix(dist(tr_matrix))
  diag(dist_matrix) <- NA
  nearest <- head(order(dist_matrix[,ncol(dist_matrix)]), n=k) %>% comb_clean[.,]
  score <- Mode(nearest$sentiment)
  return(score)
}

k <- 3
score <- numeric()
test <- comb_clean[comb_clean$dataset == 'test',sparse_99_col_index]
score <- apply(test, 1, knn_j)




# for (i in 1:nrow(test)){
# score <- c(score,knn_j(test[i,],3))
# }







