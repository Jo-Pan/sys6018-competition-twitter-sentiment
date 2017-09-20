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



# JO KNN

# knn.fn<-function(sample,y_col,x_cols,k){
#   #y_col: column number of y in sample
#   #x_cols: list of column numbers of predictors(x) in sample
#   distance<-function(myrow,compare_row){
#     return(sqrt(sum((myrow-compare_row)^2)))}
#   results<-c()
#   for (i in 1:nrow(sample)){
#     distance_list<-c()
#     thesample<-sample[-i,]
#     for (j in 1:nrow(thesample)){
#       distance_list<-c(distance_list,distance(sample[i,x_cols],thesample[j,x_cols]))}
#     voting_rows<-which(distance_list %in% head(sort(distance_list),k))[1:k]
#     votes<-sample[voting_rows,y_col]
#     votes<-factor(votes)
#     results<-c(results,names(sort(summary(votes),decreasing=TRUE)[1]))
#     print(results)
#   }
#   return(results)
# }
# knn.fn(mytrain2,3,sparse_99_col_index,9)


