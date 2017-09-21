library(tidyverse)


Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# VALIDATION
# train <- comb_clean_final[comb_clean_final$dataset == 'train', ]
# samp <- sample(1:981, 784, replace = FALSE)
# mytrain <- train[samp,]
# myvalid <- train[-samp,]
# mytrain_knn <- mytrain[,3:ncol(comb_clean_final)]
# myvalid_knn <- myvalid[,3:ncol(comb_clean_final)]

# VALIDATION
# knn_j <- function(x){
#   tr_matrix <- rbind(mytrain_knn,x)
#   dist_matrix <- as.matrix(dist(tr_matrix))
#   diag(dist_matrix) <- NA
#   nearest <- head(order(dist_matrix[,ncol(dist_matrix)]), n=k) %>% mytrain[.,]
#   score <- Mode(nearest$sentiment)
#   return(score)
# }

# VALIDATION
# k <- 15
# sentiment <- numeric()
# sentiment <- apply(myvalid_knn, 1, knn_j)
# sentiment_knn <- sentiment
# results <- as.data.frame(cbind(myvalid$sentiment, sentiment_knn))
# results$correct <- results$V1 == results$sentiment_knn
# mean(results$correct)
## Test with k=3, 48.2% accuracy
## Test with k=1, 45.7% accuracy
## Test with k=5, 54.3% accuracy
## Test with k=10, 67.0% accuracy
## Test with k=15, 69.0% accuracy
## Test with k=20, 69.0% accuracy


train <- comb_clean_final[comb_clean_final$dataset == 'train', 3:ncol(comb_clean_final)]
test <- comb_clean_final[comb_clean_final$dataset == 'test', 3:ncol(comb_clean_final)]

knn_j <- function(x){
  tr_matrix <- rbind(train,x)
  dist_matrix <- as.matrix(dist(tr_matrix))
  diag(dist_matrix) <- NA
  nearest <- head(order(dist_matrix[,ncol(dist_matrix)]), n=k) %>% comb_clean_final[.,]
  score <- Mode(nearest$sentiment)
  return(score)
}

k <- 15
sentiment <- numeric()
sentiment <- apply(test, 1, knn_j)

id <- seq(1:979)
submit <- as.data.frame(cbind(id,sentiment))
write_csv(submit, 'knnsubmission.csv')

