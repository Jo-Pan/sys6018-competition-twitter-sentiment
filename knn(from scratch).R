x1 <- sample(c(0,1), replace=TRUE, size=100)
x2 <- sample(c(0,1), replace=TRUE, size=100)
x3 <- sample(c(0,1), replace=TRUE, size=100)
y<-sample(c(1,2,3,4,5),replace=TRUE,size=100)
mydata<-as.data.frame(cbind(x1,x2,x3,y))
sample<-mydata
k<-5
y<-4
x<-c(1:3)
myrow<-c(1,1,1)
# ----------------------

knn.fn<-function(sample,y_col,x_cols,k){
  #y_col: column number of y in sample
  #x_cols: list of column numbers of predictors(x) in sample
  distance<-function(myrow,compare_row){
    return(sqrt(sum((myrow-compare_row)^2)))}
  results<-c()
  for (i in 1:nrow(sample)){
    distance_list<-c()
    thesample<-sample[-i,]
    for (j in 1:nrow(thesample)){
      distance_list<-c(distance_list,distance(sample[i,x_cols],thesample[j,x_cols]))}
    voting_rows<-which(distance_list %in% head(sort(distance_list),k))[1:k]
    votes<-sample[voting_rows,y_col]
    votes<-factor(votes)
    results<-c(results,names(sort(summary(votes),decreasing=TRUE)[1]))
    print(results)
  }
  return(results)
}
knn.fn(mytrain2,3,sparse_99_col_index,9)



knn2 <- function(sample, k){
  n <- nrow(sample)
  neighbor <- matrix(0, nrow = n, ncol = k)
  for(i in 1:n) {
    euc.dist <- colSums((sample[i, ] - t(sample)) ^ 2)  
    neighbor[i, ] <- order(euc.dist)[2:(k + 1)]
  }
  return(neighbor)
}

mytrain3 <- mytrain2[sparse_99_col_index]

scores <- knn2(mytrain3,3)

scores[333,]

mean(scores)


