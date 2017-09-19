
sample <- mytrain2
y_col <- 3
x_cols <- sparse_99_col_index
k <- 3


distance<-function(myrow,compare_row){
  return(sqrt(sum((myrow-compare_row)^2)))}

results<-c()

for (i in 1:nrow(sample)){
  distance_list<-c()
  thesample<-sample[-i,]
  for (j in 1:nrow(thesample)){
    distance_list<-c(distance_list,distance(sample[i,x_cols],thesample[j,x_cols]))
  }
  voting_rows<-which(distance_list %in% head(sort(distance_list),k))[1:k]
  votes<-sample[voting_rows,y_col]
  votes<-factor(votes)
  results<-c(results,names(sort(summary(votes),decreasing=TRUE)[1]))
  print(results)
}

mytrain3 <- mytrain2[,sparse_99_col_index]


d_l <- 1:nrow(thesample)
distance_list <- sapply(d_l,FUN = distance,myrow = sample[i,x_cols],compare_row = thesample[d_l,x_cols])
# distance_list <- lapply(1:nrow(thesample),FUN = distance, myrow=sample[i,x_cols], compare_row=thesample[x,x_cols])



k <- 3
train <- comb_clean[comb_clean$dataset == 'train',sparse_99_col_index]
di<-as.matrix(dist(train))
diag(di) <- NA

head(order(di[,1]), n=k)






