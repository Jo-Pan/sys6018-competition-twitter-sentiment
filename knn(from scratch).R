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
knn.fn<-function(myrow,sample,k,y,x){
  #y: column number of y in sample
  #x: list of column numbers of predictors(x) in sample
  distance<-function(myrow,compare_row){
   return(sqrt(sum((myrow-compare_row)^2)))}
  
  distance_list<-c()
  for (i in 1:nrow(sample)){
    distance_list<-c(distance_list,distance(myrow,sample[i,x]))}
  voting_rows<-which(distance_list %in% head(sort(distance_list),k))[1:k]
  votes<-sample[voting_rows,y]
  votes<-factor(votes)
  return(names(sort(summary(votes),decreasing=TRUE)[1]))
  }

 