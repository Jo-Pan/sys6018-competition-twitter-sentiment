library(boot)
library(DAAG)
library(caret)

# ==================== Split data =====================
#mytrainrows<-sample(1:nrow(train),0.7*nrow(train))
mytrain<-comb_clean_final[comb_clean_final$dataset=="train",]
mytrain[,1] <- NULL
mytrain[,1] <- as.factor(mytrain[,1])
mytest<-comb_clean_final[comb_clean_final$dataset=="test",]
mytest[,1:2] <- NULL

# ==================== logistic regression ===========
# ---- with .99 sparse words ------

lm1<-lm(as.numeric(sentiment) ~.,data=mytrain)

summary(lm1)                                             #TRAIN: Adjusted R-squared:  0.0838  

cv.lm(mytrain, lm1, m=nrow(mytrain), dots = 
        FALSE, seed=23, plotit=TRUE, printit=TRUE)

preds <- predict(lm1,newdata = mytest)

preds_final <- unlist(lapply(list(preds), function(x) round(x, digits = 0)))

hist(preds_final)
submission <- test[, c("id", "sentiment")]

submission$sentiment <- preds_final

write.csv(submission, "submission_lm1.csv", row.names = F)

# ---- with all var ------
lm2<-multinom(sentiment~.,data=mytrain[,c(3,6:ncol(mytrain))])
summary(lm2)                                             #TEST:Adjusted R-squared:  0.8218 
preds2<-predict(lm2,newdata = myvalid[,(6:ncol(mytrain))])
sum(as.integer(preds2)==myvalid$sentiment)/nrow(myvalid) #VALID: correction rate: 0.6440678
mse2<-sum((preds2-myvalid$sentiment)^2)/nrow(myvalid)    #VALID: MSE:0.003393332

# ---- with sentiment ------
lm3<-glm(sentiment~.,data=mytrain[,c(3,sentiment_col_index)])
summary(lm3)                                             #TEST:Adjusted R-squared:  0.5539 
preds3<-predict(lm3,newdata = myvalid[,sentiment_col_index])
sum(as.integer(preds3)==myvalid$sentiment)/nrow(myvalid) #VALID: correction rate: 0.6440678
                                                         #prediction from a rank-deficient fit may be misleading
mse3<-sum((preds3-myvalid$sentiment)^2)/nrow(myvalid)    #VALID: MSE:mse3



# ================== decision tree =================
my_tree1 <- rpart(sentiment ~., data=mytrain[,c(3,6:ncol(mytrain))], method = "class", control=rpart.control(cp=0.0001))
#summary(my_tree1)
preds_tr1<-predict(my_tree1,newdata = myvalid[,(6:ncol(mytrain))])
preds_tr1.ans<-colnames(preds_tr1)[apply(preds_tr1,1,which.max)]
sum(as.integer(preds_tr1.ans)==myvalid$sentiment)/nrow(myvalid) #VALID: correction rate: 0.5728814

my_tree2 <- rpart(sentiment ~., data=mytrain[,c(3,6:ncol(mytrain))], method = "class", control=rpart.control(cp=0.001))
#summary(my_tree2)
preds_tr2<-predict(my_tree2,newdata = myvalid[,(6:ncol(mytrain))])
preds_tr2.ans<-colnames(preds_tr2)[apply(preds_tr2,1,which.max)]
sum(as.integer(preds_tr2.ans)==myvalid$sentiment)/nrow(myvalid) #VALID: correction rate: 0.5864407

my_tree3 <- rpart(sentiment ~., data=mytrain[,c(3,6:ncol(mytrain))], method = "class", control=rpart.control(cp=0.01))
#summary(my_tree2)
preds_tr3<-predict(my_tree3,newdata = myvalid[,(6:ncol(mytrain))])
preds_tr3.ans<-colnames(preds_tr3)[apply(preds_tr3,1,which.max)]
sum(as.integer(preds_tr3.ans)==myvalid$sentiment)/nrow(myvalid) #VALID: correction rate: 0.620339
                                                                #Public Score my_tree3: 0.68098

my_tree4 <- rpart(sentiment ~., data=mytrain[,c(3,6:ncol(mytrain))], method = "class", control=rpart.control(cp=0.1))
#summary(my_tree2)
preds_tr4<-predict(my_tree4,newdata = myvalid[,(6:ncol(mytrain))])
preds_tr4.ans<-colnames(preds_tr4)[apply(preds_tr4,1,which.max)]
sum(as.integer(preds_tr4.ans)==myvalid$sentiment)/nrow(myvalid) #VALID: correction rate: 0.6169492


# Predict
#final_model<-rpart(sentiment ~., data=comb_clean[comb_clean$dataset=="train",c(3,6:ncol(comb_clean))], method = "class", control=rpart.control(cp=0.01))
final_preds<-predict(my_tree3,newdata = comb_clean[comb_clean$dataset=="test",c(6:ncol(comb_clean))])
final_preds.ans<-colnames(final_preds)[apply(final_preds,1,which.max)]
final_table<-data.frame(test$id, as.integer(final_preds.ans))
# Write files
write.table(final_table, file="dt0_01.csv", row.names=F, col.names=c("id", "sentiment"), sep=",")
