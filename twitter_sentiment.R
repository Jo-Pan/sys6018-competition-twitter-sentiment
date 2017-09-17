setwd("/Users/Pan/Google Drive/Data Science/SYS 6018/sys6018-competition-twitter-sentiment")
library(tm)
library(tidytext)

#read data & create comb
train<-read.csv("train.csv")
test<-read.csv("test.csv")
train$dataset<-"train"
test$dataset<-"test"
test$sentiment<-NA
train$id<-NA
comb<-rbind(train,test)
comb<-comb[c("id","dataset","sentiment","text")]

# =================== Preclean ==========================
remove_url<-function(x){gsub(" ?(f|ht)(tp)s?(://)(\\S*)[./](\\S*)", "", x)}
remove_tw_tag<-function(x){gsub("RT |via ", "", x)}
remove_tw_username<-function(x){gsub("[@][a-zA-Z0-9_]{1,15}", "", x)}
remove_email<-function(x){gsub("\\b[A-Za-z0-9._-]*[@](.*?)[.].{1,3}\\b", "", x)}


comb$text_precleaned<-lapply(comb$text,FUN=remove_url)
comb$text_precleaned<-lapply(comb$text_precleaned,FUN=remove_tw_tag)
comb$text_precleaned<-lapply(comb$text_precleaned,FUN=remove_tw_username)
comb$text_precleaned<-lapply(comb$text_precleaned,FUN=remove_email)

#http://stackoverflow.com/questions/18153504/removing-non-english-text-from-corpus-in-r-using-tm
remove_nonASCII<-function(mytextcell){
dat2 <- unlist(strsplit(unlist(mytextcell), split=" "))
dat3 <- grep("dat2", iconv(dat2, "latin1", "ASCII", sub="dat2"))
if (length(dat3)==0){
  return(mytextcell)
}
else{
dat4 <- dat2[-dat3]
dat5 <- paste(dat4, collapse = " ")
return(dat5)
}}

comb$text_precleaned<-rapply(comb$text_precleaned,remove_nonASCII)
# ================make all text into a corpus ======================
# convert string to vector of words
combtext<-VCorpus(VectorSource(comb$text_precleaned))
combtext[[1]]$content


# compute TF-IDF matrix and inspect sparsity
combtext.tfidf = DocumentTermMatrix(combtext, control = list(weighting = weightTfIdf))
combtext.tfidf  # non-/sparse entries indicates how many of the DTM cells are non-zero and zero, respectively.
# sparsity is number of non-zero cells divided by number of zero cells.

# inspect sub-matrix:  first 5 documents and first 5 terms
combtext.tfidf[1:5,1:5]
as.matrix(combtext.tfidf[1:5,1:5])

##### Reducing Term Sparsity #####
# there's a lot in the documents that we don't care about. clean up the corpus.
combtext.clean = tm_map(combtext, stripWhitespace)                          # remove extra whitespace
combtext.clean = tm_map(combtext.clean, removeNumbers)                      # remove numbers
combtext.clean = tm_map(combtext.clean, removePunctuation)                  # remove punctuation
combtext.clean = tm_map(combtext.clean, content_transformer(tolower))       # ignore case
combtext.clean = tm_map(combtext.clean, removeWords, stopwords("english"))  # remove stop words
combtext.clean = tm_map(combtext.clean, stemDocument)                       # stem all words

# compare original content of document 1 with cleaned content
combtext[[1]]$content
combtext.clean[[4]]$content  # do we care about misspellings resulting from stemming?

# recompute TF-IDF matrix
combtext.clean.tfidf = DocumentTermMatrix(combtext.clean, control = list(weighting = weightTfIdf))

# reinspect the first 5 documents and first 5 terms
combtext.clean.tfidf[1:5,1:5]
as.matrix(combtext.clean.tfidf[1:5,1:5])

# we've still got a very sparse document-term matrix. remove sparse terms at various thresholds.
tfidf.99 = removeSparseTerms(combtext.clean.tfidf, 0.99)  # remove terms that are absent from at least 99% of documents 110 terms left
tfidf.98 = removeSparseTerms(combtext.clean.tfidf, 0.98)  # remove terms that are absent from at least 98% of documents 110 terms left
tfidf.95 = removeSparseTerms(combtext.clean.tfidf, 0.95)  # remove terms that are absent from at least 95% of documents 8 terms left
tfidf.90 = removeSparseTerms(combtext.clean.tfidf, 0.90)  # remove terms that are absent from at least 90% of documents 5 terms left
tfidf.80 = removeSparseTerms(combtext.clean.tfidf, 0.80)  # remove terms that are absent from at least 80% of documents 3 terms left
tfidf.70 = removeSparseTerms(combtext.clean.tfidf, 0.70)  # remove terms that are absent from at least 70% of documents. 2 terms left
inspect(tfidf.99)

as.matrix(tfidf.70[1:5,])
combtext.clean[[1]]$content
# =============== convert to df ==================
combtext.clean.df<-as.data.frame(as.matrix(DocumentTermMatrix(combtext.clean)), stringsAsFactors=False)
comb_clean<-cbind(comb,combtext.clean.df)

# =============== sentiment words column index ==========
#stem the library of sentiments words
sentiments_corp<-VCorpus(VectorSource(sentiments))
sentiments_corp<-tm_map(sentiments_corp,stemDocument)
sentiments_corp.df<-as.data.frame(as.matrix(DocumentTermMatrix(sentiments_corp)), stringsAsFactors=False)

#identify the sentiment words in comb_clean
sentiment_col_index<-which(colnames(comb_clean) %in% colnames(sentiments_corp.df))
names(comb_clean[,sentiment_col_index])
length(sentiment_col_index)

# ============== .99 sparse words column index ==================
sparse_99_col_index<-which(colnames(comb_clean) %in% colnames(as.matrix(tfidf.99)))

# ========================================================================
#                                  ANALYSIS
# ========================================================================
# =================== descriptive analysis ============
sort(colSums(comb_clean[comb_clean$sentiment==5,6:ncol(comb_clean)],na.rm=TRUE),decreasing=TRUE)
sort(colSums(comb_clean[comb_clean$sentiment==4,6:ncol(comb_clean)],na.rm=TRUE),decreasing=TRUE)
sort(colSums(comb_clean[comb_clean$sentiment==3,6:ncol(comb_clean)],na.rm=TRUE),decreasing=TRUE)
sort(colSums(comb_clean[comb_clean$sentiment==2,6:ncol(comb_clean)],na.rm=TRUE),decreasing=TRUE)
sort(colSums(comb_clean[comb_clean$sentiment==1,6:ncol(comb_clean)],na.rm=TRUE),decreasing=TRUE)

# ==================== Split data =====================
set.seed(1)
mytrainrows<-sample(1:nrow(train),0.7*nrow(train))
mytrain<-comb_clean[comb_clean$dataset=="train",][mytrainrows,]
myvalid<-comb_clean[comb_clean$dataset=="train",][-mytrainrows,]

# ==================== logistic regression ===========
lm1<-lm(sentiment~.,data=mytrain[,c(3,sentiment_col_index)])
summary(lm1)                                             #TRAIN: Adjusted R-squared:   1 
preds1<-predict(lm1,newdata = myvalid[,sentiment_col_index])
sum(as.integer(preds1)==myvalid$sentiment)/nrow(myvalid) #VALID: correction rate: 0.4745763
mse1<-sum((preds1-myvalid$sentiment)^2)/nrow(myvalid)    #VALID: MSE:0.8828927

lm2<-lm(sentiment~.,data=mytrain[,c(3,6:ncol(mytrain))])
summary(lm2)                                             #TEST:Adjusted R-squared:  0.8218 
preds2<-predict(lm2,newdata = myvalid[,(6:ncol(mytrain))])
sum(as.integer(preds2)==myvalid$sentiment)/nrow(myvalid) #VALID: correction rate: 0.6440678
mse2<-sum((preds2-myvalid$sentiment)^2)/nrow(myvalid)    #VALID: MSE:0.003393332

# ================== decision tree =================
library(rpart)
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
