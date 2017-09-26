#setwd("/Users/Pan/Google Drive/Data Science/SYS 6018/sys6018-competition-twitter-sentiment")

library(tm)
library(tidytext)  #optional trek: sentiment words library
library(rpart)     #optional trek:decision tree
library(entropy)   #calculate a statistical ratio for feature selection

library(boot)
library(DAAG)
library(caret)
library(tidyverse)
# ======================================================================================
#                                  Data Cleaning
# ======================================================================================

# ================= read data & create comb ============================================
train<-read.csv("train.csv")
test<-read.csv("test.csv")
train$dataset<-"train"
test$dataset<-"test"
test$sentiment<-NA
train$id<-NA
comb<-rbind(train,test)
comb<-comb[c("id","dataset","sentiment","text")]


# ================= Preclean with gsub =================================================
# clean url, twitter tag, twitter username, email
remove_url<-function(x){gsub(" ?(f|ht)(tp)s?(://)(\\S*)[./](\\S*)", "", x)}
remove_tw_tag<-function(x){gsub("RT |via ", "", x)}
remove_tw_username<-function(x){gsub("[@][a-zA-Z0-9_]{1,15}", "", x)}
remove_email<-function(x){gsub("\\b[A-Za-z0-9._-]*[@](.*?)[.].{1,3}\\b", "", x)}

comb$text_precleaned<-lapply(comb$text,FUN=remove_url)
comb$text_precleaned<-lapply(comb$text_precleaned,FUN=remove_tw_tag)
comb$text_precleaned<-lapply(comb$text_precleaned,FUN=remove_tw_username)
comb$text_precleaned<-lapply(comb$text_precleaned,FUN=remove_email)


# ================= Clean latin character  =============================================
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


# ================= make all text into a corpus ========================================
# convert string to vector of words
combtext<-VCorpus(VectorSource(comb$text_precleaned))
#combtext[[1]]$content

# compute TF-IDF matrix and inspect sparsity
combtext.tfidf = DocumentTermMatrix(combtext, control = list(weighting = weightTfIdf))
# non-/sparse entries indicates how many of the DTM cells are non-zero and zero, respectively.
# sparsity is number of non-zero cells divided by number of zero cells.

# inspect sub-matrix:  first 5 documents and first 5 terms
#combtext.tfidf[1:5,1:5]
#as.matrix(combtext.tfidf[1:5,1:5])

# customize stopwords to be removed & exclude following reversing words
#"isn't"      "aren't"     "wasn't"     "weren't"   
#"hasn't"     "haven't"    "hadn't"     "doesn't"    "don't"      "didn't"     "won't"     
#"wouldn't"   "shan't"     "shouldn't"  "can't"      "cannot"     "couldn't"   "mustn't" 
mystopwords<-stopwords("english")[c(1:80,99:174)]

# there's a lot in the documents that we don't care about. clean up the corpus.
combtext.clean = tm_map(combtext, stripWhitespace)                          # remove extra whitespace
combtext.clean = tm_map(combtext.clean, removeNumbers)                      # remove numbers
combtext.clean = tm_map(combtext.clean, removePunctuation)                  # remove punctuation
combtext.clean = tm_map(combtext.clean, content_transformer(tolower))       # ignore case
combtext.clean = tm_map(combtext.clean, removeWords, mystopwords)           # remove stop words
combtext.clean = tm_map(combtext.clean, stemDocument)                       # stem all words

# compare original content of document 1 with cleaned content
#combtext[[1]]$content
#combtext.clean[[4]]$content  # do we care about misspellings resulting from stemming?

# recompute TF-IDF matrix
combtext.clean.tfidf = DocumentTermMatrix(combtext.clean, control = list(weighting = weightTfIdf))

# reinspect the first 5 documents and first 5 terms
#combtext.clean.tfidf[1:5,1:5]
#as.matrix(combtext.clean.tfidf[1:5,1:5])

# we've still got a very sparse document-term matrix. remove sparse terms at various thresholds.
#remove terms that are absent from at least 
tfidf.99 = removeSparseTerms(combtext.clean.tfidf, 0.99)  # ~99% of documents 110 terms left
tfidf.98 = removeSparseTerms(combtext.clean.tfidf, 0.98)  # ~98% of documents 110 terms left
tfidf.95 = removeSparseTerms(combtext.clean.tfidf, 0.95)  # ~95% of documents 8 terms left
tfidf.90 = removeSparseTerms(combtext.clean.tfidf, 0.90)  # ~90% of documents 5 terms left
tfidf.80 = removeSparseTerms(combtext.clean.tfidf, 0.80)  # ~80% of documents 3 terms left
tfidf.70 = removeSparseTerms(combtext.clean.tfidf, 0.70)  # ~70% of documents. 2 terms left
#inspect(tfidf.99)
#as.matrix(tfidf.70[1:5,])
#combtext.clean[[1]]$content


# ================= convert to df ======================================================
combtext.clean.df<-as.data.frame(as.matrix(DocumentTermMatrix(combtext.clean)), stringsAsFactors=False)
comb_clean<-cbind(comb,combtext.clean.df)


# ================= check if df contains certain punctuation(eg: !, ?) =================
comb_clean$qn_mark<-as.integer(grepl("?",comb_clean$text_precleaned))
comb_clean$ex_mark<-as.integer(grepl("!",comb_clean$text_precleaned))


# ================= calculate entropy (unigram)=========================================
#http://finzi.psych.upenn.edu/library/DescTools/html/Entropy.html
myx<-comb_clean[comb_clean$dataset=="train",6:ncol(comb_clean)]
myy<-comb_clean$sentiment[comb_clean$dataset=="train"]
entropy_list<-apply(myx,2,FUN=entropy,myy)

#sort(entropy_list,decreasing = TRUE)
high_entropy_list.uni<-entropy_list[entropy_list>2]
high_entropy_list.uni<-high_entropy_list.uni[!is.na(high_entropy_list.uni)]

# ================= bigrams ============================================================
#http://tm.r-forge.r-project.org/faq.html#Bigrams
BigramTokenizer <-
  function(x)
    unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)

tdm <- TermDocumentMatrix(combtext.clean, control = list(tokenize = BigramTokenizer))
bi.tdm<-removeSparseTerms(tdm, 0.99) #20 terms for 0.99 sparse bigrams
#inspect(bi.tdm)

# ================= trigrams ===========================================================
BigramTokenizer3 <-
  function(x)
    unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)

tdm3 <- TermDocumentMatrix(combtext.clean, control = list(tokenize = BigramTokenizer3))
tri.tdm<-removeSparseTerms(tdm3, 0.99) #10 terms
#inspect(tri.tdm)

combtext.clean.df.bi<-as.data.frame(t(as.matrix(bi.tdm)), stringsAsFactors=FALSE)
combtext.clean.df.tri<-as.data.frame(t(as.matrix(tri.tdm)), stringsAsFactors=FALSE)
comb_clean<-cbind(comb_clean,combtext.clean.df.bi,combtext.clean.df.tri)

# ================= calculate entropy (bigram)==========================================
full.df.bi<-as.data.frame(t(as.matrix(tdm)), stringsAsFactors=FALSE)
comb_full.df.bi<-cbind(comb,full.df.bi) #data frame with all bigrams

myx<-comb_full.df.bi[comb_full.df.bi$dataset=="train",6:ncol(comb_clean)]
myy<-comb_full.df.bi$sentiment[comb_full.df.bi$dataset=="train"]
entropy_list<-apply(myx,2,FUN=entropy,myy)
#sort(entropy_list,decreasing = TRUE)
high_entropy_list.bi<-entropy_list[entropy_list>2]
high_entropy_list.bi<-high_entropy_list.bi[!is.na(high_entropy_list.bi)]

hi_ent_df.bi<-comb_full.df.bi[,colnames(comb_full.df.bi) %in% names(high_entropy_list.bi)]

# ================= calculate entropy (trigram)=========================================
full.df.tri<-as.data.frame(t(as.matrix(tdm3)), stringsAsFactors=FALSE)
comb_full.df.tri<-cbind(comb,full.df.tri)

myx<-comb_full.df.tri[comb_full.df.tri$dataset=="train",6:ncol(comb_clean)]
myy<-comb_full.df.tri$sentiment[comb_full.df.tri$dataset=="train"]
entropy_list<-apply(myx,2,FUN=entropy,myy)
#sort(entropy_list,decreasing = TRUE)
high_entropy_list.tri<-entropy_list[entropy_list>2]
high_entropy_list.tri<-high_entropy_list.tri[!is.na(high_entropy_list.tri)]

hi_ent_df.tri<-comb_full.df.tri[,colnames(comb_full.df.tri) %in% names(high_entropy_list.tri)]

# ================= combine  df ========================================================
names(high_entropy_list.tri) %in% colnames(comb_clean) # high_ent_tri all NOT in comb_clean.
names(high_entropy_list.bi) %in% colnames(comb_clean) # some high_ent_bi NOT in comb_clean.

addition_bi<-high_entropy_list.bi[!names(high_entropy_list.bi) %in% colnames(comb_clean)]
addition_bi.df<-full.df.bi[,colnames(full.df.bi) %in% names(addition_bi)]  #12
addition_tri.df<-full.df.tri[,colnames(full.df.tri) %in% names(high_entropy_list.tri)] #3

comb_clean_hi_ent<-comb
comb_clean_hi_ent<-cbind(comb_clean_hi_ent,combtext.clean.df[,colnames(combtext.clean.df) %in% names(high_entropy_list.uni)])
comb_clean_hi_ent<-cbind(comb_clean_hi_ent,hi_ent_df.bi,hi_ent_df.tri)

comb_clean<-cbind(comb_clean,addition_bi.df,addition_tri.df) 

############# df ready for analysis ############
#1) comb_clean: includes all unigrams, .99sparse bigrams, high_entrophy bigrams, .99spase trigrams, high_entrophy trigrams. 
#2) comb_clean_hi_ent: includes all high entrophy varaibles (entrophy>2)

# ================= .99 sparse words column index in comb_clean=========================
sparse_99_col_index<-which(colnames(comb_clean) %in% colnames(as.matrix(tfidf.99)))

# ================= high entropy for unigram column index in comb_clean=================
high_entropy_col_index.uni<-which(colnames(comb_clean) %in% names(high_entropy_list.uni))

# ================= optional: sentiment words column index =============================
#stem the library of sentiments words
sentiments_corp<-VCorpus(VectorSource(sentiments))
sentiments_corp<-tm_map(sentiments_corp,stemDocument)
sentiments_corp.df<-as.data.frame(as.matrix(DocumentTermMatrix(sentiments_corp)), stringsAsFactors=False)

#identify the sentiment words in comb_clean
sentiment_col_index<-which(colnames(comb_clean) %in% colnames(sentiments_corp.df))
sentiment_col_index<-sentiment_col_index[2:length(sentiment_col_index)]
names(comb_clean[,sentiment_col_index])
length(sentiment_col_index)


# ======================================================================================
#                           Linear Model
# ======================================================================================

# ==================== Split data from test======================================================
set.seed(1)
mytrain_hi_ent<-comb_clean_hi_ent[comb_clean_hi_ent$dataset=="train",]

# ==================== lm submission with comb_clean_hi_ent ============================
lm.all_high_ent<-lm(sentiment ~., data=mytrain_hi_ent[,c(3,6:ncol(mytrain_hi_ent))])
summary(lm.all_high_ent)#Adjusted R-squared:  0.1623

#Select variables with a significant p-value
coef_lst<-summary(lm.all_high_ent)$coefficients[,4]
sig_coef_col_index<-which(colnames(mytrain_hi_ent) %in% names(coef_lst[coef_lst<0.05]))

#retrain model on concentrated set of columns
lm.all_high_ent<-lm(sentiment ~., data = mytrain_hi_ent[,c(3,sig_coef_col_index)])#Adjusted R-squared:  0.1438

# ==================== LOOCV ============================
cv.lm(mytrain_hi_ent[,c(3,sig_coef_col_index)], lm.all_high_ent, m=nrow(mytrain_hi_ent), dots = 
        FALSE, seed=23, plotit=TRUE, printit=TRUE) # Mean Square Error : 0.549

# ==================== prediction with test set for submission ============================
preds.all_high_ent <- round(predict(lm.all_high_ent,newdata = comb_clean_hi_ent[comb_clean_hi_ent$dataset=="test",c(3,sig_coef_col_index)]))
summary(preds.all_high_ent) #since there is 0, make it to 1
preds.all_high_ent[preds.all_high_ent==0]<-1

#Create submission
all_high_ent_table<-data.frame(test$id, preds.all_high_ent)
write.table(all_high_ent_table, file="entropy2.csv", row.names=F, col.names=c("id", "sentiment"), sep=",")

# ======================================================================================
#                           K-NN
# ======================================================================================

#Create mode function
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

train <- comb_clean_hi_ent[comb_clean_hi_ent$dataset == 'train', 6:ncol(comb_clean_hi_ent)]
test <- comb_clean_hi_ent[comb_clean_hi_ent$dataset == 'test', 6:ncol(comb_clean_hi_ent)]

knn_j <- function(x){
  tr_matrix <- rbind(train,x)
  dist_matrix <- as.matrix(dist(tr_matrix))
  diag(dist_matrix) <- NA
  nearest <- head(order(dist_matrix[,ncol(dist_matrix)]), n=k) %>% comb_clean_hi_ent[.,]
  score <- Mode(nearest$sentiment)
  return(score)
}

#Fianl choice for K
k <- 15
#Convert sentiment to numeric and apply
sentiment <- numeric()
sentiment <- apply(test, 1, knn_j)

#Create submission
id <- seq(1:979)
submit <- as.data.frame(cbind(id,sentiment))
write_csv(submit, 'knnsubmission.csv')
table(sentiment)

# ======================================================================================
#                           Decision Tree (optional)
# ======================================================================================
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
