setwd("/Users/Pan/Google Drive/Data Science/SYS 6018/sys6018-competition-twitter-sentiment")
library(tm)
library(tidytext) #sentiment words library
library(rpart)     #decision tree

# ========================================================================
#                                  Data Cleaning
# ========================================================================

# ================= read data & create comb =============================================
train<-read.csv("train.csv")
test<-read.csv("test.csv")
train$dataset<-"train"
test$dataset<-"test"
test$sentiment<-NA
train$id<-NA
comb<-rbind(train,test)
comb<-comb[c("id","dataset","sentiment","text")]


# =================== Preclean with gsub =================================================
# clean url, twitter tag, twitter username, email
remove_url<-function(x){gsub(" ?(f|ht)(tp)s?(://)(\\S*)[./](\\S*)", "", x)}
remove_tw_tag<-function(x){gsub("RT |via ", "", x)}
remove_tw_username<-function(x){gsub("[@][a-zA-Z0-9_]{1,15}", "", x)}
remove_email<-function(x){gsub("\\b[A-Za-z0-9._-]*[@](.*?)[.].{1,3}\\b", "", x)}

comb$text_precleaned<-lapply(comb$text,FUN=remove_url)
comb$text_precleaned<-lapply(comb$text_precleaned,FUN=remove_tw_tag)
comb$text_precleaned<-lapply(comb$text_precleaned,FUN=remove_tw_username)
comb$text_precleaned<-lapply(comb$text_precleaned,FUN=remove_email)


# =================== Clean latin character  =========================================
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


# ================make all text into a corpus ====================================================
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


# =============== optional: sentiment words column index ========================================
#stem the library of sentiments words
sentiments_corp<-VCorpus(VectorSource(sentiments))
sentiments_corp<-tm_map(sentiments_corp,stemDocument)
sentiments_corp.df<-as.data.frame(as.matrix(DocumentTermMatrix(sentiments_corp)), stringsAsFactors=False)

#identify the sentiment words in comb_clean
sentiment_col_index<-which(colnames(comb_clean) %in% colnames(sentiments_corp.df))
sentiment_col_index<-sentiment_col_index[2:length(sentiment_col_index)]
names(comb_clean[,sentiment_col_index])
length(sentiment_col_index)

# ============== bigrams ======================================================================
#http://tm.r-forge.r-project.org/faq.html#Bigrams
BigramTokenizer <-
  function(x)
    unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)

tdm <- TermDocumentMatrix(combtext.clean, control = list(tokenize = BigramTokenizer))
bi.tdm<-removeSparseTerms(tdm, 0.99) #20 terms
#inspect(bi.tdm)

# ============== trigrams ======================================================================
BigramTokenizer3 <-
  function(x)
    unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)

tdm3 <- TermDocumentMatrix(combtext.clean, control = list(tokenize = BigramTokenizer3))
tri.tdm<-removeSparseTerms(tdm3, 0.99) #10 terms
#inspect(tri.tdm)

# =============== convert to df ===============================================================
combtext.clean.df<-as.data.frame(as.matrix(DocumentTermMatrix(combtext.clean)), stringsAsFactors=False)
comb_clean<-cbind(comb,combtext.clean.df)

combtext.clean.df.bi<-as.data.frame(t(as.matrix(bi.tdm)), stringsAsFactors=FALSE)
combtext.clean.df.tri<-as.data.frame(t(as.matrix(tri.tdm)), stringsAsFactors=FALSE)
comb_clean<-cbind(comb_clean,combtext.clean.df.bi,combtext.clean.df.tri)

# =============== check if df contains certain punctuation(eg: !, ?) ===============================
comb_clean$qn_mark<-as.integer(grepl("?",comb_clean$text_precleaned))
comb_clean$ex_mark<-as.integer(grepl("!",comb_clean$text_precleaned))

# ============== .99 sparse words column index =====================================================
sparse_99_col_index<-which(colnames(comb_clean) %in% colnames(as.matrix(tfidf.99)))

# =============== calculate entropy =======================================
#http://finzi.psych.upenn.edu/library/DescTools/html/Entropy.html
install.packages("entropy")
library(entropy)
myx<-comb_clean[comb_clean$dataset=="train",6:ncol(comb_clean)]
myy<-comb_clean$sentiment[comb_clean$dataset=="train"]
entropy_list<-apply(myx,2,FUN=entropy,myy)
#sort(entropy_list,decreasing = TRUE)
high_entropy_list.uni<-entropy_list[entropy_list>2]
high_entropy_list.uni<-high_entropy_list.uni[!is.na(high_entropy_list.uni)]

high_entropy_col_index.uni<-which(colnames(comb_clean) %in% names(high_entropy_list.uni))

library(boot)
library(DAAG)
library(caret)

# ----- bi -----
full.df.bi<-as.data.frame(t(as.matrix(tdm)), stringsAsFactors=FALSE)
comb_full.df.bi<-cbind(comb,full.df.bi)

myx<-comb_full.df.bi[comb_full.df.bi$dataset=="train",6:ncol(comb_clean)]
myy<-comb_full.df.bi$sentiment[comb_full.df.bi$dataset=="train"]
entropy_list<-apply(myx,2,FUN=entropy,myy)
#sort(entropy_list,decreasing = TRUE)
high_entropy_list.bi<-entropy_list[entropy_list>2]
high_entropy_list.bi<-high_entropy_list.bi[!is.na(high_entropy_list.bi)]

hi_ent_df.bi<-comb_full.df.bi[,colnames(comb_full.df.bi) %in% names(high_entropy_list.bi)]

# ----- tri -----
full.df.tri<-as.data.frame(t(as.matrix(tdm3)), stringsAsFactors=FALSE)
comb_full.df.tri<-cbind(comb,full.df.tri)

myx<-comb_full.df.tri[comb_full.df.tri$dataset=="train",6:ncol(comb_clean)]
myy<-comb_full.df.tri$sentiment[comb_full.df.tri$dataset=="train"]
entropy_list<-apply(myx,2,FUN=entropy,myy)
#sort(entropy_list,decreasing = TRUE)
high_entropy_list.tri<-entropy_list[entropy_list>2]
high_entropy_list.tri<-high_entropy_list.tri[!is.na(high_entropy_list.tri)]

hi_ent_df.tri<-comb_full.df.tri[,colnames(comb_full.df.tri) %in% names(high_entropy_list.tri)]

#============ convert to df ==================
names(high_entropy_list.tri) %in% colnames(comb_clean) # high_ent_tri NOT in comb_clean.
names(high_entropy_list.bi) %in% colnames(comb_clean) # some high_ent_bi NOT in comb_clean.

addition_bi<-high_entropy_list.bi[!names(high_entropy_list.bi) %in% colnames(comb_clean)]
addition_bi.df<-full.df.bi[,colnames(full.df.bi) %in% names(addition_bi)]  #12
addition_tri.df<-full.df.tri[,colnames(full.df.tri) %in% names(high_entropy_list.tri)] #3

comb_clean_hi_ent<-comb
comb_clean_hi_ent<-cbind(comb_clean_hi_ent,combtext.clean.df[,colnames(combtext.clean.df) %in% names(high_entropy_list.uni)])
comb_clean_hi_ent<-cbind(comb_clean_hi_ent,hi_ent_df.bi,hi_ent_df.tri)

comb_clean<-cbind(comb_clean,addition_bi.df,addition_tri.df) #with all uni+sparse_bi + high_ent_bi+ 
                                                            #spacse_tri+ high_ent_tri
# ========================================================================
#                                  ANALYSIS
# ========================================================================
# ==================== Split data =====================
set.seed(1)
mytrainrows<-sample(1:nrow(train),0.7*nrow(train))
mytrain<-comb_clean[comb_clean$dataset=="train",][mytrainrows,]
myvalid<-comb_clean[comb_clean$dataset=="train",][-mytrainrows,]

mytrain_hi_ent<-comb_clean_hi_ent[comb_clean_hi_ent$dataset=="train",][mytrainrows,]
myvalid_hi_ent<-comb_clean_hi_ent[comb_clean_hi_ent$dataset=="train",][-mytrainrows,]

# ==================== all.sig =================
lm.all<-lm(sentiment ~., data=mytrain[,c(3,6:ncol(mytrain))])
summary(lm.all) # Adjusted R-squared:  0.7767
coef_lst<-summary(lm.all)$coefficients[,4]
sig_coef_col_index<-which(colnames(comb_clean) %in% names(coef_lst[coef_lst<0.05]))

lm.all.sig<-lm(sentiment~., data=mytrain[,c(3,sig_coef_col_index)])
summary(lm.all.sig) #Adjusted R-squared:  0.09475 
preds.all.sig <- predict(lm.all.sig,newdata = myvalid[,c(3,sig_coef_col_index)])
sum(round(preds.all.sig)==myvalid$sentiment)/nrow(myvalid) #0.5966102

# ==================== high_ent.sig =================
lm.high_ent<-lm(sentiment ~., data=mytrain_hi_ent[,c(3,6:ncol(mytrain_hi_ent))])
summary(lm.high_ent) # Adjusted R-squared:  0.1461 

coef_lst<-summary(lm.high_ent)$coefficients[,4]
sig_coef_col_index<-which(colnames(mytrain_hi_ent) %in% names(coef_lst[coef_lst<0.05]))

lm.high_ent.sig<-lm(sentiment~., data=mytrain_hi_ent[,c(3,sig_coef_col_index)])
summary(lm.high_ent.sig) #Adjusted R-squared:  0.1456 
preds.high_ent.sig <- predict(lm.high_ent.sig,newdata = myvalid_hi_ent[,c(3,sig_coef_col_index)])
sum(round(preds.high_ent.sig)==myvalid_hi_ent$sentiment)/nrow(myvalid_hi_ent) #0.6101695


# ============== high uni entropy ===========================

lm1<-lm(sentiment ~.,data=mytrain[,c(3,high_entropy_col_index.uni)])
summary(lm1)                                             #TRAIN: Adjusted R-squared:  0.1697 
preds <- predict(lm1,newdata = myvalid[,c(3,high_entropy_col_index.uni)])
sum(round(preds)==myvalid$sentiment)/nrow(myvalid) #0.3932203


coef_lst<-summary(lm1)$coefficients[,4]
sig_coef_col_index<-which(colnames(comb_clean) %in% names(coef_lst[coef_lst<0.05]))

lm2<-lm(sentiment ~.,data=mytrain[,c(3,sig_coef_col_index)])
summary(lm2)                                             #TRAIN: Adjusted R-squared:  0.1793 
preds2 <- predict(lm2,newdata = myvalid[,c(3,sig_coef_col_index)])
sum(as.integer(preds2)==myvalid$sentiment)/nrow(myvalid) #0.6

# ==================== all train + high uni entropy ========================
lm1<-lm(sentiment ~.,data=comb_clean[comb_clean$dataset=="train",c(3,high_entropy_col_index)])
summary(lm1)                                             #TRAIN: Adjusted R-squared:  0.1697 

coef_lst<-summary(lm1)$coefficients[,4]
sig_coef_col_index<-which(colnames(comb_clean) %in% names(coef_lst[coef_lst<0.05]))

lm2<-lm(sentiment ~.,data=comb_clean[comb_clean$dataset=="train",c(3,sig_coef_col_index)])
summary(lm2)                                             #TRAIN: Adjusted R-squared:  0.165
preds2 <- round(predict(lm2,newdata = comb_clean[comb_clean$dataset=="test",,c(3,sig_coef_col_index)]))

final_table<-data.frame(test$id, preds2)
# Write files
write.table(final_table, file="entropy.csv", row.names=F, col.names=c("id", "sentiment"), sep=",")

# ==================== all train + high all uni entropy ========================
lm.high_ent.all<-lm(sentiment ~., data=comb_clean_hi_ent[,c(3,6:ncol(comb_clean_hi_ent))])
summary(lm.high_ent.all)                                 #TRAIN: Adjusted R-squared: 0.1623 

coef_lst<-summary(lm.high_ent.all)$coefficients[,4]
sig_coef_col_index<-which(colnames(comb_clean_hi_ent) %in% names(coef_lst[coef_lst<0.05]))

lm.high_ent.all.sig<-lm(sentiment ~.,data=comb_clean_hi_ent[comb_clean_hi_ent$dataset=="train",c(3,sig_coef_col_index)])
summary(lm.high_ent.all.sig)                             #TRAIN: Adjusted R-squared:  0.1438 
preds.high_ent.all <- round(predict(lm.high_ent.all.sig,newdata = comb_clean_hi_ent[comb_clean_hi_ent$dataset=="test",,c(3,sig_coef_col_index)]))

final_table<-data.frame(test$id, preds.high_ent.all)
write.table(final_table, file="entropy.csv", row.names=F, col.names=c("id", "sentiment"), sep=",")
