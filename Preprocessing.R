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
combtext[[1]]$content
combtext.clean[[1]]$content  # do we care about misspellings resulting from stemming?

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


# # =============== optional: sentiment words column index ========================================
# #stem the library of sentiments words
# sentiments_corp<-VCorpus(VectorSource(sentiments))
# sentiments_corp<-tm_map(sentiments_corp,stemDocument)
# sentiments_corp.df<-as.data.frame(as.matrix(DocumentTermMatrix(sentiments_corp)), stringsAsFactors=False)
# 
# #identify the sentiment words in comb_clean
# sentiment_col_index<-which(colnames(comb_clean) %in% colnames(sentiments_corp.df))
# sentiment_col_index<-sentiment_col_index[2:length(sentiment_col_index)]
# names(comb_clean[,sentiment_col_index])
# length(sentiment_col_index)

# ============== bigrams ======================================================================
#http://tm.r-forge.r-project.org/faq.html#Bigrams
BigramTokenizer <-function(x) unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)

tdm <- TermDocumentMatrix(combtext.clean, control = list(tokenize = BigramTokenizer))
bi.tdm<-removeSparseTerms(tdm, 0.99) #20 terms
#inspect(bi.tdm)

# ============== trigrams ======================================================================
BigramTokenizer3 <-function(x) unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)

tdm3 <- TermDocumentMatrix(combtext.clean, control = list(tokenize = BigramTokenizer3))
tri.tdm<-removeSparseTerms(tdm3, 0.99) #10 terms
#inspect(tri.tdm)


# =============== convert to df ===============================================================
combtext.clean.df<-as.data.frame(as.matrix(DocumentTermMatrix(combtext.clean)), stringsAsFactors=False)
comb_clean<-cbind(comb,combtext.clean.df)

# ============== .99 sparse words column index =====================================================
sparse_99_col_index<-which(colnames(comb_clean) %in% colnames(as.matrix(tfidf.99)))
comb_clean_sparse99 <- comb_clean[,c(2:3,sparse_99_col_index)]

combtext.clean.df.bi<-as.data.frame(t(as.matrix(bi.tdm)), stringsAsFactors=FALSE)
combtext.clean.df.tri<-as.data.frame(t(as.matrix(tri.tdm)), stringsAsFactors=FALSE)
comb_clean_final<-cbind(comb_clean_sparse99,combtext.clean.df.bi,combtext.clean.df.tri)

# # =============== check if df contains certain punctuation(eg: !, ?) ===============================
# comb_clean$qn_mark<-as.integer(grepl("?",comb_clean$text_precleaned))
# comb_clean$ex_mark<-as.integer(grepl("!",comb_clean$text_precleaned))

