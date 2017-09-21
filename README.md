# sys6018-competition-twitter-sentiment
Kaggle Competition Link: https://inclass.kaggle.com/c/twitter-sentiment-analysis-self-driving-cars
  
# Team Members & Roles:
|    Name         | Computing ID  |    Role       |
| -------------   | ------------- | ------------- |
| Huitong(Jo) Pan | hp4zw         | data preprocessing, entrophy with lm, decision tree, organize code              |
| Abhijith Mandya | Content Cell  | lm            |
| Jack Prominski  | Content Cell  | knn           |

# Data exploration & Data cleaning 
we explored unigram, bigram and trigram variables.

we cleanned the following:
1. url
2. twitter tag
3. twitter username
4. email
5. non-english characters (latin characters)
6. customized stopwords (excluding stopwods with 'not')
7. whitespace
8. numbers
9. punctuation
10. uppercase

Then we stemmed the word document with tm package.

Our final data frames for analysis:
1. comb_clean: includes all unigrams, .99sparse bigrams, high_entrophy bigrams, .99spase trigrams, high_entrophy trigrams. 
2. comb_clean_hi_ent: includes all high entrophy varaibles (entrophy>2)

# statistical modeling methods
linear model and k-nn

# model selection approach 
cross-validation
leave-one-out-validation

# the reflection questions
1. Who might care about this problem and why?
 
2. Why might this problem be challenging?

3. What other problems resemble this problem?
