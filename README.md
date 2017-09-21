# sys6018-competition-twitter-sentiment
Kaggle Competition Link: https://inclass.kaggle.com/c/twitter-sentiment-analysis-self-driving-cars
  
# Team Members & Roles:
|    Name         | Computing ID  |    Role       |
| -------------   | ------------- | ------------- |
| Huitong(Jo) Pan | hp4zw         | data preprocessing, entrophy with lm, decision tree, organize code & github    |
| Abhijith Mandya | am6ku         | lm            |
| Jack Prominski  | jmp3mk        | knn           |

# Best Models & Scores:

|   Model Name   | Kaggle Score |   
| -------------   | ------------- | 
| lm | .69120  |
| knn | .59100 | 

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
Public relation department in companies dealing with self-driving cars might care about this problem, because it represents public's perception and helps tailoring their marketing message.
 
2. Why might this problem be challenging?
Twitter can represent some groups of people's opinions. This does not include many elderlies' and non-frequent-twitter-users' opinions.

3. What other problems resemble this problem?
Sentiment analysis for election candidates.
Sentiment analysis with customers' comments.
