# sys6018-competition-twitter-sentiment
Kaggle Competition Link: https://inclass.kaggle.com/c/twitter-sentiment-analysis-self-driving-cars
  
## Team Members & Roles:
|    Name         | Computing ID  |    Role       |
| -------------   | ------------- | ------------- |
| Huitong(Jo) Pan | hp4zw         | data preprocessing, entrophy with lm, decision tree, organize code & github    |
| Abhijith Mandya | am6ku         | lm and code organization           |
| Jack Prominski  | jmp3mk        | knn           |

## Best Models & Scores:

|   Model Name   | Kaggle Score |   
| -------------   | ------------- | 
| lm | .69120  |
| knn | .59100 | 

## Data exploration & Data cleaning 
We explored unigram, bigram and trigram variables along with different levels of sparsicity and entropy relations.

We cleanned the following:
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

entrophy: please check https://cran.r-project.org/web/packages/entropy/entropy.pdf

## statistical modeling methods
###Linear Model
Since we had such sparse data, less flexible models like the simple linear models provide better predicitons as witnessed by our submissions also. We tested multiple slices of data; right from all variables to a select set of combinations between the entropy, unigram and bigram varibles. At last the combination won. The R^2 values were hardly of any use since the data was so sparse.

###K-NN


## Validation approach 
cross-validation - K-NN. Since the model itself took very long to run, we chose to test different values of K using pre determined folds
leave-one-out-validation  - The chosen lm model was easily the best candidate for LOOCV since it took considerably less time to run   

## Reflections
1. Who might care about this problem and why?    
PR and Marketing departments in companies dealing with self-driving cars might care about this problem, because it represents the public's perception at any given time and could help tailor their message. 
    
2. Why might this problem be challenging?    
Tweets are not representative of the general population. Since tweets have far less structural integrity in terms of language rules,  it becomes harder to classify and predict using this data. 
  
3. What other problems resemble this problem?    
Sentiment analysis for election candidates.    
Sentiment analysis with customers' comments linked to scaled qualitative answers like # of stars.
