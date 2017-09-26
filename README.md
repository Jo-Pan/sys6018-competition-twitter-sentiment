# sys6018-competition-twitter-sentiment
This is part of the Kaggle Competition to [analyze Twitter sentiments for self driving cars](https://inclass.kaggle.com/c/twitter-sentiment-analysis-self-driving-cars)
  
## Team Members & Roles:
|    Name         | Computing ID  |    Role       |
| -------------   | ------------- | ------------- |
| Huitong(Jo) Pan | hp4zw         | Data preprocessing, entrophy with lm, decision tree, organize code & github    |
| Abhijith Mandya | am6ku         | Linear Models, code organization and Readme           |
| Jack Prominski  | jmp3mk        | K-NN           |

## Best Models & Scores:

|   Model Name   | Kaggle Public Score |   Kaggle Private Score |   
| -------------   | ------------- | ------------- | 
| Linear Model | **0.69120**  | 0.65102  |
| K-NN | **0.59100** | 0.52244 |
| Decision Tree | **0.68098** | 0.66122  |

## Data exploration & Data cleaning 
We explored unigram, bigram and trigram variables along with different levels of sparsicity and entropy relations.

[Entropy Documentation](https://cran.r-project.org/web/packages/entropy/entropy.pdf)

[n-grams Documentation](http://tm.r-forge.r-project.org/faq.html#Bigrams)

We cleanned the following:
1. URLs
2. Twitter Tags
3. Usernames
4. Email-ids
5. Non-english characters (latin characters)
6. Customized stopwords (excluding stopwods with 'not')
7. Whitespaces
8. Numbers
9. Punctuation
10. Uppercase

We then stemmed the word document using the tm package.

Our final data frames for analysis:
1. comb_clean: includes all unigrams, .99sparse bigrams, high_entrophy bigrams, .99spase trigrams, high_entrophy trigrams. 
2. comb_clean_hi_ent: includes all high entrophy varaibles (entrophy>2)

## Statistical modeling methods
### Linear Model
Since we had such sparse data, less flexible models like the simple linear models provide better predicitons as witnessed by our submissions also. We tested multiple slices of data; right from all variables to a select set of combinations between the entropy, unigram and bigram varibles. At last the combination won. The R^2 values were hardly of any use since the data was so sparse.

### K-NN
The non parametric model that we built from scratch proved to be far more time consuming and less capable of accurately predicting sentiments. We tested multiple K-values and cross validated the best one at K = 15

### Deicison Trees
We also ran decision trees to understand see if we can improve our scores from the linear model but even this complex model wasn't good enough.

## Validation approach 
cross-validation - K-NN. Since the model itself took very long to run, we chose to test different values of K using pre determined folds
leave-one-out-validation  - The chosen lm model was easily the best candidate for LOOCV since it took considerably less time to run   

## Reflections
1. **Who might care about this problem and why?**    
PR and Marketing departments are always on the look out for the public sentiment towards their products, because it seems to have become representative of the public's perception. City councils and larger governmental organizations that are planning the inevitable introduction of these self-driving cars onto our streets can benefit by undertsanding the public sentiment in their local constituencies to ascertian how successful the adoption of these cars will be. 
    
2. **Why might this problem be challenging?**    
Tweets are not representative of the general population. They are a small concentrated sample set of people who typically use Twitter as a social media platform. Time and again we've seen that such places become echo chambers for specific opinions and perceptions that tend to have polarizing effects. Further, All predictive models depend on pattern recognitions and need some form of structured data gain predictive capability. Since tweets have far less structural integrity in terms of lingual rules and semantics, it becomes harder to classify and predict using them.
  
3. **What other problems resemble this problem?**    
Sentiment analysis for election candidates across media outlets seems like a similar problem with lower classification intervals but more heterogeneous data sources complicating it.   
Sentiment analysis with customers' comments linked to scaled qualitative answers like # of stars are very similar to this and are probably easier to perform as the comments usually are written in more complete sentences thus making it easier to predict.
