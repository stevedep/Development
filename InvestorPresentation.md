Text prediction using Ngrams, Product Presentation
========================================================
author: Steve de Peijper
date: 30-12-2016

Introduction
========================================================

This presentation shows the final text prediction product which was created during the Data Science Capstone project. The product was designed to be:

- Fast / responsive and require little storage
- Simple to use (on mobile device)

The next slides will explain;

- The approach
- Algorithm and fine tuning
- Results and recommenations

Approach
========================================================
left: 60%
- Load sample texts (news, blog and twitter).  
- Cleansing: to lower case, parse sentence and remove non regular characters.
- Create 5,4,3,2 and single grams. 
- Only load ngrams that cover 80% of total frequency (approx 30% of ngrams), see illustration. 
- Implement algorithm for tuning and unseen ngrams (next slide).
- Used AWS Cloud server (Rstudio AMI) with parallel computing. 

***
![alt text](roc.png)


Algorithm
========================================================

Katz Back-off Model was used to deal with unseen ngrams. When f.e. no 5gram was found, an attempt will be made with a 4 gram, etc. Finally the word 'the' will be presented if all else fails. 

Additionaly, Interpolation by Jelinek and Mercer (1980) was used. This method uses the probability of a lower ngram when determining the probability. The probability of the lower ngram is given a weight (lambda). 

Supprisingly the interpolation method did not increase accuracy! Accuracy improved a bit when giving a 4gram a bit more weight when using a 5gram. 
Many values were evaluated by incrementally adding more weight to the lower ngrams. 

Results and recommenations
========================================================

The application can be viewed here. 
