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

- Load sample texts (news, blog and twitter).  
- Cleansing: to lower case and remove non regular characters.
- Create 5,4,3,2 and single grams. 
- Only load ngrams that cover 80% of total frequency (approx 30% of ngrams), see illustration. 
- Implement algorithm for tuning and unseen ngrams (next slide).

![alt text](roc.png)

Algorithm
========================================================

Katz Back-off Model was used to deal with unseen ngrams. When f.e. no 5gram was found, an attempt will be made with a 4 gram, etc. 