Data Science Capstone Project
========================================================
author: Nima Taghidoust
date: 
autosize: true
font-family: "Rockwell"

Overview
========================================================

- This project is the assignment of the 10Th course of Data Science Specialization
of Johns Hopkins University.

- In this project we had 3 types of test from twitter, blogs & news and the target
of this project was to build a model that  guesses the next word based on the previous words of the sentence. 

- This is what the company Swift Key does when we are typing on the phone.


Data Cleansing
========================================================

In this part we did some data cleansing work:

- Delete unknown characters in English
- Lowercase all the letters
- Remove the punctuation marks & numbers
- Remove the profanity words




Exploratory Data Analysis
========================================================

- At first we used a sample of data to do some EDA.
- We built our corpus, tokens and n-grams. Then We deleted the n-grams which occurred only once.
- We built 2-gram, 3-gram, 4-gram & 5-gram based on the sampled data
- We got the most used n-grams in our 3 sources and also the percentage
of n-grams that have been used only once. That was around 60 percent of the n-grams!

Model's Algorithm
========================================================
- This model was built based on "Katz's back-off"
- We used "Good-Turing" for smoothing our algorithm

For example if the model has an input like "I do not love" ,it searches in 5-grams to find
5-grams with prefix exactly like the input (which has 4 words) and give probability to the 5Th words but it smooths the probability and gives the remaining probability to the lower
n-grams.
Finding the best lambda for reduced probability needed evaluation and cross-validation.


Data Product & Source Codes
========================================================

- You can see the R Codes in github in here.
- The result of the model is shown on R Shiny and you can type the sentences and our
model can suggest the top 5 words. The link to R Shiny is here.
