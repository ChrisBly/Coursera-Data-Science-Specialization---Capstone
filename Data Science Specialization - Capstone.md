Coursera Data Science Specialization - Capstone
========================================================
author: Chris Blycha
date: "05/10/2018"
font-family: Risque
width: 1440
height: 1440
css: custom.css

Overview
========================================================
type: section
<small>
The objective of this capstone project is to develop a Shiny app that can predict the next word, like that used in mobile keyboards applications implemented by the Swiftkey.

There are many tasks to be realized such as:
- (1) Understanding the problem, getting and cleaning the data;
- (2) Making of Exploratory Data Analysis (EDA);
- (3) Tokenization of words and predictive text mining;
- (4) Writing a milestone project and a prediction model;
- (5) Developing a shiny application and Writing the Pitch.

The data came from HC Corpora with three files (Blogs, News and Twitter). The data was cleaned, processed, tokenized, and n-grams are created. 

Link to the Shiny App: https://chris1234oz.shinyapps.io/Capstone_Word_Predictor/ 

The code used to create this application is here: https://github.com/ChrisBly/Coursera-Data-Science-Specialization---Capstone.git



</small>

The Shiny Word Prediction Application:
========================================================
type: section
<small>
The Word Prediction application uses the a 30% sample from each of the HC Corpora Blogs, News and Twitter. This data is breakdown into segments. These segments are called Ngrams. This application is design for Mobile(cell) phone use & has a limited ngram size.

Ngrams defined:
In the fields of computational linguistics and probability, an n-gram is a contiguous sequence of n items from a given sample of text or speech. The items can be syllables, letters, words or base pairs according to the application
N-gram of size used to create this word prdiction application are:
- A bi-gram is two words
- A Tri-gram is three words
- A Quad-Gram is four words
Refer to the link for more information: https://en.wikipedia.org/wiki/N-gram

The algorithm used to make the prediction application:
Katz's back-off model - How does it work: 

Desciption provided by Wikipedia: 

Refer to this link: https://en.wikipedia.org/wiki/Katz%27s_back-off_model

Katz back-off is a generative n-gram language model that estimates the conditional probability of a word given its history in the n-gram. It 
accomplishes this estimation by backing off through progressively shorter history models under certain conditions[1]. By doing so, the model with the most reliable information about a given history is used to provide the better results. Essentially, this means that if the n-gram has been seen more than k times in training, the conditional probability of a word given its history is proportional to the maximum likelihood estimate of that n-gram. Otherwise, the conditional probability is equal to the back-off conditional probability of the (n − 1)-gram. The application uses 0.5 amount of discounting for Good–Turing estimation(Discounting).
</small>

How does the application use the Katz's back-off algorithm:
========================================================
type: section
<small>
This application uses Katz-Backoff algorithm which is one of the Natrual Language Processing techniques that can be used for predicting the next word.

This application is design for Mobile(Cell phone) use. To reduce load time, the ngrams data has been reduced. 
-1: The word length determines which type of ngram is used. 3 words entered it uses Quad-Gram, then backoff when two word are entered to Tri-gram, then backs off to the where one word is entered.
-2: The application predicter is trigged once a word has been enter into section 3.
-3: There are 3 main word prediction functions & one cleaning function to clean the words entered & display them one the screen. The shiny server - observe function detect the amount of words enter & backoff to the correct function to predict the next word. 

The three main functions also include cleaning functions & detect the word length entered, then use the dplyr filter & mutate to extract ngrams with the target words & display them in a table. If no target word is found a message: "We dont have that word, please try again"" will be displayed.
Also if more than three words are enterd, a message: "Word Limit Exceed, please try again"" will be displayed. The application uses 0.5 amount of discounting for Good–Turing estimation(Discounting).
</small>
Instruction for using this application are shown below:
========================================================
type: section
<small>
-  Please enter your text here: in section 3
-  Once text has been enter the application will start
-  What you have entered will be displayed Section 2 
-  The predicted next word to be displayed Section 1"

</small>

