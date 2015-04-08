library(shiny)
shinyUI(navbarPage("JHU Capstone project shiny app-NLP in R",
                   tabPanel("page1-ngram prediction",
                            sidebarLayout(
                                    sidebarPanel(
                                            
                                            textInput("text", label=h3("Text input"),value = ""),
                                            radioButtons("radiobutton", label=h3("ngram-prediction:"),
                                                         choice=list("Bigram prediction"="bi",
                                                           "Trigram prediction"="tri",
                                                           "Quadgram prediction"="qua")),
                                            br(),
                                            submitButton("Update"),
                                            helpText("note: type text into text input and select which kind of ngram
                               you want to do prediction with, and then click update button to see.
                               For example: type :
                                                     we went to new york, 
                                                     happi mother, 
                                                     we go to see south carolina gamecock")),
                   
                                    mainPanel(
                                            
                                            h4("N-grams Prediction Output"),
                                            textOutput("textoutput")
                                            
                                    )
                            )
                   ),
                   
                   tabPanel("page2-Background of NLP and references", HTML('
                   <h3>What is n-gram? </h3>
                   <p>An n-gram model is a type of probabilistic language model for 
                  predicting the next item in such a sequence in the form of a (n − 1)–order Markov model.
                  In the fields of computational linguistics and probability, an n-gram is a contiguous sequence 
                  of n items from a given sequence of text or speech. The items can be phonemes, syllables, letters, 
                  words or base pairs according to the application. The n-grams typically are collected from a text 
                  or speech corpus. When the items are words, n-grams may also be called shingles.</p>
                   <h3>Types of n-gram? </h3>
                   <p>An n-gram of size 1 is referred to as a "unigram"; size 2 is a "bigram" (or, less commonly, a 
                   "digram"); size 3 is a "trigram". Larger sizes are sometimes referred to by the value of n, e.g., 
                   "four-gram", "five-gram", and so on.</p>
                   <img src="C:/Users/stephanie song/Desktop/ngram.jpg" />
                   <h3>Reference links </h3>
                   <ol>
                   <li><a href="http://gastonsanchez.com/Handling_and_Processing_Strings_in_R.pdf">handling string in R</a></li>
                   <li><a href="http://cran.r-project.org/web/views/NaturalLanguageProcessing.html">http://CRAN NLP</a></li>                             
                   </ol>



                    ')),
                   tabPanel("page3-Instruction & visualization", HTML('
                   <h3>Procedure of prediction </h3>
                   <ul>
                   <li>Read in twitts, news and blogs data and randomly select 2000 lines of each data(my laptop RAM is very limited).</li>
                   <li>Make corpus, clean the corpus, tokenized corpus, then create termdocumentmatrix for (unigram, bigram, trigram, and 4-gram(quagram))</li>
                   <li>Convert tdm to data frame for each gram type(the data frame contains term names, count, and probability that it ocours in the data frame)</li>
                   <li>Create function that do the prediction and return terms that mostly will be the next word</li>
                   </ul>
                   <h3>How to predict with this shiny app?</h3>
                   <p>If you type nothing in the input text, then on the main panel it returns a sentence.
                   Type a sentence and select which type of ngram prediction you would like to do with, then click
                   "Update" button to see if there shows the predicted words on the main panel.</p>
                   <h3>Visualization of this project</h3>
                    <img src="C:/Users/stephanie song/Desktop/wordcloud.jpg" />
                    <img src="C:/Users/stephanie song/Desktop/word.jpg" />                                              
                                                                  
                                                                  
                                                                  
                                                                  '))))



























