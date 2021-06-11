# Needed <- c("tm", "SnowballCC", "RColorBrewer", "ggplot2", "wordcloud", "biclust", 
#             "cluster", "igraph", "fpc")
# install.packages(Needed, dependencies=TRUE) 
# install.packages("Rcampdf", repos = "http://datacube.wu.ac.at/", type = "source")
# Install
install.packages("tm")  # for text mining
install.packages("SnowballC") # for text stemming
install.packages("wordcloud") # word-cloud generator 
install.packages("RColorBrewer") # color palettes

library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

                                              #Text Mining


#filePath <- "http://www.sthda.com/sthda/RDoc/example-files/martin-luther-king-i-have-a-dream-speech.txt"

workingPath <- getwd()
textPath <- file.path(workingPath, "Romeo_Juliet.txt")
romeoJuliet <- readLines(textPath)

# Load the data as a corpus
docs <- Corpus(VectorSource(romeoJuliet))
inspect(docs) # if you do not want to see detailed information of the corpus, comment out this line

#Transformation is performed using tm_map() function to replace, for example, special characters 
#from the text. 
#Replacing ?/?, ?@? and ?|? with space
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")



                                       #Cleaning the text

# the tm_map() function is used to remove unnecessary white space, to convert the text to lower case, 
# to remove common stopwords like ?the?, ?we?.
# 
# The information value of ?stopwords? is near zero due to the fact that they are so common in a language. 
# Removing this kind of words is useful before further analysis. For ?stopwords?, supported languages are 
# danish, dutch, english, finnish, french, german, hungarian, italian, norwegian, portuguese, russian, 
# spanish and swedish. Language names are case sensitive.
# 
# I?ll also show you how to make your own list of stopwords to remove from the text.
# 
# You could also remove numbers and punctuation with removeNumbers and removePunctuation arguments.
# 
# Another important preprocessing step is to make a text stemming which reduces words to their root form. 
# In other words, this process removes suffixes from words to make it simple and to get the common origin.
# For example, a stemming process reduces the words ?moving?, ?moved? and ?movement? to the root word,
# ?move?.

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))

# Remove numbers
docs <- tm_map(docs, removeNumbers)

# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))

# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 

# Remove punctuations
docs <- tm_map(docs, removePunctuation)

# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)

# Text stemming
# docs <- tm_map(docs, stemDocument)

                                    #Build a term-document matrix

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

                                      #Generate the Word cloud

# words : the words to be plotted
# freq : their frequencies
# min.freq : words with frequency below min.freq will not be plotted
# max.words : maximum number of words to be plotted
# random.order : plot words in random order. If false, they will be plotted in decreasing frequency
# rot.per : proportion words with 90 degree rotation (vertical text)
# colors : color words from least to most frequent. Use, for example, colors =?black? for single color.
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

                            #Explore frequent terms and their associations

findFreqTerms(dtm, lowfreq = 4)
# You can analyze the association between frequent terms (i.e., terms which correlate) using 
# findAssocs() function. The R code below identifies which words are associated with ?biz?
findAssocs(dtm, terms = "bir", corlimit = 0.3)
barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")