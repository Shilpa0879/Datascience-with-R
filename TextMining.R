amazon_reviews<-apple$x
x <- as.character(amazon_reviews)
getwd()

# Corpus
install.packages("tm")  #for text mining
library(tm)

#Load the data as a corpus
x <- Corpus(VectorSource(x))
inspect(x[1])

# Data Cleansing 
#Convert the text to lower case
x1 <- tm_map(x, tolower) 
inspect(x1[1]) 

#Remove Punctuations
x1 <- tm_map(x, removePunctuation) 
inspect(x1[1]) 

#Remove Numbers
x1 <- tm_map(x1, removeNumbers) 

#Remove english common stopwords
x1 <- tm_map(x1, removeWords, stopwords('english')) 
inspect(x1[1]) 

x1 <- tm_map(x1, tolower) 
#striping white spaces 
x1 <- tm_map(x1, stripWhitespace) 
inspect(x1[1]) 

#Text stemming
install.packages(c("SnowballC","textstem"))  #for text stemming
library(SnowballC)
library(textstem)
x1 <-lemmatize_words(x1)
#x1 <-tm_map(x1,stemDocument)

# Term document matrix
# converting unstructured data to structured format using TDM 

tdm <- TermDocumentMatrix(x1) 
dtm <- t(tdm) 
tdm <- as.matrix(tdm) 

#Frequency
v <-sort(rowSums(tdm),decreasing = TRUE)
d <- data.frame(word=names(v),freq=v)
head(d,10)

# Bar plot 
w <- rowSums(tdm)
w 

w_sub <- subset(w, w >= 20)
w_sub 

windows() 
barplot(w_sub, las=3, col = rainbow(20)) 

# Term mcdonalds repeats in all most all documents 
x1 <- tm_map(x1, removeWords, 
             c('apple','air',"the",'can','will',"amazon",'mac','macbook','product','windows'))              
x1 <- tm_map(x1, stripWhitespace) 
tdm <- TermDocumentMatrix(x1) 
tdm <- as.matrix(tdm) 
w1 <- rowSums(tdm) 

# Word cloud 
install.packages("wordcloud") 
library(wordcloud)

windows()
wordcloud(words = names(w1), freq = w1) # wordcloud with only subset of words

w_sub1 <- sort(rowSums(tdm), decreasing = TRUE)

wordcloud(words = names(w_sub1), freq = w_sub1) # all words are considered

windows() 
wordcloud(words = names(w1), freq = w1, random.order = F, colors = rainbow(20), scale=c(2,.2), rot.per = 0.3) 

# lOADING +VE AND -VE dictonaries 
pos.words = scan(file.choose(), what="character", comment.char=";")	# read-in positive-words.txt
neg.words = scan(file.choose(), what="character", comment.char=";") 	# read-in negative-words.txt 
pos.words = c(pos.words,"wow", "kudos", "hurray") # including our own positive words to the existing
list 

# Positive wordcloud 
pos.matches = match(names(w_sub1), c(pos.words)) 
pos.matches = !is.na(pos.matches) 

freq_pos <- w_sub1[pos.matches] 
p_names <- names(freq_pos) 

windows() 
wordcloud(p_names,freq_pos,scale=c(3.5,.2),colors = rainbow(20)) 

# Negative wordcloud 
neg.matches = match(names(w_sub1), c(neg.words))
neg.matches = !is.na(neg.matches)
freq_neg <- w_sub1[neg.matches]
n_names <- names(freq_neg)

windows()
wordcloud(n_names,freq_neg,scale=c(2.5,.2),colors = brewer.pal(8,"Dark2"))


