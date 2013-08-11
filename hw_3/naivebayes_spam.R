#### Naive Bayes Email Clasification with TM & WordCloud Packages ####
#install.packages("tm", lib = "Rpackages")
#install.packages("wordcloud", lib = "Rpackages")
library("wordcloud")
library("tm")

# Helper function written by Conway and White
get.msg <- function(path) {
   con <- file(path,open="rt",encoding="latin1")
   text <- readLines(con)
   msg <- text[seq(which(text=="")[1]+1,length(text))]  
   close(con)
   return(paste(msg,collapse="\n"))
}

# Data Source | list path
spam.path <- "/Users/bhyman/R/data/spam/"
ham.path <- "/Users/bhyman/R/data/easyham/"


# Create vector structure in R to contain the content of each of the emails for spam and ham
spam.docs <- dir(spam.path)
spam.docs <- spam.docs[which(spam.docs!="cmds")]
all.spam <- sapply(spam.docs,
	function(p)get.msg(paste(spam.path,p,sep="")))

ham.docs <- dir(ham.path)
ham.docs <- ham.docs[which(ham.docs!="cmds")]
all.ham <- sapply(ham.docs,
	function(p)get.msg(paste(ham.path,p,sep="")))

# Create Corpus, document-term matrix (dtm) and term-document matrix (tdm) objects
library(tm)
control <- list(stopwords=TRUE,removePunctuation=TRUE,
		removeNumbers=TRUE,minDocFreq=2)

# Using the control variable to create our tdm and dtm objects
spam.corpus <- Corpus(VectorSource(all.spam))
spam.tdm <- TermDocumentMatrix(spam.corpus,control)
spam.dtm <- DocumentTermMatrix(spam.corpus,control)

ham.corpus <- Corpus(VectorSource(all.ham))
ham.tdm <- TermDocumentMatrix(ham.corpus,control)
ham.dtm <- DocumentTermMatrix(ham.corpus,control)

# Removing sparse terms in the tdm
new.spam.tdm.2 <- removeSparseTerms(spam.tdm,0.8)
new.ham.tdm.2 <- removeSparseTerms(ham.tdm,0.8)

# Looking for Overall Frequency of Terms in collections
length(findFreqTerms(spam.dtm,100))
length(findFreqTerms(spam.dtm,300))
findFreqTerms(spam.dtm,300)

# Finding terms with greatest frequency BOTH in the ham and the spam
intersect(findFreqTerms(spam.dtm,100),findFreqTerms(ham.dtm,100))

# Association Analysis
inspect(new.ham.tdm.2[,1])
inspect(new.spam.tdm.2[,1])

# Most commonly associated term with 'list' throughout the corpus
findAssocs(new.ham.tdm.2,"list",0)

# Most commonly associated with the word "free" in the spam
findAssocs(new.spam.tdm.2,"free",0)

# Squish all the documents in all.spam and all.ham into a data frame with two columns, one for each email type
allspam <- paste(all.spam,sep="",collapse=" ")
allham <- paste(all.ham,sep="",collapse=" ")
tmpText = data.frame(c(allham,allspam),row.names=c("HAM","SPAM"))
ds <- DataframeSource(tmpText)

# remove punctuation, convert all the words to lowercase, remove the numbers, and eliminate the stopwords
corp = Corpus(ds)
corp = tm_map(corp,removePunctuation)
corp = tm_map(corp,tolower)
corp = tm_map(corp,removeNumbers)
corp = tm_map(corp,function(x){removeWords(x,stopwords())})

# Create WordCloud
tdm <- TermDocumentMatrix(corp)
term.matrix <- as.matrix(tdm)
v <- sort(rowSums(term.matrix),decreasing=TRUE)
d <- data.frame(word=names(v),freq=v)	
wordcloud(d$word,d$freq,max.words=150)

# Create word clouds that show the words most commonly used in BOTH ham and spam as well as the words most uncommonly used in each category. 
# The former is called a commonality cloud, and the latter is called a comparison cloud. 
par(mfrow=c(1,2))
comparison.cloud(term.matrix,max.words=200,random.order=FALSE,
	colors=c("#999999","#000000"),main="Differences Between 
	HAM and SPAM")
commonality.cloud(term.matrix,max.words=100,random.order=FALSE,
	color="#000000",asp=2,main="Similarities Between HAM 
	and SPAM")


