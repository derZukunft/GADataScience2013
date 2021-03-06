#### Spam Classification with NaiveBayes implementation in R
####

########################################### 
# First Iteration (Training Set & Priors) #
###########################################


# training set
spam <- list(c("buy", "drugs", "online", "from", "our", "pharma"),
            c("buy", "insurance", "at", "low", "prices"))
legitimate <- list(c("newsletter", "from", "your", "favorite", "website"),
                   c("I", "was", "writing", "for", "php", "advice"),
                   c("good", "article", "on", "php"))

# training
categories = 2
priors <- c()
total <- length(spam) + length(legitimate)
priors[1] <- length(spam) / total
priors[2] <- length(legitimate) / total

# classifier
score <- function (test_mail, category) {
    priors[category]
}

classify <- function(test_mail) {
    scores = c()
    for (i in 1:categories) {
        scores[i] = score(test_mail, i)
    }
    print(scores)
    which(scores==max(scores))
}

# validation set
print(classify(c("php", "object", "oriented")))
print(classify(c("free", "drugs")))
print(classify(c("r", "article")))

print("Done with First Iteration!")

############################### 
# Second Iteration (Features) #
###############################

score <- function (test_mail, category) {
    score <- priors[category]
    categoryFeatures = features[[category]]
    for (word in test_mail) {
        if (contains(categoryFeatures, word)) {
            score <- score * categoryFeatures[[word]]
        } else {
            score <- score * zeroOccurrences[[category]]
        }
    }
    return(score)
}

features <- list();
zeroOccurrences = list()
for (category in 1:categories) {
    categoryFeatures <- list();
    singleOccurrence = 1 / length(training[[category]])
    zeroOccurrences[[category]] = singleOccurrence
    for (sampleMail in training[[category]]) {
        for (word in sampleMail) {
            if (contains(categoryFeatures, word)) {
                categoryFeatures[[word]] = categoryFeatures[[word]] + singleOccurrence
            } else {
                categoryFeatures[[word]] = zeroOccurrences[[category]] + singleOccurrence
            }
        }
    }
    features[[category]] <- categoryFeatures
}

# validation set
print(classify(c("php", "object", "oriented")))
print(classify(c("free", "drugs")))
print(classify(c("r", "article")))