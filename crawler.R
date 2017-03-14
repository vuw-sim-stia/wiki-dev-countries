library(tm)
library(RCurl)
library(XML)
library(SnowballC)
library(jsonlite)
library(RTextTools)
library(topicmodels)
library(qdap)
library(stringi)
library(proxy)
library(cluster)
library(arules)
library(textreuse)

developingC <- read.csv2("/Users/mlr/Dropbox/2017-wiki-dev-countries/developing-countries.csv",header = F,sep = ',')
developedC <- read.csv2("/Users/mlr/Dropbox/2017-wiki-dev-countries/developed-countries.csv",header = F,sep = ',')

sources <- developedC

docs <- data.frame(matrix(ncol = 4, nrow = nrow(sources)))

for(i in 1:nrow(sources)){
  theurl <- sources[i,1]
  webpage <- getURL(theurl)
  webpage <- readLines(tc <- textConnection(webpage))
  close(tc)
  pagetree <- htmlTreeParse(webpage, error=function(...){}, useInternalNodes = TRUE)
  
  # Extract table header and contents
  content <- sub("\\s+$", "",sub("^\\s+", "",gsub("( )+"," ",gsub("[[:digit:]]"," ",gsub("[[:punct:]]"," ",gsub("[\n\r]+"," ",xpathSApply(pagetree, "//*[@id='mw-content-text']", xmlValue)))))))
  
  references <- getNodeSet(pagetree, "//*/ol[@class='references']/li")
  webreferences <- getNodeSet(pagetree, "//*/ol[@class='references']/li/*/cite[@class='citation web']/a")
  
  docs[i,1] <- content
  
  #references and references with URIs
  
  docs[i,2] <- length(references)
  docs[i,3] <- length(webreferences)
  
  for(j in 1:length(webreferences)){
    nextlink <- webreferences[[j]]
    if(j==1) reflinks <- xmlAttrs(nextlink)['href']
    else reflinks <- paste(reflinks,xmlAttrs(nextlink)['href'],sep = ",")
  }
  
  docs[i,4] <- reflinks
  
  # outgoing links
  
  linkJSON <- fromJSON(paste('https://en.wikipedia.org/w/api.php?action=query&titles=',sources[i,2],'&prop=links&pllimit=500&format=json',sep=''))
  
  links <- linkJSON$query$pages[[1]]$links$title
  
  if(is.null(linkJSON$continue)){
    contd = FALSE
  } else{
    contd = TRUE
  }
  
  while(contd){
    linkJSON <- fromJSON(paste('https://en.wikipedia.org/w/api.php?action=query&plcontinue=',linkJSON$continue$plcontinue,'&titles=',sources[i,2],'&pllimit=500&prop=links&format=json',sep=''))
    morelinks <- linkJSON$query$pages[[1]]$links$title
    
    if(is.null(linkJSON$continue)){
      contd = FALSE
    }
    
    links <- c(links,morelinks)
  }
  
  for(j in 1:length(links)){
    nextlink <- links[[j]]
    if(j==1) textlinks <- nextlink
    else textlinks <- paste(textlinks,nextlink,sep = ",")
  }
  
  docs[i,5] <- textlinks
  docs[i,6] <- length(links)
  
  #incoming links
  
  linkJSON <- fromJSON(paste('https://en.wikipedia.org/w/api.php?action=query&list=backlinks&bltitle=',sources[i,2],'&bllimit=500&format=json',sep=''))
  
  inlinks <- linkJSON$query$backlinks$title
  
  if(is.null(linkJSON$continue)){
    contd = FALSE
  } else{
    contd = TRUE
  }
  
  while(contd){
    linkJSON <- fromJSON(paste('https://en.wikipedia.org/w/api.php?action=query&list=backlinks&blcontinue=',linkJSON$continue$blcontinue,'&bltitle=',sources[i,2],'&bllimit=500&format=json',sep=''))
    morelinks <- linkJSON$query$backlinks$title
    
    if(is.null(linkJSON$continue)){
      contd = FALSE
    }
    
    inlinks <- c(inlinks,morelinks)
  }
  
  for(j in 1:length(inlinks)){
    nextlink <- inlinks[[j]]
    if(j==1) textlinks <- nextlink
    else textlinks <- paste(textlinks,nextlink,sep = ",")
  }
  
  docs[i,7] <- textlinks
  docs[i,8] <- length(inlinks)
}

colnames(docs) <- c("content","references","webreferences","webreflinks","textlinks","textlinkscount","inlinks","inlinkscount")

### ARTICLE STATS

docs$country <- sources[,2]

# number of words

docs$wordcount <- sapply(docs$content,wc)

colnames(docs) <- c("content","references","webreferences","webreflinks","textlinks","textlinkscount","inlinks","inlinkscount","country","wordcount")

write.csv(docs,"/Users/mlr/Dropbox/2017-wiki-dev-countries/processed.csv",row.names = FALSE)

### PREP FOR NLP

myCorpus <- Corpus(VectorSource(docs[,1]))
myCorpus = tm_map(myCorpus, content_transformer(removePunctuation))
myCorpus = tm_map(myCorpus, content_transformer(removeNumbers))
myCorpus = tm_map(myCorpus, content_transformer(tolower))


myStopwords = c(stopwords('english'),"isbn", "pdf", "january", "february", "march", "april", "may", "june", "july", "august", "september", "october", "november", "december", "a", "about", "above", "after", "again", "against", "all", "also", "am", "an", "and", "any", "are", "aren't", "as", "at", "be", "because", "been", "before", "being", "below", "between", "both", "but", "by", "can't", "cannot", "could", "couldn't", "did", "didn't", "do", "does", "doesn't", "doing", "don't", "down", "during", "each", "few", "for", "from", "further", "had", "hadn't", "has", "hasn't", "have", "haven't", "having", "he", "he'd", "he'll", "he's", "her", "here", "here's", "hers", "herself", "him", "himself", "his", "how", "how's", "i", "i'd", "i'll", "i'm", "i've", "if", "in", "into", "is", "isn't", "it", "it's", "its", "itself", "let's", "me", "more", "most", "mustn't", "my", "myself", "no", "nor", "not", "of", "off", "on", "once", "only", "or", "other", "ought", "our", "ours	ourselves", "out", "over", "own", "same", "shan't", "she", "she'd", "she'll", "she's", "should", "shouldn't", "so", "some", "such", "than", "that", "that's", "the", "their", "theirs", "them", "themselves", "then", "there", "there's", "these", "they", "they'd", "they'll", "they're", "they've", "this", "those", "through", "to", "too", "under", "until", "up", "very", "was", "wasn't", "we", "we'd", "we'll", "we're", "we've", "were", "weren't", "what", "what's", "when", "when's", "where", "where's", "which", "while", "who", "who's", "whom", "why", "why's", "with", "won't", "would", "wouldn't", "you", "you'd", "you'll", "you're", "you've", "your", "yours", "yourself", "yourselves")
myCorpus = tm_map(myCorpus, content_transformer(removeWords), myStopwords)

#fix this
#dictCorpus = myCorpus
myCorpus = tm_map(myCorpus, content_transformer(stemDocument))
myCorpus = tm_map(myCorpus, content_transformer(stripWhitespace))
#myCorpus = tm_map(myCorpus, stemCompletion, dictionary=dictCorpus)

myDtm = DocumentTermMatrix(myCorpus, control = list(minWordLength = 3))

TDM <- TermDocumentMatrix(myCorpus, control = list(minWordLength = 3))
TDM.common = removeSparseTerms(TDM, 0.1)

myDtm.common = removeSparseTerms(myDtm, 0.1)
### BASIC ANALYSIS

findFreqTerms(TDM.common, 1000)

### CLUSTERING
docsdissim <- dist(as.matrix(myDtm.common), method = "cosine")

docsdissim2 <- as.matrix(docsdissim)
rownames(docsdissim2) <- sources[,2]
colnames(docsdissim2) <- sources[,2]

h <- hclust(docsdissim, method = "ward.D")
plot(h, labels = colnames(docsdissim2), sub = "")

### TOPIC MODELLING

k = 10
SEED = 7265
my_TM =
  list(VEM = LDA(myDtm, k = k, control = list(seed = SEED)),
       VEM_fixed = LDA(myDtm, k = k,
                       control = list(estimate.alpha = FALSE, seed = SEED)),
       Gibbs = LDA(myDtm, k = k, method = "Gibbs",
                   control = list(seed = SEED, burnin = 1000,
                                  thin = 100, iter = 1000)),
       CTM = CTM(myDtm, k = k,
                 control = list(seed = SEED,
                                var = list(tol = 10^-4), em = list(tol = 10^-3))))

Topic = topics(my_TM[["VEM"]], 1)

#top 5 terms for each topic in LDA
Terms = terms(my_TM[["VEM"]], 5)
Terms

(my_topics =
   topics(my_TM[["VEM"]]))

#top 5 terms for each topic in LDA
Terms = terms(my_TM[["Gibbs"]], 5)
Terms

(my_topics =
    topics(my_TM[["Gibbs"]]))


most_frequent = which.max(tabulate(my_topics))

terms(my_TM[["VEM"]], 10)[, most_frequent]