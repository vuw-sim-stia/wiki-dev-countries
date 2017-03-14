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
  content <- sub("\\s+$", "",sub("^\\s+", "",gsub("( )+"," ",gsub("[[:digit:]]"," ",gsub("[[:punct:]]"," ",gsub("[\n\r]+"," ",xpathSApply(pagetree, "//*/article", xmlValue)))))))
  
  references <- getNodeSet(pagetree, "//*[@id='biblio']/section/ul/p/span[@class='smallcaps']")
  links <- getNodeSet(pagetree, "//*/a[@class='reflink']")
  
  docs[i,1] <- content
  
  #outgoing reflinks
  
  docs[i,2] <- length(references)
  docs[i,3] <- length(links)
  
  for(j in 1:length(links)){
    nextlink <- links[[j]]
    if(j==1) reflinks <- xmlAttrs(nextlink)['href']
    else reflinks <- paste(reflinks,xmlAttrs(nextlink)['href'],sep = ",")
  }
  
  docs[i,4] <- reflinks
}

colnames(docs) <- c("content","references","reflinkscount","reflinks")

### ARTICLE STATS

docs$country <- sources[,2]

# number of words

docs$wordcount <- sapply(docs$content,wc)

colnames(docs) <- c("content","references","reflinkscount","reflinks","country","wordcount")

write.csv(docs,"/Users/mlr/Dropbox/2017-wiki-dev-countries/EBprocessed.csv",row.names = FALSE)
