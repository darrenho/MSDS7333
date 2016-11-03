library(XML)
library(RCurl)
webpage = getURL("https://en.wikipedia.org/wiki/List_of_popes")
webpage = readLines(tc = textConnection(webpage)); close(tc)
pagetree = htmlTreeParse(webpage, error=function(...){}, useInternalNodes = TRUE)
# parse the tree by tables
x = xpathSApply(pagetree, "//*/table", xmlValue) 

#We can use the tm package:
Sys.setenv(NOAWT=TRUE)
require(SnowballC)
require(RWeka)
require(rJava)
require(tm)

x = paste(x,collapse='')
x = gsub("\t"," ",x)
x = gsub("\n"," ",x)
corp = VCorpus(VectorSource(x))
dtm = DocumentTermMatrix(corp,
                         control=list(tolower=TRUE,
                                      removePunctuation=list(preserve_intra_word_dashes=TRUE),
                                      removeNumbers=TRUE,
                                      stemming=TRUE,
                                      stopwords = TRUE,
                                      weighting=weightTfIdf,
                                      wordLengths = c(2,15))
)

# Or we can manually process it into separate words
x = unlist(strsplit(x, "\n"))
x = gsub("\t","",x)
x = sub("^[[:space:]]*(.*?)[[:space:]]*$", "\\1", x, perl=TRUE)
x = x[!(x %in% c("", "|"))]
x = unlist(strsplit(x,' '))




