library(Matrix)
nyTimes     = read.table("/Users/darrenho/docword.nytimes.txt")
nyTimesSpar = sparseMatrix(i=nyTimes[,1],j=nyTimes[,2],x=nyTimes[,3])
p = ncol(nyTimesSpar)

nWords   = length(unique(nyTimes[,2]))
words    = unique(nyTimes[,2])
docIDs   = unique(nyTimes[,1])
nDocs    = length(unique(nyTimes[,1]))
docVar   = nyTimes[,1]

library(XML)
library(RCurl)
webpage <- getURL("http://en.wikipedia.org/wiki/List_of_popes")
webpage <- readLines(tc <- textConnection(webpage)); close(tc)
pagetree <- htmlTreeParse(webpage, error=function(...){}, useInternalNodes = TRUE)
# parse the tree by tables
x <- xpathSApply(pagetree, "//*/table", xmlValue)  
# do some clean up with regular expressions
x <- unlist(strsplit(x, "\n"))
x <- gsub("\t","",x)
x <- sub("^[[:space:]]*(.*?)[[:space:]]*$", "\\1", x, perl=TRUE)
x <- x[!(x %in% c("", "|"))]
x = unlist(strsplit(x,' '))