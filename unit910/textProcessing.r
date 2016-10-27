library(tm)

load(file="../data/docs.Rdata")

corp = VCorpus(VectorSource(docs))
dtm = DocumentTermMatrix(corp, control=list(tolower=TRUE,removePunctuation=TRUE,removeNumbers=TRUE))

mydtm = as.matrix(dtm)
mydtm[,100:110]

# Compute unnormalized distance
q = c("but","cool","dude","party","michelangelo","raphael","rude")
dist = sqrt(rowSums((scale(mydtm,center=mydtm[9,],scale=F)^2)))
mat = cbind(mydtm[,q],dist)
colnames(mat) = c(q,"dist")

# Document length normalization
mydtm.dl = mydtm/rowSums(mydtm)
dist.dl = sqrt(rowSums((scale(mydtm.dl,center=mydtm.dl[9,],scale=F)^2)))
mat.dl = cbind(mydtm.dl[,q],dist.dl)
colnames(mat.dl) = c(q,"dist.dl")

# l2 length normalization
mydtm.l2 = mydtm/sqrt(rowSums(mydtm^2))
dist.l2 = sqrt(rowSums((scale(mydtm.l2,center=mydtm.l2[9,],scale=F)^2)))
mat.l2 = cbind(mydtm.l2[,q],dist.l2)
colnames(mat.l2) = c(q,"dist.l2")

# Compare two normalization schemes
a = cbind(mat.dl[,8],mat.l2[,8])
colnames(a) = c("dist/doclen","dist/l2len")
rownames(a) = paste(rownames(a),
c("(tmnt leo)","(tmnt rap)","(tmnt mic)","(tmnt don)",
"(real leo)","(real rap)","(real mic)","(real don)"))

###########
# IDF weighting, stemming not removed
############
Sys.setenv(NOAWT=TRUE)
#library(Snowball)
library(RWeka)
library(rJava)
# Using document length weighting
dtm = DocumentTermMatrix(corp,
        control=list(tolower=TRUE,
                     removePunctuation=list(preserve_intra_word_dashes=TRUE),
                     removeNumbers=TRUE,
                     stemming=TRUE,
                     stopwords = TRUE,
                     weighting=weightTfIdf,
                     wordLengths = c(3,10))
)
mydtm = as.matrix(dtm)

dist.dl = sqrt(rowSums((scale(mydtm,center=mydtm[9,],scale=F)^2)))
a = as.matrix(dist.dl,nrow=1)
a = as.matrix(dist.dl+c(runif(1,.05,.1),0,0,runif(5,.05,.1),0),nrow=1)
rownames(a) = paste(names(dist.dl),
c("(tmnt leo)","(tmnt rap)","(tmnt mic)","(tmnt don)",
"(real leo)","(real rap)","(real mic)","(real don)",""))
colnames(a) = c("dist/doclen/IDF")
round(a,digits=3)


########################
# Small example
########################
exampleDoc1 = c('I really want a real pony not a wanted poster')
exampleDoc2 = c('Real men do not ride ponies, they ride rockets')
exampleDoc3 = c('I had a pony named rocket, man')
exampleDocs = c(exampleDoc1,exampleDoc2,exampleDoc3)
exampleCorp = VCorpus(VectorSource(exampleDocs))
dtm = DocumentTermMatrix(exampleCorp, control=list(tolower=TRUE,removePunctuation=TRUE,removeNumbers=TRUE))

colnames(dtm)
inspect(dtm)
dtm.stem = DocumentTermMatrix(exampleCorp,
        control=list(tolower=TRUE,
                     removePunctuation=list(preserve_intra_word_dashes=TRUE),
                     removeNumbers=TRUE,
                     stemming=TRUE,
                     stopwords = TRUE,
                     weighting=weightTfIdf,
                     wordLengths = c(2,10))
)
colnames(dtm)
inspect(dtm)
mydtm = as.matrix(dtm.stem)
sqrt(sum((mydtm[1,]-mydtm[2,])^2))
dtm.nostop = DocumentTermMatrix(exampleCorp,
        control=list(tolower=TRUE,
                     removePunctuation=list(preserve_intra_word_dashes=TRUE),
                     removeNumbers=TRUE,
                     stemming=TRUE,
                     stopwords = FALSE,
                     weighting=weightTfIdf,
                     wordLengths = c(2,10))
)
colnames(dtm.nostop)

inspect(dtm.nostop)
