library(wordcloud)
library(RColorBrewer)

library(tm)
Sys.setenv(NOAWT=TRUE)
library(Snowball)
library(RWeka)
library(rJava)

data(crude)
corp = VCorpus(VectorSource(crude))
dtmOil = DocumentTermMatrix(corp,
            control=list(tolower=TRUE,
            removePunctuation=list(preserve_intra_word_dashes=TRUE),
            removeNumbers=TRUE,
            stemming=TRUE,
            stopwords = TRUE,
            weighting=weightTfIdf,
            wordLengths = c(2,15))
)

mydtm   = as.matrix(dtmOil)
out.pca = prcomp(mydtm)

out.lsi = out.pca$rotation

signif(sort(out.lsi[,1],decreasing=TRUE)[1:24],2)
signif(sort(out.lsi[,1],decreasing=FALSE)[1:24],2)

pdf('../lectures/figures/lsiOilWordCloud1pos.pdf')
wordcloud(colnames(mydtm),out.lsi[,1], scale=c(4,.5),
          max.words=100, random.order=T, rot.per=.15)
dev.off()

pdf('../lectures/figures/lsiOilWordCloud1neg.pdf')
wordcloud(colnames(mydtm),-out.lsi[,1], scale=c(4,.5),
          max.words=100, random.order=T, rot.per=.15)
dev.off()

pdf('../lectures/figures/lsiOilWordCloud2pos.pdf')
wordcloud(colnames(mydtm),out.lsi[,2], scale=c(4,.5),
          max.words=100, random.order=T, rot.per=.15)
dev.off()

pdf('../lectures/figures/lsiOilWordCloud2neg.pdf')
wordcloud(colnames(mydtm),-out.lsi[,2], scale=c(4,.5),
          max.words=100, random.order=T, rot.per=.15)
dev.off()

##### Add color
negPos = out.lsi[,1] > 0
colors = rep('red',ncol(mydtm))
colors[negPos] = 'blue'
pdf('../lectures/figures/lsiOilWordCloud1.pdf')
wordcloud(colnames(mydtm),abs(out.lsi[,1]), scale=c(4,.5),
          colors = colors,ordered.colors=T,
          max.words=100, random.order=T, rot.per=.15)
dev.off()
negPos = out.lsi[,2] > 0
colors = rep('red',ncol(mydtm))
colors[negPos] = 'blue'
pdf('../lectures/figures/lsiOilWordCloud2.pdf')
wordcloud(colnames(mydtm),abs(out.lsi[,2]), scale=c(4,.5),
          colors = colors,ordered.colors=T,
          max.words=100, random.order=T, rot.per=.15)
dev.off()
###########


################
# TMNT
################ 
load(file="../data/docs.Rdata")
corp = VCorpus(VectorSource(docs))

# Using document length weighting
dtmTMNT = DocumentTermMatrix(corp,
                         control=list(tolower=TRUE,
                                      removePunctuation=TRUE,#list(preserve_intra_word_dashes=TRUE),
                                      removeNumbers=TRUE,
                                      stemming=TRUE,
                                      stopwords = TRUE,
                                      weighting=weightTfIdf,
                                      wordLengths = c(3,10))
)

mydtm   = as.matrix(dtmTMNT)

require(glmnet)
Y = c(0,0,0,0,1,1,1,1)
out.glmnet = glmnet(y=Y,x=mydtm[1:8,],family='binomial')
out.cv.glmnet = cv.glmnet(y=Y,x=mydtm[1:8,],family='binomial')

beta.glmnet = out.glmnet$beta[,100]
words.glmnet = colnames(dtmTMNT)[abs(beta.glmnet)>0]

beta.glmnet.nonzero = beta.glmnet[abs(beta.glmnet)>0]

negPos = beta.glmnet.nonzero > 0
colors = rep('red',length(beta.glmnet.nonzero))
colors[negPos] = 'blue'
pdf('../lectures/figures/glmnetWordCloudTMNT.pdf')
wordcloud(words.glmnet,abs(beta.glmnet.nonzero), scale=c(4,.5),
          colors = colors,ordered.colors=T,
          max.words=100, random.order=T, rot.per=.15)
dev.off()
