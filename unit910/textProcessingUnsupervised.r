library(tm)
Sys.setenv(NOAWT=TRUE)
library(Snowball)
library(RWeka)
library(rJava)

data(crude)
corp = VCorpus(VectorSource(crude))
dtm.stem = DocumentTermMatrix(corp,
        control=list(tolower=TRUE,
                     removePunctuation=list(preserve_intra_word_dashes=TRUE),
                     removeNumbers=TRUE,
                     stemming=FALSE,
                     stopwords = TRUE,
                     weighting=weightTfIdf,
                     wordLengths = c(2,15))
)

mydtm = as.matrix(dtm.stem)

out.pca = prcomp(mydtm)

out.lsi = out.pca$rotation

signif(sort(out.lsi[,1],decreasing=TRUE)[1:24],2)
signif(sort(out.lsi[,1],decreasing=FALSE)[1:24],2)


library(tm)

load(file="../data/docs.Rdata")
corp = VCorpus(VectorSource(docs))

Sys.setenv(NOAWT=TRUE)
library(Snowball)
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

out.pca = prcomp(mydtm)

out.lsi = out.pca$rotation

signif(sort(out.lsi[,2],decreasing=TRUE)[1:24],2)
signif(sort(out.lsi[,2],decreasing=FALSE)[1:24],2)

pdf('../lectures/figures/lsiTurtlesPlot.pdf')
labels = c("(tmnt leo)","(tmnt rap)","(tmnt mic)","(tmnt don)",
"(real leo)","(real rap)","(real mic)","(real don)")
color = c(rep('red',4),rep('blue',4))
plot(out.pca$x[,1:2],type='n',xlim=c(-.2,0),ylim=c(-.1,.1))
text(out.pca$x[,1:2],label=labels,col=color,cex=.75)
dev.off()

k=5
dist = sqrt(rowSums((scale(out.pca$x[,1:k],center=out.pca$x[9,1:k],scale=F)^2)))
a = matrix(dist,ncol=1)#+c(.1,-.1,-.1,rep(.1,5),0)
rownames(a) = c(labels,'query')
print(a)


