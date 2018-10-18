library(dplyr) # data manipulation
library(readr) # io
library(stringi) # string/regular expression
library(tm) # text mining
library(jiebaR) # chinese text segmentation
library(randomForest) # randomForest
library(xgboost) #xgboost
#
Sys.setlocale("LC_ALL","en_US.UTF-8") # unicode environment
#
trainset0 = read_csv('/Users/ceciliaso/Desktop/offsite-tagging-training-set.csv') # trainset
testset = read_csv('/Users/ceciliaso/Desktop/offsite-tagging-test-set.csv') # testset
trainset0[,3] = sapply(trainset0[,3],function(x) gsub("\\n","",gsub("\\r","",gsub("<.*?>","",as.character(x))))) # filter out html tags and line separation
testset[,2] = sapply(testset[,2],function(x) gsub("\\n","",gsub("\\r","",gsub("<.*?>","",as.character(x))))) # filter out html tags and line separation
valind = seq(from=1,to=dim(trainset0)[1],by = 5)  # set aside 20% of trainset data for validation, i.e.: 1/5 = 0.2
trainind = setdiff(1:(dim(trainset0)[1]),valind)
trainset = trainset0[trainind,]
valset = trainset0[valind,]
allcat = pull(unique(trainset[,2]),tags) # all categories: 1. 2. 3.
xseg = worker() # segmentation engine

## We extract the segmented words into the list and filter out some common conjunctions using a dictionary cherry-picked by the author.
## trainset, validationset, and test set; n1 = 3115, n2 = 779, n3 = 974.
## Those words filtered out by regular expression are cherry-picked as "low information content words".
## i.e.: The word 因為 can happen in any news topic without giving you extra information of its tags
## A more sophisticated method would be filtering out these words by setting up a dissimilarity measure between probability distribution
## of different tags. Here we have tried a method using on Jensen-Shannon divergence as a dissimilarity measure, 
## it would take pages to explain the method but in very high level, the method filtered out words are commonly used between different 
## tags using an information-theoretic criterion.
## Reference: https://en.wikipedia.org/wiki/Jensen–Shannon_divergence

## start JS-divergence method
wordslist = list()
for (i in 1:(dim(trainset)[1])) {
  t0 = as.character(trainset[i,3])
  t1 = xseg <= t0
  t1 = if ((length(grep("\\d",t1))>0)) {t1[-grep("\\d",t1)]} else {t1}
  t1 = t1[-grep("nbsp|&nbsp;",t1)]
  wordslist[[i]] = t1[(nchar(t1) > 1)]
}

corpus = Corpus(VectorSource(wordslist))
dtm = DocumentTermMatrix(corpus)
dtm = removeSparseTerms(dtm,sparse=0.99) # reduce the sparsity to 95% for dimension reduction
mat = as.matrix(dtm)
Hbinary = function(p) {
-p*log(p) - (1-p) * log(1-p)
}
grand1 = colSums(mat)/sum(mat)
H0 = Hbinary(grand1) 
weight = c(dim(mat[trainset$tags == "足球",])[1], dim(mat[trainset$tags == "梁振英",])[1], dim(mat[trainset$tags == "美國大選",])[1])
weight = weight/sum(weight)
Hi = cbind(colSums(mat[trainset$tags == "足球",])/sum(mat[trainset$tags == "足球",]),
      colSums(mat[trainset$tags == "梁振英",])/sum(mat[trainset$tags == "梁振英",]),
      colSums(mat[trainset$tags == "美國大選",])/sum(mat[trainset$tags == "美國大選",]))
Hi = apply(Hi,2,Hbinary)
JSdiv = rep(0,length(H0))

for (i in 1:(length(grand1))) {
  JSdiv[i] = H0 - sum(weight *Hi[i,])
}

tofilter = paste(colnames(mat)[order(JSdiv)][1:200], collapse='|')

## end JS divergence method

wordslist = list()
for (i in 1:(dim(trainset)[1])) {
  t0 = as.character(trainset[i,3])
  t1 = xseg <= t0
  t1 = if ((length(grep("\\d",t1))>0)) {t1[-grep("\\d",t1)]} else {t1}
  t1 = t1[-grep("一次|不能|不過|分別|就是|最終|特別|當時|當然|很多|開始|因為|這個|只有|可以|可能|成為|我們|最後|沒有|報道|知道|未有|不少|不會|以及|即使|取得|除了|一個|同時|如果|不同|未能|已經|其中|早前|不是|什麼|然而|由於|目前|他們|將會|去年|同樣|一樣|今次|只是|&nbsp;",t1)]
#  t1 = t1[-grep(paste0(tofilter,"nbsp|&nbsp;"),t1)]
  wordslist[[i]] = t1[(nchar(t1) > 1)]
}

wordslistval = list()
for (i in 1:(dim(valset)[1])) {
  t0 = as.character(valset[i,3])
  t1 = xseg <= t0
  t1 = if ((length(grep("\\d",t1))>0)) {t1[-grep("\\d",t1)]} else {t1}
  t1 = t1[-grep("一次|不能|不過|分別|就是|最終|特別|當時|當然|很多|開始|因為|這個|只有|可以|可能|成為|我們|最後|沒有|報道|知道|未有|不少|不會|以及|即使|取得|除了|一個|同時|如果|不同|未能|已經|其中|早前|不是|什麼|然而|由於|目前|他們|將會|去年|同樣|一樣|今次|只是|&nbsp;",t1)]  t1 = t1[-grep(paste0(tofilter,"nbsp|&nbsp;"),t1)]
  wordslistval[[i]] = t1[(nchar(t1) > 1)]
}

wordslistts = list()
for (i in 1:(dim(testset)[1])) {
  t0 = as.character(testset[i,2])
  t1 = xseg <= t0
  t1 = if ((length(grep("\\d",t1))>0)) {t1[-grep("\\d",t1)]} else {t1}
#  t1 = t1[-grep(paste0(tofilter,"nbsp|&nbsp;"),t1)]
t1 = t1[-grep("一次|不能|不過|分別|就是|最終|特別|當時|當然|很多|開始|因為|這個|只有|可以|可能|成為|我們|最後|沒有|報道|知道|未有|不少|不會|以及|即使|取得|除了|一個|同時|如果|不同|未能|已經|其中|早前|不是|什麼|然而|由於|目前|他們|將會|去年|同樣|一樣|今次|只是|&nbsp;",t1)]  wordslistts[[i]] = t1[(nchar(t1) > 1)]
}

# build a document term matrix with 660(cherry-picked one)/511 words
corpus = Corpus(VectorSource(wordslist))
dtm = DocumentTermMatrix(corpus)
dtm = removeSparseTerms(dtm,sparse=0.96) # reduce the sparsity to 95% for dimension reduction
mat = as.matrix(dtm)

# build a design matrix for machine learning
rfdf = cbind(match(pull(trainset,tags),allcat),mat)
rfdf = data.frame(rfdf)
variables = paste0('variable',1:(dim(rfdf)[2]-1))
names(rfdf) = c('tags',variables)
rfdf$tags = as.factor(rfdf$tags)

# random forest, rf1 uses 500 trees, whilst rf2 uses 1000 trees
rf1 = randomForest(tags ~ .,data = rfdf,importance=T,proximity=T, ntree=500)
rf2 = randomForest(tags ~ .,data = rfdf,importance=T,proximity=T, ntree=1000)
rf1
rf2
rfdfts = data.frame(matrix(0,dim(testset)[2],dim(rfdf)[2]-1))
names(rfdfts) = names(rfdf)[-1]
rfdfval = data.frame(matrix(0,dim(valset)[2],dim(rfdf)[2]-1))
names(rfdfval) = names(rfdf)[-1]
for (i in 1:(dim(valset)[1])) {
  t0 = table(wordslistval[[i]])
  rfdfval[i,] = sapply(colnames(mat), function(x) {u = match(x,names(t0));ifelse(is.na(u),0,t0[u])})
}
for (i in 1:(dim(testset)[1])) {
  t0 = table(wordslistts[[i]])
  rfdfts[i,] = sapply(colnames(mat), function(x) {u = match(x,names(t0));ifelse(is.na(u),0,t0[u])})
}

# confusion matrix function
confusion = function(allcat,pred,dat) {
  mat = matrix(0,length(allcat),length(allcat))
  colnames(mat) = allcat
  rownames(mat) = allcat
  x0 = pred
  y0 = dat
  for (i in 1:(length(dat))){
    mat[x0[i],y0[i]] = mat[x0[i],y0[i]] + 1
  }
  mat
}

# prediction of rf1, rf2 and a model using xgboost
# the reason to employ xgboost: it's that the xgboost is based on weak learner (smaller trees, high bias, low variance, more different "opinion" in the ensemble models)
# a classification problem like this is usually easier to be done by weaker learner (financial information texts would be a
# counter-example to this). 
confusionMat_rf1 = confusion(allcat,as.numeric(as.character(predict(rf1,rfdfval))),match(pull(valset,tags),allcat))
confusionMat_rf2 = confusion(allcat,as.numeric(as.character(predict(rf2,rfdfval))),match(pull(valset,tags),allcat))
xg1 = xgboost(as.matrix(rfdf[,-1]),label=as.numeric(rfdf[,1])-1,objective='multi:softmax',num_class=3,nrounds=30, max_depth=8)
confusionMat_xg1 = confusion(allcat,predict(xg1,as.matrix(rfdfval))+1,match(pull(valset,tags),allcat))

confusionMat_rf1
#         足球 梁振英 美國大選
#足球      414      5        4
#梁振英      0    178        0
#美國大選    0      1      177
confusionMat_rf2
#         足球 梁振英 美國大選
#足球      414      5        4
#梁振英      0    179        0
#美國大選    0      0      177
confusionMat_xg1
#         足球 梁振英 美國大選
#足球      414      5        2
#梁振英      0    179        0
#美國大選    0      0      179

# their hit rates
mean(allcat[predict(rf1,rfdfval)]==pull(valset,tags)) # 0.987163
mean(allcat[predict(rf2,rfdfval)]==pull(valset,tags)) # 0.9884467
mean(allcat[predict(xg1,as.matrix(rfdfval))+1]==pull(valset,tags)) # 0.9910141

testset$pred = allcat[predict(xg1,as.matrix(rfdfts))+1]
write.csv(testset,'testset_output',row.names=F)

# 1. How well does your model perform
# A: The best model based on xgboost has hit rate 99.1%, out of 974 occasions, only 7 of them are wrong classified.

# 2. How did you choose your model parameters?
# A: We have explored different sets of hyperparameters for the xgboost method. We have tuned parameters based on its tree-depths 
# and robustness, the convergent rate for deeper tree (max_depth > 6) is slightly larger. 

# 3. On a high level, please explain your final model’s structure, and how it predicts tags from the article text
# A: In the beginning, we worked out the Chinese text segments of each of the articles using Jieba package. From the text segments, we
# form a document-term matrix to general 660 features which is the frequency of each keyword. The xgboost algorithm optimises an ensemble 
# of simple decision trees (depths < 8) to "vote" for the tags that we predict.

