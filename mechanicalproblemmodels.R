#Text mining in R
#part 5.1 code Document Classification
# update.packages()
# install.packages('installr')



setwd("D:\\python projects\\seminar")

# remove.packages("cli")
# 
# install.packages("rlang")
library(tm) #preprocessing, term document matrix, corpus
library(readr)
library(devtools)
library(stringi)

locale()
locale("he")

options(stringsAsFactors = FALSE)
Sys.setlocale("LC_ALL", 'Hebrew')
Sys.setenv(LANG = "Hebrew")


##########################################################################################
library(tm) #preprocessing, term document matrix, corpus
library(Matrix) #provides memory efficient methods for manipulating sparse matrices.
library(glmnet) #regression model
library(caret) #data partitioning, confusion matrix
library(pROC) #ROC curves
library(ggthemes) #visualization
library(ggplot2) #visualization
library(tidyverse)

options(stringsAsFactors = F)


custom.stopwords <- c(stopwords("english"),'enter')

#cleaning function
comment.clean <-function(text){
  corpus <- VCorpus(VectorSource(text))
  corpus <- tm_map(corpus, content_transformer(LinksRemover))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <-tm_map(corpus, content_transformer(function(x) gsub(x, pattern = ",", replacement = ", ")))
  # corpus <-tm_map(corpus, content_transformer(function(x) gsub(x, pattern = ".", replacement = ". ")))
  # corpus <-tm_map(corpus, content_transformer(function(x) gsub(x, pattern = "?", replacement = "? ")))
  # corpus <-tm_map(corpus, content_transformer(function(x) gsub(x, pattern = ";", replacement = "; ")))
  #remove stopwords
  # corpus <- tm_map(corpus, removeWords, stopwords('english'))
  corpus <- tm_map(corpus, content_transformer(PlainTextDocument))
  
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeWords, custom.stopwords) 
  return(corpus)
}

####################################################################################

#Partition the data
#The createDataPartition function selects random number of rows based on the dependent variable distribution.

mech_problem<-read.csv('D:\\python projects\\seminar\\dataforanalysishandlyclasificationnew .csv',encoding='iso-8859-8') #'iso-8859-8'
mech_problem<-mech_problem %>% drop_na()
mech_problem= mech_problem[sample(1:nrow(mech_problem)), ]
mech_problem= mech_problem[sample(1:nrow(mech_problem)), ]
mech_problem= mech_problem[sample(1:nrow(mech_problem)), ]
mech_problem= mech_problem[sample(1:nrow(mech_problem)), ]
mech_problem= mech_problem[sample(1:nrow(mech_problem)), ]

#CLEANING ALL COMMENTS
LinksRemover<- function(x){
  newval<-gsub(" ?(f|ht)tp(s?)://(.*)[.][a-z]+/", "", x)
  return(newval)
}



mech_problem$comment<-LinksRemover(mech_problem$comment)
mech_problem$comment<-iconv(mech_problem$comment,to="UTF-8")
mech_problem$comment<- gsub(mech_problem$comment, pattern = ",", replacement = ", ")
mech_problem$comment <- removeNumbers(mech_problem$comment)
mech_problem$comment <- removePunctuation(mech_problem$comment)
mech_problem$comment <- stripWhitespace(mech_problem$comment)

for(i in 1:5){
  print(mech_problem$comment[i])
}





dictio_replace= c('במאווררים' =  'מאוורר', 
                  'המאווררים' =  'מאוורר', 
                  'והמאווררים' =  'מאוורר', 
                  'למאווררים' =  'מאוורר', 
                  'מאווררים' =  'מאוורר', 
                  'מהמאווררים' =  'מאוורר', 
                  'ממאווררים' =  'מאוורר', 
                  'שהמאווררים' =  'מאוורר', 
                  'בטרמוסטט' =  'טרמוסטט', 
                  'הטרמוסטט' =  'טרמוסטט', 
                  'והטרמוסטט' =  'טרמוסטט', 
                  'טרמוסטט' =  'טרמוסטט', 
                  'טרמוסטטים' =  'טרמוסטט', 
                  'כשטרמוסטט' =  'טרמוסטט', 
                  'לטרמוסטט' =  'טרמוסטט', 
                  'מהטרמוסטט' =  'טרמוסטט', 
                  'מטרמוסטט' =  'טרמוסטט', 
                  'שהטרמוסטט' =  'טרמוסטט', 
                  'שטרמוסטט' =  'טרמוסטט', 
                  'בתרמוסטט' =  'טרמוסטט', 
                  'התרמוסטט' =  'טרמוסטט', 
                  'והתרמוסטט' =  'טרמוסטט', 
                  'לתרמוסטט' =  'טרמוסטט', 
                  'לתרמוסטטים' =  'טרמוסטט', 
                  'מהתרמוסטט' =  'טרמוסטט', 
                  'מתרמוסטט' =  'טרמוסטט', 
                  'שהתרמוסטט' =  'טרמוסטט', 
                  'שתרמוסטט' =  'טרמוסטט', 
                  'תרמוסטט' =  'טרמוסטט', 
                  'תרמוסטטים' =  'טרמוסטט', 
                  'במשאבת' =  'משאבת-מים', 
                  'המשאבת' =  'משאבת-מים', 
                  'למשאבת' =  'משאבת-מים', 
                  'מהמשאבת' =  'משאבת-מים', 
                  'ממשאבת' =  'משאבת-מים', 
                  'משאבת' =  'משאבת-מים', 
                  'שהמשאבת' =  'משאבת-מים', 
                  'שמשאבת' =  'משאבת-מים', 
                  'המשאבות' =  'משאבת-מים', 
                  'משאבות' =  'משאבת-מים', 
                  'בראדיאטור' =  'רדיאטור', 
                  'הראדיאטור' =  'רדיאטור', 
                  'כשראדיאטור' =  'רדיאטור', 
                  'לראדיאטור' =  'רדיאטור', 
                  'מהראדיאטור' =  'רדיאטור', 
                  'מראדיאטור' =  'רדיאטור', 
                  'ראדיאטור' =  'רדיאטור', 
                  'שהראדיאטור' =  'רדיאטור', 
                  'שראדיאטור' =  'רדיאטור', 
                  'ברדיאטור' =  'רדיאטור', 
                  'ברדיאטורים' =  'רדיאטור', 
                  'הרדיאטור' =  'רדיאטור', 
                  'הרדיאטורים' =  'רדיאטור', 
                  'והרדיאטור' =  'רדיאטור', 
                  'לרדיאטור' =  'רדיאטור', 
                  'לרדיאטורים' =  'רדיאטור', 
                  'מהרדיאטור' =  'רדיאטור', 
                  'מרדיאטור' =  'רדיאטור', 
                  'רדיאטור' =  'רדיאטור', 
                  'רדיאטורים' =  'רדיאטור', 
                  'שהרדיאטור' =  'רדיאטור', 
                  'שרדיאטור' =  'רדיאטור', 
                  'במשאבה' =  'משאבת-מים', 
                  'המשאבה' =  'משאבת-מים', 
                  'והמשאבה' =  'משאבת-מים', 
                  'כשמשאבה' =  'משאבת-מים', 
                  'למשאבה' =  'משאבת-מים', 
                  'מהמשאבה' =  'משאבת-מים', 
                  'משאבה' =  'משאבת-מים', 
                  'שהמשאבה' =  'משאבת-מים', 
                  'שמשאבה' =  'משאבת-מים', 
                  'במאוורר' =  'מאוורר', 
                  'המאוורר' =  'מאוורר', 
                  'והמאוורר' =  'מאוורר', 
                  'כשהמאוורר' =  'מאוורר', 
                  'למאוורר' =  'מאוורר', 
                  'מאוורר' =  'מאוורר', 
                  'מהמאוורר' =  'מאוורר', 
                  'ממאוורר' =  'מאוורר', 
                  'שהמאוורר' =  'מאוורר', 
                  'שמאוורר' =  'מאוורר', 
                  'בוונטה' =  'מאוורר', 
                  'הוונטה' =  'מאוורר', 
                  'והוונטה' =  'מאוורר', 
                  'וונטה' =  'מאוורר', 
                  'כשהוונטה' =  'מאוורר', 
                  'לוונטה' =  'מאוורר', 
                  'מהוונטה' =  'מאוורר', 
                  'מוונטה' =  'מאוורר', 
                  'שהוונטה' =  'מאוורר', 
                  'במאוור' =  'מאוורר', 
                  'המאוור' =  'מאוורר', 
                  'והמאוור' =  'מאוורר', 
                  'כשהמאוור' =  'מאוורר', 
                  'למאוור' =  'מאוורר', 
                  'מאוור' =  'מאוורר', 
                  'מהמאוור' =  'מאוורר', 
                  'ממאוור' =  'מאוורר', 
                  'שהמאוור' =  'מאוורר', 
                  'שמאוור' =  'מאוורר', 
                  'במאורר' =  'מאוורר', 
                  'במאוררים' =  'מאווררים', 
                  'המאורר' =  'מאוורר', 
                  'המאוררים' =  'מאווררים', 
                  'והמאורר' =  'מאוורר', 
                  'כשמאורר' =  'מאוורר', 
                  'למאורר' =  'מאוורר', 
                  'למאוררים' =  'מאווררים', 
                  'מאורר' =  'מאוורר', 
                  'מאוררים' =  'מאוורר', 
                  'שהמאורר' =  'מאוורר', 
                  'שהמאוררים' =  'מאווררים', 
                  'שמאורר' =  'מאוורר', 
                  'הונטילטור' =  'מאוורר', 
                  'ונטילטור' =  'מאוורר', 
                  'וונטילטור' =  'מאוורר', 
                  'ראדיאטורים' =  'רדיאטור', 
                  'מאשבות' =  'משאבת-מים', 
                  'מאשבה' =  'משאבת-מים', 
                  'מאשבת' =  'משאבת-מים', 
                  'משאבת-מים-מים' =  'משאבת-מים',
                  'מאווררררר' =  'מאוורר',
                  'מאוורררר' =  'מאוורר',
                  'מאווררר' =  'מאוורר'
)







mech_problem$comment <- stringr::str_replace_all(string = mech_problem$comment,
                                                 pattern= dictio_replace)
mech_problem$comment <- stringr::str_replace_all(string = mech_problem$comment,
                                                 pattern= dictio_replace)
mech_problem$comment <- stringr::str_replace_all(string = mech_problem$comment,
                                                 pattern= dictio_replace)

library(udpipe)
# heb_model <- udpipe_download_model(language = "hebrew-htb")
udmodel_heb <- udpipe_load_model(file ="D:\\python projects\\seminar\\hebrew-htb-ud-2.5-191206.udpipe" )
# udpipe_annotate(udmodel_heb, x = iconv('your text', to = 'UTF-8'))
x <- udpipe_annotate(udmodel_heb, x = mech_problem$comment)
x <- as.data.frame(x)

str(x)
View(x) #check the lemma column
table(x$upos)
table(x$dep_rel) #dependency relation to the HEAD


lemmaDF<-as.data.frame(x[, c("token", "lemma", "upos")])
lemmaDF<-lemmaDF[!(lemmaDF$lemma %in% c('ה', 'ב', 'ג', 'ל', 'ו')),]
lemmaDF<-lemmaDF[order(lemmaDF$token),]
lemmaDF<-lemmaDF %>% distinct()


lemmaDF$goodreplace<-if_else(is.na(lemmaDF$lemma) & is.na(lemmaDF$upos)  ,stri_sub(lemmaDF$token,-(nchar(lemmaDF$token)-1)),if_else(is.na(lemmaDF$lemma) & (!is.na(lemmaDF$upos)), lemmaDF$token, lemmaDF$lemma ))


lemmaDF<-lemmaDF %>%   filter(!is.na(goodreplace))
lemmaDF<-lemmaDF  %>%  filter_("goodreplace!=token")
lemmaDF <- subset(lemmaDF, select = c('token', 'goodreplace'))
# lemmaDF<-lemmaDF %>% distinct()
lemmaDF<-lemmaDF[order(lemmaDF$token),]
lemmaDF$token<- gsub(lemmaDF$token, pattern = "_", replacement = "")
# lemmaDF$lemma<- gsub(lemmaDF$lemma, pattern = "_", replacement = "")
lemmaDF<-lemmaDF %>% distinct()
lemmaDF<-lemmaDF %>% filter(nchar(lemmaDF$goodreplace)>1)


lemmadict<-lemmaDF
lemmadict_replace <- lemmadict$goodreplace
names(lemmadict_replace) <- lemmadict$token
lemmadict_replace[1]

typeof(lemmadict_replace)
is.vector(lemmadict_replace)

lemmadict_replace[1]

mech_problem$comment <- stringr::str_replace_all(string = mech_problem$comment,
                                                 pattern= lemmadict_replace)





#clean the training set
all.clean<-comment.clean(mech_problem$comment) #clean all data

for(i in 1:5){
  print(all.clean[[i]][1])
}




# Model BUILDING


# mech_problem$comment<-iconv(mech_problem$comment,to='iso-8859-8')

mech_problem_pos<-mech_problem %>% filter(Radiator_FG >0)
dim(mech_problem_pos)[1]

mech_problem_neg<-mech_problem %>% filter(Radiator_FG <1)
mech_problem_neg<-mech_problem_neg[sample(1:dim(mech_problem_pos)[1]), ]
dim(mech_problem_neg)[1]

mech_problem<-union_all(mech_problem_neg,mech_problem_pos)

set.seed(134)
mech_problem= mech_problem[sample(1:nrow(mech_problem)), ]



set.seed(1)
train<-createDataPartition(mech_problem$Radiator_FG,p=0.5,list = F) # p -> the percentage of data that goes to training. #list = F -> the result will be in a matrix and not a list
train.mech_problem<-mech_problem[train,]
test.mech_problem<-mech_problem[-train,]
# dim(test.mech_problem)




#n-grams
NLP_tokenizer_words <- function(x) {
  unlist(lapply(ngrams(words(x), 1:3), paste, collapse = "_"), use.names = F)
}


library(RWeka)
NLP_tokenizer_words <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 3))

# NLP_tokenizer_letters <- function(x) {
#   unlist(lapply(ngrams(words(unlist(strsplit(as.character(x), split = ""))), 1:6), paste, collapse = "_"), use.names = F)
# }
# 
clean.train<-comment.clean(train.mech_problem$comment) #clean the training set

typeof(clean.train)


clean.train[[40]][1]

#create a DTM
train.dtm <- DocumentTermMatrix(clean.train,control = list(
                                                           removePunctuation = F,
                                                           removeNumbers = T,
                                                           tokenize=NLP_tokenizer_words,
                                                           # stopwords = stopwords("english"), 
                                                           tolower = T,
                                                           stemming = T,
                                                           weighting = function(x)
                                                             weightTf(x)
))

#the orig DTM is null in this case

train.dtm
inspect(train.dtm)

train.matrix<-as.matrix(train.dtm)
#change the object into a sparse matrix (memory efficient)
#train.matrix<-Matrix(train.matrix, sparse = T) #sparse = T will automatically become true if more than half the values are 0
dim(train.matrix)



#build the model
cv<-cv.glmnet(train.matrix, y=as.factor(train.mech_problem$Radiator_FG), 
				alpha=0.3, 
				family='binomial', 
				nfolds = 10, 
				intercept = F, 
				type.measure = 'class')
###################
#advanced
#alpha=1 is the default. alpha can be between 0 to 1. alpha=1 is the lasso penalty, and alpha=0 the ridge penalty.
#alpha = 1 means the model will be simpler, more generic.
#family - use 'binomial' when y is binary. Use 'multinomial' when there are multiple classes.
#nfolds - performs cross validation when constructing a glmnet model, number of folds - 10, folds are chosen randomly
#type.measure - loss to use for cross-validation. "class" applies to binomial and multinomial logistic regression only and gives misclassification error.
#intercept = F, the intercept will be set to zero.
###################

#apply the model to the training set
preds<-predict(cv,train.matrix,type='class',s=cv$lambda.1se)
head(preds,10) #1 = clickbait




#predict for all the data
clean.alldata<-comment.clean(mech_problem$comment)
alldata.dtm <- DocumentTermMatrix(clean.alldata, control = list(dictionary=Terms(train.dtm), weighting = weightTfIdf))
alldata.dtm
alldata.matrix<-as.matrix(alldata.dtm)
alldata.matrix<-Matrix(alldata.matrix)

predsall<-predict(cv,alldata.matrix,type='class',s=cv$lambda.1se)
head(preds,10) #1 = clickbait
mech_problem$pred<-predsall

confusion.train <- confusionMatrix(as.factor(mech_problem$pred),as.factor(mech_problem$Radiator_FG),  positive = '1')
confusion.train

confusion.mechanicmodel <- confusionMatrix(as.factor(mech_problem$pred),as.factor(mech_problem$Radiator),  positive = '1')
confusion.mechanicmodel
mechanicmodel.auc<-roc(mech_problem$Radiator,as.numeric(mech_problem$pred))
plot(mechanicmodel.auc)
train.auc<-roc(mech_problem$Radiator_FG,as.numeric(mech_problem$pred))
train.auc
plot(train.auc)

#compare train and test
plot(train.auc,col="blue",main="RED = test, BLUE = train", adj=0) #adj=0, title aligned left
plot(mechanicmodel.auc, add=TRUE, col="red", lty=2)#lty=2, line is dashed

confusion.train <- confusionMatrix(as.factor(mech_problem$Radiator_FG),as.factor(mech_problem$Radiator),  positive = '1')
confusion.train


###################
#advanced
#type='class', the return information will be 1 or 0. type="response" will return probabilities.
#can change s to be cv$lambda.min to use a more complex model that minimizes the misclassification rate.
###################

#Evaluate the model
#create a roc curve
train.auc<-roc(train.mech_problem$Radiator_FG,as.numeric(preds))
train.auc
plot(train.auc)

#create the confusion matrix
confusion.train <- confusionMatrix(as.factor(preds),as.factor(train.mech_problem$Radiator_FG),  positive = '1')
confusion.train




#Test the model on new data
#clean the test set

# test.mech_problem$comment <- stringr::str_replace_all(string = test.mech_problem$comment,
#                                                        pattern= dictio_replace)
# test.mech_problem$comment <- stringr::str_replace_all(string = test.mech_problem$comment,
#                                                        pattern= dictio_replace)
# test.mech_problem$comment <- stringr::str_replace_all(string = test.mech_problem$comment,
#                                                        pattern= dictio_replace)



clean.test<-comment.clean(test.mech_problem$comment)




for(i in 1:5){
  print(clean.test[[i]][1])
}






#create the DTM
#match the matrix to contain same words as the training matrix
test.dtm <- DocumentTermMatrix(clean.test, control = list(dictionary=Terms(train.dtm)))
test.dtm
test.matrix<-as.matrix(test.dtm)
test.matrix<-Matrix(test.matrix)
dim(test.matrix)

#CREATE THE DTM TEST AS THE TRAINING
# test.dtm <- DocumentTermMatrix(clean.test,control = list(Terms(train.dtm),
#   removePunctuation = F,
#   removeNumbers = T, 
#   tokenize=NLP_tokenizer_letters,
#   # stopwords = stopwords("english"), 
#   tolower = T,
#   stemming = T,
#   weighting = function(x)
#     weightTf(x)
# ))


#the orig DTM is null in this case
test.dtm
inspect(test.dtm)

test.matrix<-as.matrix(test.dtm)
#change the object into a sparse matrix (memory efficient)
# test.matrix<-Matrix(test.matrix, sparse = T) #sparse = T will automatically become true if more than half the values are 0
dim(test.matrix)
dim(train.matrix)

#predict on the test data
preds<-predict(cv,test.matrix,type='class',s=cv$lambda.min, weighting = weightTfIdf)
head(preds)

#create a data frame that contains the doc row and the predicition
comment.preds<-data.frame(doc_row = rownames(test.mech_problem),class=preds[,1])
# head(comment.preds)

#calculate AUC
test.auc<-roc(test.mech_problem$Radiator_FG,as.numeric(preds))
test.auc
plot(test.auc)

#compare train and test
plot(train.auc,col="blue",main="RED = test, BLUE = train", adj=0) #adj=0, title aligned left
plot(test.auc, add=TRUE, col="red", lty=2)#lty=2, line is dashed

#confusion matrix
confusion.test <- confusionMatrix(as.factor(comment.preds[,2]),as.factor(test.mech_problem$Radiator_FG))
confusion.test


#######################################################################################
#finding the most impactful words
#model coefficients
glmnet.coef<-as.matrix(coef(cv, s='lambda.min'))

#add column names (words,glmnet_coefficients)
glmnet.coef<-data.frame(words=row.names(glmnet.coef),glmnet_coefficients=glmnet.coef[,1]) 

#sort in decreasing order
glmnet.coef<-glmnet.coef[order(glmnet.coef$glmnet_coefficients, decreasing = T),]

#convert words to factor (for ggplot)
glmnet.coef$words<-factor(glmnet.coef$words,levels=unique(glmnet.coef$words)) 

#1st and 3rd quartiles are 0 because the lasso regression forced many of the term coefficients to 0.
summary(glmnet.coef$glmnet_coefficients) 

#number of positive coefficients
length(subset(glmnet.coef$glmnet_coefficients,glmnet.coef$glmnet_coefficients>0)) 
#number of negative coefficients
length(subset(glmnet.coef$glmnet_coefficients,glmnet.coef$glmnet_coefficients<0)) 
#number of 0 coefficients
length(subset(glmnet.coef$glmnet_coefficients,glmnet.coef$glmnet_coefficients==0))

#create the density plot for word coefficients
ggplot(glmnet.coef,aes(x=glmnet_coefficients)) +
	 geom_line(stat = 'density', color='darkred',size=1) + 
		theme_gdocs()

#inspect the meaningful terms
top.coef<-rbind(head(glmnet.coef,10),tail(glmnet.coef,10))
top.coef$impact<-ifelse(top.coef$glmnet_coefficients>0,"Positive","Negative")#positive=clickbait
top.coef

ggplot(top.coef, aes(x=glmnet_coefficients, y=words)) + geom_segment(aes(yend=words), xend=0, colour="grey50")+
geom_point(size=3, aes(colour=impact)) + theme_few()
#geom_segment draws a straight line between points (x, y) and (xend, yend)








