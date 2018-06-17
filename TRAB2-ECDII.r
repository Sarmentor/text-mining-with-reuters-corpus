
setwd("C:\\Users\\Rui Sarmento\\Documents\\Mestrado\\Ano 1\\ECD 2\\Trabalho2")
library(tm)


##########################################
###### TRABALHO ECDII - TEXT MINING ######
##########################################

#Create the Corpus of documents
moneyfx <- Corpus( DirSource ("money-fx"), readerControl=list(reader=readPlain,language="en_US" ) )
crude <- Corpus( DirSource ("crude"), readerControl=list(reader=readPlain,language="en_US" ) )

#Criamos dados de treino e de teste para cada um dos classificadores

moneyfx.train <- moneyfx[1:as.integer(length(moneyfx)*0.7)]
moneyfx.test <- moneyfx[(as.integer(length(moneyfx)*0.7)+1):length(moneyfx)]

crude.train <- crude[1:as.integer(length(crude)*0.7)]
crude.test <- crude[(as.integer(length(crude)*0.7)+1):length(crude)]


l1 <- length(crude.train)
l2 <- length(moneyfx.train)
l3 <- length(crude.test)
l4 <- length(moneyfx.test)

docs <- c(crude.train, moneyfx.train, crude.test, moneyfx.test)

#preprocessamento do texto de todos os conjuntos de teste e treino
docs.p <- docs
docs.p <- tm_map(docs.p, PlainTextDocument)
docs.p <- tm_map(docs.p, removeWords, stopwords(language="english"))
docs.p <- tm_map(docs.p, stripWhitespace)
docs.p <- tm_map(docs.p, tolower)
docs.p <- tm_map(docs.p, removePunctuation)
docs.p <- tm_map(docs.p, removeNumbers)

#browser()

#matriz DTM Document-Term Matrix
dtm.mx <- DocumentTermMatrix(docs.p,control=list(minWordLength=3, minDocFreq=2))
#matriz DTM com Tfidf
dtm.mx.tfidf <- DocumentTermMatrix( docs.p, control=list(weighting=weightTfIdf,
minWordLength=3, minDocFreq=2))

#browser()

#procura termos frequentes que aparecem pelo menos 100 vezes em um dos documentos
freqterms100 <- findFreqTerms( dtm.mx, 100)

#procura termos frequentes que aparecem pelo menos 100 vezes em um dos documentos
freqterms40 <- findFreqTerms( dtm.mx, 40)

#browser()


##Ciclo que varia a quantidade de remoção de termos menos frequentes para encontrar os melhores resultados
##da classificação com os diversos classificadores

#inicialização de algumas variaveis
sparse <- 0.71
sparse1 <- sparse
resultados <- data.frame(row.names = c("Decision Trees", "K-Nearest Neighbours","Neural Networks","Naive Bayes","Support Vector Machines"))
resultados_recall <- data.frame(row.names = c("Decision Trees", "K-Nearest Neighbours","Neural Networks","Naive Bayes","Support Vector Machines"))
resultados_precision <- data.frame(row.names = c("Decision Trees", "K-Nearest Neighbours","Neural Networks","Naive Bayes","Support Vector Machines"))
resultados_f1 <- data.frame(row.names = c("Decision Trees", "K-Nearest Neighbours","Neural Networks","Naive Bayes","Support Vector Machines"))
var.sparse <- data.frame(row.names = c("Sparse Value", "Number of Terms"))

while (sparse < 1){

#remove os termos menos frequentes, variar valor de input para procurar melhores classificações
dtm.mx.aux <- removeSparseTerms(dtm.mx, sparse)
dtm.mx.tfidf.aux <- removeSparseTerms(dtm.mx.tfidf, sparse)

#browser()

#transforma DTM em Dataframe para posterior classificação
dtm <- as.data.frame(inspect(dtm.mx.aux))
var.sparse <- cbind(var.sparse,c(sparse,ncol(dtm)))
dtm.tfidf <- as.data.frame(inspect(dtm.mx.tfidf.aux))

#browser()

# Prepara o classificador para a classificação
# com classe na última coluna 

class <- c(rep("crude", 233),rep("moneyfx", 240),rep("crude", 101),rep("moneyfx", 104))

class.ts <- class[474:678]
class.tr <- class[1:473]

dtm <- cbind(dtm, class)
dtm.tfidf <- cbind(dtm.tfidf, class)

#Generate the training data with the appropriate lines of dtm
dtm.tr <- dtm[1:(l1+l2), 1:ncol(dtm)]
#O mesmo para DTM com tfidf
dtm.tfidf.tr <- dtm.tfidf[1:(l1+l2), 1:ncol(dtm.tfidf)]
#dim(dtm.tr)

#Generate the test data with the appropriate lines of dtm
#retira a coluna das classes
dtm.ts <- dtm[(l1+l2+1):(l1+l2+l3+l4),1:(ncol(dtm)-1)]
dtm.tfidf.ts <- dtm.tfidf[(l1+l2+1):(l1+l2+l3+l4),1:(ncol(dtm.tfidf)-1)]

#Teste com tfidf - é comentado para classificação sem tfidf
# dtm <- dtm.tfidf
# dtm.ts <- dtm.tfidf.ts
# dtm.tr <- dtm.tfidf.tr

#########################################
####Classificação com Decision Trees#####
#########################################

library(rpart)

info.terms <- vector()
find.info.terms <- function(dtm.tr,min.info){
ix.class <- ncol(dtm.tr)
default.info <- info(table(dtm.tr[,ix.class]))
#cat("default.info: ", default.info, "\n")
n.atr <- ncol(dtm.tr)-1
n.info.terms <- 0
info.term.ixs <- vector()
col.names <- names(dtm.tr)

for (atri in 1:n.atr) {
if (sum(dtm.tr[,atri])>0)
{ # begin if
no.dif.atr.val<-length(table(dtm.tr[,atri]))
atr.class.table<-table(dtm.tr[,atri],dtm.tr[,ix.class])
n.rows<-nrow(dtm.tr)
atr.info <- 0
for (atr.val in 1: no.dif.atr.val)
{ # begin for
atr.peso <- sum( atr.class.table[atr.val,]) / n.rows
atr.info1 <- atr.peso * info(atr.class.table[atr.val,])
atr.info<-atr.info + atr.info1 }
info.gain <- default.info - atr.info
if (info.gain > min.info)
{ info.term.ixs[n.info.terms] <- atri
n.info.terms <- n.info.terms+1 }
} #end for
} # end if

#cat("\n", "Vão ser mantidos ", n.info.terms, " atributos: ", "\n")
#cat( col.names[info.term.ixs[1:10]], " etc. ")
#cat( col.names[info.term.ixs[n.info.terms-1]],"\n")
#cat("Vão ser eliminados ", n.atr-n.info.terms, " atributos", "\n")
return(col.names[info.term.ixs]) # Corrected 29 June 2011
} # end function

#Function “info” calculates information relative to a list specifying a distribution:
info <- function(x){
inf <- 0
sumx <- sum(x)
for (i in x) {
pi <- i/sumx
infi <- (pi)*log2(pi)
if (is.na(infi)) infi <- 0
inf <- inf - infi }
return(inf)
}

info.terms <- find.info.terms(dtm.tr,0.005)

rename.terms.in.list <- function(list) {
for (i in 1:length(list)) {
#cat("replaced", list[i], "at", i, "with", paste(list[i],".t", sep=""), "\n")
list[i]<- paste(list[i],".t", sep="")
} #end for i
return(list)
}

#renomeia os termos para evitar problemas na classificação com DT
info.terms.dt <- rename.terms.in.list(info.terms)

rename.terms.in.dtm <- function(dtm) {
for (i in 1:length(dtm)) {
#cat("replaced", names(dtm)[i], "at", i, "with", paste(names(dtm)[i],".t", sep=""), "\n")
names(dtm)[i] <- paste(names(dtm)[i],".t", sep="")
} #end for i
return(dtm)
}

dtm.tr.dt <- rename.terms.in.dtm(dtm.tr)
dtm.ts.dt <- rename.terms.in.dtm(dtm.ts)
names.tr.dt <- paste(info.terms.dt, collapse="+")
names.tr <- paste(info.terms, collapse="+")

#Formula para a classificação com decision trees
clas.formula <- as.formula( paste("class.t", names.tr.dt, sep="~") )

#browser()
dt <- rpart(clas.formula, dtm.tr.dt)


#Prediction 
preds.dt <- predict(dt, dtm.ts.dt, type="class")

#browser()

#Construct a confusion matrix:
conf.mx.dt <- table(class.ts, preds.dt)

#Percentagem de erros de previsão
error.rate.dt <- (sum(conf.mx.dt) - sum(diag(conf.mx.dt))) / sum(conf.mx.dt)

#Avaliação do classificador
tp.dt <- conf.mx.dt[1,1] #(true positives)
fp.dt <- conf.mx.dt[2,1] #(false positives)
tn.dt <- conf.mx.dt[2,2] #(true negatives)
fn.dt <- conf.mx.dt[1,2] #(false negatives)
error.rate.dt <- (tp.dt + tn.dt) / (tp.dt + tn.dt + fp.dt + fn.dt)
recall.dt = tp.dt / (tp.dt + fn.dt)
precision.dt = tp.dt / (tp.dt + fp.dt)
f1.dt = 2 * precision.dt * recall.dt / (precision.dt + recall.dt)

#Formula para a classificação dos restantes classificadores
clas.formula <- as.formula( paste("class", names.tr, sep="~") )

#########################################
####Classificação com Redes Neuronais####
#########################################

#browser()

library(nnet)
nnet.classifier <- nnet(clas.formula, data = dtm.tr, size=2, rang=0.1,decay=5e-4, maxit=200)
preds.nn <- predict(nnet.classifier, dtm.ts, type="class")
conf.mx.nn <- table(class.ts, preds.nn)
error.rate.nn <- (sum(conf.mx.nn) - sum(diag(conf.mx.nn))) / sum(conf.mx.nn)

#Avaliação do classificador
tp.nn <- conf.mx.nn[1,1] #(true positives)
fp.nn <- conf.mx.nn[2,1] #(false positives)
tn.nn <- conf.mx.nn[2,2] #(true negatives)
fn.nn <- conf.mx.nn[1,2] #(false negatives)
error.rate.nn <- (tp.nn + tn.nn) / (tp.nn + tn.nn + fp.nn + fn.nn)
recall.nn = tp.nn / (tp.nn + fn.nn)
precision.nn = tp.nn / (tp.nn + fp.nn)
f1.nn = 2 * precision.nn * recall.nn / (precision.nn + recall.nn)

############################################
####Classificação com Classificador K-NN####
############################################

#browser()

#Notes:
#Usage of tfidf coding could improve this result

library(class)
preds.knn <- knn(dtm.tr[, info.terms], dtm.ts[, info.terms], class.tr, k=1)
conf.mx.knn <- table(class.ts, preds.knn)
error.rate.knn <- (sum(conf.mx.knn) - sum(diag(conf.mx.knn))) / sum(conf.mx.knn)

#Avaliação do classificador
tp.knn <- conf.mx.knn[1,1] #(true positives)
fp.knn <- conf.mx.knn[2,1] #(false positives)
tn.knn <- conf.mx.knn[2,2] #(true negatives)
fn.knn <- conf.mx.knn[1,2] #(false negatives)
error.rate.knn <- (tp.knn + tn.knn) / (tp.knn + tn.knn + fp.knn + fn.knn)
recall.knn = tp.knn / (tp.knn + fn.knn)
precision.knn = tp.knn / (tp.knn + fp.knn)
f1.knn = 2 * precision.knn * recall.knn / (precision.knn + recall.knn)

#############################
####Classificação com SVM####
#############################

#browser()

library(e1071)
svm.classifier <- svm(clas.formula, dtm.tr)
preds.svm <- predict(svm.classifier, dtm.ts)
conf.mx.svm <- table(class.ts, preds.svm)
error.rate.svm <- (sum(conf.mx.svm) - sum(diag(conf.mx.svm))) / sum(conf.mx.svm)

#Avaliação do classificador
tp.svm <- conf.mx.svm[1,1] #(true positives)
fp.svm <- conf.mx.svm[2,1] #(false positives)
tn.svm <- conf.mx.svm[2,2] #(true negatives)
fn.svm <- conf.mx.svm[1,2] #(false negatives)
error.rate.svm <- (tp.svm + tn.svm) / (tp.svm + tn.svm + fp.svm + fn.svm)
recall.svm = tp.svm / (tp.svm + fn.svm)
precision.svm = tp.svm / (tp.svm + fp.svm)
f1.svm = 2 * precision.svm * recall.svm / (precision.svm + recall.svm)


#####################################
####Classificação com Naive Bayes####
#####################################

#browser()

library(RWeka)
NB<-make_Weka_classifier("weka/classifiers/bayes/NaiveBayes")
nb.classifier<-NB(clas.formula, dtm.tr)
preds.nb<-predict(nb.classifier, dtm.ts)
conf.mx.nb<-table(class.ts, preds.nb)
error.rate.nb <- (sum(conf.mx.nb) - sum(diag(conf.mx.nb))) / sum(conf.mx.nb)

#Avaliação do classificador
tp.nb <- conf.mx.nb[1,1] #(true positives)
fp.nb <- conf.mx.nb[2,1] #(false positives)
tn.nb <- conf.mx.nb[2,2] #(true negatives)
fn.nb <- conf.mx.nb[1,2] #(false negatives)
error.rate.nb <- (tp.nb + tn.nb) / (tp.nb + tn.nb + fp.nb + fn.nb)
recall.nb = tp.nb / (tp.nb + fn.nb)
precision.nb = tp.nb / (tp.nb + fp.nb)
f1.nb = 2 * precision.nb * recall.nb / (precision.nb + recall.nb)



resultados <- cbind(resultados, c(error.rate.dt,error.rate.knn,error.rate.nn,error.rate.nb,error.rate.svm))
resultados_recall <- cbind(resultados_recall, c(recall.dt,recall.knn,recall.nn,recall.nb,recall.svm))
resultados_precision <- cbind(resultados_precision, c(precision.dt,precision.knn,precision.nn,precision.nb,precision.svm))
resultados_f1 <- cbind(resultados_f1, c(f1.dt,f1.knn,f1.nn,f1.nb,f1.svm))

sparse <- sparse + 0.01
}
colnames(resultados) = c(as.character(seq(sparse1,0.99,0.01)))
colnames(resultados_precision) = c(as.character(seq(sparse1,0.99,0.01)))
colnames(resultados_recall) = c(as.character(seq(sparse1,0.99,0.01)))
colnames(resultados_f1) = c(as.character(seq(sparse1,0.99,0.01)))