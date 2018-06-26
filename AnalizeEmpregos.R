library(tidyverse)
library(readxl)
library(keras)
library(reticulate)
library(tidyverse)
library(tm)
library(stringi)
library(Zelig)
library(stylo)
library(MASS)
library(caret)
library(glmnet)
#Read the data
empregos.df<-read.csv("Data\\Empregos.csv")
#Read the Formulario (INFORMAÇÃO E COMUNICAÇÃO)
inform.df<-read_excel("Data\\Formulario.xlsx",1)
#Read the Formulario (OUTRAS ATIVIDADES DE SERVIÇOS)
outros.df<-read_excel("Data\\Formulario.xlsx",2)
#Read the Formulario (EDUCAÇÃO)
educaco.df<-read_excel("Data\\Formulario.xlsx",3)

#########################################################################################
###########################     GENERAL TEXT MINING    ##################################
#########################################################################################

#Read the data
profile<-read.csv2("Data/Occupation Structure/CBO2002 - PerfilOcupacional.csv")
profile<-profile[,c(5,7,9)]
profile$Description<-paste(profile$NOME_GRANDE_AREA,profile$NOME_ATIVIDADE)
profile<-profile[,c(-2,-3)]

#To lower
profile<-profile %>% mutate(Description = tolower(Description))

#Remove stopwords
profile$Description<-removeWords(profile$Description, c("\\f", stopwords("portuguese")))

#Remove acents
profile$Description<-stri_trans_general(profile$Description, "Latin-ASCII")

#Count the number of words by CBO (unigram)
count1 <- profile %>%
  unnest_tokens(word, Description, token = "ngrams", n = 1)
count1 <- count1 %>%
  group_by(COD_OCUPACAO) %>% 
  count(word, sort = TRUE)

#Count the number of words by CBO (bigram)
count2 <- profile %>%
  unnest_tokens(word, Description, token = "ngrams", n = 2)
count2 <- count2 %>%
  group_by(COD_OCUPACAO) %>% 
  count(word, sort = TRUE)

#Rbind
listWords<-bind_rows(count1,count2)

#Select the words with the highest variance
stat <- listWords %>% 
  group_by(word) %>% 
  summarise(variance = var(n),
            Mean = mean(n)) 
stat[ is.na(stat) ] <- 0 
stat$CV <- sqrt(stat$variance)/stat$Mean
stat[ is.na(stat) ] <- 0 
hist(stat$CV)
quantile(stat$CV,probs=seq(0,1,length.out = 100))

#Keep the top 5%
statCV <- stat %>% 
  filter(CV>0.5) %>% 
  select(word)

#Merge with listWords
listWordsFilter<- listWords %>% 
  inner_join(statCV,"word")

#Check if all CBO´s are here
length(unique(listWords$COD_OCUPACAO))
length(unique(listWordsFilter$COD_OCUPACAO))

#Create the data matrix
dataWords<- reshape2::dcast(listWordsFilter, COD_OCUPACAO ~ word, sum, fill=0) 

#Create the correlation plot
M<-cor(dataWords[,-1])
hist(M)
saveRDS(dataWords,"Data/CBOwords.rds")

#########################################################################################
###########################  INFORMAÇÃO E COMUNICAÇÃO  ##################################
#########################################################################################
#Read the data
dataWords<-readRDS("Data/CBOwords.rds")

#Create the outcome
inform.df$Information<-1
inform.df<-inform.df[,-2]
colnames(inform.df)<-c("COD_OCUPACAO","Y")
inform.df<-unique(inform.df)

#Not found
inform.df$COD_OCUPACAO[!inform.df$COD_OCUPACAO%in%dataWords$COD_OCUPACAO]

#Merge the dataset
full.df<-full_join(dataWords,inform.df,by="COD_OCUPACAO")

#Complete the prediction
full.df$Y[is.na(full.df$Y)]<-0

#https://stackoverflow.com/questions/43877848/lasso-logistic-regression-suitable-for-imbalanced-data
#up-sampling
balanced.df<-rbind(full.df,full.df[sample(which(full.df$Y==1),2584,TRUE),])

#Regularized Logistic Regression
balanced.df<-balanced.df[complete.cases(balanced.df),]
x <- as.matrix(balanced.df[,2:8607])
Y <- factor(balanced.df$Y)
glmmod <- glmnet(x, y=as.factor(balanced.df$Y),alpha=1, family="binomial")
plot(glmmod)
cvfit <- cv.glmnet(x, y=Y, alpha=1, family="binomial")
plot(cvfit)
cvfit$lambda.min
coef(cvfit, s = "lambda.min")


#Feature Selection (Stepwise)
#full.model <- glm(Y ~., data = full.df, family = binomial)
#step.model <- full.model %>% stepAIC(trace = FALSE)


#Feature Selection (http://r-statistics.co/Variable-Selection-and-Importance-With-R.html)
#full.df<-full.df[,-1]
#full.df<-na.omit(full.df)
#library(earth)
#marsModel <- earth(Y ~ ., data=full.df) # build model
#ev <- evimp (marsModel) # estimate variable importance
#plot(ev)
#full.df[1:3,c(4053,7100,7630)]
#colvars<-ev[which(colvars[,"used"]==1),1]
#colvars<-c(4165,2455,7712,1689,489,1690,1688,6471,7695)
#colvars<-ev[,1]
#few.df<-full.df[,colvars]
#few.df$Y<-full.df$Y

# Regression in Rare Events Data. 
#http://docs.zeligproject.org/articles/zelig_relogit.html

#z.out <- zelig(eval(step.model$call[[2]]), model = "relogit", tau = 8/2000,
#               bias.correct = TRUE,
#               data = few.df)
#summary(z.out)

#Remove stopwords
emprego.txt<-removeWords(as.character(empregos.df$Descrição.), c("\\f", stopwords("portuguese")))

#Remove acents, pounctuation, space and lowecase
emprego.txt<-stri_trans_general(emprego.txt, "Latin-ASCII")
emprego.txt<-gsub('[[:punct:] ]+',' ',emprego.txt)
emprego.txt<-gsub("\\s+"," ",emprego.txt)
emprego.txt<-tolower(emprego.txt)

#Predicting the probability length(emprego.txt)
pred.df<-balanced.df[1,c(-1,-8608)]
pred.df[1,]<-NA
for(i in 1:length(emprego.txt)){
  pred<-balanced.df[1,c(-1,-8608)]
  pred[1,]<-0
  words.sel<- colnames(pred)
  txt<-emprego.txt[i]
  empre.txt<- txt.to.words(txt)
  empre.txt<-try(c(empre.txt, make.ngrams(empre.txt, ngram.size = 2)),silent = T)
  empre.txt<-table(empre.txt)
  inter<-intersect(words.sel, names(empre.txt))
  empre.txt<- empre.txt[inter]
  if(!identical(inter, character(0))){
    ind<-words.sel%in%names(empre.txt)
    pred[1,ind]<-as.numeric(empre.txt)
  }
  pred.df<-rbind(pred.df,pred)
}
pred.df<-pred.df[-1,]
newx <- as.matrix(pred.df)
pred.df$Prob<-  predict(cvfit, newx = newx, s = "lambda.min", type="response")
#pred.df$Prob1<- as.numeric(unlist(predict(z.out, newdata=pred.df, type="response")))
#pred.df$Prob2<- as.numeric(unlist(predict(step.model, newdata=pred.df, type="response")))

save.image("Data\\EmpregosCount.RData")
  
#Text Features
indKeep<-which(pred.df$Prob>0.5)
text<-emprego.txt[indKeep]
