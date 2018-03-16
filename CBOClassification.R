rm(list=ls())
library(writexl)

#Read empregos
empregos<-readRDS("Data\\Empregos.rds")
write.csv(empregos,"Data\\Empregos.csv")
#Read LoveMondays
love<-readRDS("Data\\LoveMondays.rds")
love <- apply(love,2,as.character)
write.csv(love,"Data\\LoveMondays.csv")
#Read Catho
catho<-readRDS("Data\\Catho.rds")
write.csv(catho,"Data\\Catho.csv")

#Create the list of positions (LoveMondays)
love.pos<-love$Position
love.pos<-gsub("Vaga de ","",love.pos)
love.freq<-table(love.pos)
love.freq<-data.frame("Position"=names(love.freq))
write_xlsx(love.freq,"Data\\CBO1.xlsx")

#Create the list of positions (Empregos)
emp.pos<-empregos$Position
emp.freq<-table(emp.pos)
emp.freq<-data.frame("Position"=names(emp.freq))
write_xlsx(emp.freq,"Data\\CBO2.xlsx")

#Create the list of positions (Catho)
catho.pos<-catho$Position
catho.freq<-table(catho.pos)
catho.freq<-data.frame("Position"=names(catho.freq))
write_xlsx(catho.freq,"Data\\CBO3.xlsx")


#Read the CBO
cbo<- read.csv2("CBO\\CBO2002.csv")

#keras::install_keras()
#keras::install_keras(tensorflow = "gpu")
library(keras)
#install_keras()

imdb <- dataset_imdb(num_words = 10000)
train_data <- imdb$train$x
train_labels <- imdb$train$y
test_data <- imdb$test$x
test_labels <- imdb$test$y

#https://tensorflow.rstudio.com/blog/word-embeddings-with-keras.html
#https://tensorflow.rstudio.com/blog/text-classification-with-keras.html
#https://blog.rstudio.com/2017/09/05/keras-for-r/
#https://github.com/rstudio/keras/issues/318

