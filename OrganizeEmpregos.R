#Read the Empregos Data
rm(list=ls())
library(tidyverse)
empregos<-read.csv2("Data\\EmpregosScraping.txt",header=F)

#Create the descriptive stat
temp<-empregos[seq(1,nrow(empregos),by=2),]
col1<-labels(table(temp[,1]))
col2<-labels(table(temp[,2]))
col3<-labels(table(temp[,3]))
col4<-labels(table(temp[,4]))
col5<-labels(table(temp[,5]))
col6<-labels(table(temp[,6]))
col7<-labels(table(temp[,7]))
col8<-labels(table(temp[,8]))
col9<-labels(table(temp[,9]))
col10<-labels(table(temp[,10]))
col11<-labels(table(temp[,11]))


#Create loop to organize the data
dataEmpregos<-data.frame("Ramo"="","Qualificação"="","Nome"="","Local"="","Idiomas"="","Formação"="","Descrição"="")
for(i in 1:nrow(empregos)){
  data<-empregos[i,]
  temp<-data.frame("Ramo"="","Qualificação"="","Nome"="","Local"="","Idiomas"="","Formação"="","Descrição"="")
  for(c in 1:ncol(data)){
    text<-as.character(data[1,c])
    if(str_detect(text, "Ramo")==TRUE){
      txt<-gsub("[[:punct:]]", "", text)
      txt<-str_replace(txt,"Ramo","")
      temp[,"Ramo"]<-paste(temp[,"Ramo"],txt)
    }
    else if(str_detect(text, "Nome")==TRUE){
      txt<-gsub("[[:punct:]]", "", text)
      txt<-str_replace(txt,"Nome","")
      temp[,"Nome"]<-paste(temp[,"Nome"],txt)
    }
    else if(str_detect(text, "Qualificação")==TRUE){
      txt<-gsub("[[:punct:]]", "", text)
      txt<-str_replace(txt,"Qualificação","")
      temp[,"Qualificação"]<-paste(temp[,"Qualificação"],txt)
    }
    else if(str_detect(text, "Local de Trabalho")==TRUE){
      txt<-gsub("[[:punct:]]", "", text)
      txt<-str_replace(txt,"Local de Trabalho","")
      temp[,"Local"]<-paste(temp[,"Local"],txt)
    }
    else if(str_detect(text, "Idiomas")==TRUE){
      txt<-gsub("[[:punct:]]", "",text)
      txt<-str_replace(txt,"Idiomas","")
      temp[,"Idiomas"]<-paste(temp[,"Idiomas"],txt)
    }
    else if(str_detect(text, "Formação")==TRUE){
      txt<-gsub("[[:punct:]]", "", text)
      txt<-str_replace(txt,"Formação","")
      temp[,"Formação"]<-paste(temp[,"Formação"],txt)
    }
    else if(str_detect(text, "Descrição")==TRUE){
      txt<-gsub("[[:punct:]]", "",text)
      txt<-str_replace(txt,"Descrição","")
      temp[,"Descrição"]<-paste(temp[,"Descrição"],txt)
    }
  }
  dataEmpregos<-rbind(dataEmpregos,temp)
}
dataEmpregos<-dataEmpregos[-1,]

dataEmpregos$Descrição<-str_replace(dataEmpregos$Descrição,"1","")
iDelete<-(gsub(" ","",dataEmpregos$Ramo)=="") & (gsub(" ","",dataEmpregos$Qualificação)=="") & (gsub(" ","",dataEmpregos$Nome)=="") & (gsub(" ","",dataEmpregos$Local)=="") & (gsub(" ","",dataEmpregos$Idiomas)=="") & (gsub(" ","",dataEmpregos$Formação)=="") & (gsub(" ","",dataEmpregos$Descrição)=="") 
final<-dataEmpregos[!iDelete,]

final$Local<-gsub('[0-9]+', '', final$Local)
final$Local<-gsub(' vaga', '', final$Local)
saveRDS(final,"Data\\Empregos.rds")

