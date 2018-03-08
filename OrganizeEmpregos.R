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
dataEmpregos<-data.frame("Ramo"="","Qualifica��o"="","Nome"="","Local"="","Idiomas"="","Forma��o"="","Descri��o"="")
for(i in 1:nrow(empregos)){
  data<-empregos[i,]
  temp<-data.frame("Ramo"="","Qualifica��o"="","Nome"="","Local"="","Idiomas"="","Forma��o"="","Descri��o"="")
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
    else if(str_detect(text, "Qualifica��o")==TRUE){
      txt<-gsub("[[:punct:]]", "", text)
      txt<-str_replace(txt,"Qualifica��o","")
      temp[,"Qualifica��o"]<-paste(temp[,"Qualifica��o"],txt)
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
    else if(str_detect(text, "Forma��o")==TRUE){
      txt<-gsub("[[:punct:]]", "", text)
      txt<-str_replace(txt,"Forma��o","")
      temp[,"Forma��o"]<-paste(temp[,"Forma��o"],txt)
    }
    else if(str_detect(text, "Descri��o")==TRUE){
      txt<-gsub("[[:punct:]]", "",text)
      txt<-str_replace(txt,"Descri��o","")
      temp[,"Descri��o"]<-paste(temp[,"Descri��o"],txt)
    }
  }
  dataEmpregos<-rbind(dataEmpregos,temp)
}
dataEmpregos<-dataEmpregos[-1,]

dataEmpregos$Descri��o<-str_replace(dataEmpregos$Descri��o,"1","")
iDelete<-(gsub(" ","",dataEmpregos$Ramo)=="") & (gsub(" ","",dataEmpregos$Qualifica��o)=="") & (gsub(" ","",dataEmpregos$Nome)=="") & (gsub(" ","",dataEmpregos$Local)=="") & (gsub(" ","",dataEmpregos$Idiomas)=="") & (gsub(" ","",dataEmpregos$Forma��o)=="") & (gsub(" ","",dataEmpregos$Descri��o)=="") 
final<-dataEmpregos[!iDelete,]

final$Local<-gsub('[0-9]+', '', final$Local)
final$Local<-gsub(' vaga', '', final$Local)
saveRDS(final,"Data\\Empregos.rds")
