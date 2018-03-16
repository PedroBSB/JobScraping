#Read the Empregos Data
rm(list=ls())
library(tidyverse)
stack<-read.csv2("Data\\EmpregosStackOverflow.txt",header=T)
stack<-stack[-1,]
iDelete<-(gsub(" ","",stack$Firm)=="") & (gsub(" ","",stack$Description)=="") & (gsub(" ","",stack$Link)=="")
stack<-stack[!iDelete,]
stack$Description<-str_replace(stack$Description,"Job description ","")
iDelete<-(gsub(" ","",stack$Link)=="")
stack<-stack[!iDelete,]
saveRDS(stack,"Data\\StackOverFlow.rds")
