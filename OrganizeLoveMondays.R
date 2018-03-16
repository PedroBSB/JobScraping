#Read the Empregos Data
rm(list=ls())
library(tidyverse)

dat2 <- readLines("Data\\LoveMondaysScrapping.txt") # read whatever is in there, one line at a time
varnames <- unlist(strsplit(dat2[1], "@")) # extract variable names
nvar <- length(varnames)

k <- 1 # setting up a counter
dat1 <- matrix(NA, ncol = nvar, dimnames = list(NULL, varnames))

while(k <= length(dat2)){
  k <- k + 1
  if(dat2[k] == "") {k <- k + 1
  print(paste("data line", k, "is an empty string"))
  if(k > length(dat2)) {break}
  }
  temp <- dat2[k]
  # checks if there are enough commas or if the line was broken
  while((stringr::str_count(temp, "@") < nvar-1) || (stringr::str_count(dat2[k+1], "@") == 0)){
    k <- k + 1
    temp <- paste0(temp, stringr::str_trim(dat2[k]))
  }
  temp <- unlist(strsplit(temp, "@"))
  #message(k)
  dat1 <- rbind(dat1, temp)
}

love <- dat1[-1,] # delete the empty initial row    
love <- data.frame(love)

#Create description
descr<-as.character(love$Description)
splitStr<- strsplit(descr,"\\|")

#Position
position<-lapply(splitStr,function(x) x[2])
love$Poistion2<-position

#Responsability
pos <- regexpr('Principais Responsabilidades:\\|', descr)
respon<-ifelse(pos>0,substr(descr,pos+nchar('Principais Responsabilidades:'),nchar(descr)),"")
respon<-ifelse(pos>0,substr(respon,1,regexpr('\\|\\|', respon)),"")
respon<-gsub("\\|","",respon)
love$Responsability<-respon

#Knowledge
pos <- regexpr('Conhecimentos Aplicados:', descr)
know<-ifelse(pos>0,substr(descr,pos+nchar('Conhecimentos Aplicados:'),nchar(descr)),"")
know<-ifelse(pos>0,substr(know,1,regexpr('\\|', know)),"")
know<-gsub("\\|","",know)
love$Knowledge<-know

#Professional
pos <- regexpr('Experiência Profissional:', descr)
prof<-ifelse(pos>0,substr(descr,pos+nchar('Experiência Profissional:'),nchar(descr)),"")
prof<-ifelse(pos>0,substr(prof,1,regexpr('\\|', prof)),"")
prof<-gsub("\\|","",prof)
love$Professional<-prof

#Formation
pos <- regexpr('Formação:', descr)
form<-ifelse(pos>0,substr(descr,pos+nchar('Formação:'),nchar(descr)),"")
form<-ifelse(pos>0,substr(form,1,regexpr('\\|', form)),"")
form<-gsub("\\|","",form)
love$Formation<-form

#Techcnical
pos <- regexpr('Conhecimentos Técnicos:', descr)
tech<-ifelse(pos>0,substr(descr,pos+nchar('Conhecimentos Técnicos:'),nchar(descr)),"")
tech<-ifelse(pos>0,substr(tech,1,regexpr('\\|', tech)),"")
tech<-gsub("\\|","",tech)
love$Techcnical<-tech

#Requirements
pos <- regexpr('_Requisitos para seleção:_', descr)
requ<-ifelse(pos>0,substr(descr,pos+nchar('\\_Requisitos para seleção:\\_'),nchar(descr)),"")
requ<-ifelse(pos>0,substr(requ,1,regexpr('\\|', requ)),"")
requ<-gsub("\\|","",requ)

#Informatioon
pos <- regexpr('_Dados Gerais:_', descr)
info<-ifelse(pos>0,substr(descr,pos+nchar('\\_Dados Gerais:\\_'),nchar(descr)),"")
info<-ifelse(pos>0,substr(info,1,regexpr('\\|', info)),"")
info<-gsub("\\|","",info)
love$Information<-info

#Location
pos <- regexpr('Local de trabalho:', descr)
loca<-ifelse(pos>0,substr(descr,pos+nchar('Local de trabalho:'),nchar(descr)),"")
loca<-ifelse(pos>0,substr(loca,1,regexpr('\\|', loca)),"")
loca<-gsub("\\|","",loca)
love$Location<-loca

#Salary
pos <- regexpr('Pacote de Remuneração:', descr)
salary<-ifelse(pos>0,substr(descr,pos+nchar('Pacote de Remuneração:||*'),nchar(descr)),"")
salary<-ifelse(pos>0,substr(salary,1,regexpr('\\|', salary)),"")
salary<-gsub("\\|","",salary)


#Atribution
pos <- regexpr('\\_Principais atribuições do cargo:\\_', descr)
attrib<-ifelse(pos>0,substr(descr,pos+nchar('_Principais atribuições do cargo:_|'),nchar(descr)),"")
attrib<-ifelse(pos>0,substr(attrib,1,regexpr('\\|', attrib)),"")
attrib<-gsub("\\|","",attrib)
love$Atribution<-attrib


#Requirements 2
pos <- regexpr('\\|Exigências', descr)
requ2<-ifelse(pos>0,substr(descr,pos+nchar('\\|Exigências'),nchar(descr)),"")
requ2<-ifelse(pos>0,substr(requ2,1,regexpr('\\|', requ2)),"")
requ2<-gsub("\\|","",requ2)
requ[requ==""]<-requ2[requ==""]

#Firm
pos <- regexpr('Empresa\\|', descr)
firm<-ifelse(pos>0,substr(descr,pos+nchar('Empresa|'),nchar(descr)),"")
firm<-ifelse(pos>0,substr(firm,1,regexpr('\\|', firm)),"")
firm<-gsub("\\|","",firm)
love$Firm2<-firm

#Contract
pos <- regexpr('Tipo de contrato\\|', descr)
contr<-ifelse(pos>0,substr(descr,pos+nchar('ETipo de contrato|'),nchar(descr)),"")
contr<-ifelse(pos>0,substr(contr,1,regexpr('\\|', contr)),"")
contr<-gsub("\\|","",contr)
love$Contract<-contr


#Salary 2
pos <- regexpr('Salário\\|', descr)
salary2<-ifelse(pos>0,substr(descr,pos+nchar('Salário|'),nchar(descr)),"")
salary2<-ifelse(pos>0,substr(salary2,1,regexpr('\\|', salary2)),"")
salary2<-gsub("\\|","",salary2)
salary[salary==""]<-salary2[salary==""]
love$Salary<-salary

#Requirements 3
pos <- regexpr('Área e especialização profissional:', descr)
requ3<-ifelse(pos>0,substr(descr,pos+nchar('Área e especialização profissional:'),nchar(descr)),"")
requ3<-ifelse(pos>0,substr(requ3,1,regexpr('\\|', requ3)),"")
requ3<-gsub("\\|","",requ3)
requ[requ==""]<-requ3[requ==""]
love$Requirements<-requ


saveRDS(love,"Data\\LoveMondays.rds")

