#Call the libraries
library(rvest)
library(purrr)
library(RSelenium)
library(tidyverse)
library(stringr)
#Define the link with all positions
link <- read_html("https://www.empregos.com.br/vagas")

#Pages
pages <- link %>% 
  html_nodes(xpath='//*[@id="ctl00_ContentBody_pPaiResultadoTopo"]/strong[2]') %>% 
  html_text()

#Find the last page
posIni <- regexpr('de ', pages)+3
subsPage<- substr(pages,posIni,nchar(pages))
posEnd <- regexpr('\r',subsPage)-1
last<- as.numeric(gsub("[.]","",substr(subsPage,1,posEnd)))


pb <- txtProgressBar(min = 0, max = last)
for(i in 1:last){
  #Define the link with all positions
  link <- read_html(paste0("https://www.empregos.com.br/vagas/p",i))
  
  #Links
  jobLinks <- link %>% 
    html_nodes(xpath='//*[@id="ctl00_ContentBody_divPaiMioloBusca"]/ul')%>% 
    html_nodes('a') %>% html_attr('href')
  #Filter the links
  links<-grepl("vagas", jobLinks)
  jobLinks<-jobLinks[links]
  write.table(jobLinks, file="Data\\EmpregosLinks.txt", append=TRUE, col.names = FALSE, row.names=FALSE, quote = FALSE) 
  setTxtProgressBar(pb, i)
}
close(pb)

#Read the links
links<-read.table("Data\\EmpregosLinks.txt")
links<-unique(links)
last<-nrow(links)
pb <- txtProgressBar(min = 0, max = last)
for(i in 1:last){
  #Define the link with all positions
  link <- read_html(as.character(links[i,]))
  
  #Position
  position <- link %>% 
    html_nodes(xpath='//*[@id="ctl00_ContentBody_h1TituloVaga"]')%>% 
    html_text()
  
  #Firm
  firm <- link %>% 
    html_nodes(xpath='//*[@id="ctl00_ContentBody_hplEmpresa"]')%>% 
    html_text()
  
  #Information Job
  information <- link %>% 
    html_nodes(xpath='//*[@id="ctl00_ContentBody_divDetalheVaga"]/div[2]/div/article/div[4]/div/table[1]')%>% 
    html_table() 
  info.df1<-information[[1]]
  info.df1 <-info.df1 %>% spread(X1,X2) 
  if(ncol(info.df1)==4){
    info.df1<-cbind("",info.df1)
  }
  colnames(info.df1)<-c("Obs", "Description", "Requirements", "Place", "Qualification")
  #Information Firm
  information <- link %>% 
    html_nodes(xpath='//*[@id="ctl00_ContentBody_divDetalheVaga"]/div[2]/div/article/div[4]/div/table[2]')%>% 
    html_table()
  info.df2<-information[[1]]
  info.df2 <-info.df2 %>% spread(X1,X2) 
  colnames(info.df2)<-c("DescriptionFirm", "Firm", "Sector")  
  temp<-cbind(info.df1,info.df2)
  temp2<-as.data.frame(lapply(temp,function(x)str_trim(gsub("[\r\n]", "", as.character(x)))))
  
  if(i==1){
    write.table(temp2, file="Data\\EmpregosScraping.txt",sep = ";", append=FALSE, col.names = TRUE, row.names=FALSE, quote = FALSE)
  }
  else{
    write.table(temp2, file="Data\\EmpregosScraping.txt",sep = ";", append=TRUE, col.names = FALSE, row.names=FALSE, quote = FALSE)
  }
  
  setTxtProgressBar(pb, i)
}
close(pb)