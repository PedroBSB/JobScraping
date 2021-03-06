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
for(j in 1:last){
  #Define the link with all positions
  link <- read_html(paste0("https://www.empregos.com.br/vagas/p",i))
  
  #Links
  jobLinks <- link %>% 
    html_nodes(xpath='//*[@id="ctl00_ContentBody_divPaiMioloBusca"]/ul')%>% 
    html_nodes('a') %>% html_attr('href')
  #Filter the links
  links<-grepl("vagas", jobLinks)
  jobLinks<-data.frame(j,jobLinks[links])
  write.table(jobLinks, file="Data\\EmpregosLinks.txt", append=TRUE, col.names = FALSE, row.names=FALSE, quote = FALSE) 
  setTxtProgressBar(pb, j)
}
close(pb)

#Read the links
links<-read.table("Data\\EmpregosLinks.txt",sep=";")
links<-unique(links)
last<-nrow(links)
pb <- txtProgressBar(min = 0, max = last)
for(i in 1:last){
  tryCatch({
    #Define the link with all positions
    link <- read_html(as.character(links[i,2]))
    
    #Position
    position <- link %>% 
      html_nodes(xpath='//*[@id="ctl00_ContentBody_h1TituloVaga"]')%>% 
      html_text()
    if(identical(position, character(0))) position<-""
    #Firm
    firm <- link %>% 
      html_nodes(xpath='//*[@id="ctl00_ContentBody_hplEmpresa"]')%>% 
      html_text()
    if(identical(firm, character(0))) firm<-""
    #Information Job
    information <- link %>% 
      html_nodes(xpath='//*[@id="ctl00_ContentBody_divDetalheVaga"]/div[2]/div/article/div[4]/div/table[1]')%>% 
      html_table() 
    if(length(information)>0){
      info.df1<-information[[1]]
      info.df1 <-info.df1 %>% spread(X1,X2) 
      info.df1$Position<-position
      info.df1$Firm<-firm
      
      if(ncol(info.df1)==4){
        info.df1<-cbind("",info.df1)
      }
      if(ncol(info.df1)==3){
        info.df1<-cbind("","",info.df1)
      }
      #Information Firm
      information <- link %>% 
        html_nodes(xpath='//*[@id="ctl00_ContentBody_divDetalheVaga"]/div[2]/div/article/div[4]/div/table[2]')%>% 
        html_table()
      if(identical(information, character(0))) information<-""
      
      info.df2<-information[[1]]
      info.df2 <-info.df2 %>% spread(X1,X2) 
      temp<-cbind(info.df1,info.df2)
      temp2<-as.data.frame(lapply(temp,function(x)str_trim(gsub("[\r\n]", "", as.character(x)))))
      
      temp2<-data.frame("I"=i,temp2)
      
      if(i==1){
        write.table(temp2, file="Data\\EmpregosScraping.txt",sep = ";", append=FALSE, col.names = TRUE, row.names=FALSE, quote = FALSE)
      }
      else{
        write.table(temp2, file="Data\\EmpregosScraping.txt",sep = ";", append=TRUE, col.names = TRUE, row.names=FALSE, quote = FALSE)
      }
    }
    
    setTxtProgressBar(pb, i)
  }, error = function(err) {
    print(paste("MY_ERROR:  ",err,"iter=",i))
    i<- (i-1)
    Sys.sleep(10) 
  })
}
close(pb)