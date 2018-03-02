#Call the libraries
library(rvest)
library(purrr)
library(RSelenium)
library(tidyverse)
library(stringr)

#List all firms
firms<-LETTERS

for(i in firms){
  tryCatch({
    #List all firms
    link <- read_html("https://www.lovemondays.com.br/pesquisa/vaga?q=",i)
    
    #Pages
    pages <- link %>% 
      html_nodes(xpath='//*[@id="search-result-header"]') %>% 
      html_text()
    
    #Get all results
    posIni <- regexpr('de ', pages)+3
    posEnd <- regexpr(' res', pages)
    subsPage<- substr(pages,posIni,posEnd)
    subsPage<-gsub(" ","",subsPage)
    subsPage<-as.numeric(gsub("[.]","",subsPage))
    totalPages<- floor(subsPage/10)
      
      for(pageNum in 1:totalPages){
      #List all firms
      link <- read_html(paste0("https://www.lovemondays.com.br/pesquisa/vaga/pagina/",pageNum,"?q=",i))
      
      #Positions
      posLinks <- link %>% 
        html_nodes(xpath='/html/body/main/div/div/div/div/ul') %>% 
        html_nodes('a') %>% html_attr('href')
      posLinks<-paste0("https://www.lovemondays.com.br",posLinks)
      jobLinks<-data.frame(i,pageNum,posLinks)
      write.table(jobLinks, file="Data\\LoveMondaysLinks.txt", append=TRUE, col.names = FALSE, row.names=FALSE, quote = FALSE) 
      }
  }, error=function(e){})
}
    

links<-read.table("Data\\LoveMondaysLinks.txt")    
links<-unique(as.character(links$V3))
    
    