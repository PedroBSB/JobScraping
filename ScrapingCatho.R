#Call the libraries
library(rvest)
library(purrr)
library(RSelenium)
library(tidyverse)
library(stringr)
library(rlist)
#Define the link with all positions 
link <- read_html("https://www.catho.com.br/vagas/?pais_id=31&faixa_sal_id_combinar=1&order=score&where_search=1&how_search=2&perfil_id=1&page=1")

#Pages
pagesNum <- link %>% 
  html_nodes(xpath='//*[@id="resultado-da-sua-busca-de-vaga"]/nav/a[5]') %>% 
  html_text()
pagesNum<-as.numeric(pagesNum)
pb <- txtProgressBar(min = 0, max = pagesNum)
for(j in 1:pagesNum){
  tryCatch({
  #Define the link with all positions
  link <- read_html(paste0("https://www.catho.com.br/vagas/?pais_id=31&faixa_sal_id_combinar=1&order=score&where_search=1&how_search=2&perfil_id=1&page=",j))
  
  #Links
  jobLinks <- link %>% 
    html_nodes(xpath='//*[@id="listagemVagas"]')%>% 
    html_nodes('a') %>% html_attr('href')
  
  
  #Filter the links 
  links<-!(grepl("seguro.catho.com.br", jobLinks) | grepl("javascript", jobLinks))
  jobLinks<-data.frame(j,jobLinks[links])
  write.table(jobLinks, file="Data\\CathoLinks.txt", append=TRUE, col.names = FALSE, row.names=FALSE, quote = FALSE) 
  setTxtProgressBar(pb, j)
  }, error = function(err) {
    print(paste("MY_ERROR:  ",err))
    j<- (j-1)
    Sys.sleep(10) 
  })
}
close(pb)

#Read the links
links<-readLines("Data\\CathoLinks.txt")
links<-strsplit(links," ")
links <- lapply(links, function(x){if(length(x)>2) x <- NULL; x})
links <- Filter(Negate(is.null), links)
links<- do.call(rbind.data.frame, links)
colnames(links)<-c("V1","V2")
links<-unique(as.character(links$V2))
last<-length(links)

pb <- txtProgressBar(min = 0, max = last)
for(i in 1:last){
    tryCatch({
    #Define the link with all positions
    linkSel<-links[i]
    link <- read_html(linkSel)
    
    #Position
    position <- link %>% 
      html_nodes(xpath='//*[@id="anchorTituloVaga"]')%>% 
      html_text()
    
    
    #Position
    state <- link %>% 
      html_nodes(xpath='//*[@id="content"]/div/header/em/span[2]/a[1]')%>% 
      html_text()
    
    city <- link %>% 
      html_nodes(xpath='//*[@id="content"]/div/header/em/span[3]/a[1]')%>% 
      html_text()
    
    salary <- link %>% 
      html_nodes(xpath='//*[@id="content"]/div/header/em/span[1]')%>% 
      html_text() %>% 
      gsub(pattern = 'R\\$', replacement = "")  %>% 
      gsub(pattern = '\\n', replacement = "") %>% 
      gsub(pattern = '\\.', replacement = "") %>% 
      gsub(pattern = ',', replacement = ".")
    salary<-as.numeric(salary)
    
    description <- link %>% 
      html_nodes(xpath='//*[@id="descricao-da-vaga"]/p')%>% 
      html_text()
    
    type <- link %>% 
      html_nodes(xpath='//*[@id="regime-de-contratacao-da-vaga"]/article/p')%>% 
      html_text()
    
    if(i==1){
      header<-("Position@State@City@Salary@Description@Type@Link")
      write(header, file = "Data\\CathoScrapping.txt", append=FALSE)
      temp<-paste(position,state,city,salary,description,type,i,sep="@")
      write(temp, file = "Data\\CathoScrapping.txt", append=TRUE)
    }
    else{
      temp<-paste(position,state,city,salary,description,type,i,sep="@")
      write(temp, file = "Data\\CathoScrapping.txt", append=TRUE)
    }
      setTxtProgressBar(pb, i)
      
    }, error = function(err) {
      print(paste("MY_ERROR:  ",err))
      i<- (i-1)
      Sys.sleep(10) 
    })
  }

close(pb)
