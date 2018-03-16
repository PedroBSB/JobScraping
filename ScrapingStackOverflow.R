#Call the libraries
library(rvest)
library(purrr)
library(RSelenium)
library(tidyverse)
library(stringr)
#Define the link with all positions
link <- read_html("https://stackoverflow.com/jobs?sort=i")

#Pages
pages <- link %>% 
  html_nodes(xpath='//*[@id="index-hed"]/div[1]/span') %>% 
  html_text()

#Find the last page
pages <- gsub('[\r\n\t]', '', pages)
posEnd <- regexpr(' ', pages)[1]-1
last<- as.numeric(substr(pages[1],1,posEnd))
pagesNum <- ceiling(last/25)

pb <- txtProgressBar(min = 0, max = last)
for(j in 1:pagesNum){
  #Define the link with all positions
  link <- read_html(paste0("https://stackoverflow.com/jobs?sort=i&pg=",i))
  
  #Links
  jobLinks <- link %>% 
    html_nodes(xpath='//*[@id="content"]/div/div[2]/div/div[1]/div[2]/div')%>% 
    html_nodes('a') %>% html_attr('href')
  #Filter the links
  links<-!(grepl("/jobs/developer-", jobLinks) | grepl("/jobs/devops", jobLinks) | grepl("/jobs/docker-", jobLinks) |  grepl("/jobs/designer-", jobLinks) | grepl("/jobs/system-", jobLinks))
  jobLinks<-data.frame(j,jobLinks[links])
  write.table(jobLinks, file="Data\\StackOverFlowLinks.txt", append=TRUE, col.names = FALSE, row.names=FALSE, quote = FALSE) 
  setTxtProgressBar(pb, j)
}
close(pb)

#Read the links
links<-read.table("Data\\StackOverFlowLinks.txt")
links<-unique(links)
last<-nrow(links)
final<-data.frame("Position"="","Firm"="","Description"="","Link"="")
pb <- txtProgressBar(min = 0, max = last)
for(i in 1:last){
  #Define the link with all positions
  link <- read_html(paste0("https://stackoverflow.com/",as.character(links[i,2])))
  
  #Position
  position <- link %>% 
    html_nodes(xpath='//*[@id="job-detail"]/div[1]/div/div[2]/div[1]/h1/a')%>% 
    html_text()
  
  if(!identical(position, character(0))){
    #Firm
    firm <- link %>% 
      html_nodes(xpath='//*[@id="job-detail"]/div[1]/div/div[2]/div[2]/div[1]/a')%>% 
      html_text()
    
    #Information Job
    technologies <- link %>% 
      html_nodes(xpath='//*[@id="overview-items"]/section[2]/div') %>% 
      html_nodes('a') %>% html_attr('href')
    description <- link %>%   html_node(xpath='//*[@id="overview-items"]/section[3]') %>%     html_text()
    description <- gsub('[\r\n\t]', '', description)
    temp2<-data.frame("Position"=position,"Firm"=firm,"Description"=description,"Link"=paste0("https://stackoverflow.com/",as.character(links[i,2])))
    final<-rbind(final,temp2)
  }
  setTxtProgressBar(pb, i)
}
close(pb)
write.table(final, file="Data\\StackOverflowScrapping.txt",sep = ";", append=FALSE, col.names = TRUE, row.names=FALSE, quote = FALSE)

