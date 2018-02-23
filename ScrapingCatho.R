#Call the libraries
library(rvest)
library(purrr)
library(RSelenium)
#Define the link with all positions
link <- read_html("https://www.catho.com.br/vagas/?q=&pais_id=31&estado_id%5B%5D=&regiao_id%5B%5D=&cidade_id%5B%5D=&cidade=")

#Pages
pages <- link %>% 
         html_nodes(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "paginacao", " " ))]') %>% 
         html_nodes('a') %>% html_attr('href')

#Find the last page
pos <- regexpr('page=', pages)+5
last<- max(as.numeric(substr(pages,pos[1],nchar(pages))))

for(i in 1:last){
  link<-paste0("https://www.catho.com.br/vagas/?pais_id=31&where_search=1&how_search=2&faixa_sal_id_combinar=1&order=score&perfil_id=1&page=",i)
  
  pages <- link %>% 
    html_nodes(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "viewVagaAction", " " ))]') 
  
  
  %>% 
    html_nodes('a') %>% html_attr('href')
  
  
  
}



%>% 
  html_nodes("ul") %>% 
  html_nodes("li") %>% 
  html_nodes('a') %>% html_attr('href')
  

#City link
link.cty<-paste0("https://www.vagas.com.br",city[1])

#Define the download folder
eCaps <- list(
  chromeOptions = 
    list(prefs = list(
      "profile.default_content_settings.popups" = 0L,
      "download.prompt_for_download" = FALSE,
      "download.default_directory" = getwd()
    )
    )
)

#Check drivers
rD <- rsDriver(extraCapabilities = eCaps)

#Client RSelenium
remDr <- rD$client
remDr <- remoteDriver(port = 4567L, browserName = "chrome")

#Open Browser
remDr$open()

#Maximize window
remDr$maxWindowSize()

#Go to web page
remDr$navigate(link.cty)

#Encontra o objeto da caixa de texto
webElem <- remDr$findElement('xpath', '//*[@id="maisDeOntem"]')

#Clica no botao
webElem$clickElement()

#Wait the download
Sys.sleep(5)





#Fecha as conexoes
remDr$close()
remDr$closeServer()

