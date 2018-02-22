#Call the libraries
library(rvest)
library(purrr)
library(RSelenium)
#Define the link with all positions
link <- read_html("https://www.vagas.com.br/vagas-de-Brasil?")

#City
city <- link %>% 
  html_nodes(xpath='//*[@id="pesquisaFiltros"]/div[2]') %>% 
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
webElem <- remDr$findElement(using = "name", "submit")

#Clica no botao
webElem$clickElement()

#Wait the download
Sys.sleep(5)

#Fecha as conexoes
remDr$close()
remDr$closeServer()

