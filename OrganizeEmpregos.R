#Read the Empregos Data
rm(list=ls())
library(tidyverse)

dat <- readLines("Data\\EmpregosScraping.txt") # read whatever is in there, one line at a time
varnames <- unlist(strsplit(dat[1], ";")) # extract variable names
nvar <- length(varnames)

k <- 1 # setting up a counter
dat1 <- matrix(NA, ncol = nvar, dimnames = list(NULL, varnames))

while(k <= length(dat)){
  k <- k + 1
  if(dat[k] == "") {k <- k + 1
  print(paste("data line", k, "is an empty string"))
  if(k > length(dat)) {break}
  }
  temp <- dat[k]
  # checks if there are enough commas or if the line was broken
  while(length(gregexpr(";", temp)[[1]]) < nvar-1){
    k <- k + 1
    temp <- paste0(temp, stringr::str_trim(dat[k]))
  }
  temp <- unlist(strsplit(temp, ";"))
  #message(k)
  dat1 <- rbind(dat1, temp)
}

empregos <- data.frame(dat1)
empregos<-empregos[-1,] 
empregos2<-empregos[which(empregos$I!="I"),]

saveRDS(empregos2,"Data\\Empregos.rds")

