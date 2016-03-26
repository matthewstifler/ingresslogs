require(rvest)
require(stringr)
require(dplyr)

#here should be a cycle getting a player's name and loading to the page variable file with name

#cycle basics
for i in (1:length(namelist)){
    filename <- str_c(namelist[i],".html")
    page <- str_c(filename)
}
#/cycle basics

page <- 
div[@id = "logs"]