require(rvest)
require(stringr)
require(dplyr)
require(tm)

#cycle takes a name of a player from a given list and loads according html page


for (i in (1:length(namelist))){
    #loading page, extracting logs from it
    page <- str_c("~/ingresslogs/", str_c(namelist[1],".html")) %>% html()
    logs <- html_nodes(page, xpath = "//div[@id = 'logs']")

    #
    #logs processing
    #
    
    #cleaning up
    logs <- str_replace(logs, '<div([\\s\\S]*)\t\t\t\t\t', "") %>% str_replace('\t\t\t\t([\\s\\S]*)</div>', "") #cleaning up beginning and ending
    logs <- str_split(logs, pattern = "<br/>")[[1]] #splitting by original lines
    logs <- logs[1:5000]
    
    #extracting data
    date <- str_extract_all(logs, '[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}')[[1]]
    time <- str_extract_all(logs, '[[:digit:]]{2}:[[:digit:]]{2}:[[:digit:]]{2}')[[1]]
    for (i in 1:5000){
      action[i] = word(logs[[1]][i],4)
      if (action[i] == "destroyed") {
        action[i] = word(logs[[1]][i], 4, 6)
        action[i] = str_replace(action[i], " a", "")
        action[i] = str_replace(action[i], " the", "")
        if (action[i] == "destroyed Control"){
          action[i] = "destroyed control field"
        }
      }
    }
    action <- word(logs[[1]][action == "destroyed"])
    
}

for (i in 1:5000){
  action[i] = word(logs[[1]][i],4)
  cat(action[i])
}