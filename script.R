require(rvest)
require(stringr)
require(dplyr)
require(tm)

#cycle takes a name of a player from a given list and loads according html page, cleans logs up, splits them by <br> tag and pulls info from each record


for (i in (1:length(namelist))){
    action <- c()
    places <- c()
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
        action[i] = tolower(action[i])
      }
    }
    
    coord <- sapply(logs[[1]], function(x) str_extract_all(x, '[[:digit:]]{2}\\.[[:digit:]]*,[[:digit:]]{2}\\.[[:digit:]]*')[[1]][1]) %>% na.omit()
    ##places <- sapply(logs[[1]], function(x) ifelse(action == "destroyed link", str_extract_all(x, '\">[\\s\\S]*</a> [\\s\\S]* to'), str_extract_all(x, '\">[\\s\\S]*</a>'))
    ##further work required to avoid loops
    for (i in 1:5000){ #hate this but hey it works
      if (action[i] %in% c("destroyed link", "linked")){
        places[i] = str_extract(logs[[1]][i], '\">[\\s\\S]*</a> [\\s\\S]* to') %>% str_replace('</a> ([\\s\\S]*) to', "") %>% str_replace('\">', "") %>% paste()
      } else {
        places[i] = str_extract(logs[[1]][i], '\">[\\s\\S]*</a>') %>% str_replace('\">', "") %>% str_replace('</a>', "") %>% paste()
      }
    }
    
}