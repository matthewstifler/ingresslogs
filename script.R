require(rvest)
require(stringr)
require(dplyr)
require(tm)

#cycle takes a name of a player from a given list and loads according html page, cleans logs up, splits them by <br> tag and pulls info from each record

time.begin = Sys.time() 
time.total = Sys.time() - time.begin

for (i in (1:length(namelist))){
  action <- c()
  places <- c()
  
  #loading page, extracting logs from it
  page <- str_c("~/ingresslogs/save/", str_c(namelist[i],".html")) %>% read_html()
  logs <- html_nodes(page, xpath = "//div[@id = 'logs']")
  
  #
  #logs processing
  #
  
  #cleaning up
  logs <- str_replace(logs, '<div([\\s\\S]*)\t\t\t\t\t', "") %>% str_replace('\t\t\t\t([\\s\\S]*)</div>', "") #cleaning up beginning and ending
  logs <- str_split(logs, pattern = "<br/>")[[1]] #splitting by original lines
  logs <- logs[1:(length(logs)-1)]
  
  #extracting data
  date <- str_extract_all(logs, '[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}') %>% unlist()
  time <- str_extract_all(logs, '[[:digit:]]{2}:[[:digit:]]{2}:[[:digit:]]{2}') %>% unlist()
  for (j in 1:length(logs)){
    action[j] = word(logs[j],4)
    if (action[j] == "destroyed") {
      action[j] = word(logs[j], 4, 6)
      action[j] = str_replace(action[j], " a", "")
      action[j] = str_replace(action[j], " the", "")
      if (action[j] == "destroyed Control"){
        action[j] = "destroyed control field"
      }
      action[j] = tolower(action[j])
    }
  }
  
  coord <- sapply(logs, function(x) str_extract_all(x, '[[:digit:]]{2}\\.[[:digit:]]*,[[:digit:]]{2}\\.[[:digit:]]*')[[1]][1])
  ##places <- sapply(logs[[1]], function(x) ifelse(action == "destroyed link", str_extract_all(x, '\">[\\s\\S]*</a> [\\s\\S]* to'), str_extract_all(x, '\">[\\s\\S]*</a>'))
  ##further work required to avoid loops
  for (j in 1:length(logs)){ #hate this but hey it works
    if (action[j] %in% c("destroyed link", "linked")){
      places[j] = str_extract(logs[j], '\">[\\s\\S]*</a> [\\s\\S]* to') %>% str_replace('</a> ([\\s\\S]*) to', "") %>% str_replace('\">', "") %>% paste()
    } else {
      places[j] = str_extract(logs[j], '\">[\\s\\S]*</a>') %>% str_replace('\">', "") %>% str_replace('</a>', "") %>% paste()
    }
  }
  #results
  df = data.frame(date = date, time = time, action = action, coord = coord, place = places)
  
  #saving 'em
  path = str_c("~/ingresslogs/logs/",namelist[i]) %>% str_c(".csv")
  write.csv(df, path)
  
  #current status
  time.total <- Sys.time() - time.begin + time.total
  cat("Обработаны данные", i, " игроков", "\nВсего прошло", as.integer(time.total)/60, "минут\n")
}