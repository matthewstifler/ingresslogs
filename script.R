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
    regex <- as.character(str_c(namelist[i], "([\\s\\S]*)<a", sep = " "))
    action <- str_extract_all(logs[[1]], regex) %>% str_replace_all(namelist[i], "") %>% str_replace_all() #issue to solve: bad <a-end definition of action, bc of links and two links in line
}

