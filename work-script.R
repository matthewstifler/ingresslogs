petro = read.table("~/ingresslogs/save/petro.txt")
namelist = petro$V1

#...
#file names to lower
files = list.files("~/ingresslogs/save/", pattern=".html")
files = str_c("~/ingresslogs/save/", files)
sapply(files,function(x)file.rename(from=x,to=tolower(x)))

namelist <- namelist[str_c(namelist, ".html") %in% list.files("~/ingresslogs/save/", pattern=".html") == TRUE] #required to extract only data for people both in list & logs

#leroooy

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

#work is performed on joined df for all players
#coord -> lon, lat
coords <- combined.df$coord %>% as.character() %>% strsplit(",") %>% unlist()
lat <- coords[c(TRUE, FALSE)]
lon <- coords[c(FALSE, TRUE)]
combined.df$lat <- as.numeric(lat)
combined.df$lon <- as.numeric(lon)
combined.df<- combined.df[,c(1:3, 5:ncol(df))]

#remove coord NAs and "" action rows (logs bugs), 
#change all level-resonator destroy actions with "destroyed resonator", 
#replace "destroyedn <a" with "destroyed portal", happens b/c of the absense of word for action in log
combined.df = combined.df[!is.na(combined.df$coord),]
combined.df = combined.df[!(combined.df$action == ""),]
levels(combined.df$action) <- str_replace(levels(combined.df$action), "destroyedn l[0-9]", "destroyed resonator")
levels(combined.df$action) <- str_replace(levels(combined.df$action), "destroyedn <a", "destroyed portal")


#maps, engage coefficient and whatever not
map9 = get_map(location = c("санкт петербург"), zoom = 9, color = "bw", maptype = "roadmap")
map10 = get_map(location = c("санкт петербург"), zoom = 10, color = "bw", maptype = "roadmap")
for (i in 1:length(levels(combined.df$district))) {
  #point maps for two zooms for each district, with automatic saving!
  filename = str_c(levels(combined.df$district)[i], "-points") %>% str_c("-9") %>% str_c(".png")
  (ggmap(map9) + geom_point(data = combined.df[combined.df$district == levels(combined.df$district)[i],], aes(x = lon, y = lat, color = district))) %>% ggsave(filename = filename, width = 16, height = 7.61, units = "in", dpi = 75)
  filename = str_c(levels(combined.df$district)[i], "-points") %>% str_c("-10") %>% str_c(".png")
  (ggmap(map10) + geom_point(data = combined.df[combined.df$district == levels(combined.df$district)[i],], aes(x = lon, y = lat, color = district))) %>% ggsave(filename = filename, width = 16, height = 7.61, units = "in", dpi = 75)
}
ggmap(map9) #heatmap for a single district, change name for others
  + scale_fill_gradientn(guide="none", colours=heat.colors(20, alpha = 0.15))
  + stat_density2d(data = combined.df[combined.df$district == "nevsky",], aes(x = lon, y = lat ,fill = ..level.. ,alpha=..level..), geom = 'polygon')
  + scale_alpha_continuous(guide="none",range=c(.05,.5))

#proportions in and out of home
#calculate where event took place: district -> player-district, portal-district
#most popular portals
#traminer - sequences, most popular trajectories (center district)


combined.df$coord = str_c(combined.df$lat, ",") %>% str_c(combined.df$lon) #restoring coord, serves to create unique ids
combined.df = merge(x = combined.df, y = data.frame(coord = unique(combined.df$coord), id = 1:length(unique(combined.df$coord))), by.x = "coord", by.y = "coord") #ids
combined.df = combined.df[,-ncol(combined.df)] #remove id.y
#next 4 lines is some mess of reordering and rearranging df
colnames(combined.df)[c(1,4,5,6,10)] = c("portal-coord", "player-event", "portal-name", "player-name", "portal-id") #pretty names
combined.df = combined.df[,c(2,3,1,8,9,10,5,4,6,7)] #tidy order
colnames(combined.df)[c(3, 6:10)] = c("portal.coord", "portal.id", "portal.name", "player.event", "player.name", "player.district")
combined.df = combined.df[order(combined.df$portal.id),] 
#creating df for just the portal info
portals.df = combined.df[,c(3:6)]
portals.df = portals.df[!duplicated(portals.df$portal.id),]


#working w/shapefiles
library(mapproj)
library(rgdal)
setwd("~/ingresslogs")
distr.shp = readOGR("./shapefiles-boundary/", "boundary-polygon")
shapes = distr.shp[distr.shp@data$NAME %in% c("Невский район", "Петроградский район","Петродворцовый район","Центральный район"),]
shapes.df = fortify(shapes)
ggmap(map9) + geom_polygon(aes(x=long, y=lat, group=group), fill='blue', size=.2,color='black', data=shapes.df, alpha=0.2) #wow shapes mapped

#let's check now in which disrict each portal is
#honestly, this part looks like I was aiming for as many lines as possible, this needs to be reworked
districts = c("Невский район", "Петроградский район","Петродворцовый район","Центральный район")
nevsky.shp = shapes[shapes@data$NAME %in% districts[1],] #shape for each dstrict
petro.shp = shapes[shapes@data$NAME %in% districts[2],]
petrodv.shp = shapes[shapes@data$NAME %in% districts[3],]
center.shp = shapes[shapes@data$NAME %in% districts[4],]

coord.df = portals.df
coordinates(coord.df) <- ~lon+lat

proj4string(coord.df) <- proj4string(petro.shp) #sync projections between coordinate df and shape file
petro.portals = over(petro.shp, coord.df, returnList = TRUE)[[1]]$portal.id #intersection of points' coordinates w/ shapes. ReturnList is really important!
proj4string(coord.df) <- proj4string(nevsky.shp)
nevsky.portals = over(nevsky.shp, coord.df, returnList = TRUE)[[1]]$portal.id
proj4string(coord.df) <- proj4string(center.shp)
center.portals = over(center.shp, coord.df, returnList = TRUE)[[1]]$portal.id
proj4string(coord.df) <- proj4string(petrodv.shp)
petrodv.portals = over(petrodv.shp, coord.df, returnList = TRUE)[[1]]$portal.id
portals.df$petro = portals.df$portal.id %in% petro.portals
portals.df$nevsky = portals.df$portal.id %in% nevsky.portals
portals.df$center = portals.df$portal.id %in% center.portals
portals.df$petrodv = portals.df$portal.id %in% petrodv.portals

selected.portals.df = portals.df[portals.df$petro == TRUE | portals.df$nevsky == TRUE  | portals.df$center == TRUE | portals.df$petrodv == TRUE, ] #pick only those portals in the 4 districts
selected.df = combined.df[combined.df$portal.id %in% selected.portals.df$portal.id, ] #same for events
selected.df$portal.distrcit = ifelse(selected.df$portal.id %in% selected.portals.df[selected.portals.df$nevsky == TRUE,]$portal.id, "nevsky", 
       ifelse(selected.df$portal.id %in% selected.portals.df[selected.portals.df$petro == TRUE,]$portal.id, "petro",
              ifelse(selected.df$portal.id %in% selected.portals.df[selected.portals.df$petrodv == TRUE,]$portal.id, "petrodv",
                     ifelse(selected.df$portal.id %in% selected.portals.df[selected.portals.df$center == TRUE,]$portal.id, "center", "none"))))
selected.df$portal.distrcit = as.factor(selected.df$portal.distrcit)
