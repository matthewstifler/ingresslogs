require(stringr)
require(geosphere)

#create timestaps (works hella long)
for (i in 1:nrow(combined.df)){
  combined.df$timestamp[i] = as.POSIXct(str_c(as.character(combined.df[i,1]), as.character(combined.df[i,2]), sep = " ")) %>% as.numeric()
}

order.by.time.combined.df = combined.df[order(combined.df$timestamp),] #sort by timestamp
dist.player = c(rep(0, 154))
n.portals = c()
#this awful nested loops does exactly this. it takes all players, counts unique portals they have
#events at, then loads a list of all portals a certain player ever visited, sorted by time (!). the nested loops then
#iterates through every one of these portals one by one, takes a portal and the next one, calculates
#distance between them and adds it too existing distance, in meters. the idea is to get for each player
#distance travelled and then divide it by number of unique portals, so to get a metric of how far away a player goes, or
#basically, how local he is
#i = 1
#j = 1
for (i in i:length(levels(combined.df$player.name))){
  n.portals[i] = combined.df[combined.df$player.name == levels(combined.df$player.name)[i],]$portal.id %>% unique() %>% length
  list.portals = order.by.time.combined.df[order.by.time.combined.df$player.name == levels(order.by.time.combined.df$player.name)[i],]$portal.id
  if (list.portals %>% length > 1){
    for (j in j:(length(list.portals))){
      first.portal.coord = c(portals.df[portals.df$portal.id %in% list.portals[j], ]$lat, portals.df[portals.df$portal.id %in% list.portals[j], ]$lon)
      second.portal.coord = c(portals.df[portals.df$portal.id %in% list.portals[j + 1], ]$lat, portals.df[portals.df$portal.id %in% list.portals[j + 1], ]$lon)
      if (!(first.portal.coord %>% length == 0) & !(second.portal.coord %>% length == 0)){
        dist.player[i] = dist.player[i] + distm(first.portal.coord, second.portal.coord, fun = distHaversine) %>% as.numeric
      }
    } 
  }
}

data.frame(name = levels(combined.df$player.name), dist = dist.player, n.portals = n.portals, locality = dist.player / n.portals) %>% View
