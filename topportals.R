require(dplyr)
#counting frequencies of actions for portals: whole city
freq.df = count(combined.df, portal.id) %>% as.data.frame()
portals.df$freq = freq.df$n
portals.df.backup = portals.df
for (i in 1:nrow(portals.df)){
  portals.df$name[i] = as.character(combined.df[combined.df$portal.id == portals.df$portal.id[i],]$portal.name) %>% unique()
}
portals.df[order(portals.df$freq, decreasing = T),][1:20,c("name", "freq", "portal.coord")]

#now, proportions of actions between players districts
for (i in 1:nrow(portals.df)){
  tmp.summary = combined.df[combined.df$portal.id == i,]$player.district %>% summary() / combined.df[combined.df$portal.id == i,]$player.district %>% summary() %>% sum
  portals.df$center.players[i] = tmp.summary[1]
  portals.df$nevsky.players[i] = tmp.summary[2]
  portals.df$petrodv.players[i] = tmp.summary[3]
  portals.df$petro.players[i] = tmp.summary[4]
}

#top portals for each district
top.petro.df = portals.df[portals.df$portal.id %in% over(petro.shp, coord.df, returnList = TRUE)[[1]]$portal.id,]
top.nevsky.df = portals.df[portals.df$portal.id %in% over(nevsky.shp, coord.df, returnList = TRUE)[[1]]$portal.id,]
top.petrodv.df = portals.df[portals.df$portal.id %in% over(petrodv.shp, coord.df, returnList = TRUE)[[1]]$portal.id,]
top.center.df = portals.df[portals.df$portal.id %in% over(center.shp, coord.df, returnList = TRUE)[[1]]$portal.id,]
top.petro.df = top.petro.df[order(top.petro.df$freq, decreasing = TRUE),]
top.nevsky.df = top.nevsky.df[order(top.nevsky.df$freq, decreasing = TRUE),]
top.petrodv.df = top.petrodv.df[order(top.petrodv.df$freq, decreasing = TRUE),]
top.center.df = top.center.df[order(top.center.df$freq, decreasing = TRUE),]
write.csv(top.center.df, "~/ingresslogs/top-center.csv")
write.csv(top.petro.df, "~/ingresslogs/top-petro.csv")
write.csv(top.petrodv.df, "~/ingresslogs/top-petrodv.csv")
write.csv(top.nevsky.df, "~/ingresslogs/top-nevsky.csv")

#actions top
