players = future['Name']
players_clean = vector(mode = "list", length = nrow(players))
for (i in 1:nrow(players)) {
  players_clean[i] = gsub('\\.','',players[i,])
  players_clean[i] = gsub(' ', '-', players_clean[i])
  players_clean[i] = tolower(players_clean[i])}

ids = future['playerid']

players_clean[1]
ids[1,]

urls = 1:length(players_clean)
for (i in 1:length(players_clean)) {
  urls[i] = paste("https://www.fangraph.com/players/", players_clean[i], '/', ids[i,], sep = "")
}

urls
