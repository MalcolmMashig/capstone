players = future['Name']
players_clean = vector(mode = "list", length = nrow(players))
for (i in 1:nrow(players)) {
  players_clean[i] = gsub('\\.','',players[i,])
  players_clean[i] = gsub(' ', '-', players_clean[i])
  players_clean[i] = tolower(players_clean[i])}

fangraphsids = future['playerid']

fangraphs_urls = 1:length(players_clean)
for (i in 1:length(players_clean)) {
  fangraphs_urls[i] = paste("https://www.fangraphs.com/players/", players_clean[i], '/', fangraphsids[i,], sep = "")}

fangraphs_urls

mlbids = 

mlb_urls = 1:length(mlbids)
for (i in 1:length(mlbids)) {
  mlb_urls[i] = paste("https://securea.mlb.com/mlb/images/players/head_shot/", mlbids[i], ".jpg", sep = "")}

mlb_urls

