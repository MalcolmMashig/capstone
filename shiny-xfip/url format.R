players = future['Name']
players = gsub('\\.','',players)
players = gsub(' ', '-', players)
players = tolower(players)
players

ids = future['playerid']
ids
