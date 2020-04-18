install.packages("rvest")
library(rvest)


## 1) get players URL from fangraphs and read it
cole <- 'https://www.fangraphs.com/players/gerrit-cole/13125/stats?position=P'
ht <- read_html(cole)

## 2) get body  node, then table nodes
body <- html_nodes(ht, 'body') 
tables <- html_nodes(body, 'table')

##3) subset to 8th and get dataframe
table2 <- tables[8]
df <- html_table(table2, fill=TRUE)
df <- df[[1]]
