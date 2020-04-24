# install.packages("rvest")
library(rvest)

get_table <- function(link){
  ht <- read_html(link)
  body <- html_nodes(ht, 'body') 
  tables <- html_nodes(body, 'table')
  table2 <- tables[8]
  df <- html_table(table2, fill=TRUE)
  df <- df[[1]]
  df <- df[,0:2]
  other_html <- textreadr::read_html(link)
  if (substr(other_html[521], 1, 3) == "Age"){
    hw <- other_html[523]
    hwvec <- c("Height/Weight:", hw, 2)
    hand <- other_html[522]
    handvec <- c("Handedness:", hand, 3)
  } else {
    hw <- other_html[524]
    hwvec <- c("Height/Weight:", hw, 2)
    hand <- other_html[523]
    handvec <- c("Handedness:", hand, 3)
  }
  names(df)[1] <- "Player Information"
  names(df)[2] <- "L"
  len <- nrow(df)
  x <- 1
  y <- seq(2,len,1) + 2
  x <- x %>% append(y)
  df$Order <- x
  df <- df %>%
    rbind(hwvec)  %>%
    rbind(handvec)
  df <- df %>% arrange(Order)
  names(df)[2] <- ""
  df <- df[,1:2]
  info <- df[,1]
  pos <- match("AAV:",info)
  if (!is.na(pos)){
    df[pos, 1] <- "Average Annual Value:"
  }
  len <- nrow(df)
  df[len,1] <- "Service Time: Reported as the pre-2020 value"
  time <- as.numeric(df[len,2])
  yr <- trunc(time)
  days <- round((1000 * (time - yr)), 0)
  yr <- as.character(yr)
  days <- as.character(days)
  str <- "years and"
  str2 <- "days"
  df[len,2] <- paste(yr, str, days, str2)
  return(df)
}

