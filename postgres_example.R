library(RPostgres)
library(dplyr) 
db_name <- "your_db_name"
conn <- dbConnect(RPostgres::Postgres(), dbname=db_name)
flights_db <- tbl(conn, "raw")
beep <- flights_db %>% 
  filter(time > '2021-09-30' & station_id == "your_station_id") %>%
  collect()

tag <- flights_db %>% 
  filter(tagid == "your_tag_id") %>%
  collect()