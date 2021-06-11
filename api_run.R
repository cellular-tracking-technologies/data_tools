source("functions/api_postgres.R")
start <- Sys.time()

####SETTINGS####
outpath <- "this is the path to the folder that will contain your downloaded files" 
my_token <- "your token"
db_name <- "your database name"
conn <- dbConnect(RPostgres::Postgres(), dbname=db_name)
################

get_my_data(my_token, outpath, conn)

update_db(conn, outpath)
dbDisconnect(conn)

time_elapse <- Sys.time() - start
print(time_elapse)

