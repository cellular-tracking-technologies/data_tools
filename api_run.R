source("functions/api_postgres.R")
start <- Sys.time()

####SETTINGS####
outpath <- "~/Documents/data/radio_projects/myproject" 
my_token <- "your token here"
db_name <- "mydb"
myproject <- "CTT Project Name" #this is your project name on your CTT account
conn <- dbConnect(RPostgres::Postgres(), dbname=db_name)
################

get_my_data(my_token, outpath, conn, myproject)

update_db(conn, outpath, myproject)
dbDisconnect(conn)

time_elapse <- Sys.time() - start
print(time_elapse)

