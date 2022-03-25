source("functions/api_postgres.R")
start <- Sys.time()

####SETTINGS####
my_token <- "your token here"
db_name <- "mydb"
myproject <- "CTT Project Name" #this is your project name on your CTT account
conn <- dbConnect(RPostgres::Postgres(), dbname=db_name)
################

get_my_data(my_token, "~/Documents/data/radio_projects/myproject" , conn, myproject) #the folder path is where you want your downloaded files to go

update_db(conn, outpath, myproject)
dbDisconnect(conn)

time_elapse <- Sys.time() - start
print(time_elapse)

