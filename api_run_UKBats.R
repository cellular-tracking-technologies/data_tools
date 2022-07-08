source("functions/api_postgres.R")
start <- Sys.time()

####SETTINGS####
my_token <- "64cdf5ae10a6b82368b4bbb3120a93da3e90d189b7f519d30416055b307e9427"
outpath <- "C:/Users/katie/OneDrive/Documents/Uni/PhD/CTT_Motus/UK_Bats_Sensor_Data" 
db_name <- "UK_Bats"
myproject <- "UK Bats" #this is your project name on your CTT account
conn <- dbConnect(RPostgres::Postgres(), dbname=db_name)
################

get_my_data(my_token, outpath, conn, myproject)

update_db(conn, outpath, myproject)
dbDisconnect(conn)

time_elapse <- Sys.time() - start
print(time_elapse)

