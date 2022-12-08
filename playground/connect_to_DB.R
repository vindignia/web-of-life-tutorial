source('Configuration.R') # => defines the variable server
library("DBI")
library("RMariaDB") # MySQL => RMariaDB see https://github.com/r-dbi/RMySQL/blob/main/README.md

connect_to_DB_helper <- function() {

  print(paste0("try to connect to ",server," DB"))
  if(server == "local"){
    return_value <- dbConnect(RMariaDB::MariaDB(), group="client_weboflife_local", dbname="Networks")
  } else if(server == "development") {
    # ssh tunnel needed for remote DB
    return_value <- dbConnect(RMariaDB::MariaDB(), group="client_weboflife20", dbname="Networks_dev")
  } else if(server == "production_webuser") {
    # ssh tunnel needed for remote DB
    return_value <- dbConnect(RMariaDB::MariaDB(), group="client_weboflife", dbname="Networks")
  } else if(server == "production_root") {
    # ssh tunnel needed for remote DB
    return_value <- dbConnect(RMariaDB::MariaDB(), group="client_weboflife20_root", dbname="Networks")
  } else if(server == "macbook") {
    my_db="Networks"
    return_value <- DBI::dbConnect(RMariaDB::MariaDB(),
                                   default.file = "/usr/local/etc/my.cnf", 
                                   group="client_weboflife", 
                                   dbname=my_db)
  } else {
    return_value <- "No DB provided"
    stop("Invalid DB server name: execution stops") #
  }
  return(return_value)
}



connect_to_DB <- function() {
  con <- tryCatch({
    connect_to_DB_helper()
  }, error = function(err) {
    # error handler picks up where error was generated
    print(paste("MY_ERROR:"))
    stop(err)
  }, finally = {
    print("END tryCatch")
  })
  return(con)
}



kill_DB_connections <- function () {
  
  all_cons <- dbListConnections(MySQL())
  
  print(all_cons)
  
  for(con in all_cons)
    +  dbDisconnect(con)
  
  print(paste(length(all_cons), " connections killed."))
 
  return()
}


#kill_DB_connections()
con <- connect_to_DB()

