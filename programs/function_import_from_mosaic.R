#' Import sql file in R
#'
#' @param filepath the path of the query as sql file
#'
#' @return
#' @export
#'
#' @examples
read_sql_query <- function(filepath){
  con = file(filepath, "r")
  lines <- readLines(con)
  for (i in seq_along(lines)){
    lines[i] <- gsub("\\t", " ", lines[i])
    if(grepl("--",lines[i]) == TRUE){
      lines[i] <- paste(sub("--","/*",lines[i]),"*/")
    }
  }
  sql.string <- paste(lines, collapse = " ")
  close(con)
  return(sql.string)
}



#' Import data from mosaic database
#'
#' @param query A sql query as string 
#' @param database_name Database name as string
#'
#' @return Results from the query as dataframe
#' @export
#'
#' @examples
import_from_mosaic <- function(query, database_name, force_UTF8 = FALSE, prod = TRUE){
  library(RMySQL)
  
  # parameters
  db_user <- Sys.getenv('DB_USER')
  if (prod) {
    db_password <- Sys.getenv('DB_PASSWORD')
    db_host <- Sys.getenv('DB_HOST')
  }else{
    db_password <- Sys.getenv('DB_PASSWORD_PREPROD')
    db_host <- Sys.getenv('DB_HOST_PREPROD')
  }
  db_port <- strtoi(Sys.getenv('DB_PORT'))
  
  # 3. Read data from db
  mydb <-  dbConnect(MySQL(), user = db_user, password = db_password,
                     dbname = database_name, host = db_host, port = db_port)
  
  raw_query_result <- dbSendQuery(mydb, query)
  query_result <-  fetch(raw_query_result, n = -1)
  
  #4. Force UTF8 encoding if column is char
  if(force_UTF8) {
    query_result <- query_result %>% 
      mutate_if(is.character, 
                function(x) {Encoding(x) <- "UTF-8"
                return(x)
                })} 
  on.exit(dbDisconnect(mydb))
  return(query_result)
}