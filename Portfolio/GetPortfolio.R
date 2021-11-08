####################################################################
#' Get Personal Portfolio's Data
#' 
#' This function lets the user fetch his 
#' Portfolio's data, works locally.
#' 
#' @family Investment
#' @family Credentials
#' @param Database Character. Import your database name
#' @export Three tables, portfolio, transactions and cash

stocks_file <- function(Database = "stock") {
  file <- function(db){
    
    if(db != "stock"){ stop("Check the database name")}
    
    else{
      
      ## Set up for SQL connection
      options(mysql = list(
        "host" = "localhost",
        "port" = 3306,
        "user" = "henri",
        "password" = names(read.csv("Teksti.csv"))
      ))
      databaseName <- "stock"
      
      # Date function
      FormatDate <- function(ChDate){
        ChDate <- as.Date(ChDate)
        return(ChDate)
      }
      
      
      # Connect to the database (MySQL)
      con <-  dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host, 
                        port = options()$mysql$port, user = options()$mysql$user, 
                        password = options()$mysql$password)
      
      
      dataTrans <- dbGetQuery(con, "SELECT * FROM transaction")
      dataDeposit <- dbGetQuery(con, "SELECT * FROM deposit")
      dataPortfolio <- dbGetQuery(con, "SELECT * FROM summary")
      
      #Disconnect
      dbDisconnect(con)
      
      dataTrans$Date <- FormatDate(dataTrans$Date)
      dataDeposit$Date <- FormatDate(dataDeposit$Date)
      dataPortfolio$StartDate <- FormatDate(dataPortfolio$StartDate)
      
      
      dataTrans <- subset(dataTrans, select = c("Date", "Ticker", "Code", "Description",
                                        "Quant", "Value", "Cost", "Amount", "Currency"))
      
      mylist <- list("portfolio" = dataPortfolio, "transactions" = dataTrans, "cash" = dataDeposit)
      return(mylist)
    }
  }
  
  results <- file(Database)
  
  attr(results$portfolio, "type") <- "stocks_file_portfolio"
  attr(results$transactions, "type") <- "stocks_file_transactions"
  attr(results$cash, "type") <- "stocks_file_cash"
  attr(results, "type") <- "stocks_file"
  
  
  
  
  
  
  message("File imported succesfully!")
  return(results)   
}