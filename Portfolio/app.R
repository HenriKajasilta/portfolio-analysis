####################################################################
#' App to add Stocks, Transactions and Finance information into the Database
#' 
#' This file contains connection information to MySQL database. Database stock
#' is divided into three separate sections: company, transaction and finance.
#' This application creates an interface to update databases. Results are
#' visible in the database but also in the application interface.
#' 
#' @family Investment
#' @export Application to update Stock database
#' 
#' 

## Libraries
library(shiny)
library(RMySQL)


## Set up for SQL connection
options(mysql = list(
  "host" = "localhost",
  "port" = 3306,
  "user" = "henri",
  "password" = names(read.csv("Teksti.csv")) # Change accordingly
))
databaseName <- "stock"

# Set up for information in StockMySQL
country.names <- c("FIN","SWE","NOR", "GER","US")
sector.names <- c("Financials", "Utilities", "Consumer Discretionary", "Consumer Staples",
                  "Energy", "Healtcare", "Industrials", "Technology", "Telecom",
                  "Materials", "Mixed")

# Connect to the database (MySQL)
con <-  dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host, 
                  port = options()$mysql$port, user = options()$mysql$user, 
                  password = options()$mysql$password)


## Fetch the initial datatables
dataCompany <- dbGetQuery(con, "SELECT * FROM company")
dataTrans <- dbGetQuery(con, "SELECT * FROM transaction")
dataFinance <- dbGetQuery(con, "SELECT * FROM finance")
dataDeposit <- dbGetQuery(con, "SELECT * FROM deposit")

#Disconnect
dbDisconnect(con)





## UI and Server
source("StockMySQL.R")

## RVs for companies, transactions and finances

# Stock
RVCompany <- reactiveValues(data = dataCompany, rem = " ")

# Transaction
RVTrans <- reactiveValues(data = dataTrans, rem = " ")

# Finance
RVFinance <- reactiveValues(data = dataFinance, rem = " ")

# Deposit
RVDeposit <- reactiveValues(data = dataDeposit, rem = " ")


## Define UI for interactive investing updates ----
ui <- fluidPage(
  navbarPage(
    # App title ----
    "Financial Data",
  tabPanel("Company List", CompanyTableUI("Company")),
  
  tabPanel("Transaction", TransactionTableUI("Transaction")),
  
  tabPanel("Financial Sheet", FinanceTableUI("Finance")),
  
  tabPanel("Deposit", DepositTableUI("Deposit"))
  )
)




# Define servers to handle the updates from UI, simultaneously the
# database in updated ----
server <- function(input, output, session) {
  CompanyTableServer("Company")
  TransactionTableServer("Transaction")
  FinanceTableServer("Finance")
  DepositTableServer("Deposit")
  
}



# ShinyApp
shinyApp(ui, server)
