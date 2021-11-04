################################################
################# Company ######################
################################################


####################################################################
#' UI to add Companies on Database
#' 
#' This function  shows the interface for adding the company list and also the
#' updated list of companies in the database.
#' 
#' @family Investment
#' @param company dataframe. Result from \MySQL{stock}
#' @export company updated




## UI for Company
CompanyTableUI <- function(id){
  tagList(
    # Sidebar panel for inputs ----
    sidebarPanel(
      tags$h3("Company Details"),
      
      # Ticker for the stock, given input should be found from YAHOO
      textInput(NS(id, "Ticker"), "Enter the Ticker"),
      
      # Input: Selector for the stock's country ----
      selectizeInput(NS(id, "Country"),
                     label = "Country:", 
                     choices = country.names,
                     options = list(
                       placeholder = 'Please select an option',
                       onInitialize = I('function() { this.setValue(""); }')
                     )),
      
      # Select the sector that stock / company is working
      selectInput(NS(id, "Sector"),
                  label = "Sector:", 
                  choices = sector.names),
      
      # Select the product that company is working is producing
      textInput(NS(id, "Product"), "Enter the Product"),
      
      # Select the type; Either stock is unique stock or ETF
      selectInput(NS(id, "Type"), "Type:", 
                  c("Stock" = "Stock",
                    "ETF" = "ETF")),
      
      # Update the company list
      actionButton(NS(id, "update"), "Add Company"),
      
      # Remove the company ---
      tags$h3("Remove"),
      
      textInput(NS(id, "RemoveCompany"), "Ticker to be Removed"),
      
      actionButton(NS(id, "remove"), "Remove Company")
      
    ),
    
    ## Main panel for displaying outputs ----
    mainPanel(fluidRow(
      DT::dataTableOutput(NS(id, "CompanyOverall"), width = "80%"),
      
      span(textOutput(NS(id, "CompanyInfoUpdate")), style = "color:mediumvioletred")
      
    ))
  )
}


####################################################################
#' Server to add Companies on Database
#' 
#' This function  listens the input from the UI and update the company list
#' accordingly with the transmitted information. Updated table is send back
#' to UI view.
#' 

## Server for Company
CompanyTableServer <- function(id){
  moduleServer(id, function(input, output, session) {
    output$CompanyOverall <- DT::renderDataTable(RVCompany$data,
                                                 options = list(pageLength = 8, dom = 'tip'),
                                                  rownames = F)
                            
    
                            ## Add company                    
                            observeEvent(input$update, {
                              newrow <- data.frame(
                                Ticker = toupper(input$Ticker),
                                Country = input$Country,
                                Sector = input$Sector,
                                Product = input$Product,
                                Type = input$Type)
                              
                              RVCompany$data <- rbind(RVCompany$data, newrow)
                              InsertCompanyUpdate(newrow)
                              
                              RVCompany$rem <- " "
                            })
    # Message                        
    output$CompanyInfoUpdate <- renderText(RVCompany$rem)
                            
    
                            ## Remove company      
                            observeEvent(input$remove, {
                              
                               if(toupper(input$RemoveCompany) %in% RVCompany$data$Ticker){
                                  RVCompany$rem <- paste0("Ticker ", toupper(input$RemoveCompany),
                                                  " removed from the company list!")
                                 
                                  RVCompany$data <- subset(RVCompany$data, Ticker != toupper(input$RemoveCompany))
                                  InsertCompanyRemove(toupper(input$RemoveCompany))
                               }
                               else 
                                  RVCompany$rem <- paste0("Ticker ", toupper(input$RemoveCompany) , " not found!")
                              
                            })
  })
}
    



## Functions to update company database.
# Functions are called when adding and removing company
# from the database.
# Function to update a new company to the SQL database
InsertCompanyUpdate <- function(newrow){
  con <-  dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host, 
                    port = options()$mysql$port, user = options()$mysql$user, 
                    password = options()$mysql$password)
  # Add newrow to company list
  dbGetQuery(con, sprintf("INSERT INTO company (Ticker, Country, Sector, Product, Type)
                        VALUES ('%s')", paste(newrow, collapse = "', '")))
  
  dbDisconnect(con)
}


# Function to remove unnecessary company from the list
InsertCompanyRemove <- function(companyticker){
  con <-  dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host, 
                    port = options()$mysql$port, user = options()$mysql$user, 
                    password = options()$mysql$password)
  
  dbGetQuery(con, sprintf("DELETE FROM company WHERE Ticker = %s", paste0("'", companyticker, "'")))
  
  dbDisconnect(con)
}








################################################
################# TRANSACTIONS #################
################################################

####################################################################
#' UI to add Transactions on Database
#' 
#' This function  shows the interface for adding the transactions to the list and also the
#' updated list of stransactions in the database.
#' 
#' @family Investment
#' @param transaction dataframe. Result from \MySQL{stock}
#' @export transaction updated




## UI for Transactions
TransactionTableUI <- function(id){
  tagList(
    # Sidebar panel for inputs ----
    sidebarPanel(
      tags$h3("Transaction Details"),
      
      # Ticker for the stock, it should be found from YAHOO with this
      textInput(NS(id, "Ticker"), "Enter the Ticker"),
      
      # Input: Date when transaction has been made
      dateInput(NS(id, "Date"),
                label = "Date:",
                value = Sys.Date()),
      
      # Quantity for the purchases
      numericInput(NS(id, "Quant"),
                  label = "Quantity:", 
                  value = 1,
                  step = 1),
      
      # One item's value in the stock market
      numericInput(NS(id, "Value"),
                   label = "Value (1 pcs):",
                   value = 1),
      
      # Transactions costs
      numericInput(NS(id, "Cost"),
                   label = "Broker's Charge:",
                   value = 9),
      
      # Currency in the transaction
      selectInput(NS(id, "Currency"), "Currency:", 
                  c("EUR" = "EUR",
                    "NOK" = "NOK",
                    "SEK" = "SEK",
                    "USD" = "USD")),

      
      # Update the transaction list
      actionButton(NS(id, "updateTrans"), "Add Transaction"),
      
      # Remove
      tags$h3("Remove"),
      
      actionButton(NS(id, "removeTrans"), "Remove Transaction")
      
    ),
    
    ## Main panel for displaying outputs ----
    mainPanel(fluidRow(
      DT::dataTableOutput(NS(id, "TransactionOverall"), width = "80%"),
      
      span(textOutput(NS(id, "TransactionInfoUpdate")), style = "color:mediumvioletred")
      
    ))
  )
}



####################################################################
#' Server to add Transactions on Database
#' 
#' This function  listens the input from the UI and update the transaction list
#' accordingly with the transmitted information. Updated table is send back
#' to UI.
#' 

## Server for Transactions
TransactionTableServer <- function(id){
  moduleServer(id, function(input, output, session) {
    output$TransactionOverall <- DT::renderDataTable(RVTrans$data,
                                                 options = list(pageLength = 10, dom = 'tip'),
                                                 rownames = F)
    
    
    
    ## Add new transaction to the database
    observeEvent(input$updateTrans, {
      newrow <- InsertTransaction(input$Date, input$Ticker, input$Quant, input$Value,
                                  input$Cost, input$Currency)
      
      RVTrans$data <- rbind(RVTrans$data, newrow)
      
      RVTrans$rem <- " "
    })
    
    # Message
    output$TransactionInfoUpdate <- renderText(RVTrans$rem)
    
    
    ## Remove the transaction from the list
    observeEvent(input$removeTrans, {
      
      if(RVTrans$data$TransactionId >= 1){
        RVTrans$rem <- "Last transaction input removed!"
        
        Max <- max(RVTrans$data$TransactionId)
        RVTrans$data <- subset(RVTrans$data, TransactionId != Max)
        InsertTransactionRemove(Max)
      }
      else 
        RVTrans$rem <- "No transactions to be removed!"
      
    })
  })
}






## Functions to execute the transaction datatable updates
# Add newrow to transaction list
InsertTransaction <- function(Date, Ticker, Quant, Value, Cost, Currency){
    
    # Connection
    con <-  dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host, 
                    port = options()$mysql$port, user = options()$mysql$user, 
                    password = options()$mysql$password)
    
    # Max transaction order number
    Max <- dbGetQuery(con, "SELECT max(TransactionId) FROM transaction")
    # Check how many times have been invested to the given stock
    InvestCount <- dbGetQuery(con, sprintf("SELECT max(Inv) FROM transaction WHERE Ticker = '%s'", Ticker))
    InvestCount <- ifelse(is.na(InvestCount), 0, InvestCount)
    
    # Make dataframe (newrow) from the given information
    Insert <- data.frame(TransactionId = Max[[1]] + 1, Inv = InvestCount[[1]] + 1,
                         Code = paste0(toupper(Ticker), InvestCount[[1]] + 1),
                         Ticker = toupper(Ticker), Date = format(as.Date(Date), "%Y-%m-%d %H:%M:%S"),
                         Quant = Quant, Value = Value, Cost = Cost,
                         Amount = Quant * Value + Cost,
                         Description = paste0(Quant, " @", Value),
                         Currency = Currency)
    
    
    # Update the database
    dbGetQuery(con, sprintf("INSERT INTO transaction (TransactionId, Inv, Code, Ticker, Date, Quant,
                            Value, Cost, Amount, Description, Currency)
                            VALUES ('%s')", paste(Insert, collapse = "', '")))
    
    # Disconnect
    dbDisconnect(con)
    
    #Return
    Insert
}



# Remove the latest transaction addition
InsertTransactionRemove <- function(MaxInvestNum){
  con <-  dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host, 
                    port = options()$mysql$port, user = options()$mysql$user, 
                    password = options()$mysql$password)
  
  dbGetQuery(con, sprintf("DELETE FROM transaction WHERE TransactionId = %s", MaxInvestNum))
  
  dbDisconnect(con)
}







################################################
################# FINANCIALS ###################
################################################

####################################################################
#' UI to add Finance Information into the Database
#' 
#' This function  shows the interface for adding the financial information to the list and also the
#' updated list of finances in the database.
#' 
#' @family Investment
#' @param finance dataframe. Result from \MySQL{stock}
#' @export finance updated

## UI for Finance information
FinanceTableUI <- function(id){
  tagList(
    # Sidebar panel for inputs ----
    sidebarPanel(
      tags$h3("Finance Details"),
      
      # Ticker for the stock, it should be found from YAHOO with this
      textInput(NS(id, "Ticker"), "Enter the Ticker"),
      
      # Finance sheet's year
      numericInput(NS(id, "Year"),
                   label = "Sheet's Year:",
                   value = as.numeric(format(Sys.Date(), "%Y"))),
      
      
      # CurrentAssets
      numericInput(NS(id, "CurrentAssets"),
                   label = "Current Assets:",
                   value = 0,
                   min = 0),
      
      # CurrentLiabilities
      numericInput(NS(id, "CurrentLiabilities"),
                   label = "Current Liabilities:",
                   value = 0,
                   min = 0),
      
      # Total Assets
      numericInput(NS(id, "TotalAssets"),
                   label = "Total Assets:",
                   value = 0,
                   min = 0),
      
      # Total Liabilities
      numericInput(NS(id, "TotalLiabilities"),
                   label = "Total Liabilities:",
                   value = 0,
                   min = 0),
      
      # Average Total Assets
      numericInput(NS(id, "AverageTotalAssets"),
                   label = "Average Total Assets:",
                   value = 0,
                   min = 0),
      
      # Net Sales
      numericInput(NS(id, "NetSales"),
                   label = "Net Sales:",
                   value = 0,
                   min = 0),
      
      # Cost of Goods Sold
      numericInput(NS(id, "COGS"),
                   label = "Cost of Goods Sold:",
                   value = 0),
      
      # Earnings Before Interest and Taxes
      numericInput(NS(id, "EBIT"),
                   label = "EBIT:",
                   value = 0),
      
      # Net Income
      numericInput(NS(id, "NetIncome"),
                   label = "Net Income:",
                   value = 0),
      
      # Average Inventory
      numericInput(NS(id, "AverageInventory"),
                   label = "Average Inventory:",
                   value = 0,
                   min = 0),
      
      # Interest Expenses
      numericInput(NS(id, "InterestExpense"),
                   label = "Interest Expenses:",
                   value = 0,
                   min = 0),
      
      # Preferred Dividends
      numericInput(NS(id, "PreferredDividends"),
                   label = "Preferred Dividends:",
                   value = 0,
                   min = 0),
      
      # Total Capital Invested
      numericInput(NS(id, "TotalCapitalInvested"),
                   label = "Total Capital Invested:",
                   value = 0,
                   min = 0),
      
      # Weighted Average Common Shares Standing
      numericInput(NS(id, "WeightedAverageCommonSharesStanding"),
                   label = "Weighted Average Common Shares Standing:",
                   value = 0,
                   min = 0),
      
      # Market Value Price Per Share
      numericInput(NS(id, "MarketValuePricePerShare"),
                   label = "Market Value Price Per Share:",
                   value = 0,
                   min = 0),

      
      # Update the finance sheet list
      actionButton(NS(id, "updateFinance"), "Add Finance Info"),
      
      # Remove
      tags$h3("Remove"),
      
      textInput(NS(id, "RemoveFinanceTicker"), "Ticker to be Removed"),
      
      numericInput(NS(id, "RemoveFinanceYear"), "Year to be Removed",
                   value = as.numeric(format(Sys.Date(), "%Y"))),
      
      actionButton(NS(id, "removeFinance"), "Remove Finance Info")
      
    ),
    
    ## Main panel for displaying outputs ----
    mainPanel(fluidRow(
      DT::dataTableOutput(NS(id, "FinanceOverall"), width = "80%"),
      
      span(textOutput(NS(id, "FinanceInfoUpdate")), style = "color:mediumvioletred")
      
    ))
  )
}



####################################################################
#' Server to add Finances on Database
#' 
#' This function  listens the input from the UI and update the transaction list
#' accordingly with the transmitted information. Updated table is send back
#' to UI.
#' 

## Server for Finances
FinanceTableServer <- function(id){
  moduleServer(id, function(input, output, session) {
    output$FinanceOverall <- DT::renderDataTable(RVFinance$data[,-1],
                                                     options = list(pageLength = 45, dom = 'tip',
                                                                    scrollX = TRUE), rownames = F)
    
    
    
    ## Add new row to finance database
    observeEvent(input$updateFinance, {
      newrow <- InsertFinance(input$Ticker, input$Year, input$CurrentAssets, input$CurrentLiabilities,
                              input$TotalAssets, input$TotalLiabilities, input$AverageTotalAssets,
                              input$NetSales, input$COGS, input$EBIT, input$NetIncome, input$AverageInventory,
                              input$InterestExpense, input$PreferredDividends, input$TotalCapitalInvested,
                              input$WeightedAverageCommonSharesStanding, input$MarketValuePricePerShare)
      
      RVFinance$data <- rbind(RVFinance$data, newrow)
      
      RVFinance$rem <- " "
    })
    
    # Message
    output$FinanceInfoUpdate <- renderText(RVFinance$rem)
    
    
    ## Remove the requested ticker + year compination from the finance list
    observeEvent(input$removeFinance, {
      
      if(length(which(RVFinance$data$Ticker == toupper(input$RemoveFinanceTicker) &
                      RVFinance$data$Year == input$RemoveFinanceYear)) >= 1){
        
        RVFinance$rem <- paste0("Ticker ", toupper(input$RemoveFinanceTicker),
                                " (", input$RemoveFinanceYear, ") ", "removed from the list!")
        
        RVFinance$data <- RVFinance$data[which(
          RVFinance$data$Ticker != toupper(input$RemoveFinanceTicker) |
          RVFinance$data$Year != input$RemoveFinanceYear),]
        
        InsertFinanceRemove(input$RemoveFinanceTicker, input$RemoveFinanceYear)
        
      }
      else 
        RVFinance$rem <- "Requested financial information not found and therefore can't be removed!"
      
    })
  })
}



## Functions to update finance database in MySQL
# Add newrow to finance list
InsertFinance <- function(Ticker, Year, CA, CL, TA, TL, ATA, NS, COGS, EBIT, NI, AI, IE, PD,
                          TCI, WACSS, MVPPS){
  
  # Connection
  con <-  dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host, 
                    port = options()$mysql$port, user = options()$mysql$user, 
                    password = options()$mysql$password)
  
  # Max finance id number
  Max <- dbGetQuery(con, "SELECT max(FinanceId) FROM finance")
  
  # Make dataframe (newrow) from the given information
  Insert <- data.frame(FinanceId = Max[[1]] + 1, Ticker = toupper(Ticker), CurrentAssets = CA,
                       CurrentLiabilities = CL, TotalAssets = TA, TotalLiabilities = TL,
                       AverageTotalAssets = ATA, TotalEquity = TA - TL,
                       ShareHoldersEquity = TA - TL, NetSales = NS, COGS = COGS,
                       EBIT = EBIT, NetIncome = NI, AverageInventory = AI,
                       InterestExpense = IE, PreferredDividends = PD,
                       TotalCapitalInvested = TCI, WeightedAverageCommonSharesStanding = WACSS,
                       MarketValuePricePerShare = MVPPS, Year = Year)
  
  
  # Update the database
  dbGetQuery(con, sprintf("INSERT INTO finance (FinanceId, Ticker, CurrentAssets, CurrentLiabilities,
                           TotalAssets, TotalLiabilities, AverageTotalAssets, TotalEquity,
                           ShareHoldersEquity, NetSales, COGS, EBIT, NetIncome, AverageInventory,
                           InterestExpense, PreferredDividends, TotalCapitalInvested,
                           WeightedAverageCommonSharesStanding, MarketValuePricePerShare, Year)
                            VALUES ('%s')", paste(Insert, collapse = "', '")))
  
  # Disconnect
  dbDisconnect(con)
  
  #Return
  Insert
}



# Remove the requested finance information from the list
InsertFinanceRemove <- function(RFT, RFY){
  con <-  dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host, 
                    port = options()$mysql$port, user = options()$mysql$user, 
                    password = options()$mysql$password)
  
  dbGetQuery(con, paste0(sprintf("DELETE FROM finance WHERE Ticker = %s", paste0("'", RFT, "'")),
                         sprintf(" AND Year = %s", RFY)))
  
  dbDisconnect(con)
}



################################################
################# Deposit ######################
################################################


####################################################################
#' UI to add Deposits on Database
#' 
#' This function  shows the interface for adding cash to the investment
#' balance sheet. Also updated list of deposits is shown in the database.
#' 
#' @family Investment
#' @param deposit dataframe. Result from \MySQL{stock}
#' @export deposit updated




## UI for Deposit
DepositTableUI <- function(id){
  tagList(
    # Sidebar panel for inputs ----
    sidebarPanel(
      tags$h3("Deposits"),
      
      
      # Input: Date when deposit has been made
      dateInput(NS(id, "Date"),
                label = "Date:",
                value = Sys.Date()),
      
      # Sum of deposit
      numericInput(NS(id, "DepositSum"),
                   label = "Sum of the Deposit:",
                   value = 0,
                   min = 0),
      
      # Update the company list
      actionButton(NS(id, "updateDeposit"), "Deposit"),
      
      # Remove the company ---
      tags$h3("Remove"),
      
      
      actionButton(NS(id, "removeDeposit"), "Remove Deposit")
      
    ),
    
    ## Main panel for displaying outputs ----
    mainPanel(fluidRow(
      DT::dataTableOutput(NS(id, "DepositOverall"), width = "80%"),
      
      span(textOutput(NS(id, "DepositInfoUpdate")), style = "color:mediumvioletred")
      
    ))
  )
}


####################################################################
#' Server to add Deposits on Database
#' 
#' This function  listens the input from the UI and update the deposit list
#' accordingly with the transmitted information. Updated table is send back
#' to UI view.
#' 

## Server for Deposit
DepositTableServer <- function(id){
  moduleServer(id, function(input, output, session) {
    output$DepositOverall <- DT::renderDataTable(RVDeposit$data,
                                                 options = list(pageLength = 10, dom = 'tip'),
                                                 rownames = F)
    
    
    ## Add Deposit                   
    observeEvent(input$updateDeposit, {
      newrow <- InsertDepositUpdate(input$Date, input$DepositSum)
      
      RVDeposit$data <- rbind(RVDeposit$data, newrow)
      
      RVDeposit$rem <- " "
    })
    # Message                        
    output$DepositInfoUpdate <- renderText(RVDeposit$rem)
    
    
    ## Remove deposit      
    observeEvent(input$removeDeposit, {
      
      if(RVDeposit$data$DepositId >= 1){
        RVDeposit$rem <- "Last deposit input removed!"
        
        Max <- max(RVDeposit$data$DepositId)
        RVDeposit$data <- subset(RVDeposit$data, DepositId != Max)
        InsertDepositRemove(Max)
      }
      else 
        RVDeposit$rem <- "No deposit to be removed!"
      
    })
  })
}




## Functions to update company database.
# Functions are called when adding and removing company
# from the database.
# Function to update a new company to the SQL database
InsertDepositUpdate <- function(Date, Deposit){
  con <-  dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host, 
                    port = options()$mysql$port, user = options()$mysql$user, 
                    password = options()$mysql$password)
  
  
  # Max deposit order number
  Max <- dbGetQuery(con, "SELECT max(DepositId) FROM deposit")
  
  Insert <- data.frame(DepositId = Max[[1]] + 1,
                       Date = format(as.Date(Date), "%Y-%m-%d %H:%M:%S"),
                       Amount= Deposit)
  
  # Add newrow to deposit list
  dbGetQuery(con, sprintf("INSERT INTO deposit (DepositId, Date, Amount)
                        VALUES ('%s')", paste(Insert, collapse = "', '")))
  
  dbDisconnect(con)
  
  # Return
  Insert
}


# Function to remove last deposit input from the list
InsertDepositRemove <- function(LastDeposit){
  con <-  dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host, 
                    port = options()$mysql$port, user = options()$mysql$user, 
                    password = options()$mysql$password)
  
  dbGetQuery(con, sprintf("DELETE FROM deposit WHERE DepositId = %s", LastDeposit))
  
  dbDisconnect(con)
}