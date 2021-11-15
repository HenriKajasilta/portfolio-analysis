## Libraries
library(dplyr)
library(ggplot2)
library(RMySQL)


## Business Cycle
WDI::WDIsearch('gdp.')

WDI::WDI(indicator = 'NY.GDP.MKTP.KD.ZG', country = c('US', 'FIN', 'DEU', "SWE", "NOR"), start = 2016, end = 2020) %>%
  group_by(country) %>% summarise(MCGG = mean(6.0.GDP_growth))
  
  
## Sector in cycles

## Risky

# Financials (begining - value)
# Information Technology (begining - growth)
# Consumer Discretionary (upward - value/growth)
# Materials (upward - value)
# Industrials (upward - value)
# Energy (end - value/growth)


## Defensive

# Communication Services (recession - growth)
# Consumer Staples (recession - value/growth)
# Utilities (recession - value)
# Healthcare (recession - value/growth)




####################################################################
#' Calculate the different scenarios of Intrinsic Value based on the
#' given parameters and shows all the related relevant information.
#' 
#' @family Investment
#' @family Portfolio Analysis
#' @param Ticker Company ticker in case that values are used in finance table.
#' @param GrowthRate Estimation for company's growth rate, first 5 years and the years 5-10.
#' @param GrowtWrost Same as growth rate, but worst case
#' @param GrowthBest Same as growth rate, but best case
#' @param Discount The used discount rate for profits. This doesn't need to be related to RF returns,
#' but it is personally adjusted value of the discount that is calculated to stock.
#' @param ScenarioProb Probabilities for worst, normal and best case scenarios
#' @param PEFuture The estimations of company's PE values in 10 years
#' @param Dividend Used value for dividends. If NA, then either tickers last dividend value or
#' EPS value is used, based on the value in UseEPS.
#' @param EPS Used value for earning per share, works similarly to dividends.
#' @param UseEPS If EPS is used or not, in case of FALSE, then the value of dividend is used.
#' @export SummaryIV Gives the calculated Intrinsic value with the given scenario probabilities
#' @export Casevalue For each scenario, there is inform of the used parameters and obtained IV
#' @export CaseCF For each scenario, there is inform of the calculated cashflows in given time span.
#' @export Perspective This is a plot, which shows the EPS and ROA histories, but
#' also the estimation, if normal scenarios growth numbers transform directly to obtained numbers.


StockIntrisicValue <- function(Ticker, GrowthRate = c(5,5), GrowthWorst = c(0,0),
                               GrowthBest = c(10,10), Discount, ScenarioProb = c(0.2,0.6,0.2),
                               PEFuture = c(10,15,20), Dividend = NA, EPS = NA, UseEPS = TRUE){

  ## Set up for db connetion
  options(mysql = list(
    "host" = "localhost",
    "port" = 3306,
    "user" = "henri",
    "password" = names(read.csv("Teksti.csv"))
  ))
  databaseName <- "stock"
  
  message(">>> Checking the input values...")
  
  # Connection to database
  con <-  dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host, 
                    port = options()$mysql$port, user = options()$mysql$user, 
                    password = options()$mysql$password)
  
  
  
  data <- dbGetQuery(con, sprintf("SELECT * FROM finance WHERE Ticker = %s ", paste0("'", Ticker, "'")))
  
  # Disconnect
  dbDisconnect(con)
  
  if(nrow(data) == 0 && (is.na(Dividend) || is.na(EPS))){
   message("Data for given Ticker was not found, and also no given Dividend or EPS values")}
  else if(sum(ScenarioProb) != 1 || any(ScenarioProb < 0)){
    message("Check that scenario probabilities are correctly set --- Calculations stopped")}
  else if(!is.logical(UseEPS)){
    message("UseEPS not logical --- Calculations stopped")}
  else{
    message("Input values checked...")
    
    # If EPS is used, otherwise Dividends is used
    if(UseEPS){
      message(">>> Using EPS in calculations...")
      if(is.na(EPS)){
        EPS <- (data$NetIncome / data$WeightedAverageCommonSharesStanding)[1]
        PE <- data$MarketValuePricePerShare[1] / EPS
      }
      
      # Message to inform about used EPS
      message(paste0(">>> Used EPS is ", round(EPS,3)))
    }
    else{
      message(">>> Using Dividends in calculations...")
      if(is.na(Dividend)){
        Dividend <- data$PreferredDividends[1]
      }
      
      # Dividends in use
      message(paste0(">>> Used dividend is ", round(Dividend,2)))
    }
    
    
    
    ## Datatables for Intrinsic value and other relevant information.
    ## Gathers all the scenarios in case user is interested to inspect
    ## them individually
    
    # CashFlow Calculations
    WorstCase <- CashFlowFunction(EPS, Dividend, GrowthRate0 = GrowthWorst[1], GrowthRate1 = GrowthWorst[2], Discount, PEFuture[1], UseEPS)
    NormalCase <- CashFlowFunction(EPS, Dividend, GrowthRate[1], GrowthRate[2], Discount, PEFuture[2], UseEPS)
    BestCase <- CashFlowFunction(EPS, Dividend, GrowthBest[1], GrowthBest[2], Discount, PEFuture[3], UseEPS)
      
    # Intrinsic value summary
    SummaryIV <- round(sum(c(as.numeric(WorstCase$Value[1]), as.numeric(NormalCase$Value[1]),
                         as.numeric(BestCase$Value[1]))*ScenarioProb),2)
      
      
      
    ## Plots to keep support or possible validation regarding the used numbers
    if(nrow(data) != 0){
      EPSinYears <- data.frame(EPS = data$NetIncome / data$WeightedAverageCommonSharesStanding,
                               ROA = data$NetIncome/data$TotalAssets, Year = data$Year, Type = "solid")
      # Initialize values for Profit Margin
      ROA_L <- EPSinYears$ROA[1]
      
      for(i in (data$Year[1]+1):(data$Year[1]+10)){
        if(i - data$Year[1] <= 5){Growth <- GrowthRate[1]}
        else{Growth <- GrowthRate[2]}
        
        ROA_L <- ROA_L*(1 + Growth/100)
        EPSinYears <- data.frame(rbind(EPSinYears, data.frame(EPS = NormalCase$CF$Growth[i-data$Year[1]],
                                                              ROA = ROA_L, Year = i, Type = "dashed")))
 
      }
      
      EPSinYears <- EPSinYears %>% tidyr::gather(key = "Statistic", value = "Value", -c(Year, Type))
      
      EPSPlot <- ggplot() +
        geom_line(data = filter(EPSinYears, Type == "solid"), aes(x = Year, y = Value), linetype = "solid", size = 1.2, colour = "gray42") +
        geom_line(data = EPSinYears, aes(x = Year, y = Value), linetype = "dashed", size = 1, colour = "gray48") +
        facet_grid(Statistic ~ ., scales = "free") + xlim(c(min(EPSinYears$Year)-1,max(EPSinYears$Year)+1)) +
        ggtitle(Ticker) + theme(axis.text.y = element_text(colour = 'black', size = 12), 
              axis.title.y = element_text(size = 12, hjust = 0.5, vjust = 0.2)) + 
        theme_bw() + theme(strip.text.y = element_text(size = 11, hjust = 0.5, vjust =    0.5, face = 'bold'),
              panel.grid.major = element_line(colour = "grey70", size = 0.2),
              panel.grid.minor = element_blank()) + ylab("")
    }
      
      
      
    # Create the returned table
    StockSummary <- list(SummaryIV = data.frame(IV = SummaryIV, Probs = paste0(ScenarioProb, collapse = " ")))
      
      
      
    StockSummary$WorstCaseValue <- WorstCase$Value
    StockSummary$WorstCaseCF <- WorstCase$CF
      
    StockSummary$NormalCaseValue <- NormalCase$Value
    StockSummary$NormalCaseCF <- NormalCase$CF
      
    StockSummary$BestCaseValue <- BestCase$Value
    StockSummary$BestCaseCF <- BestCase$CF
    
    # Add plot if possible
    if(nrow(data) != 0){
      StockSummary$Perspective <- EPSPlot
    }
      
    # Return two tables
    return(StockSummary)
    
  }
}
  

####################################################################
#' Calculate the Intrinsic Value based on the given parameters. Helps
#' to evaluate stock price and therefore can be used as an assist of
#' buy and sell decisions
#' 
#' @family Investment
#' @family Portfolio Analysis
#' @param EPS Used value for earning per share.
#' @param Dividend Used value for dividends.
#' @param GrowthRate0 Estimation for company's growth rate for first 5 years (percent, for example 10).
#' @param GrowthRate1 Estimation for company's growth rate for years 5-10.
#' @param Discount The used discount rate for profits. This doesn't need to be related to RF returns, but
#' it is personally adjusted value of the discount that is calculated to stock.
#' @param PEFuture The estimation of company's PE value in 10 years
#' @param UseEPS If EPS is used or not, in case of no, then the value of dividend is used.
#' @export Value Gives the calculated Intrinsic value with the given parameters
#' @export CF Cashflow calculations for the given time span (10 years)

 
CashFlowFunction <- function(EPS, Dividend, GrowthRate0, GrowthRate1, Discount, PEFuture, UseEPS){
  CashFlow <- data.frame(Growth = numeric(), PV = numeric())
  i <- 1
  
  # Returns
  if(UseEPS){
    E <- EPS
    while(i <= 5){
      E <- E*(1 + GrowthRate0/100)
      D <- 0
      # Add to table
      CashFlow <- rbind(CashFlow, data.frame(Growth = E, PV = D))
      i <- i + 1
    }
    while(i <= 10){
      E <- E*(1 + GrowthRate1/100)
      D <- 0
      # Add to table
      CashFlow <- rbind(CashFlow, data.frame(Growth = E, PV = D))
      i <- i + 1
    }
  }
  else{
    E <- Dividend
    while(i <= 5){
      E <- E*(1 + GrowthRate0/100)
      D <- E/(1+Discount/100)^i
      # Add to table
      CashFlow <- rbind(CashFlow, data.frame(Growth = E, PV = D))
      i <- i + 1
    }
    while(i <= 10){
      E <- E*(1 + GrowthRate1/100)
      D <- E/(1+Discount/100)^i
      # Add to table
      CashFlow <- rbind(CashFlow, data.frame(Growth = E, PV = D))
      i <- i + 1
    }
  }
  TerminalValue = CashFlow$Growth[10]*PEFuture/(1 + GrowthRate1/100)
  TerminalPV <- TerminalValue/(1 + Discount/100)^10
  IntrinsicValue <- TerminalPV + sum(CashFlow$PV)
  
  # Collect meaningful values into table
  Gather <- t(data.frame(IV = round(IntrinsicValue,2), TPV = round(TerminalPV,2), Grow0 = paste0(GrowthRate0,"%"),
                         Grow1 = paste0(GrowthRate1,"%"), DC = paste0(Discount, "%"),
                         Div = Dividend, EPS = EPS, EPS_Use = UseEPS)) %>% `colnames<-`("Stat")
  
  # List the obtained information
  StockAnalysis <- list(Value = Gather, CF = CashFlow)
  
  # Return
  return(StockAnalysis)
}




a <- StockIntrisicValue("OLVAS.HE", GrowthRate = c(7,8), GrowthWorst = c(3,3), GrowthBest = c(9,13), Discount = 8, ScenarioProb = c(0.25, 0.45, 0.3), PEFuture = c(15,22,29), UseEPS = T)
