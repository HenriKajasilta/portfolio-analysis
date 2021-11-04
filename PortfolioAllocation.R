#### Portfolio Analysis #####
library(tidyquant)
library(data.table)
library(dplyr)





####################################################################
#' Get Simulated Stock Returns for Missing Rows. Also Returns the
#' Statistics of the simulated values
#' 
#' This function lets the user to complete the unfilled data
#' 
#' @family Investment
#' @family Portfolio Analysis
#' @param Table datatable or dataframe. Add returns table in order to fill missing values.
#' @param Index Character. Name of the index that the simulated values are intended to correlate with.
#' @param Seed Random Seed. This helps to achieve constant results in simulated values
#' @param rm Remove last rows. How many last rows is removed from the data set. Automatically remove
#' the last month, because it usually contains several missing values
#' @export


PortfolioAnalysisPreparation <- function(Portfolio, StocksToDrop, FillFrom = "2010-01-01", Periodic = "monthly", RF = 0.0, SH = FALSE, Port = 25){
  # Choose only those stocks, which is still owned.
  Symb <- Portfolio[Portfolio$Count > 0,]$Ticker
  Symb <- Symb[!(Symb %in% StocksToDrop)]


  message(">>> Downloading historical data for each stock...")
  # We choose the performance timeframe based on the FillFrom value, overall this has been good upside period
  Symb_Return <- tq_get(Symb, from = FillFrom, to = Sys.Date(), periodicity = Periodic) %>%
    group_by(symbol) %>% mutate(ret := adjusted / shift(adjusted, 1) - 1)


  message("Calculations ready...")
  
  # Order investments based on the missing returning values
  Return_Spread <- Symb_Return[!is.na(Symb_Return$ret),] %>% select(c("date", "symbol", "ret")) %>%
    tidyr::spread(symbol, ret) %>% select(order(sapply(., function(x) sum(is.na(x)))))




  # Creating a table that shows expected returns and volatilities of stocks
  tab <- Symb_Return[!is.na(Symb_Return$ret),] %>% select(c("symbol", "ret")) %>%
    group_by(symbol) %>% summarise(er = round(mean(ret),4), sd = round(sd(ret),4)) %>%
    mutate(er = er*100, sd = sd*100) %>% `colnames<-`(c("Symbol", "Expect. Ret (%)", "Stand. Dev. (%)"))
  
  
  FilledReturnTable <- FillReturnTable(Return_Spread)
  FilledReturnTable$Volatility <- tab
  
  # Add Efficient Frontier
  EfFront <- EfficientFrontierForStocks(FilledReturnTable, RiskFree = RF, Short = SH, NPort = Port)
  
  FilledReturnTable$FrontierWeights <- EfFront$FrontierWeights
  FilledReturnTable$FrontierPlot <- EfFront$FrontierPlot
  
  
  attr(FilledReturnTable$ReturnsFill, "type") <- "returns_file_filled"
  attr(FilledReturnTable$SimulationStatistics, "type") <- "returns_file_simulationstatistics"
  attr(FilledReturnTable$Volatility, "type") <- "returns_file_volatility"
  
  return(FilledReturnTable)

}  


####################################################################
#' Get Simulated Stock Returns for Missing Rows. Also Returns the
#' Statistics of the simulated values
#' 
#' This function lets the user to complete the unfilled data
#' 
#' @family Investment
#' @family Portfolio Analysis
#' @param Table datatable or dataframe. Add returns table in order to fill missing values.
#' @param Index Character. Name of the index that the simulated values are intended to correlate with.
#' @param Seed Random Seed. This helps to achieve constant results in simulated values
#' @param rm Remove last rows. How many last rows is removed from the data set. Automatically remove
#' the last month, because it usually contains several missing values
#' @export

FillReturnTable <- function(Table, Index = "DXET.DE", Seed = 133463456, rm = 0){
  # Seed
  set.seed(Seed)
  
  # Collect the stats of simulated values to the table, so it is easier to value simulated results
  # This is saved to the list and can be inspected as a second argument in results
  StatData <- data.frame(Stock = character(), Mean = numeric(), Mean_Sim = numeric(),
                         StD = numeric(), StD_Sim = numeric(), Cor = numeric(),
                        Cor_Sim = numeric(),  NaShare = numeric())
  
  # Remove rm last rows of data
  Table <- as.data.frame(Table[-((nrow(Table)-rm):nrow(Table)),])
  
  
  # Check that table is correct
  if(names(Table)[1] != "date"){"Check the table name"}
  
  # Function
  else{
    for(i in 1:length(Table)){
      if(!any(is.na(Table[,i]))){
        next
      }
      else{
        # Company with NA values
        CompWithNA <- names(Table)[i]
        
        
        
        # Divide the rows with values and with no values (NA)
        RowsWithValue <- which(!is.na(Table[,i]))
        RowsWithNA <-which(is.na(Table[,i]))
        
        
        
        # Statistics
        me <- mean(Table[RowsWithValue, CompWithNA])
        sd <- sd(Table[RowsWithValue, CompWithNA])
        na <- sum(is.na(Table[RowsWithNA, CompWithNA])) / nrow(Table)
        
        cor_real <- cor(Table[RowsWithValue, Index], Table[RowsWithValue, CompWithNA])
        
        # Create the formula by using the determined index to follow
        if(Index %in% names(Table)){
          formula = paste0(CompWithNA, " ~ ", Index)
          
          Model <- glm(formula, data = Table[RowsWithValue,])
        }
        else{"Index not valid, not in the portfolio or missing values"; break}
        
        # Brown motion with the real correlation, first term is used to keep some covariance with among
        # the simulated values (based on the measured variance in the latest values)
        p <- cor_real
        BrownMotion <- p * Table[RowsWithNA, Index] + sqrt(1-p^2)*rnorm(length(RowsWithNA), me, sd)
        
        # Statistics for simulated values
        me_simu <- mean(BrownMotion)
        sd_simu <- sd(BrownMotion)
        
        cor_simu <- cor(Table[RowsWithNA, Index], BrownMotion)
        
        
        # Update the missing rows
        Table[RowsWithNA, CompWithNA] <- BrownMotion
        
        StatData <- rbind(StatData, data.frame(Stock = CompWithNA,
                                               Mean = round(me,3),
                                               Mean_Sim = round(me_simu,3),
                                               Std = round(sd,3),
                                               Std_Sim = round(sd_simu,3),
                                               Cor = round(cor_real,4),
                                               Cor_Sim = round(cor_simu,4),
                                               NaShare = round(na,2)))
        
      }
      
    }
    listUpd <- list("ReturnsFill" = Table,  "SimulationStatistics" = StatData)
  }
  return(listUpd)
}




####################################################################
#' Get Efficient Frontier, also return the weight table
#' 
#' This function lets the user to inspect efficient frontier based on the history values
#' 
#' @family Investment
#' @family Portfolio Analysis
#' @param Table datatable or dataframe. Add returns table in order to fill missing values.
#' @param Index Character. Name of the index that the simulated values are intended to correlate with.
#' @param Seed Random Seed. This helps to achieve constant results in simulated values
#' @param rm Remove last rows. How many last rows is removed from the data set. Automatically remove
#' the last month, because it usually contains several missing values
#' @export



EfficientFrontierForStocks <- function(FilledReturns, RiskFree, Short, NPort){
  
  # construct the data
  asset.names <- FilledReturns$Volatility$Symbol
  er <- FilledReturns$Volatility$`Expect. Ret (%)`
  names(er) <- asset.names
  covmat <- cov(as.matrix(FilledReturns$ReturnsFill[-1]))
  
  er <- er[dimnames(covmat)[[1]]]
  
  
  r.free = RiskFree
  Vol = FilledReturns$Volatility
  
  # tangency portfolio
  tan.port <- IntroCompFinR::tangency.portfolio(er, covmat, r.free, shorts = Short)
  # compute global minimum variance portfolio
  gmin.port = IntroCompFinR::globalMin.portfolio(er, covmat)
  
  # compute portfolio frontier
  ef <- IntroCompFinR::efficient.frontier(er, covmat, alpha.min=-2,
                                          alpha.max=1.5, nport=NPort, shorts = Short)
  
  
  
  p <- ggplot(as.data.frame(cbind(er = ef$er, sd = ef$sd)), aes(sd, er)) + geom_point(col = "blue") +
      geom_text(data = Vol, aes(x = `Stand. Dev. (%)`/100, y = `Expect. Ret (%)`, label = Symbol)) +
      geom_point(aes(gmin.port$sd, gmin.port$er), col ="green", pch = 16, cex = 4) +
      geom_text(aes(gmin.port$sd, gmin.port$er, label="GLOBAL MIN"), col = "green", cex  = 2, hjust = -0.2) +
      geom_point(aes(tan.port$sd, tan.port$er), col ="mediumvioletred", pch = 16, cex = 4) +
      geom_text(aes(tan.port$sd, tan.port$er, label="TANGENCY"), col = "mediumvioletred", cex  = 2, hjust = -0.2) +
      geom_abline(intercept = r.free, slope = (tan.port$er - r.free)/tan.port$sd, col="steelblue", lwd=.7) +
      xlab("Stand. Deviation") + ylab("Expected Return (%)") + ggtitle("Efficient Frontier")
    
  
  
  listUpdEff <- list("FrontierWeights" = ef$weights,  "FrontierPlot" = p)
  return(listUpdEff)
}
