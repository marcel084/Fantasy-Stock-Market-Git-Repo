# Stock Trading Competiton | First Round | Marcel Socorro
library(jsonlite)
library(stringr)
library(tidyverse)
library(alphavantager)
library(forecast)

# Buy | Sell Function
BUY <- function(quantity, symbol) {
  url <-
    paste(
      "https://projectrex.net/stocks/?key=3ba96214&av_key=Y7NKAR5J8U73V8TN&request=buy&quantity=",
      quantity,
      "&symbol=",
      symbol,
      sep = ""
    )
  transaction <- as.data.frame(fromJSON(url))
  return(transaction)
}

SELL <- function(quantity, symbol) {
  url <-
    paste(
      "https://projectrex.net/stocks/?key=3ba96214&av_key=Y7NKAR5J8U73V8TN&request=sell&quantity=",
      quantity,
      "&symbol=",
      symbol,
      sep = ""
    )
  transaction <- as.data.frame(fromJSON(url))
  return(transaction)
}


# Extract number Function
numextract <- function(string) {
  str_extract(string, "\\-*\\d+\\.*\\d*")
}


# Get Time Series for several companies Function
getTS <- function(companies, h_minutes) {
  df <-
    data.frame(
      Company = character(),
      PointForecast = double(),
      First = double(),
      Last = double(),
      Change = double()
    )
  n_companies <- length(companies)
  PAUSES <- seq(from = 0, to = n_companies, by = 5)
  MV <- 1
  for (i in 1:length(companies)) {
    ts_intadate <- av_get(
      symbol = companies[i],
      av_fun = "TIME_SERIES_INTRADAY",
      interval = "1min",
      outputsize = "full"
    )
    ts_intadate$ave <- (ts_intadate$high + ts_intadate$low) / 2
    tsData <- ts(ts_intadate$ave, frequency = 390)
    arima <- auto.arima(tsData)
    arimaForecast <- forecast(arima, h = h_minutes)
    Forecast <- data.frame(arimaForecast)
    predictedMean <- mean(Forecast$Point.Forecast)
    predictedFirst <- head(Forecast$Point.Forecast, 1)
    predictedLast <- tail(Forecast$Point.Forecast, 1)
    predictedChange <-
      (head(Forecast$Point.Forecast, 1)) - (tail(Forecast$Point.Forecast, 1))
    result <-
      data.frame(
        Company = companies[i],
        PointForecast = predictedMean,
        First = predictedFirst,
        Last = predictedLast,
        Change = predictedChange
      )
    df <- rbind(df, result)
    if (MV %in% PAUSES) {
      Sys.sleep(60)
    }
    MV <- MV + 1
  }
  return(df)
}

# Get Descition table Function
getDescitionTable <- function(log_url, companies) {
  showLog <- fromJSON(log_url)
  stocksLog <- data.frame(showLog[3])
  stocksLog <- subset(stocksLog,stocksLog$log.result == "success")
  stocksLog$PriceTransaction <-
    numextract(
      read.csv(
        text = stocksLog$log.details,
        colClasses = c("NULL", "NULL", "NULL", "character", "NULL", "NULL"),
        header = FALSE,
        strip.white = TRUE
      )[[1L]]
    )
  stocksLog$log.quantity <- as.double(stocksLog$log.quantity)
  stocksLog$PriceTransaction <-
    as.double(stocksLog$PriceTransaction)
  stocksLog$log.budget <- as.double(stocksLog$log.budget)
  
  quotes <- data.frame(
    symbol = character(),
    open = integer(),
    high = integer(),
    low = integer(),
    price = integer(),
    volume = integer(),
    latestday = as.Date(character()),
    previousclose = integer(),
    change = integer(),
    changepercent = character()
  )
  n_companies <- length(companies)
  PAUSES <- seq(from = 0, to = n_companies, by = 5)
  
  MV <- 1
  for (i in companies) {
    c <- av_get(symbol = i, av_fun = "GLOBAL_QUOTE")
    quotes <- rbind(quotes, c)
    if (MV %in% PAUSES) {
      Sys.sleep(60)
    }
    MV <- MV + 1
  }
  
  compare = merge(stocksLog[, c(
    "log.date",
    "log.action",
    "log.symbol",
    "log.quantity",
    "log.budget",
    "PriceTransaction"
  )],
  quotes, by.x = "log.symbol", by.y = "symbol")
  
  table <- compare %>%
    rename(
      Symbol = log.symbol,
      Date = log.date,
      Action = log.action,
      Quantity = log.quantity,
      Budget = log.budget,
      CurrentPrice = price
    )
  
  table$profit <-
    table$CurrentPrice * table$Quantity - table$PriceTransaction * table$Quantity
  
  descitionTable <- drop_na(table)
  descitionTable <- descitionTable[order(descitionTable$Date), ]
  Order <- c(1:nrow(descitionTable))
  descitionTable$Order <- Order
  stonks <-
    descitionTable %>%  group_by(Symbol) %>% slice(which.max(Order))
  
  qohTable <- descitionTable %>% select(Symbol, Action, Quantity)
  qohTable$Quantity[qohTable$Action == "sell"] <-
    qohTable$Quantity[qohTable$Action == "sell"] * (-1)
  qohTable <-
    qohTable %>% group_by(Symbol) %>% summarize(qoh = sum(Quantity))
  FinalTable <-
    merge(stonks[, c("Symbol",
                     "Budget",
                     "PriceTransaction",
                     "CurrentPrice",
                     "change",
                     "profit")], qohTable, by.x = "Symbol", by.y = "Symbol")
  return(FinalTable)
}

# Get Latest Budget Fuction
getBudget <- function(log_url) {
  showLog <- fromJSON(log_url)
  stocksLog <- data.frame(showLog[3])
  BUDGET <- head(stocksLog$log.budget, 1)
  return(BUDGET)
}

# PORFTOLIO ALLOCATION
FIRST10 <-
  c("AMZN",
    "NFLX",
    "AAPL",
    "HD",
    "FB",
    "QCOM",
    "SBUX",
    "SQ",
    "ZM",
    "KO")
SECOND10 <-
  c("TSLA",
    "MSFT",
    "NVDA",
    "VOO",
    "NKE",
    "BA",
    "CRM",
    "GOOGL",
    "SHOP",
    "TGT")

# Alphavantanger API Key
av_api_key("Y7NKAR5J8U73V8TN")

# Log URL
log_url <-
  "https://projectrex.net/stocks/?key=3ba96214&av_key=Y7NKAR5J8U73V8TN&request=log"


TRANSACTION_MADE <- 0
#####################################
TRANS_LOG <-
  data.frame(
    status = integer(),
    description = factor(),
    symbol = factor(),
    price = integer(),
    quantity = integer(),
    budget = integer(),
    Predicted = integer()
  ) 
#####################################
while (TRANSACTION_MADE <= 295) {
  # GET TIME SERIES DATA | WILL HAPEN EVERY 30 MINS
  ALL_COMPANIES <- c(FIRST10, SECOND10)
  COMPARISON_POINTS <- getTS(ALL_COMPANIES, 60)
  
  
  FOR_60_MIN <- Sys.time() + 3600
  while (Sys.time() <= FOR_60_MIN) {
    ###### FIRST 10 ######
    # Get the log of past transactions
    descitionTable <- getDescitionTable(log_url, FIRST10)
    LATEST_BUDGET <- getBudget(log_url)
    
    
    ### THE PLAYING WITH FIRE PART WITH THE FIRST 10 ###
    print("~~~~~ TIME TO PLAY WITH FIRE WITH THE FIRST 10 COMPANIES ~~~~~")
    print(Sys.time())
    print("Now trading:")
    print(FIRST10)
    MAX_SPEND <- 1000
    
    toSell <- descitionTable[which.max(descitionTable$profit), ]
    toBuy <- descitionTable[which.min(descitionTable$profit), ]
    QTY_TO_BUY <- floor(MAX_SPEND / toBuy$CurrentPrice)
    QTY_TO_SELL <- floor(toSell$qoh / 2)
    COMPARE_TO <-
      COMPARISON_POINTS$PointForecast[COMPARISON_POINTS$Company == toBuy$Symbol]
    RISK <- 1 - (COMPARE_TO / toBuy$CurrentPrice)
    cat(
      "Attempting to Buy",
      QTY_TO_BUY,
      "shares of",
      toBuy$Symbol,
      "at",
      toBuy$CurrentPrice
    )
    if (LATEST_BUDGET > 20000) {
      if (RISK < 0.015) {
        t <- BUY(QTY_TO_BUY, toBuy$Symbol)
        t$Predicted <-
          COMPARISON_POINTS$PointForecast[COMPARISON_POINTS$Company == toBuy$Symbol]
        TRANS_LOG <- rbind(TRANS_LOG, t)
        print("Bought:")
        print(t)
        TRANSACTION_MADE <- TRANSACTION_MADE + 1
        print(TRANSACTION_MADE)
      } else{
        print("RISK MANAGEMENT: DESPLOMATION DETECTED. Buying Next")
        descitionTable <-
          descitionTable[descitionTable$Symbol != toBuy$Symbol,]
        toBuy <- descitionTable[which.min(descitionTable$profit), ]
        QTY_TO_BUY <- floor(MAX_SPEND / toBuy$CurrentPrice)
        cat(
          "Attempting to Buy",
          QTY_TO_BUY,
          "shares of",
          toBuy$Symbol,
          "at",
          toBuy$CurrentPrice
        )
        t <- BUY(QTY_TO_BUY, toBuy$Symbol)
        t$Predicted <-
          COMPARISON_POINTS$PointForecast[COMPARISON_POINTS$Company == toBuy$Symbol]
        TRANS_LOG <- rbind(TRANS_LOG, t)
        print("Bought:")
        print(t)
        TRANSACTION_MADE <- TRANSACTION_MADE + 1
        print(TRANSACTION_MADE)
      }
      
    } else {
      print("Budget not over $20,000, Selling the most profit")
      cat(
        "Attempting to sell",
        QTY_TO_SELL,
        "shares of",
        toSell$Symbol,
        "at",
        toSell$CurrentPrice
      )
      if (toSell$qoh > 1) {
        if (toSell$profit >= 0) {
          t <- SELL(QTY_TO_SELL, toSell$Symbol)
          t$Predicted <-
            COMPARISON_POINTS$PointForecast[COMPARISON_POINTS$Company == toSell$Symbol]
          TRANS_LOG <- rbind(TRANS_LOG, t)
          print("Sold:")
          print(t)
          TRANSACTION_MADE <- TRANSACTION_MADE + 1
          print(TRANSACTION_MADE)
          descitionTable <-
            descitionTable[descitionTable$Symbol != toSell$Symbol,]
        } else {
          print("Didn't Sell Anything: No Profit would be made")
        }
      } else{
        cat(
          "Didn't Sell Anything: Not Enough Stock, removing",
          toSell$Symbol,
          "from table and trying again"
        )
        descitionTable <-
          descitionTable[descitionTable$Symbol != toSell$Symbol,]
        toSell <- descitionTable[which.max(descitionTable$profit), ]
        QTY_TO_SELL <- floor(toSell$qoh / 2)
        cat(
          "Attempting to sell",
          QTY_TO_SELL,
          "shares of",
          toSell$Symbol,
          "at",
          toSell$CurrentPrice
        )
        if (toSell$qoh > 1) {
          if (toSell$profit >= 0) {
            t <- SELL(QTY_TO_SELL, toSell$Symbol)
            t$Predicted <-
              COMPARISON_POINTS$PointForecast[COMPARISON_POINTS$Company == toSell$Symbol]
            TRANS_LOG <- rbind(TRANS_LOG, t)
            print("Sold:")
            print(t)
            TRANSACTION_MADE <- TRANSACTION_MADE + 1
            print(TRANSACTION_MADE)
          } else {
            print("Didn't Sell Anything: No Profit would be made")
          }
        } else{
          print("Didn't Sell Anything: Not Enough Stock, Attempting the next with most profit")
          descitionTable <-
            descitionTable[descitionTable$Symbol != toSell$Symbol,]
          toSell <-
            descitionTable[which.max(descitionTable$profit), ]
          QTY_TO_SELL <- floor(toSell$qoh / 2)
          cat(
            "Attempting to sell",
            QTY_TO_SELL,
            "shares of",
            toSell$Symbol,
            "at",
            toSell$CurrentPrice
          )
          if (toSell$qoh > 1) {
            if (toSell$profit >= 0) {
              t <- SELL(QTY_TO_SELL, toSell$Symbol)
              t$Predicted <-
                COMPARISON_POINTS$PointForecast[COMPARISON_POINTS$Company == toSell$Symbol]
              TRANS_LOG <- rbind(TRANS_LOG, t)
              print("Sold:")
              print(t)
              TRANSACTION_MADE <- TRANSACTION_MADE + 1
              print(TRANSACTION_MADE)
            } else {
              print("Didn't Sell Anything: No Profit would be made")
            }
          } else{
            print(
              "Didn't Sell Anything: Not Enough Stock, Attempting the next with most profit"
            )
            descitionTable <-
              descitionTable[descitionTable$Symbol != toSell$Symbol,]
            toSell <-
              descitionTable[which.max(descitionTable$profit), ]
            QTY_TO_SELL <- floor(toSell$qoh / 2)
            cat(
              "Attempting to sell",
              QTY_TO_SELL,
              "shares of",
              toSell$Symbol,
              "at",
              toSell$CurrentPrice
            )
            if (toSell$qoh > 1) {
              if (toSell$profit >= 0) {
                t <- SELL(QTY_TO_SELL, toSell$Symbol)
                t$Predicted <-
                  COMPARISON_POINTS$PointForecast[COMPARISON_POINTS$Company == toSell$Symbol]
                TRANS_LOG <- rbind(TRANS_LOG, t)
                print("Sold:")
                print(t)
                TRANSACTION_MADE <- TRANSACTION_MADE + 1
                print(TRANSACTION_MADE)
              } else {
                print("Didn't Sell Anything: No Profit would be made")
              }
            } else{
              print(
                "Didn't Sell Anything: Not Enough Stock, Attempting the next with most profit"
              )
              descitionTable <-
                descitionTable[descitionTable$Symbol != toSell$Symbol,]
              toSell <-
                descitionTable[which.max(descitionTable$profit), ]
              QTY_TO_SELL <- floor(toSell$qoh / 2)
              cat(
                "Attempting to sell",
                QTY_TO_SELL,
                "shares of",
                toSell$Symbol,
                "at",
                toSell$CurrentPrice
              )
              if (toSell$qoh > 1) {
                if (toSell$profit >= 0) {
                  t <- SELL(QTY_TO_SELL, toSell$Symbol)
                  t$Predicted <-
                    COMPARISON_POINTS$PointForecast[COMPARISON_POINTS$Company == toSell$Symbol]
                  TRANS_LOG <- rbind(TRANS_LOG, t)
                  print("Sold:")
                  print(t)
                  TRANSACTION_MADE <- TRANSACTION_MADE + 1
                  print(TRANSACTION_MADE)
                } else {
                  print("Didn't Sell Anything: No Profit would be made")
                }
              } else{
                print(
                  "Didn't Sell Anything: Not Enough Stock, Attempting the next with most profit"
                )
                descitionTable <-
                  descitionTable[descitionTable$Symbol != toSell$Symbol,]
                toSell <-
                  descitionTable[which.max(descitionTable$profit), ]
                QTY_TO_SELL <- floor(toSell$qoh / 2)
                cat(
                  "Attempting to sell",
                  QTY_TO_SELL,
                  "shares of",
                  toSell$Symbol,
                  "at",
                  toSell$CurrentPrice
                )
                if (toSell$qoh > 1) {
                  if (toSell$profit >= 0) {
                    t <- SELL(QTY_TO_SELL, toSell$Symbol)
                    t$Predicted <-
                      COMPARISON_POINTS$PointForecast[COMPARISON_POINTS$Company == toSell$Symbol]
                    TRANS_LOG <- rbind(TRANS_LOG, t)
                    print("Sold:")
                    print(t)
                    TRANSACTION_MADE <- TRANSACTION_MADE + 1
                    print(TRANSACTION_MADE)
                  } else {
                    print("Didn't Sell Anything: No Profit would be made")
                  }
                } else{
                  print(
                    "Didn't Sell Anything: Not Enough Stock, Attempting the next with most profit"
                  )
                  descitionTable <-
                    descitionTable[descitionTable$Symbol != toSell$Symbol,]
                  toSell <-
                    descitionTable[which.max(descitionTable$profit), ]
                  QTY_TO_SELL <- floor(toSell$qoh / 2)
                  cat(
                    "Attempting to sell",
                    QTY_TO_SELL,
                    "shares of",
                    toSell$Symbol,
                    "at",
                    toSell$CurrentPrice
                  )
                  if (toSell$qoh > 1) {
                    if (toSell$profit >= 0) {
                      t <- SELL(QTY_TO_SELL, toSell$Symbol)
                      t$Predicted <-
                        COMPARISON_POINTS$PointForecast[COMPARISON_POINTS$Company == toSell$Symbol]
                      TRANS_LOG <- rbind(TRANS_LOG, t)
                      print("Sold:")
                      print(t)
                      TRANSACTION_MADE <- TRANSACTION_MADE + 1
                      print(TRANSACTION_MADE)
                    } else {
                      print("Didn't Sell Anything: No Profit would be made")
                    }
                  } else{
                    print(
                      "Didn't Sell Anything: Not Enough Stock, Attempting the next with most profit"
                    )
                    descitionTable <-
                      descitionTable[descitionTable$Symbol != toSell$Symbol,]
                    toSell <-
                      descitionTable[which.max(descitionTable$profit), ]
                    QTY_TO_SELL <- floor(toSell$qoh / 2)
                    cat(
                      "Attempting to sell",
                      QTY_TO_SELL,
                      "shares of",
                      toSell$Symbol,
                      "at",
                      toSell$CurrentPrice
                    )
                    if (toSell$qoh > 1) {
                      if (toSell$profit >= 0) {
                        t <- SELL(QTY_TO_SELL, toSell$Symbol)
                        t$Predicted <-
                          COMPARISON_POINTS$PointForecast[COMPARISON_POINTS$Company == toSell$Symbol]
                        TRANS_LOG <- rbind(TRANS_LOG, t)
                        print("Sold:")
                        print(t)
                        TRANSACTION_MADE <- TRANSACTION_MADE + 1
                        print(TRANSACTION_MADE)
                      } else {
                        print("Didn't Sell Anything: No Profit would be made")
                      }
                    } else{
                      print(
                        "Didn't Sell Anything: Not Enough Stock, Attempting the next with most profit"
                      )
                      descitionTable <-
                        descitionTable[descitionTable$Symbol != toSell$Symbol,]
                      toSell <-
                        descitionTable[which.max(descitionTable$profit), ]
                      QTY_TO_SELL <- floor(toSell$qoh / 2)
                      cat(
                        "Attempting to sell",
                        QTY_TO_SELL,
                        "shares of",
                        toSell$Symbol,
                        "at",
                        toSell$CurrentPrice
                      )
                      if (toSell$qoh > 1) {
                        if (toSell$profit >= 0) {
                          t <- SELL(QTY_TO_SELL, toSell$Symbol)
                          t$Predicted <-
                            COMPARISON_POINTS$PointForecast[COMPARISON_POINTS$Company == toSell$Symbol]
                          TRANS_LOG <- rbind(TRANS_LOG, t)
                          print("Sold:")
                          print(t)
                          TRANSACTION_MADE <- TRANSACTION_MADE + 1
                          print(TRANSACTION_MADE)
                          descitionTable <-
                            descitionTable[descitionTable$Symbol != toSell$Symbol,]
                        } else {
                          print("Didn't Sell Anything: No Profit would be made")
                        }
                      } else{
                        print("Didn't Sell Anything: Not Enough Stock, No more attempts")
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
    
    
    toSell <- descitionTable[which.max(descitionTable$profit), ]
    QTY_TO_SELL <- floor(toSell$qoh / 2)
    cat(
      "Attempting to sell",
      QTY_TO_SELL,
      "shares of",
      toSell$Symbol,
      "at",
      toSell$CurrentPrice
    )
    if (toSell$qoh > 1) {
      if (toSell$profit >= 0) {
        t <- SELL(QTY_TO_SELL, toSell$Symbol)
        t$Predicted <-
          COMPARISON_POINTS$PointForecast[COMPARISON_POINTS$Company == toSell$Symbol]
        TRANS_LOG <- rbind(TRANS_LOG, t)
        print("Sold:")
        print(t)
        TRANSACTION_MADE <- TRANSACTION_MADE + 1
        print(TRANSACTION_MADE)
      } else {
        print("Didn't Sell Anything: No Profit would be made")
      }
    } else{
      print("Didn't Sell Anything: Not Enough Stock, Attempting the next with most profit")
      descitionTable <-
        descitionTable[descitionTable$Symbol != toSell$Symbol,]
      toSell <- descitionTable[which.max(descitionTable$profit), ]
      QTY_TO_SELL <- floor(toSell$qoh / 2)
      cat(
        "Attempting to sell",
        QTY_TO_SELL,
        "shares of",
        toSell$Symbol,
        "at",
        toSell$CurrentPrice
      )
      if (toSell$qoh > 1) {
        if (toSell$profit >= 0) {
          t <- SELL(QTY_TO_SELL, toSell$Symbol)
          t$Predicted <-
            COMPARISON_POINTS$PointForecast[COMPARISON_POINTS$Company == toSell$Symbol]
          TRANS_LOG <- rbind(TRANS_LOG, t)
          print("Sold:")
          print(t)
          TRANSACTION_MADE <- TRANSACTION_MADE + 1
          print(TRANSACTION_MADE)
        } else {
          print("Didn't Sell Anything: No Profit would be made")
        }
      } else{
        print("Didn't Sell Anything: Not Enough Stock, Attempting the next with most profit")
        descitionTable <-
          descitionTable[descitionTable$Symbol != toSell$Symbol,]
        toSell <-
          descitionTable[which.max(descitionTable$profit), ]
        QTY_TO_SELL <- floor(toSell$qoh / 2)
        cat(
          "Attempting to sell",
          QTY_TO_SELL,
          "shares of",
          toSell$Symbol,
          "at",
          toSell$CurrentPrice
        )
        if (toSell$qoh > 1) {
          if (toSell$profit >= 0) {
            t <- SELL(QTY_TO_SELL, toSell$Symbol)
            t$Predicted <-
              COMPARISON_POINTS$PointForecast[COMPARISON_POINTS$Company == toSell$Symbol]
            TRANS_LOG <- rbind(TRANS_LOG, t)
            print("Sold:")
            print(t)
            TRANSACTION_MADE <- TRANSACTION_MADE + 1
            print(TRANSACTION_MADE)
          } else {
            print("Didn't Sell Anything: No Profit would be made")
          }
        } else{
          print("Didn't Sell Anything: Not Enough Stock, Attempting the next with most profit")
          descitionTable <-
            descitionTable[descitionTable$Symbol != toSell$Symbol,]
          toSell <-
            descitionTable[which.max(descitionTable$profit), ]
          QTY_TO_SELL <- floor(toSell$qoh / 2)
          cat(
            "Attempting to sell",
            QTY_TO_SELL,
            "shares of",
            toSell$Symbol,
            "at",
            toSell$CurrentPrice
          )
          if (toSell$qoh > 1) {
            if (toSell$profit >= 0) {
              t <- SELL(QTY_TO_SELL, toSell$Symbol)
              t$Predicted <-
                COMPARISON_POINTS$PointForecast[COMPARISON_POINTS$Company == toSell$Symbol]
              TRANS_LOG <- rbind(TRANS_LOG, t)
              print("Sold:")
              print(t)
              TRANSACTION_MADE <- TRANSACTION_MADE + 1
              print(TRANSACTION_MADE)
            } else {
              print("Didn't Sell Anything: No Profit would be made")
            }
          } else{
            print(
              "Didn't Sell Anything: Not Enough Stock, Attempting the next with most profit"
            )
            descitionTable <-
              descitionTable[descitionTable$Symbol != toSell$Symbol,]
            toSell <-
              descitionTable[which.max(descitionTable$profit), ]
            QTY_TO_SELL <- floor(toSell$qoh / 2)
            cat(
              "Attempting to sell",
              QTY_TO_SELL,
              "shares of",
              toSell$Symbol,
              "at",
              toSell$CurrentPrice
            )
            if (toSell$qoh > 1) {
              if (toSell$profit >= 0) {
                t <- SELL(QTY_TO_SELL, toSell$Symbol)
                t$Predicted <-
                  COMPARISON_POINTS$PointForecast[COMPARISON_POINTS$Company == toSell$Symbol]
                TRANS_LOG <- rbind(TRANS_LOG, t)
                print("Sold:")
                print(t)
                TRANSACTION_MADE <- TRANSACTION_MADE + 1
                print(TRANSACTION_MADE)
              } else {
                print("Didn't Sell Anything: No Profit would be made")
              }
            } else{
              print(
                "Didn't Sell Anything: Not Enough Stock, Attempting the next with most profit"
              )
              descitionTable <-
                descitionTable[descitionTable$Symbol != toSell$Symbol,]
              toSell <-
                descitionTable[which.max(descitionTable$profit), ]
              QTY_TO_SELL <- floor(toSell$qoh / 2)
              cat(
                "Attempting to sell",
                QTY_TO_SELL,
                "shares of",
                toSell$Symbol,
                "at",
                toSell$CurrentPrice
              )
              if (toSell$qoh > 1) {
                if (toSell$profit >= 0) {
                  t <- SELL(QTY_TO_SELL, toSell$Symbol)
                  t$Predicted <-
                    COMPARISON_POINTS$PointForecast[COMPARISON_POINTS$Company == toSell$Symbol]
                  TRANS_LOG <- rbind(TRANS_LOG, t)
                  print("Sold:")
                  print(t)
                  TRANSACTION_MADE <- TRANSACTION_MADE + 1
                  print(TRANSACTION_MADE)
                } else {
                  print("Didn't Sell Anything: No Profit would be made")
                }
              } else{
                print(
                  "Didn't Sell Anything: Not Enough Stock, Attempting the next with most profit"
                )
                descitionTable <-
                  descitionTable[descitionTable$Symbol != toSell$Symbol,]
                toSell <-
                  descitionTable[which.max(descitionTable$profit), ]
                QTY_TO_SELL <- floor(toSell$qoh / 2)
                cat(
                  "Attempting to sell",
                  QTY_TO_SELL,
                  "shares of",
                  toSell$Symbol,
                  "at",
                  toSell$CurrentPrice
                )
                if (toSell$qoh > 1) {
                  if (toSell$profit >= 0) {
                    t <- SELL(QTY_TO_SELL, toSell$Symbol)
                    t$Predicted <-
                      COMPARISON_POINTS$PointForecast[COMPARISON_POINTS$Company == toSell$Symbol]
                    TRANS_LOG <- rbind(TRANS_LOG, t)
                    print("Sold:")
                    print(t)
                    TRANSACTION_MADE <- TRANSACTION_MADE + 1
                    print(TRANSACTION_MADE)
                  } else {
                    print("Didn't Sell Anything: No Profit would be made")
                  }
                } else{
                  print(
                    "Didn't Sell Anything: Not Enough Stock, Attempting the next with most profit"
                  )
                  descitionTable <-
                    descitionTable[descitionTable$Symbol != toSell$Symbol,]
                  toSell <-
                    descitionTable[which.max(descitionTable$profit), ]
                  QTY_TO_SELL <- floor(toSell$qoh / 2)
                  cat(
                    "Attempting to sell",
                    QTY_TO_SELL,
                    "shares of",
                    toSell$Symbol,
                    "at",
                    toSell$CurrentPrice
                  )
                  if (toSell$qoh > 1) {
                    if (toSell$profit >= 0) {
                      t <- SELL(QTY_TO_SELL, toSell$Symbol)
                      t$Predicted <-
                        COMPARISON_POINTS$PointForecast[COMPARISON_POINTS$Company == toSell$Symbol]
                      TRANS_LOG <- rbind(TRANS_LOG, t)
                      print("Sold:")
                      print(t)
                      TRANSACTION_MADE <- TRANSACTION_MADE + 1
                      print(TRANSACTION_MADE)
                    } else {
                      print("Didn't Sell Anything: No Profit would be made")
                    }
                  } else{
                    print(
                      "Didn't Sell Anything: Not Enough Stock, Attempting the next with most profit"
                    )
                    descitionTable <-
                      descitionTable[descitionTable$Symbol != toSell$Symbol,]
                    toSell <-
                      descitionTable[which.max(descitionTable$profit), ]
                    QTY_TO_SELL <- floor(toSell$qoh / 2)
                    cat(
                      "Attempting to sell",
                      QTY_TO_SELL,
                      "shares of",
                      toSell$Symbol,
                      "at",
                      toSell$CurrentPrice
                    )
                    if (toSell$qoh > 1) {
                      if (toSell$profit >= 0) {
                        t <- SELL(QTY_TO_SELL, toSell$Symbol)
                        t$Predicted <-
                          COMPARISON_POINTS$PointForecast[COMPARISON_POINTS$Company == toSell$Symbol]
                        TRANS_LOG <- rbind(TRANS_LOG, t)
                        print("Sold:")
                        print(t)
                        TRANSACTION_MADE <- TRANSACTION_MADE + 1
                        print(TRANSACTION_MADE)
                      } else {
                        print("Didn't Sell Anything: No Profit would be made")
                      }
                    } else{
                      print("Didn't Sell Anything: Not Enough Stock, No more attempts")
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
    
    ###### SECOND 10 ######
    # Get the log of past transactions
    descitionTable <- getDescitionTable(log_url, SECOND10)
    LATEST_BUDGET <- getBudget(log_url)
    
    
    ### THE PLAYING WITH FIRE PART WITH THE SECOND 10 ###
    print("~~~~~ TIME TO PLAY WITH FIRE WITH THE SECOND 10 COMPANIES ~~~~~")
    print(Sys.time())
    print("Now trading:")
    print(SECOND10)
    MAX_SPEND <- 1000
    
    toSell <- descitionTable[which.max(descitionTable$profit), ]
    toBuy <- descitionTable[which.min(descitionTable$profit), ]
    QTY_TO_BUY <- floor(MAX_SPEND / toBuy$CurrentPrice)
    QTY_TO_SELL <- floor(toSell$qoh / 2)
    COMPARE_TO <-
      COMPARISON_POINTS$PointForecast[COMPARISON_POINTS$Company == toBuy$Symbol]
    RISK <- 1 - (COMPARE_TO / toBuy$CurrentPrice)
    cat(
      "Attempting to Buy",
      QTY_TO_BUY,
      "shares of",
      toBuy$Symbol,
      "at",
      toBuy$CurrentPrice
    )
    if (LATEST_BUDGET > 20000) {
      if (RISK < 0.015) {
        t <- BUY(QTY_TO_BUY, toBuy$Symbol)
        t$Predicted <-
          COMPARISON_POINTS$PointForecast[COMPARISON_POINTS$Company == toBuy$Symbol]
        TRANS_LOG <- rbind(TRANS_LOG, t)
        print("Bought:")
        print(t)
        TRANSACTION_MADE <- TRANSACTION_MADE + 1
        print(TRANSACTION_MADE)
      } else{
        print("RISK MANAGEMENT: DESPLOMATION DETECTED. Buying Next")
        descitionTable <-
          descitionTable[descitionTable$Symbol != toBuy$Symbol,]
        toBuy <- descitionTable[which.min(descitionTable$profit), ]
        QTY_TO_BUY <- floor(MAX_SPEND / toBuy$CurrentPrice)
        cat(
          "Attempting to Buy",
          QTY_TO_BUY,
          "shares of",
          toBuy$Symbol,
          "at",
          toBuy$CurrentPrice
        )
        t <- BUY(QTY_TO_BUY, toBuy$Symbol)
        t$Predicted <-
          COMPARISON_POINTS$PointForecast[COMPARISON_POINTS$Company == toBuy$Symbol]
        TRANS_LOG <- rbind(TRANS_LOG, t)
        print("Bought:")
        print(t)
        TRANSACTION_MADE <- TRANSACTION_MADE + 1
        print(TRANSACTION_MADE)
      }
      
    } else {
      print("Budget not over $20,000, Selling the most profit")
      cat(
        "Attempting to sell",
        QTY_TO_SELL,
        "shares of",
        toSell$Symbol,
        "at",
        toSell$CurrentPrice
      )
      if (toSell$qoh > 1) {
        if (toSell$profit >= 0) {
          t <- SELL(QTY_TO_SELL, toSell$Symbol)
          t$Predicted <-
            COMPARISON_POINTS$PointForecast[COMPARISON_POINTS$Company == toSell$Symbol]
          TRANS_LOG <- rbind(TRANS_LOG, t)
          print("Sold:")
          print(t)
          TRANSACTION_MADE <- TRANSACTION_MADE + 1
          print(TRANSACTION_MADE)
          descitionTable <-
            descitionTable[descitionTable$Symbol != toSell$Symbol,]
        } else {
          print("Didn't Sell Anything: No Profit would be made")
        }
      } else{
        cat(
          "Didn't Sell Anything: Not Enough Stock, removing",
          toSell$Symbol,
          "from table and trying again"
        )
        descitionTable <-
          descitionTable[descitionTable$Symbol != toSell$Symbol,]
        toSell <- descitionTable[which.max(descitionTable$profit), ]
        QTY_TO_SELL <- floor(toSell$qoh / 2)
        cat(
          "Attempting to sell",
          QTY_TO_SELL,
          "shares of",
          toSell$Symbol,
          "at",
          toSell$CurrentPrice
        )
        if (toSell$qoh > 1) {
          if (toSell$profit >= 0) {
            t <- SELL(QTY_TO_SELL, toSell$Symbol)
            t$Predicted <-
              COMPARISON_POINTS$PointForecast[COMPARISON_POINTS$Company == toSell$Symbol]
            TRANS_LOG <- rbind(TRANS_LOG, t)
            print("Sold:")
            print(t)
            TRANSACTION_MADE <- TRANSACTION_MADE + 1
            print(TRANSACTION_MADE)
          } else {
            print("Didn't Sell Anything: No Profit would be made")
          }
        } else{
          print("Didn't Sell Anything: Not Enough Stock, Attempting the next with most profit")
          descitionTable <-
            descitionTable[descitionTable$Symbol != toSell$Symbol,]
          toSell <-
            descitionTable[which.max(descitionTable$profit), ]
          QTY_TO_SELL <- floor(toSell$qoh / 2)
          cat(
            "Attempting to sell",
            QTY_TO_SELL,
            "shares of",
            toSell$Symbol,
            "at",
            toSell$CurrentPrice
          )
          if (toSell$qoh > 1) {
            if (toSell$profit >= 0) {
              t <- SELL(QTY_TO_SELL, toSell$Symbol)
              t$Predicted <-
                COMPARISON_POINTS$PointForecast[COMPARISON_POINTS$Company == toSell$Symbol]
              TRANS_LOG <- rbind(TRANS_LOG, t)
              print("Sold:")
              print(t)
              TRANSACTION_MADE <- TRANSACTION_MADE + 1
              print(TRANSACTION_MADE)
            } else {
              print("Didn't Sell Anything: No Profit would be made")
            }
          } else{
            print(
              "Didn't Sell Anything: Not Enough Stock, Attempting the next with most profit"
            )
            descitionTable <-
              descitionTable[descitionTable$Symbol != toSell$Symbol,]
            toSell <-
              descitionTable[which.max(descitionTable$profit), ]
            QTY_TO_SELL <- floor(toSell$qoh / 2)
            cat(
              "Attempting to sell",
              QTY_TO_SELL,
              "shares of",
              toSell$Symbol,
              "at",
              toSell$CurrentPrice
            )
            if (toSell$qoh > 1) {
              if (toSell$profit >= 0) {
                t <- SELL(QTY_TO_SELL, toSell$Symbol)
                t$Predicted <-
                  COMPARISON_POINTS$PointForecast[COMPARISON_POINTS$Company == toSell$Symbol]
                TRANS_LOG <- rbind(TRANS_LOG, t)
                print("Sold:")
                print(t)
                TRANSACTION_MADE <- TRANSACTION_MADE + 1
                print(TRANSACTION_MADE)
              } else {
                print("Didn't Sell Anything: No Profit would be made")
              }
            } else{
              print(
                "Didn't Sell Anything: Not Enough Stock, Attempting the next with most profit"
              )
              descitionTable <-
                descitionTable[descitionTable$Symbol != toSell$Symbol,]
              toSell <-
                descitionTable[which.max(descitionTable$profit), ]
              QTY_TO_SELL <- floor(toSell$qoh / 2)
              cat(
                "Attempting to sell",
                QTY_TO_SELL,
                "shares of",
                toSell$Symbol,
                "at",
                toSell$CurrentPrice
              )
              if (toSell$qoh > 1) {
                if (toSell$profit >= 0) {
                  t <- SELL(QTY_TO_SELL, toSell$Symbol)
                  t$Predicted <-
                    COMPARISON_POINTS$PointForecast[COMPARISON_POINTS$Company == toSell$Symbol]
                  TRANS_LOG <- rbind(TRANS_LOG, t)
                  print("Sold:")
                  print(t)
                  TRANSACTION_MADE <- TRANSACTION_MADE + 1
                  print(TRANSACTION_MADE)
                } else {
                  print("Didn't Sell Anything: No Profit would be made")
                }
              } else{
                print(
                  "Didn't Sell Anything: Not Enough Stock, Attempting the next with most profit"
                )
                descitionTable <-
                  descitionTable[descitionTable$Symbol != toSell$Symbol,]
                toSell <-
                  descitionTable[which.max(descitionTable$profit), ]
                QTY_TO_SELL <- floor(toSell$qoh / 2)
                cat(
                  "Attempting to sell",
                  QTY_TO_SELL,
                  "shares of",
                  toSell$Symbol,
                  "at",
                  toSell$CurrentPrice
                )
                if (toSell$qoh > 1) {
                  if (toSell$profit >= 0) {
                    t <- SELL(QTY_TO_SELL, toSell$Symbol)
                    t$Predicted <-
                      COMPARISON_POINTS$PointForecast[COMPARISON_POINTS$Company == toSell$Symbol]
                    TRANS_LOG <- rbind(TRANS_LOG, t)
                    print("Sold:")
                    print(t)
                    TRANSACTION_MADE <- TRANSACTION_MADE + 1
                    print(TRANSACTION_MADE)
                  } else {
                    print("Didn't Sell Anything: No Profit would be made")
                  }
                } else{
                  print(
                    "Didn't Sell Anything: Not Enough Stock, Attempting the next with most profit"
                  )
                  descitionTable <-
                    descitionTable[descitionTable$Symbol != toSell$Symbol,]
                  toSell <-
                    descitionTable[which.max(descitionTable$profit), ]
                  QTY_TO_SELL <- floor(toSell$qoh / 2)
                  cat(
                    "Attempting to sell",
                    QTY_TO_SELL,
                    "shares of",
                    toSell$Symbol,
                    "at",
                    toSell$CurrentPrice
                  )
                  if (toSell$qoh > 1) {
                    if (toSell$profit >= 0) {
                      t <- SELL(QTY_TO_SELL, toSell$Symbol)
                      t$Predicted <-
                        COMPARISON_POINTS$PointForecast[COMPARISON_POINTS$Company == toSell$Symbol]
                      TRANS_LOG <- rbind(TRANS_LOG, t)
                      print("Sold:")
                      print(t)
                      TRANSACTION_MADE <- TRANSACTION_MADE + 1
                      print(TRANSACTION_MADE)
                    } else {
                      print("Didn't Sell Anything: No Profit would be made")
                    }
                  } else{
                    print(
                      "Didn't Sell Anything: Not Enough Stock, Attempting the next with most profit"
                    )
                    descitionTable <-
                      descitionTable[descitionTable$Symbol != toSell$Symbol,]
                    toSell <-
                      descitionTable[which.max(descitionTable$profit), ]
                    QTY_TO_SELL <- floor(toSell$qoh / 2)
                    cat(
                      "Attempting to sell",
                      QTY_TO_SELL,
                      "shares of",
                      toSell$Symbol,
                      "at",
                      toSell$CurrentPrice
                    )
                    if (toSell$qoh > 1) {
                      if (toSell$profit >= 0) {
                        t <- SELL(QTY_TO_SELL, toSell$Symbol)
                        t$Predicted <-
                          COMPARISON_POINTS$PointForecast[COMPARISON_POINTS$Company == toSell$Symbol]
                        TRANS_LOG <- rbind(TRANS_LOG, t)
                        print("Sold:")
                        print(t)
                        TRANSACTION_MADE <- TRANSACTION_MADE + 1
                        print(TRANSACTION_MADE)
                      } else {
                        print("Didn't Sell Anything: No Profit would be made")
                      }
                    } else{
                      print(
                        "Didn't Sell Anything: Not Enough Stock, Attempting the next with most profit"
                      )
                      descitionTable <-
                        descitionTable[descitionTable$Symbol != toSell$Symbol,]
                      toSell <-
                        descitionTable[which.max(descitionTable$profit), ]
                      QTY_TO_SELL <- floor(toSell$qoh / 2)
                      cat(
                        "Attempting to sell",
                        QTY_TO_SELL,
                        "shares of",
                        toSell$Symbol,
                        "at",
                        toSell$CurrentPrice
                      )
                      if (toSell$qoh > 1) {
                        if (toSell$profit >= 0) {
                          t <- SELL(QTY_TO_SELL, toSell$Symbol)
                          t$Predicted <-
                            COMPARISON_POINTS$PointForecast[COMPARISON_POINTS$Company == toSell$Symbol]
                          TRANS_LOG <- rbind(TRANS_LOG, t)
                          print("Sold:")
                          print(t)
                          TRANSACTION_MADE <- TRANSACTION_MADE + 1
                          print(TRANSACTION_MADE)
                        } else {
                          print("Didn't Sell Anything: No Profit would be made")
                        }
                      } else{
                        print(
                          "Didn't Sell Anything: Not Enough Stock, Attempting the next with most profit"
                        )
                        descitionTable <-
                          descitionTable[descitionTable$Symbol != toSell$Symbol,]
                        toSell <-
                          descitionTable[which.max(descitionTable$profit), ]
                        QTY_TO_SELL <- floor(toSell$qoh / 2)
                        cat(
                          "Attempting to sell",
                          QTY_TO_SELL,
                          "shares of",
                          toSell$Symbol,
                          "at",
                          toSell$CurrentPrice
                        )
                        if (toSell$qoh > 1) {
                          if (toSell$profit >= 0) {
                            t <- SELL(QTY_TO_SELL, toSell$Symbol)
                            t$Predicted <-
                              COMPARISON_POINTS$PointForecast[COMPARISON_POINTS$Company == toSell$Symbol]
                            TRANS_LOG <- rbind(TRANS_LOG, t)
                            print("Sold:")
                            print(t)
                            TRANSACTION_MADE <- TRANSACTION_MADE + 1
                            print(TRANSACTION_MADE)
                            descitionTable <-
                              descitionTable[descitionTable$Symbol != toSell$Symbol,]
                          } else {
                            print("Didn't Sell Anything: No Profit would be made")
                          }
                        } else{
                          print("Didn't Sell Anything: Not Enough Stock, No more attempts")
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
    
    
    toSell <- descitionTable[which.max(descitionTable$profit), ]
    QTY_TO_SELL <- floor(toSell$qoh / 2)
    cat(
      "Attempting to sell",
      QTY_TO_SELL,
      "shares of",
      toSell$Symbol,
      "at",
      toSell$CurrentPrice
    )
    if (toSell$qoh > 1) {
      if (toSell$profit >= 0) {
        t <- SELL(QTY_TO_SELL, toSell$Symbol)
        t$Predicted <-
          COMPARISON_POINTS$PointForecast[COMPARISON_POINTS$Company == toSell$Symbol]
        TRANS_LOG <- rbind(TRANS_LOG, t)
        print("Sold:")
        print(t)
        TRANSACTION_MADE <- TRANSACTION_MADE + 1
        print(TRANSACTION_MADE)
      } else {
        print("Didn't Sell Anything: No Profit would be made")
      }
    } else{
      print("Didn't Sell Anything: Not Enough Stock, Attempting the next with most profit")
      descitionTable <-
        descitionTable[descitionTable$Symbol != toSell$Symbol,]
      toSell <- descitionTable[which.max(descitionTable$profit), ]
      QTY_TO_SELL <- floor(toSell$qoh / 2)
      cat(
        "Attempting to sell",
        QTY_TO_SELL,
        "shares of",
        toSell$Symbol,
        "at",
        toSell$CurrentPrice
      )
      if (toSell$qoh > 1) {
        if (toSell$profit >= 0) {
          t <- SELL(QTY_TO_SELL, toSell$Symbol)
          t$Predicted <-
            COMPARISON_POINTS$PointForecast[COMPARISON_POINTS$Company == toSell$Symbol]
          TRANS_LOG <- rbind(TRANS_LOG, t)
          print("Sold:")
          print(t)
          TRANSACTION_MADE <- TRANSACTION_MADE + 1
          print(TRANSACTION_MADE)
        } else {
          print("Didn't Sell Anything: No Profit would be made")
        }
      } else{
        print("Didn't Sell Anything: Not Enough Stock, Attempting the next with most profit")
        descitionTable <-
          descitionTable[descitionTable$Symbol != toSell$Symbol,]
        toSell <-
          descitionTable[which.max(descitionTable$profit), ]
        QTY_TO_SELL <- floor(toSell$qoh / 2)
        cat(
          "Attempting to sell",
          QTY_TO_SELL,
          "shares of",
          toSell$Symbol,
          "at",
          toSell$CurrentPrice
        )
        if (toSell$qoh > 1) {
          if (toSell$profit >= 0) {
            t <- SELL(QTY_TO_SELL, toSell$Symbol)
            t$Predicted <-
              COMPARISON_POINTS$PointForecast[COMPARISON_POINTS$Company == toSell$Symbol]
            TRANS_LOG <- rbind(TRANS_LOG, t)
            print("Sold:")
            print(t)
            TRANSACTION_MADE <- TRANSACTION_MADE + 1
            print(TRANSACTION_MADE)
          } else {
            print("Didn't Sell Anything: No Profit would be made")
          }
        } else{
          print("Didn't Sell Anything: Not Enough Stock, Attempting the next with most profit")
          descitionTable <-
            descitionTable[descitionTable$Symbol != toSell$Symbol,]
          toSell <-
            descitionTable[which.max(descitionTable$profit), ]
          QTY_TO_SELL <- floor(toSell$qoh / 2)
          cat(
            "Attempting to sell",
            QTY_TO_SELL,
            "shares of",
            toSell$Symbol,
            "at",
            toSell$CurrentPrice
          )
          if (toSell$qoh > 1) {
            if (toSell$profit >= 0) {
              t <- SELL(QTY_TO_SELL, toSell$Symbol)
              t$Predicted <-
                COMPARISON_POINTS$PointForecast[COMPARISON_POINTS$Company == toSell$Symbol]
              TRANS_LOG <- rbind(TRANS_LOG, t)
              print("Sold:")
              print(t)
              TRANSACTION_MADE <- TRANSACTION_MADE + 1
              print(TRANSACTION_MADE)
            } else {
              print("Didn't Sell Anything: No Profit would be made")
            }
          } else{
            print(
              "Didn't Sell Anything: Not Enough Stock, Attempting the next with most profit"
            )
            descitionTable <-
              descitionTable[descitionTable$Symbol != toSell$Symbol,]
            toSell <-
              descitionTable[which.max(descitionTable$profit), ]
            QTY_TO_SELL <- floor(toSell$qoh / 2)
            cat(
              "Attempting to sell",
              QTY_TO_SELL,
              "shares of",
              toSell$Symbol,
              "at",
              toSell$CurrentPrice
            )
            if (toSell$qoh > 1) {
              if (toSell$profit >= 0) {
                t <- SELL(QTY_TO_SELL, toSell$Symbol)
                t$Predicted <-
                  COMPARISON_POINTS$PointForecast[COMPARISON_POINTS$Company == toSell$Symbol]
                TRANS_LOG <- rbind(TRANS_LOG, t)
                print("Sold:")
                print(t)
                TRANSACTION_MADE <- TRANSACTION_MADE + 1
                print(TRANSACTION_MADE)
              } else {
                print("Didn't Sell Anything: No Profit would be made")
              }
            } else{
              print(
                "Didn't Sell Anything: Not Enough Stock, Attempting the next with most profit"
              )
              descitionTable <-
                descitionTable[descitionTable$Symbol != toSell$Symbol,]
              toSell <-
                descitionTable[which.max(descitionTable$profit), ]
              QTY_TO_SELL <- floor(toSell$qoh / 2)
              cat(
                "Attempting to sell",
                QTY_TO_SELL,
                "shares of",
                toSell$Symbol,
                "at",
                toSell$CurrentPrice
              )
              if (toSell$qoh > 1) {
                if (toSell$profit >= 0) {
                  t <- SELL(QTY_TO_SELL, toSell$Symbol)
                  t$Predicted <-
                    COMPARISON_POINTS$PointForecast[COMPARISON_POINTS$Company == toSell$Symbol]
                  TRANS_LOG <- rbind(TRANS_LOG, t)
                  print("Sold:")
                  print(t)
                  TRANSACTION_MADE <- TRANSACTION_MADE + 1
                  print(TRANSACTION_MADE)
                } else {
                  print("Didn't Sell Anything: No Profit would be made")
                }
              } else{
                print(
                  "Didn't Sell Anything: Not Enough Stock, Attempting the next with most profit"
                )
                descitionTable <-
                  descitionTable[descitionTable$Symbol != toSell$Symbol,]
                toSell <-
                  descitionTable[which.max(descitionTable$profit), ]
                QTY_TO_SELL <- floor(toSell$qoh / 2)
                cat(
                  "Attempting to sell",
                  QTY_TO_SELL,
                  "shares of",
                  toSell$Symbol,
                  "at",
                  toSell$CurrentPrice
                )
                if (toSell$qoh > 1) {
                  if (toSell$profit >= 0) {
                    t <- SELL(QTY_TO_SELL, toSell$Symbol)
                    t$Predicted <-
                      COMPARISON_POINTS$PointForecast[COMPARISON_POINTS$Company == toSell$Symbol]
                    TRANS_LOG <- rbind(TRANS_LOG, t)
                    print("Sold:")
                    print(t)
                    TRANSACTION_MADE <- TRANSACTION_MADE + 1
                    print(TRANSACTION_MADE)
                  } else {
                    print("Didn't Sell Anything: No Profit would be made")
                  }
                } else{
                  print(
                    "Didn't Sell Anything: Not Enough Stock, Attempting the next with most profit"
                  )
                  descitionTable <-
                    descitionTable[descitionTable$Symbol != toSell$Symbol,]
                  toSell <-
                    descitionTable[which.max(descitionTable$profit), ]
                  QTY_TO_SELL <- floor(toSell$qoh / 2)
                  cat(
                    "Attempting to sell",
                    QTY_TO_SELL,
                    "shares of",
                    toSell$Symbol,
                    "at",
                    toSell$CurrentPrice
                  )
                  if (toSell$qoh > 1) {
                    if (toSell$profit >= 0) {
                      t <- SELL(QTY_TO_SELL, toSell$Symbol)
                      t$Predicted <-
                        COMPARISON_POINTS$PointForecast[COMPARISON_POINTS$Company == toSell$Symbol]
                      TRANS_LOG <- rbind(TRANS_LOG, t)
                      print("Sold:")
                      print(t)
                      TRANSACTION_MADE <- TRANSACTION_MADE + 1
                      print(TRANSACTION_MADE)
                    } else {
                      print("Didn't Sell Anything: No Profit would be made")
                    }
                  } else{
                    print(
                      "Didn't Sell Anything: Not Enough Stock, Attempting the next with most profit"
                    )
                    descitionTable <-
                      descitionTable[descitionTable$Symbol != toSell$Symbol,]
                    toSell <-
                      descitionTable[which.max(descitionTable$profit), ]
                    QTY_TO_SELL <- floor(toSell$qoh / 2)
                    cat(
                      "Attempting to sell",
                      QTY_TO_SELL,
                      "shares of",
                      toSell$Symbol,
                      "at",
                      toSell$CurrentPrice
                    )
                    if (toSell$qoh > 1) {
                      if (toSell$profit >= 0) {
                        t <- SELL(QTY_TO_SELL, toSell$Symbol)
                        t$Predicted <-
                          COMPARISON_POINTS$PointForecast[COMPARISON_POINTS$Company == toSell$Symbol]
                        TRANS_LOG <- rbind(TRANS_LOG, t)
                        print("Sold:")
                        print(t)
                        TRANSACTION_MADE <- TRANSACTION_MADE + 1
                        print(TRANSACTION_MADE)
                      } else {
                        print("Didn't Sell Anything: No Profit would be made")
                      }
                    } else{
                      print(
                        "Didn't Sell Anything: Not Enough Stock, Attempting the next with most profit"
                      )
                      descitionTable <-
                        descitionTable[descitionTable$Symbol != toSell$Symbol,]
                      toSell <-
                        descitionTable[which.max(descitionTable$profit), ]
                      QTY_TO_SELL <- floor(toSell$qoh / 2)
                      cat(
                        "Attempting to sell",
                        QTY_TO_SELL,
                        "shares of",
                        toSell$Symbol,
                        "at",
                        toSell$CurrentPrice
                      )
                      if (toSell$qoh > 1) {
                        if (toSell$profit >= 0) {
                          t <- SELL(QTY_TO_SELL, toSell$Symbol)
                          t$Predicted <-
                            COMPARISON_POINTS$PointForecast[COMPARISON_POINTS$Company == toSell$Symbol]
                          TRANS_LOG <- rbind(TRANS_LOG, t)
                          print("Sold:")
                          print(t)
                          TRANSACTION_MADE <- TRANSACTION_MADE + 1
                          print(TRANSACTION_MADE)
                        } else {
                          print("Didn't Sell Anything: No Profit would be made")
                        }
                      } else{
                        print(
                          "Didn't Sell Anything: Not Enough Stock, Attempting the next with most profit"
                        )
                        descitionTable <-
                          descitionTable[descitionTable$Symbol != toSell$Symbol,]
                        toSell <-
                          descitionTable[which.max(descitionTable$profit), ]
                        QTY_TO_SELL <- floor(toSell$qoh / 2)
                        cat(
                          "Attempting to sell",
                          QTY_TO_SELL,
                          "shares of",
                          toSell$Symbol,
                          "at",
                          toSell$CurrentPrice
                        )
                        if (toSell$qoh > 1) {
                          if (toSell$profit >= 0) {
                            t <- SELL(QTY_TO_SELL, toSell$Symbol)
                            print("Sold:")
                            print(t)
                            TRANSACTION_MADE <- TRANSACTION_MADE + 1
                            print(TRANSACTION_MADE)
                          } else {
                            print("Didn't Sell Anything: No Profit would be made")
                          }
                        } else{
                          print("Didn't Sell Anything: Not Enough Stock, No more attempts")
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
    
    
    
  }
}

write.csv(TRANS_LOG, file = "TRANS_LOG.csv")