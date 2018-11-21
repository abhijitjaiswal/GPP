gold_price_dates <- timeBasedSeq('1979-01-02::2018-10-19')
gold <- read.csv("/Users/abhijitj/Downloads/GoldPrices.csv")
#gold$Indian.rupee <- gold$Indian.rupee * 0.0311035
prices <- c()
for(i in seq(1, length(gold_price_dates))){
               res = match(gold_price_dates[i], 
                           as.Date(as.character(gold$Name), format = "%m/%d/%Y"))
               if(is.na(res)){
                 prices <- append(prices, tail(prices, 1))
               }
               else{
                 prices <- append(prices, gold$Indian.rupee[[res]])
               }
}

gold_final <- data.frame(gold_price_dates, prices)
gold_final$prices <- gold_final$prices * 0.0311035
write.csv(gold_final, "/Users/abhijitj/gold/final_price_list.csv")
gold_final_ts <- ts(gold_final$prices, start = 1979, end=2018, frequency = 365)
plot.ts(gold_final_ts)
#log Transformation for Seasonal and random fluctuations
gold_final_ts_log <- log(gold_final_ts)
plot.ts(gold_final_ts_log)

#Decomposing TS
library(TTR)

#smoothing
#SMA
gold_final_ts_sma3 <- SMA(gold_final_ts, 3)
plot.ts(gold_final_ts_sma3)
gold_final_ts_sma150 <- SMA(gold_final_ts, 150)
plot.ts(gold_final_ts_sma150)
gold_final_ts_sma250 <- SMA(gold_final_ts, 250)
plot.ts(gold_final_ts_sma250)

#Exponential moving average
gold_final_ts_ema3 <- EMA(gold_final_ts, 3, ratio = 0.25)
plot.ts(gold_final_ts_ema3)
gold_final_ts_ema150 <- EMA(gold_final_ts, 150, ratio = 0.25)
plot.ts(gold_final_ts_ema150)
gold_final_ts_ema150_5 <- EMA(gold_final_ts, 150, ratio = 0.75)
plot.ts(gold_final_ts_ema150_5)


gold_final_ts_decomp <- decompose(gold_final_ts)
plot(gold_final_ts_decomp)
gold_final_ts_decomp

gold_final_ts_hw <- HoltWinters(gold_final_ts, beta = F, gamma = F)
gold_final_ts_hw
gold_final_ts_hw$fitted

forecast(gold_final_ts_hw)
library(forecast)

#not working hence commented
#forecast::forecast.HoltWinters(gold_final_ts_hw, h=3)