# portfolio optimisation in R - youtube

# https://youtu.be/u3wsio61ZU8

install.packages('PortfolioAnalytics')
install.packages("ROI")
library(ROI)
"ROI" %in% search()

install.packages("openxlsx")
library(quantmod)
library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(openxlsx)

tickers = c("RELIANCE.NS", "HDFCBANK.NS", "ITC.NS", "INFY.NS", "IFBIND.NS", 
            "HINDUNILVR.NS", "TATAPOWER.NS", "BIOCON.NS", "BHARTIARTL.NS",
            "INDIGO.NS", "ALKYLAMINE.NS", "HEROMOTOCO.NS", "HDFCLIFE.NS",
            "ADANIGREEN.NS","BOMDYEING.NS")

since = "2023-01-01"

portfolioprices = NULL
for(ticker in tickers){
  portfolioprices = cbind(portfolioprices, 
                        getSymbols.yahoo(ticker, from = since, periodicity = 'daily',auto.assign=F)[,6])
}

portfolioret = na.omit(ROC(portfolioprices))
Excelfile <- "C:/Users/user/Downloads/Jordan_forecasts_2024-04-24.xlsx"
PredictedPortfolioRet <- read.xlsx(Excelfile)

num_rows <- nrow(PredictedPortfolioRet)

# Assign company names to each row
PredictedPortfolioRet <- PredictedPortfolioRet[, -1]
PredictedPortfolioRet <- data.frame(PredictedPortfolioRet)
rownames(PredictedPortfolioRet) <- tickers
colnames(PredictedPortfolioRet) <- "2024-04-24"
PredictedPortfolioRet <- t(PredictedPortfolioRet)


funds = colnames(PredictedPortfolioRet)
funds

# optimisation - random
init.portf = portfolio.spec(assets = funds)
init.portf = add.constraint(portfolio =  init.portf, type = "full_investment")
init.portf = add.constraint(portfolio =  init.portf, type = "long_only")
init.portf = add.objective(portfolio =  init.portf, type = "return", name = "mean")
init.portf

init.portf$constraints[[1]]$min_sum = 0.99
init.portf$constraints[[1]]$max_sum = 1.01

init.portf = add.constraint(portfolio =  init.portf, type = "risk", name = 'StdDev', multiplier = 0)

port1 = add.constraint(portfolio = init.portf, type = 'diversification', min=0, max=1, indexnum = 2)
port1 = add.constraint(portfolio = init.portf, type = 'risk', name = 'StdDev')

maxSRport.rp = optimize.portfolio(R = PredictedPortfolioRet, portfolio = port1, optimize_method = 'random', search_size = 2000, maxSR = T, trace = T)

maxSR.weight.rp = extractWeights(maxSRport.rp)
maxSR.weight.rp
maxSR.ret.rp = Return.portfolio(PredictedPortfolioRet, weights = maxSR.weight.rp, wealth.index = F, contribution = F, geometric = T, rebalance_on = c(NA),value = 1, verbose = F)

colnames(maxSR.ret.rp) = c('Random Portf Return')

# optimisation - ROI
init.portf2 = portfolio.spec(assets = funds)
init.portf2 = add.constraint(portfolio =  init.portf2, type = "full_investment")
init.portf2 = add.constraint(portfolio =  init.portf2, type = "long_only")
init.portf2 = add.objective(portfolio =  init.portf2, type = "return", name = "mean")
init.portf2

init.portf2$constraints[[1]]$min_sum = 1
init.portf2$constraints[[1]]$max_sum = 1

init.portf2 = add.constraint(portfolio =  init.portf2, type = "risk", name = 'StdDev', multiplier = 0)
port2 = add.constraint(portfolio = init.portf2, type = 'diversification', min=0, max=1, indexnum = 2)
port2 = add.constraint(portfolio = init.portf2, type = 'risk', name = 'StdDev')

maxSRport.roi = optimize.portfolio(R = portfolioret , portfolio = port2, optimize_method = 'ROI', search_size = 20000, maxSR = T, trace = T)
maxSRport.roi

maxSR.weight.roi = extractWeights(maxSRport.roi)
maxSR.ret.roi = Return.portfolio(portfolioret, weights = maxSR.weight.rp, wealth.index = F, contribution = F, geometric = T, rebalance_on = c(NA),value = 1, verbose = F)
colnames(maxSR.ret.roi) = c('ROI Portf Return')

# benchmark
nifty50price = getSymbols.yahoo('^NSEI', from = since, periodicity = 'daily', auto.assign=F)[,6]
nifty50ret = na.omit(ROC(nifty50price))
colnames(nifty50ret) = 'Nifty50 Returns'

SR.roi = table.AnnualizedReturns(maxSR.ret.roi, scale = NA, Rf = 0, geometric = T, digits = 4)
SR.rp = table.AnnualizedReturns(maxSR.ret.rp, scale = NA, Rf = 0, geometric = T, digits = 4)
SR.nifty = table.AnnualizedReturns(nifty50ret, scale = NA, Rf = 0, geometric = T, digits = 4)
SR.table = cbind(SR.roi, SR.rp, SR.nifty)
SR.table
# similar sharpe ratio

ret.df = na.omit(cbind(maxSR.ret.roi, maxSR.ret.rp, nifty50ret))
charts.PerformanceSummary(ret.df, plot.engine = 'plotly', main='Profit Loss over time')
# top part is cumulative return

weights = sort(maxSR.weight.rp, decreasing = T)
weights


Symbols <- c("RELIANCE.NS", "HDFCBANK.NS", "ITC.NS", "INFY.NS", "IFBIND.NS", 
                            "HINDUNILVR.NS", "TATAPOWER.NS", "BIOCON.NS", "BHARTIARTL.NS",
                            "INDIGO.NS", "ALKYLAMINE.NS", "HEROMOTOCO.NS", "HDFCLIFE.NS",
                            "ADANIGREEN.NS","BOMDYEING.NS") 

Symbols
