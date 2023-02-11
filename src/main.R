library(dplyr)
library(purrr)
library(forecast)
library(ggplot2)
library(cowplot)
library(ggrepel)
library(tidyr)

Sys.setlocale("LC_TIME", "en_US.UTF-8")
source("lib.R")
source("load_data.R")

HISTORY_PERIODS=25

boolC <- c("TRUE"="#035e3c", "FALSE"="#a50000")
boolScale <- scale_colour_manual(name="boolean", values=boolC)
currencies<-raw_types$Currencies[!is.na(raw_types$Currencies)]
currColor <- setNames(raw_types$`Currencies Colors`, currencies)
currColor<-currColor[!is.na(currColor)]
currSecColor <- setNames(raw_types$`Currencies Second Colors`, currencies)
currSecColor<-currSecColor[!is.na(currSecColor)]
currScale <- scale_colour_manual(name="currency", values=currColor)
holdingTypes<-raw_types$`Holdings Types`
holdingTypes<-holdingTypes[!is.na(holdingTypes)]
holdScale<- setNames(raw_types$`Holdings Types Colors`, raw_types$`Holdings Types`)
holdScale<-holdScale[!is.na(holdScale)]
Places<-raw_types$Places
Places<-Places[!is.na(Places)]
placesScale<- setNames(raw_types$`Places Colors`, raw_types$Places)
placesScale<-placesScale[!is.na(placesScale)]

LAST_DATE<-last(uah_rates$time)
HOLDINGS<-colnames(raw_holdings)[-1]
FLOWS<-colnames(raw_flows)[-1]

#FLOWS
uah_flw<-extract_uah(raw_flows,uah_rates,1, c(1,2))
flows_dat<-extract_native(raw_flows, c(1,2))
currency_summary<-currencySummarize(raw_flows, 1, c(1,2))
currency_summary$hold_diff<-0
uah_flows<-uah_flw
flw_x_ace<-factor(row.names(uah_flw), levels = row.names(uah_flw))

uah_flw$Income<-apply(t(uah_flw), 2, function(x) sum(x[x > 0], na.rm = TRUE))
uah_flw$Expenses<-apply(t(uah_flw), 2, function(x) -sum(x[x < 0], na.rm = TRUE))
uah_flw$Profit<-uah_flw$Income-uah_flw$Expenses
uah_flw$gross_margin = uah_flw$Profit/uah_flw$Income
uah_flw$margin_label <-paste0(formatC(uah_flw$gross_margin*100, digits = 1, format = "f", drop0trailing=T),'%')
uah_flw$margin_diff <-c(NaN, diff(uah_flw$gross_margin,1)*100)
uah_flw$margin_diff_label <- ifelse(is.nan(uah_flw$margin_diff), "", paste0(formatC(uah_flw$margin_diff, digits = 1, format = "f", drop0trailing=T,flag='+'),'%'))
uah_flw$Income_label <- ifelse(uah_flw$Income, paste0(formatC(uah_flw$Income/1000, digits = 1, format = "f", drop0trailing=T),'k'), NA)
uah_flw$Profit_label <- ifelse(uah_flw$Profit, paste0(formatC(uah_flw$Profit/1000, digits = 1, format = "f", drop0trailing=T),'k'), NA)
uah_flw$expences_label <- ifelse(uah_flw$Expenses>0, paste0(formatC(-uah_flw$Expenses/1000, digits = 1, format = "f", drop0trailing=T),'k'), NA)

shareDf<-as.data.frame(t(tail(uah_flows,1)))
colnames(shareDf)<-c("value")
shareDf$name<-rownames(shareDf)
exp_sum<-sum(shareDf$value[shareDf$value<0],na.rm = T)
shareDf$expRate<-ifelse(shareDf$value<0,shareDf$value/exp_sum,NA)
shareDf$expType<-ifelse(shareDf$expRate>0.05, shareDf$name, "other")
inc_sum<-sum(shareDf$value[shareDf$value>0],na.rm = T)
shareDf$incRate<-ifelse(shareDf$value>0,shareDf$value/inc_sum,NA)
shareDf$incType<-ifelse(shareDf$incRate>0.05, shareDf$name, "other")

#HOLDINGS

hold_uah<-extract_uah(raw_holdings,uah_rates,1, c(1,2,3))
holdings<-extract_native(raw_holdings, c(1,2,3))
hold_meta<-as.data.frame(raw_holdings[c(1,2,3),-c(1)])
hold_total<-as.data.frame(matrix(nrow = nrow(hold_uah), ncol = 0))
placeConsolidation = data.frame(matrix(NA, nrow=nrow(hold_total), ncol = 0))
placeConsolidation[,Places]<-0

hold_total[, "abs"]<-0
hold_total[, "abs_diff"]<-0

for(currency in currencies){
  abs_colname=paste0("abs_diff_", currency)
  rel_colname=paste0("rel_diff_", currency)
  uah_colname=paste0("uah_", currency)
  cols<-as.vector(which(apply(hold_meta, 2, function(x) as.character(x[1])==currency)))
  hold_total[, currency]<-apply(as.data.frame(holdings[,cols]), 1, function(x){sum(unlist(x), na.rm = T)})
  hold_total[, abs_colname]<-c(NA, diff(hold_total[, currency],1))
  
  currency_summary[currency, 'hold_diff'] = as.numeric(tail(hold_total[abs_colname], n=1))
  currency_summary[currency, 'hold_abs'] = as.numeric(tail(hold_total[currency], n=1))
  
  hold_total[, rel_colname]<-c(NA, ifelse(hold_total[-1,currency]==0, 0, hold_total[-c(1), abs_colname]/hold_total[-1,currency]))
  rates<-uah_rates[, currency]
  hold_total[, uah_colname]<-hold_total[, currency]*rates
  currency_summary[currency, 'hold_abs_uah'] = as.numeric(tail(hold_total[uah_colname], n=1))
  
  hold_total[, "abs"]<-hold_total[, "abs"]+hold_total[, currency]*rates
  hold_total[, "abs_diff"]<-hold_total[, "abs_diff"]+hold_total[, abs_colname]*rates
  
  for(place in Places){
    cols<-as.vector(which(apply(hold_meta, 2, function(x) as.character(x[3])==place && as.character(x[1])==currency)))
    if(length(cols)!=0){
      currency_colname=paste0(place,'_',currency)
      currency_abs_diff_colname=paste0(currency_colname,'_abs_diff')
      hold_total[, currency_colname]<-apply(as.data.frame(holdings[,cols]), 1, function(x){sum(unlist(x), na.rm = T)})
      hold_total[, currency_abs_diff_colname]<-c(NA, diff(hold_total[, currency_colname],1))
      placeConsolidation[, place] <- placeConsolidation[, place] + hold_total[, currency_colname] * rates
    }
  }
}

hold_total$flows_profit<-c(NA,uah_flw$Profit)
hold_total$abs_error<-abs(hold_total$abs_diff-hold_total$flows_profit)
hold_total$rel_error<-hold_total$abs_error/pmax(abs(hold_total$abs_diff), abs(hold_total$flows_profit))

row.names(hold_total) <- row.names(holdings)
row.names(placeConsolidation)<-rownames(hold_total)
hold_total$Names<-factor(row.names(hold_total), levels = row.names(hold_total))

hold_uah$total<-apply(t(hold_uah), 2, function(x) sum(x[x > 0], na.rm = TRUE))
hold_uah$abs_diff<-c(NA, diff(hold_uah$total,1))

for(ht in holdingTypes){
  cols<-as.vector(which(apply(hold_meta, 2, function(x) as.character(x[2])==ht)))
  hold_total[, ht]<-apply(hold_uah[,cols,drop=F], 1, function(x){sum(x, na.rm = T)})
}

# Investments
investments<-as.data.frame(t(head(raw_flows,2)))
colnames(investments)<-as.character(unlist(investments[1,]))
investments<-tail(investments, -1)
investments<-investments %>% filter(Source!='NULL')

uah_flw$Consumed<-apply(t(uah_flows %>% select(-c('charity', rownames(investments)))), 2, function(x) -sum(x[x < 0], na.rm = TRUE))

investments_uah<-as.data.frame(tail(hold_total["abs"],n=-1))
colnames(investments_uah)<-c("Capital")
investments_uah$Income<-uah_flw[,"Income"]
investments_uah$Expenses<-uah_flw[,"Expenses"]
investments_uah$Investments<-0
investments_uah$Return<-0
investments_uah$IncomeSources<-0
investments_uah$ProfitableHoldings<-0

for(investmentType in rownames(investments)){
  holdName=as.character(investments[investmentType, "Source"])
  invholding<-tail(hold_uah[,holdName], n=-1)
  invholding[is.na(invholding)] <- 0
  investments_uah$Investments<-investments_uah$Investments+invholding
  invflow<-uah_flw[,investmentType]
  invflow[is.na(invflow)] <- 0
  investments_uah$Return<-investments_uah$Return+invflow
}
for(investmentType in rownames(investments)){
  holdName=as.character(investments[investmentType, "Source"])
  invholding<-tail(hold_uah[,holdName], n=-1)
  invholding[is.na(invholding)] <- 0
  holdShare<-ifelse(invholding/abs(investments_uah$Investments)>0.01, 1, 0)
  investments_uah$ProfitableHoldings<-investments_uah$ProfitableHoldings+holdShare
  invflow<-uah_flw[,investmentType]
  invflow[is.na(invflow)] <- 0
  flowShare<-ifelse(invflow/abs(investments_uah$Return)>0.01, 1, 0)
  investments_uah$IncomeSources<-investments_uah$IncomeSources+flowShare
}

investments_uah$InvestmentsRate<-investments_uah$Investments/investments_uah$Capital
investments_uah$ReturnRate<-12*investments_uah$Return/investments_uah$Investments
investments_uah$IncomeRate<-investments_uah$Return/investments_uah$Income
investments_uah$ExpensesRate<-investments_uah$Return/investments_uah$Expenses

# APPENDIX
STORED_YEARS<-unique(format(as.POSIXct(unlist(tail(raw_holdings$Name, n=-3)), origin='1970-01-01'), '%Y'))

holdings_history<-holdings
flows_history<-flows_dat
investments_history<-investments_uah

uah_flows<-tail(uah_flows, n=HISTORY_PERIODS-1)
uah_flw<-tail(uah_flw, n=HISTORY_PERIODS-1)
flw_x_ace <-tail(flw_x_ace, n=HISTORY_PERIODS-1)
flows_dat<-tail(flows_dat, n=HISTORY_PERIODS-1)

uah_rates<-tail(uah_rates, n=HISTORY_PERIODS)
placeConsolidation<-tail(placeConsolidation, n=HISTORY_PERIODS)
investments_uah<-tail(investments_uah, n=HISTORY_PERIODS)
holdings<-tail(holdings, n=HISTORY_PERIODS)
hold_uah<-tail(hold_uah, n=HISTORY_PERIODS)
hold_total<-tail(hold_total, n=HISTORY_PERIODS)
