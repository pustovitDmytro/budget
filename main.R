library(dplyr)
library(purrr)
library(forecast)
library(ggplot2)
library(cowplot)
library(ggrepel)
Sys.setlocale("LC_TIME", "en_US.UTF-8")
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
source("lib.R")

boolC <- c("TRUE"="#035e3c", "FALSE"="#a50000")
boolScale <- scale_colour_manual(name="boolean", values=boolC)
currColor <- c("UAH"="#695c3a", "USD"="#727a55", "EUR"="#73a9bf")
currSecColor <- c("UAH"="#a64f48", "USD"="#ddd9b4", "EUR"="#d1dccf")
currScale <- scale_colour_manual(name="currency", values=currColor)
currencies<-c("UAH", "USD", "EUR")
holdingTypes<-c("Cash", "Current Account", "Savings Account", "Domestic Bonds", "Short Deposit", "Long Deposit", "Shares", "Bank Metals")
holdScale<- c("Cash"="#ff6500", "Current Account"="#db3a34", "Savings Account"="#084c61", "Domestic Bonds"="#177e89", "Short Deposit"="#C26DBC", "Long Deposit"="#708b75", "Shares"="#4c956c", "Bank Metals"="#bc4749")

source("load_data.R") # load raw_flows, raw_holdings, uah_rates

LAST_DATE<-last(uah_rates$time)
HOLDINGS<-colnames(raw_holdings)[-1]
FLOWS<-colnames(raw_flows)[-1]

#FLOWS
uah_flw<-extract_uah(raw_flows,uah_rates,1, c(1,2))
flows_dat<-extract_native(raw_flows, c(1,2))
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

hold_uah<-extract_uah(raw_holdings,uah_rates,1, c(1,2))
holdings<-extract_native(raw_holdings, c(1,2))
hold_meta<-as.data.frame(raw_holdings[c(1,2),-c(1)])
hold_total<-as.data.frame(matrix(nrow = nrow(hold_uah), ncol = 0))

for(currency in currencies){
  abs_colname=paste0("abs_diff_", currency)
  rel_colname=paste0("rel_diff_", currency)
  uah_colname=paste0("uah_", currency)
  cols<-as.vector(which(apply(hold_meta, 2, function(x) as.character(x[1])==currency)))
  hold_total[, currency]<-apply(holdings[,cols], 1, function(x){sum(unlist(x), na.rm = T)})
  hold_total[, abs_colname]<-c(NA, diff(hold_total[, currency],1))
  hold_total[, rel_colname]<-c(NA, hold_total[-c(1), abs_colname]/hold_total[-1,currency])
  hold_total[, uah_colname]<-hold_total[, currency]*uah_rates[, currency]
}
hold_total[, "abs"]<-hold_total$UAH+hold_total$USD*uah_rates[,"USD"]+hold_total$EUR*uah_rates[,"EUR"]
hold_total[, "abs_diff"]<-hold_total$abs_diff_UAH+hold_total$abs_diff_USD*uah_rates[,"USD"]+hold_total$abs_diff_EUR*uah_rates[,"EUR"]
hold_total$flows_profit<-c(NA,uah_flw$Profit)
hold_total$abs_error<-abs(hold_total$abs_diff-hold_total$flows_profit)
hold_total$rel_error<-hold_total$abs_error/pmax(abs(hold_total$abs_diff), abs(hold_total$flows_profit))


row.names(hold_total) <- row.names(holdings)
hold_total$Names<-factor(row.names(hold_total), levels = row.names(hold_total))

hold_uah$total<-apply(t(hold_uah), 2, function(x) sum(x[x > 0], na.rm = TRUE))
hold_uah$abs_diff<-c(NA, diff(hold_uah$total,1))

for(ht in holdingTypes){
  cols<-as.vector(which(apply(hold_meta, 2, function(x) as.character(x[2])==ht)))
  hold_total[, ht]<-apply(hold_uah[,cols,drop=F], 1, function(x){sum(x, na.rm = T)})
}

