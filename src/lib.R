flowAbsDynamicsPlot <- function(data, column){
  raw<-data[,column]
  period<-nrow(data)
  
  spline_int <- as.data.frame(spline(1:period, raw, n = 10*period))
  prediction<-data.frame(x=1:length(raw),y=raw)
  tryCatch({
    smoo <- with(prediction[!is.na(prediction$y),],smooth.spline(x,y))
    prediction <- predict(smoo, period:(period+2))
  }, error=function(e){warning("ERROR :",conditionMessage(e), "\n")})
  
  p<-ggplot(data=NULL, aes(x=flw_x_ace, y=raw, group = 1)) + 
    geom_point(size=2, aes(colour=raw>0)) +
    geom_line(data = spline_int, aes(x = x, y = y), size=.2, alpha=0.5, linetype = "dotted") + 
    geom_line(aes(x = flw_x_ace, y = raw, colour=raw>0), size=.5) + 
    geom_text(label=asKLabel(raw, ""), vjust=2, size=2.5)+ boolScale +
    theme(plot.title = element_text(hjust = 0.5), legend.position = "none",axis.title.x=element_blank(),axis.title.y=element_blank()) 
    if(exists("smoo")){
      p<-p+geom_line(data = as.data.frame(prediction), aes(x = x, y = y), linetype = "dashed")+stat_smooth(method = glm)
    }
    p
}

expencesDynamicsPlot <- function(){
  smooth_periods = 4
  ma<-TTR::WMA(uah_flw$Expenses, n=smooth_periods, wts=(1:smooth_periods)^2)
  bollinger_bands<-as.data.frame(TTR::BBands(uah_flw$Expenses,n=smooth_periods, sd=1))
  bollinger_bands$dn<-ifelse(bollinger_bands$dn>0,bollinger_bands$dn,0)
  final_ticks = c(last(bollinger_bands$dn), last(bollinger_bands$up), last(ma))
  
  ggplot(uah_flw, aes(y=Expenses, x=flw_x_ace, group = 1)) +
    geom_ribbon(aes(ymin=bollinger_bands$dn,ymax=bollinger_bands$up), fill="#FEE3EC", alpha=0.5, color = "#F9C5D5")+
    geom_line(aes(y=ma), color = "#F999B7") +
    geom_bar(stat = "identity", color="#9A0680", fill="#79018C", alpha=0.1, width=0.75)+
    geom_text(aes(label=asKLabel(Expenses)), vjust=2, size=2.2, color="#9A0680")+
    theme(axis.title.x = element_blank(), axis.title.y = element_blank(),  legend.position = "none", axis.text.y.right = element_text(color = "#F2789F"))+
    scale_y_continuous(labels=asKLabel, sec.axis = sec_axis(~., breaks = final_ticks, labels = asKLabel(final_ticks)))
}

incomeDynamicsPlot <- function(){
  smooth_periods = 4
  ma<-TTR::WMA(uah_flw$Income, n=smooth_periods, wts=(1:smooth_periods)^2)
  bollinger_bands<-as.data.frame(TTR::BBands(uah_flw$Income,n=smooth_periods, sd=1))
  bollinger_bands$dn<-ifelse(bollinger_bands$dn>0,bollinger_bands$dn,0)
  final_ticks = c(last(bollinger_bands$dn), last(bollinger_bands$up), last(ma))
  
  ggplot(uah_flw, aes(y=Income, x=flw_x_ace, group = 1)) +
    geom_ribbon(aes(ymin=bollinger_bands$dn,ymax=bollinger_bands$up), fill="#B1D0E0", alpha=0.25, color = "#406882")+
    geom_line(aes(y=ma), color = "#1A374D") +
    geom_bar(stat = "identity", color="#064635", fill="#519259", alpha=0.1, width=0.75)+
    geom_text(aes(label=asKLabel(Income)), vjust=2, size=2.2, color="#064635")+
    theme(axis.title.x = element_blank(), axis.title.y = element_blank(),  legend.position = "none", axis.text.y.right = element_text(color = "#406882"))+
    scale_y_continuous(labels=asKLabel, sec.axis = sec_axis(~., breaks = final_ticks, labels = asKLabel(final_ticks)))
}

flowsTotalShares <- function (df){
  inc_palete<-c('#1fab89','#62d2a2', '#d7fbe8')
  exp_palete<-c('#ff4646', '#ff8585', '#fff5c0')
  df$inc_position<-match(1:nrow(df), order(df$incRate,decreasing=T))
  df$exp_position<-match(1:nrow(df), order(df$expRate,decreasing=T))
  
  df$color=ifelse(df$value>0, map(df$inc_position,~inc_palete[.x]),map(df$exp_position,~exp_palete[.x]))
  colors <- alpha(as.character(df$color), 0.9)
  names(colors) <- df$name
  colors <- c(colors, other=alpha("#7d7d7d", 0.2))
  
  ggplot() + 
    geom_bar(data=df %>% filter(df$expRate>0), aes(x=2.5, y=expRate, fill=reorder(expType, expRate)), position="fill", stat = "identity") +
    geom_bar(data=df %>% filter(df$incRate>0), aes(x=4, y=incRate, fill=reorder(incType, incRate)), position="fill", stat = "identity") +
    coord_polar(theta="y") + xlim(c(1, 5)) + scale_fill_manual(values = colors) + labs(fill=NULL) +
    theme(axis.text=element_blank(), axis.ticks=element_blank(),  axis.title = element_blank(), legend.position=c(0.5,0.1), legend.direction='horizontal', legend.background = element_rect(fill="transparent"), legend.title = element_blank(), panel.background = element_blank())
}

currencySharePlot<-function(){
    labels <- asPercentLabel(currency_summary$hold_abs_uah/sum(currency_summary$hold_abs_uah), dropZero = T)
    names <- as.factor(rownames(currency_summary))
    avg <- mean(currency_summary$hold_abs_uah)/2
    ggplot(currency_summary) + 
    geom_bar(aes(x=1, y=hold_abs_uah, fill=names, colour=names), position="fill", stat = "identity")+
    geom_text(aes(y=hold_abs_uah+avg, label=labels, x=1.2, colour=names), vjust=2, size=5, position="fill", stat = "identity") +
    coord_polar(theta="y") + labs(fill=NULL) +
    scale_fill_manual("currency", values=currSecColor) +
    scale_color_manual("currency", values=currColor) +
    theme(axis.text=element_blank(), axis.ticks=element_blank(),  axis.title = element_blank(), legend.text=element_text(size=15), legend.position=c(1,0.5), legend.direction='vertical', legend.background = element_rect(fill="transparent"), legend.title = element_blank(), panel.background = element_blank())
}

flowsTypeShare<-function(dat, expenses=F){
  df<-dat
  color = ifelse(expenses, boolC["FALSE"], boolC["TRUE"])
  df$rate<-if(expenses) df$expRate else  df$incRate
  df$type<-if(expenses) df$expType else df$incType
  
  main<-ggplot(data=df %>% filter(rate>0), aes(x=reorder(gsub(" ", "\n",name), rate), y=rate)) + geom_bar( stat = "identity", colour = color, fill=alpha(color, 0.5), width=0.5) +
    geom_text(aes(label=asPercentLabel(rate), hjust=ifelse(rate>0.5, 1.2, -0.5)), vjust=-0.75, size=2.75, colour = color)+
    geom_text(aes(label=asKLabel(value), hjust=ifelse(rate>0.5, 1.2, -0.5)), vjust=1.5, size=2.75, colour = color)+
    coord_flip() + theme(axis.title.x = element_blank(), axis.title.y = element_blank()) + 
    scale_y_continuous(labels=function(x) paste0(x*100,'%'))

  other<-ggplot() + geom_bar(data=df %>% filter(df$type=='other'), aes(x=reorder(gsub(" ", "\n",name),rate), y=rate), stat = "identity", colour = color, fill=alpha(color, 0.5), width=0.5) + 
    coord_flip() + theme(axis.title.x = element_blank(), axis.title.y = element_blank()) + 
    scale_y_continuous(labels=function(x) paste0(x*100,'%')) +
    theme(plot.background = element_rect(fill = "transparent", color = NA))
  
  ggdraw() +
    draw_plot(main) +
    draw_plot(other, x = .5, y = 0.1, width = .45, height = .5)
}

flowsDistributionPlot<-function(uah_flw, expenses=T){
  df<-uah_flw %>% gather(type, value, all_of(FLOWS))
  sign=ifelse(expenses, -1, 1)
  filtered<-df %>% filter(value*sign>0)
  ylim = quantile(filtered$value, c(0.05, 0.95))
  ggplot(data=filtered, aes(x=type, y=value, fill=type, colour=type)) +
    geom_boxplot(outlier.shape = NA, alpha=0.25) +
    geom_jitter(size=0.4) +
    scale_y_continuous(labels=asKLabel) +
    coord_flip(ylim = ylim*1.5)+
    theme(plot.title = element_text(hjust = 0.5), legend.position = "none",axis.title.x=element_blank(),axis.title.y=element_blank()) 
}

flowRelDynamicsPlot <- function(data, column){
  value<-data[,column]
  total<-ifelse(value<0, data$Expenses, data$Income)
  share<-value/total
  period<-nrow(data)-1
  labels<-paste0(formatC(ifelse(share>0, 1, -1)*share*100, digits = 1, format = "f", drop0trailing=T),'%')

  ggplot(data=NULL, aes(x=flw_x_ace, y=share, group = 1)) + 
    geom_point(size=3, aes(colour=value>0)) +
    geom_segment(data=NULL, aes(x=flw_x_ace, xend=flw_x_ace, y=ifelse(share>0, max(0, min(share, na.rm = T)), min(0, max(share, na.rm = T))), yend=share, colour=value>0)) +
    geom_text(label=labels, vjust=ifelse(share>0, -1.5, 2), size=3)+
    boolScale +
    scale_y_continuous(labels=asPercentLabel) +
    theme(plot.title = element_text(hjust = 0.5), legend.position = "none",axis.title.x=element_blank(),axis.title.y=element_blank()) 
}

marginDynamicsPlot <- function(uah){
  ggplot(uah, aes(x=flw_x_ace, y=gross_margin)) + 
    geom_bar(stat = "identity", color="#000080", fill="#00abfa", width=0.75) + 
    geom_text(aes(label=margin_label, vjust=ifelse(gross_margin>0,-0.5,1.5)), colour ="#000080", check_overlap = TRUE) + 
    geom_text(aes(label=margin_diff_label, colour = margin_diff>0, vjust=ifelse(gross_margin>0,2,-2)), size=3) + boolScale +
    theme(axis.title.x = element_blank(), axis.title.y = element_blank(),  legend.position = "none")+
    scale_y_continuous(labels=asPercentLabel)
}

flowsTotalsPlot <- function(uah){
  totals = data.frame(
    label=c(paste0(last(row.names(uah)),":\n"), 'Av:', 'Tot:'),
    margin=c(last(uah$gross_margin), mean(uah$gross_margin, na.rm=T), sum(uah$Profit)/sum(uah$Income)),
    profit=c(last(uah$Profit),mean(uah$Profit[uah$Profit!=0], na.rm=T), sum(uah$Profit[uah$Profit!=0], na.rm=T)),
    fraction=c(1,0.5,0.5/length(uah$Profit[uah$Profit!=0]))
  )
  
  totals$fraction<-totals$profit*totals$fraction/sum(totals$profit*totals$fraction)
  totals$x_max <- 10+5*cumsum(totals$fraction)
  totals$x_min <- c(10, head(totals$x_max, n=-1))
  totals$label <- paste0(totals$label," ",formatC(totals$profit/1000, digits = 1, format = "f", drop0trailing=T),'k', ' / ',formatC(totals$margin*100, digits = 1, format = "f", drop0trailing=T), '%' )
  
  g1 = ggplot()
  for (row in 1:nrow(totals)) {
    label<-paste0('"',totals[row, "label"],'"')
    g1 = g1 +
      geom_rect(aes_string(
        ymax=c(totals[row, "margin"],1),
        ymin=c(0,totals[row, "margin"]),
        xmax=totals[row, "x_max"]-0.1,
        xmin=totals[row, "x_min"]+0.1, 
        colour=c(T,F), 
        fill=c(T,F)
      )) + 
      geom_text(
        x=totals[row, "x_max"], 
        aes_string(y=0.01, label=label), 
        size=3, 
        vjust=1.5, 
        hjust=0, 
        angle=0.65*pi/4 * 180/pi,
        color="white"
      )
  }
  
  main.plot <- g1 + ylim(0,1/0.75) + xlim(c(5, 15)) +
    coord_polar(theta="y", start=0-0.25*2*pi/2) +
    annotate("text", label = paste0("Profit: ", last(uah$Profit_label)), x=Inf,y=-Inf,hjust=0.2,vjust=6,size = 8, colour = "#000080")+
    annotate("text", label = paste0("Income: ", last(uah$Income_label)), x=Inf,y=-Inf,hjust=0.2,vjust=13, size = 4.5, colour = boolC["TRUE"])+
    annotate("text", label = paste0("Expenses: ", last(uah$expences_label)), x=Inf,y=-Inf,hjust=0.2,vjust=14.5, size = 4.5, colour = boolC["FALSE"])+
    annotate("text", label = paste0("Margin: ", last(uah$margin_label)), x=Inf,y=-Inf, hjust=0.2,vjust=16,size = 4.5, colour = "#000080")+
    scale_fill_manual(values = alpha(boolC,0.7)) +theme_void()+ theme(legend.position = "none") 
  
  inset.plot <- ggplot(tail(uah,5), aes(x=tail(flw_x_ace,5), y=Profit)) + geom_bar(stat = "identity", color="#000080", fill=alpha("#00abfa",0.5), width=0.75) +theme_void()
  
  ggdraw() +
    draw_plot(main.plot) +
    draw_plot(inset.plot, x = 0.46, y = .44, width = .1, height = .15)
}

flowsAbsPlot <- function(uah){
  ggplot(uah, aes(x=flw_x_ace, group = 1)) + 
    geom_ribbon(aes(ymin=0, ymax=pmax(Income, 0)), fill=boolC["TRUE"], col=boolC["TRUE"], alpha=0.2)+
    geom_ribbon(aes(ymin=0, ymax=pmax(Expenses, 0)), fill=boolC["FALSE"], col=boolC["FALSE"], alpha=0.2) +
    geom_text(aes(y=Income, label=Income_label), colour=boolC["TRUE"], vjust=-0.5, hjust=-0.1, check_overlap = TRUE, size=3.5) +
    geom_text(aes(y=Expenses, label=expences_label), colour=boolC["FALSE"], vjust=-0.5, hjust=-0.1, check_overlap = TRUE, size=3.5) +
    geom_label(aes(y=Profit, label=Profit_label), colour="#000080", vjust=1.2, hjust=0.5, size=3.5) +
    geom_bar(stat = "identity", aes(y=Profit), colour=alpha("#000080",0.3), fill=alpha("#00abfa",0.1), linetype = "dashed") +
    scale_y_continuous(labels=asKLabel) +
    theme(axis.title.x = element_blank(), axis.title.y = element_blank()) 
}

errorSourceColors<-c('Holdings' = alpha("#808000", 0.85),'Flows' = alpha("#000080", 0.85))

profitsPlot<-function(hold_total){
  ggplot(hold_total, aes(x=Names))+
  geom_line(aes(y=abs_diff, colour='Holdings'), size=.5, group = 1)+
  geom_text(aes(label=asKLabel(abs_diff), y=abs_diff, colour='Holdings'),hjust=-.2, size=3, check_overlap = T) +
  geom_line(aes(x=hold_total$Names, y=hold_total$flows_profit, colour='Flows'), size=.5, group = 1) +
  geom_text(aes(label=ifelse(abs_error>2000, asKLabel(flows_profit), NA), y=flows_profit, colour='Flows'),hjust=-.2, vjust=1.2, size=3, check_overlap = T) +
  scale_y_continuous(labels=asKLabel) +
  scale_colour_manual(name = '', guide = 'legend',values = errorSourceColors)+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position=c(1,1),  legend.justification=c(1,1), legend.direction='horizontal', legend.background = element_rect(fill="transparent")) 
}

profitErrorAbs<-function(hold_total){
  ggplot(hold_total, aes(x=Names,y=abs_error))+
  geom_bar(stat = "identity", colour=alpha(boolC["FALSE"],0.8), fill=alpha(boolC["FALSE"], 0.3))+
  geom_text(aes(label=asKLabel(abs_error), vjust=-1), colour=boolC["FALSE"], size=3) +
  scale_y_continuous(labels=asKLabel) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())
}

currencyErrors<-function(){
  ggplot(currency_summary, aes(x=rownames(currency_summary)))+
    geom_bar(aes(y=hold_diff*rate), stat = "identity", colour=alpha(errorSourceColors["Holdings"],0.8), fill=alpha(errorSourceColors["Holdings"], 0.3))+
    geom_text(aes(y=hold_diff*rate, label=asKLabel(hold_diff), colour='Holdings'), vjust=1.5, hjust=-0.2, size=3) +
    geom_bar(aes(y=sum*rate), stat = "identity", colour=alpha(errorSourceColors["Flows"],0.8), fill=alpha(errorSourceColors["Flows"], 0.3))+
    geom_text(aes(y=sum*rate, label=asKLabel(sum), colour='Flows'), vjust=-0.5, hjust=-0.2, size=3) +
    scale_y_continuous(labels=asKLabel) +
      scale_colour_manual(name = '', guide = 'legend',values = errorSourceColors)+
    theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position=c(1,1),  legend.justification=c(1,1), legend.direction='horizontal', legend.background = element_rect(fill="transparent"))
}

getPlaceStatsTable<-function(currency){
  table = data.frame()
  for(place in Places){
    isMatch = sum(hold_total[(nrow(hold_total)-2):nrow(hold_total),paste0(place,'_',currency)], na.rm=T)>0
    if(isMatch){
      table[place,c("total", 'abs_diff')]<-c(hold_total[nrow(hold_total),c(paste0(place,'_',currency), paste0(place,'_',currency,'_abs_diff'))])
    }
  }
  table$Names<-rownames(table)
  table
}

getPlaceHistoryTable<-function(currency){
  table = data.frame(matrix(NA, nrow=nrow(hold_total), ncol = 0))
  
  for(place in Places){
    isMatch = sum(hold_total[(nrow(hold_total)-2):nrow(hold_total),paste0(place,'_',currency)], na.rm=T)>0
    if(isMatch){
      column <- paste0(place,'_',currency)
      table[, place] <- hold_total[, column]
    }
  }
  rownames(table)<-rownames(hold_total)
  table
}

placeChangePlot<-function(currency){
  table <- getPlaceStatsTable(currency)
  
  ggplot(table, aes(x=reorder(gsub(" ", "\n",Names),abs_diff)))+
    geom_bar(aes(y=abs_diff, colour=Names, fill=Names), stat = "identity")+
    geom_text(aes(y=abs_diff, label=asKLabel(abs_diff), colour=Names), vjust=1.5, hjust=-0.2, size=3) +
    scale_y_continuous(labels=asKLabel) +
    scale_fill_manual(name = '',  values=alpha(placesScale, 0.4)) +
    scale_color_manual(name = '',  values=placesScale) +
    coord_flip() +
    theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.key.size = unit(0.8, 'cm'), legend.position=c(1, 0.5),  legend.justification=c(1,1), legend.direction='vertical', legend.background=element_rect(fill="transparent"))
}

placeSharePlot<-function(currency){
  table <- getPlaceStatsTable(currency)
  
  labels <- asPercentLabel(table$total/sum(table$total), dropZero=T, min=0.01)
  avg <- mean(table$total)/5
  
  ggplot(table) + 
    geom_bar(aes(x=1, y=total, fill=reorder(Names, total), colour=reorder(Names, total)), position="fill", stat = "identity")+
    geom_text(aes(y=total+avg, label=labels, x=1.2, colour=reorder(Names, total)), vjust=3, size=4, position="fill", stat = "identity") +
    coord_polar(theta="y") + labs(fill=NULL) +
    scale_fill_manual("currency", values=alpha(placesScale, 0.5)) +
    scale_color_manual("currency", values=placesScale) +
    theme(axis.text=element_blank(), axis.ticks=element_blank(),  axis.title = element_blank(), legend.text=element_text(size=15), legend.position=c(0.5,0), legend.direction="horizontal", legend.background = element_rect(fill="transparent"), legend.title = element_blank(), panel.background = element_blank())
}

profitErrorRel<-function(hold_total){
  ggplot(hold_total, aes(x=Names))+
  geom_line(aes(x=Names, y=rel_error), size=0.5, group = 1, colour=boolC["FALSE"]) +
  geom_point(aes(y=rel_error, size=10*abs(abs_error)/sum(abs(abs_error), na.rm = T)), colour=boolC["FALSE"], fill=alpha(boolC["FALSE"], 0.1), shape = 21) +
  geom_text(aes(y=rel_error, label=asPercentLabel(rel_error)), colour=boolC["FALSE"], vjust=1.5, hjust=-0.2, size=3) +
  geom_text(aes(y=rel_error, label=asKLabel(abs_error)), colour=boolC["FALSE"], vjust=-1, hjust=-0.3, size=2.5) +
  scale_y_continuous(labels=asPercentLabel) +
  theme(axis.title = element_blank(), legend.position = "none") 
}

asPercentLabel <- function (value, flags="", dropZero=F, min=-Inf){
  ifelse(
    is.na(value) | dropZero & value==0 | value<min, 
    NA,
    paste0(formatC(value*100, digits = 1, format = "f", drop0trailing=T,flag=flags),'%')
  )
}

asKLabel <-function(value, flags="", dropZero=F, min=-Inf, digits=2){
  ifelse(
    is.na(value) | dropZero & value==0 | value<min, 
    NA, 
    ifelse(
      abs(value)>=1000,
      paste0(formatC(value/1000, digits = digits, format = "f", drop0trailing=T,flag=flags),'k'),
      formatC(value, digits = digits, format = "f", drop0trailing=T,flag=flags) 
    )
  )
}

currencyHoldingsPlot <- function(hold_total, currency, type){
  tot<-hold_total[, currency]
  dif<-hold_total[, paste0("abs_diff_", currency)]
  rel<-hold_total[, paste0("rel_diff_", currency)]
  col<-currColor[currency]
  secCol<-currSecColor[currency]
  
  labels<-asKLabel(tot)
  diffRelLabel<-asPercentLabel(rel)
  diffAbsLabel<-asKLabel(dif, '+')
  
  p1<-ggplot(data=NULL, aes(x=hold_total$Names, y=tot))+
    geom_bar(position="stack", stat="identity", colour=col, fill=alpha(secCol, 0.7))+
    geom_text(aes(label=labels), colour=col , vjust=-0.5) +
    geom_text(aes(label=diffAbsLabel),colour=col, vjust=2, size=3) + 
    geom_text(aes(label=diffRelLabel),colour=col, vjust=4, size=3) +
    scale_y_continuous(labels=asKLabel) +
    theme(axis.title = element_blank(), axis.text.y.right = element_text(color = currSecColor["UAH"])) 
  
  if(currency!='UAH'){
    p1<-addCompareLine(p1, hold_total[, currency], hold_total[, paste0('uah_', currency)])
  }
  
  p2<-ggplot(data=NULL, aes(x=hold_total$Names, y=dif))+
    geom_bar(position="stack", stat="identity", colour=col, fill=alpha(secCol, 0.7))+
    geom_text(aes(label=diffAbsLabel, vjust=ifelse(dif>0,-0.5,1.5)), colour=col) +
    geom_text(aes(label=diffRelLabel, vjust=ifelse(dif>0,2,-2)),colour=col, size=3) + 
    scale_y_continuous(labels=asKLabel) +
    theme(axis.title.x = element_blank(), axis.title.y = element_blank()) 
  
  p3<-ggplot(data=NULL, aes(x=hold_total$Names, y=rel, group = 1)) + 
    geom_point(aes(size=5*abs(dif)/sum(abs(dif), na.rm = T)), colour=col, fill=secCol, shape = 21) +
    geom_text(aes(label=diffRelLabel),colour=col, vjust=-1, hjust=-0.25, size=2.5) + 
    geom_text(aes(label=diffAbsLabel),colour=col, vjust=1, hjust=-0.25, size=2.5) + 
    geom_segment(data=NULL, aes(x=hold_total$Names, xend=hold_total$Names, y=ifelse(rel>0, max(0, min(rel, na.rm = T)), min(0, max(rel, na.rm = T))), yend=rel), colour=col) +
    scale_y_continuous(labels=function(x) paste0(x*100,'%')) +
    theme(axis.title.x = element_blank(), axis.title.y = element_blank(),  legend.position = "none") 
  
  switch(type, "currencyDynamics" = p1, "currencyDiffAbs" = p2, "currencyDiffRel" = p3)
}

addCompareLine <-function(plot, original, compare, color=alpha(currSecColor["UAH"], 0.25)){
  ylim.prim <- c(0, first(na.omit(original)))
  ylim.sec <- c(0, first(na.omit(compare)))
  b <- diff(ylim.prim)/diff(ylim.sec)
  a <- b*(ylim.prim[1] - ylim.sec[1])
  plot + geom_line(aes(y=a+b*compare), color=color, group=1) +
    scale_y_continuous(labels=asKLabel, sec.axis = sec_axis(~ (. - a)/b, labels=asKLabel))
}

uahRatePlot <- function (uah_rates, currency){
  col=currColor[currency]
  holdCol=alpha(currSecColor[currency], 0.5)
  line<-unlist(uah_rates[-c(1), currency])
  hold<-hold_total[-c(1), currency]
  
  ylim.prim <- c(first(na.omit(line)), last(na.omit(line)))
  ylim.sec <- c(0, last(na.omit(hold)))
  
  b <- diff(ylim.prim)/diff(ylim.sec)
  a <- ylim.prim[1] - b*ylim.sec[1]
  
  ggplot(NULL, aes(x=flw_x_ace))+
    geom_segment(aes(y=min(line), yend = a + hold*b, x=flw_x_ace, xend=flw_x_ace), size=10, color = holdCol) +
    geom_line(aes(y=line), group=1, colour=col)+
    geom_text(aes(y=line, label=asKLabel(line)),colour=col, vjust=-0.5, hjust=-0.15, size=2.5) +
    theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
    scale_y_continuous(labels=asKLabel, sec.axis = sec_axis(~ (. - a)/b, labels=asKLabel))
}

currencyDynamicsPlot<-function(hold_total){
  time <- rep(hold_total$Names,each=length(currencies))
  group <- rep(currencies,times=nrow(hold_total))
  dd <- data.frame(time,group)
  dd$uah_value<-sapply(1:nrow(dd), function(i) hold_total[dd[i, "time"], paste0("uah_", dd[i, "group"])])
  dd$x<-rep(1:nrow(hold_total),each=length(currencies))
  
  ggplot(dd, aes(x=x, y=uah_value, fill=group, colour=group)) + 
    geom_area(alpha=0.6 , size=0.5) + 
    scale_fill_manual("currency", values=currSecColor) +
    scale_color_manual("currency", values=currColor) +
    scale_y_continuous(labels=asKLabel) +
    scale_x_continuous(breaks=1:nrow(hold_total), labels=hold_total$Names) +
    geom_text(aes(label=asKLabel(uah_value, dropZero = T), colour=group), position = "stack", vjust=1.5, hjust=-0.1, size=2.5) +
    theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position=c(0,1),  legend.justification=c("left", "top"), legend.direction='horizontal', legend.background = element_rect(fill="transparent"), legend.title = element_blank())
}

currencyPortfolioPlot<-function(hold_total){
  time <- rep(hold_total$Names,each=length(currencies))
  group <- rep(currencies,times=nrow(hold_total))
  dd <- data.frame(time,group)
  dd$uah_rate<-sapply(1:nrow(dd), function(i) hold_total[dd[i, "time"], paste0("uah_", dd[i, "group"])]/hold_total[dd[i, "time"], "abs"])
  dd$x<-rep(1:nrow(hold_total),each=length(currencies))
  
  ggplot(dd, aes(x=x, y=uah_rate, fill=group, colour=group)) + 
    geom_area(alpha=0.6 , size=0.5) + 
    scale_fill_manual(values=currSecColor) +
    scale_color_manual(values=currColor) +
    scale_y_continuous(labels=asPercentLabel) +
    scale_x_continuous(breaks=1:nrow(hold_total), labels=hold_total$Names) +
    geom_text(aes(label=asPercentLabel(uah_rate, dropZero = T), colour=group), position = "stack", vjust=1.5, hjust=-0.1, size=2.5) +
    theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position=c(0,0),  legend.justification=c("left", "bottom"), legend.direction='vertical', legend.background = element_rect(fill="transparent"), legend.title = element_blank())
}

extract_uah<-function(raw, rates, currencyRow=1, categoryRows=c(1,2)){
  data<-as.data.frame(raw)
  uah<-data[-categoryRows,-c(1)]
  dates<-as.POSIXct(unlist(data$Name[-categoryRows]), origin='1970-01-01')
  row.names(uah) <- format(dates, '%b %y')
  dataRates<-rates %>% filter(rates$time %in% dates)
  
  for (column in colnames(uah)) {
    currency<-as.character(data[currencyRow, column])
    rate<-dataRates[, currency]
    amount<-as.numeric(as.character((data[-categoryRows, column])))
    uah[,column]<-unlist(amount)*unlist(rate)
  }
  uah
}

extract_native<-function(raw, categoryRows=c(1)){
  data<-as.data.frame(raw)
  dates<-as.POSIXct(unlist(data$Name[-categoryRows]), origin='1970-01-01')
  data<-data[-categoryRows,-c(1)]
  row.names(data) <- format(dates, '%b %y')
  for (column in colnames(data)) {
    data[,column]<-as.numeric(as.character((data[, column])))
  }
  data
}

currencySummarize<-function(raw, currencyRow, categoryRows){
  data<-as.data.frame(currencies)
  rownames(data)<-data$currencies
  data$sum<-0
  data$rate=as.numeric(tail(uah_rates[-c(1)], n=1))

  for (column in colnames(raw)) {
    currency<-as.character(unlist(raw[currencyRow, column]))
    amount<-unlist((raw[c(nrow(raw)), column]))
    if(length(amount)> 0) {
      data[currency, 'sum'] <- data[currency, 'sum'] + as.numeric(amount)
    }
  }
  head(data[-c(1)], -1)
}

holdingTypesDynamicsPlot<-function(hold_total){
  time <- rep(hold_total$Names,each=length(holdingTypes))
  group <- rep(holdingTypes,times=nrow(hold_total))
  dd <- data.frame(time,group)
  dd$uah_value<-sapply(1:nrow(dd), function(i) hold_total[as.character(dd[i, "time"]), as.character(dd[i, "group"])])
  dd$x<-rep(1:nrow(hold_total),each=length(holdingTypes))
  
  ggplot(dd, aes(x=x, y=uah_value, fill=factor(group, levels=holdingTypes), colour=factor(group, levels=holdingTypes))) + 
    geom_area(alpha=0.4 , size=0.5) + 
    scale_color_manual(values=holdScale) +
    scale_fill_manual(values=holdScale) +
    scale_y_continuous(labels=asKLabel) +
    scale_x_continuous(breaks=1:nrow(hold_total), labels=hold_total$Names) +
    geom_text_repel(aes(label=asKLabel(uah_value, min=5000), colour=factor(group, levels=holdingTypes)), position = "stack", size=3) +
    theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position=c(0,1),  legend.justification=c("left", "top"), legend.direction='horizontal', legend.background = element_rect(fill="transparent"), legend.title = element_blank())
}

holdingTypesPortfolioPlot<-function(hold_total){
  time <- rep(hold_total$Names,each=length(holdingTypes))
  group <- rep(holdingTypes,times=nrow(hold_total))
  dd <- data.frame(time,group)
  dd$uah_value<-sapply(1:nrow(dd), function(i) hold_total[as.character(dd[i, "time"]), as.character(dd[i, "group"])]/hold_total[dd[i, "time"], "abs"])
  dd$x<-rep(1:nrow(hold_total),each=length(holdingTypes))
  
  ggplot(dd, aes(x=x, y=uah_value, fill=factor(group, levels=holdingTypes), colour=factor(group, levels=holdingTypes))) + 
    geom_area(alpha=0.4 , size=0.5) + 
    scale_color_manual(values=holdScale) +
    scale_fill_manual(values=holdScale) +
    scale_x_continuous(breaks=1:nrow(hold_total), labels=hold_total$Names) +
    scale_y_continuous(labels=asPercentLabel) +
    geom_text_repel(aes(label=asPercentLabel(uah_value, min=0.02), colour=factor(group, levels=holdingTypes)), position = "stack", size=3) +
    theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position=c(0,0),  legend.justification=c("left", "bottom"), legend.direction='vertical', legend.background = element_rect(fill="transparent"), legend.title = element_blank())
}

holdAbsDynamicsPlot <- function(holdings,hold_meta,hold_uah, col){
  holdings$Names<-factor(row.names(holdings), levels = row.names(holdings))
  currency = as.character(hold_meta[1,col])
  
  p1<-ggplot(data=holdings, aes(x=Names, y=holdings[,col]))+
    geom_bar(stat="identity", colour=currColor[currency], fill=alpha(currSecColor[currency], 0.7)) +
    geom_text(aes(label=asKLabel(holdings[,col])), colour=currColor[currency] , vjust=-0.5, size=3) +
    scale_y_continuous(labels=asKLabel) +
    theme(axis.title = element_blank(), axis.text.y.right = element_text(color = currSecColor["UAH"]))
  
  if(currency!='UAH'){
    p1<-addCompareLine(p1, holdings[, col], hold_uah[, col])
  }
  
  p1
}

diversificationTable <- function (consolidation, pretty=FALSE){
  tab = data.frame(matrix(NA, nrow=nrow(consolidation), ncol = 0))
  tab$Total  <- rowSums(consolidation)
  weights<-consolidation/tab$Total 

  tab$Herfindail <- rowSums(weights^2)
  tab$Shanon <- -rowSums(weights*log(weights), na.rm = T)
  tab$No <- rowSums(weights>0.005)
  
  rownames(tab)<-rownames(consolidation)
  if(pretty){
    tab$Total <- asKLabel(tab$Total, digits = 2)
    tab$Shanon <- asKLabel(tab$Shanon, digits = 3)
    tab$No <- asKLabel(tab$No, digits = 1)
  }
  tab
}

diversificationBarPlot<-function(consolidation, palette){
  total <- sum(consolidation)
  labelsPercent <- asPercentLabel(consolidation/total, dropZero=T, min=0.005)
  Names<-rownames(consolidation)
  colnames(consolidation)<-c("Total")
  
  ggplot(NULL, aes(x=reorder(Names, consolidation), colour=Names))+
    geom_bar(aes(y=consolidation, fill=Names), stat = "identity")+
    geom_text(aes(y=consolidation, label=asKLabel(consolidation), colour=Names), vjust=1.5, hjust=-0.2, size=3) +
    scale_y_continuous(labels=asKLabel) +
    scale_fill_manual(name = '',  values=alpha(palette, 0.4)) +
    scale_color_manual(name = '',  values=palette) +
    coord_flip() +
    theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.key.size = unit(0.8, 'cm'), legend.position=c(1, 0.2),  legend.justification=c(1,1), legend.direction='horizontal', legend.background=element_rect(fill="transparent"))
}

diversificationPiePlot<-function(consolidation, palette){
  relates <- consolidation/sum(consolidation)
  labels <- asPercentLabel(ifelse(relates>0.01,relates,0), dropZero = T)
  Names <- as.factor(rownames(consolidation))
  
  ggplot(NULL, aes(y=consolidation, fill=reorder(Names, consolidation), colour=reorder(Names, consolidation))) + 
    geom_bar(aes(x=1), position="fill", stat = "identity")+
    geom_label(aes(label=labels, x=1.2), fill=alpha('white', 0.5), vjust=1, size=4, position="fill", stat = "identity", na.rm = T) +
    coord_polar(theta="y") + labs(fill=NULL) +
    scale_fill_manual("",values= alpha(palette, 0.5)) +
    scale_color_manual("",values=palette) +
    theme(axis.text=element_blank(), axis.ticks=element_blank(),  axis.title = element_blank(), legend.text=element_text(size=15), legend.position=c(0.5,0.05), legend.direction='horizontal', legend.background = element_rect(fill="transparent"), legend.title = element_blank(), panel.background = element_blank())
}

diversificationPerformancePlot <- function(consolidation){
  pallete = c("#EFBBCF", '#FFD5CD', '#8675A9', '#C3AED6')

  ylim.prim <- c(min(na.omit(consolidation$Shanon)), max(na.omit(consolidation$Shanon)))
  ylim.sec <- c(max(na.omit(consolidation$Herfindail)), min(na.omit(consolidation$Herfindail)))
  ylim.third <- c(min(na.omit(consolidation$No)), max(na.omit(consolidation$No)))
  
  b <- diff(ylim.prim)/diff(ylim.sec)
  a <- ylim.prim[1] - b*ylim.sec[1]
  d <- diff(ylim.prim)/diff(ylim.third)
  c <- ylim.prim[1] - d*ylim.third[1]
  
  ggplot(NULL, aes(x=reorder(row.names(consolidation), consolidation$Total)))+
    geom_line(aes(y=a+consolidation$Herfindail*b), group=1, size=1.5, colour=alpha(pallete[1], .75)) +
    geom_text(aes(y=a+consolidation$Herfindail*b, label=asKLabel(consolidation$Herfindail, digits=3)), vjust=-0.5, hjust=-0.15, size=2.5, colour=darken(pallete[1], 1.5)) +
    geom_line(aes(y=consolidation$Shanon), group=1, size=1.5, colour=alpha(pallete[2], .75))+
    geom_text(aes(y=consolidation$Shanon, label=asKLabel(consolidation$Shanon, digits=3)), vjust=1, hjust=0.5, size=2.5, colour=darken(pallete[2], 1.5)) +
    geom_point(size=6*consolidation$Total/max(consolidation$Total), aes(y=c+consolidation$No*d), colour=alpha(pallete[3], 0.5)) +
    geom_segment(aes(xend=row.names(consolidation), y=0, yend=c+consolidation$No*d), size=1, colour=alpha(pallete[3], .35)) +
    geom_text(aes(y=0, label=asKLabel(consolidation$No, digits=0)), vjust=-1, hjust=-1, size=3, colour=darken(pallete[3], 1.5)) +
    theme(axis.title.y.left = element_text(colour = darken(pallete[2], 1.5)),
          axis.line.y.left = element_line(color = pallete[2], size=1.5), 
          axis.title.y.right = element_text(colour = darken(pallete[1], 1.5)),
          axis.line.y.right = element_line(color = pallete[1], size=1.5), 
          axis.title.x = element_blank(), 
          panel.background = element_rect(fill = alpha(pallete[4], 0.15))) +
    scale_y_continuous(sec.axis = sec_axis(~ (. - a)/b, name="Herfindail")) + ylab("Shanon")
}

darken <- function(color, factor=1.4){
  col <- col2rgb(color)
  col <- col/factor
  col <- rgb(t(col), maxColorValue=255)
  col
}
