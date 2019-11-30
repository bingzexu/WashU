library(readxl)
library(tidyr)
library(dplyr)
library(fastDummies)
# !diagnostics off
setwd("~/Downloads/")
big_df = data.frame()
for(i in 1:13){
  df = read_excel("Seminar_data_1119.xlsx", sheet = i, na = " ")
  #df = df[-1, ]
  df$`capital/asset`=as.numeric(df$`capital/asset`)
  df$`expense/revenue`=as.numeric(df$`expense/revenue`)
  df$`Efficiency Ratio`=as.numeric(df$`Efficiency Ratio`)
  df$`expense asset ratio`=as.numeric(df$`expense asset ratio`)
  df$ROE=as.numeric(df$ROE)
  df$ROA=as.numeric(df$ROA)
  df$`Total asset`=as.numeric(df$`Total asset`)
  df$`capital structure`=as.numeric(df$`capital structure`)
  df$`funding structure`=as.numeric(df$`funding structure`)
  df$`organizational complexity` = as.numeric(df$`organizational complexity`)
  df['log_org'] = log(df$`organizational complexity`)
  #df$Year = as.factor(df$Year)
  big_df <- rbind(big_df, df)
}
big_df$`Total asset` <- log(big_df$`Total asset`)
df_capital = big_df[complete.cases(big_df$`capital/asset`),]
df_expense =big_df[complete.cases(big_df$`expense/revenue`),]

df_capital = dummy_cols(df_capital, select_columns = "Bank Name", remove_first_dummy = TRUE)
for(j in 17:ncol(df_capital)){
  df_capital[[colnames(df_capital[j])]] = factor(df_capital[[colnames(df_capital[j])]])
}
df_capital = dummy_cols(df_capital, select_columns = "Year", remove_first_dummy = TRUE)
for(j in 58:ncol(df_capital)){
  df_capital[[colnames(df_capital[j])]] = factor(df_capital[[colnames(df_capital[j])]])
}

df_expense = dummy_cols(df_expense, select_columns = "Bank Name", remove_first_dummy = TRUE)
for(j in 17:ncol(df_expense)){
  df_expense[[colnames(df_expense[j])]] = factor(df_expense[[colnames(df_expense[j])]])
}
df_expense = dummy_cols(df_expense, select_columns = "Year", remove_first_dummy = TRUE)
for(j in 57:ncol(df_expense)){
  df_expense[[colnames(df_expense[j])]] = factor(df_expense[[colnames(df_expense[j])]])
}
# column 4,5,6,7 are our dependent variables in both dataframes
# 8 (log asset),9,10,16(log subs) are our control variables in both dataframes
# 14 is scaled IT capital and 15 is scaled IT expense
# 17 to the end are dummys

mod_capital1 <- lm(df_capital$`Efficiency Ratio`~., data=df_capital[c(4,9:10,14,17:ncol(df_capital))])
mod_capital2 <- lm(df_capital$`expense asset ratio`~., data=df_capital[c(5,8:10,14,17:ncol(df_capital))])
mod_capital3 <- lm(df_capital$ROE~., data=df_capital[c(6,8:10,14,17:ncol(df_capital))])
mod_capital4 <- lm(df_capital$ROA~., data=df_capital[c(7,8:10,14,17:ncol(df_capital))])

mod_expense1 <- lm(df_expense$`Efficiency Ratio`~., data=df_expense[c(4,8:10,15,17:ncol(df_expense))])
mod_expense2 <- lm(df_expense$`expense asset ratio`~., data=df_expense[c(5,8:10,15,17:ncol(df_expense))])
mod_expense3 <- lm(df_expense$ROE~., data=df_expense[c(6,8:10,15,17:ncol(df_expense))])
mod_expense4 <- lm(df_expense$ROA~., data=df_expense[c(7,8:10,15,17:ncol(df_expense))])

# setwd("~/Downloads/Fintech_Seminar")
df_large = data.frame()
df_med = data.frame()
df_small = data.frame()
for(i in 1:11){
df = read_excel("XBZ-Bank.xlsx", sheet = i, na = " ")
df <- df[-1, ]
df$`IT capital`=as.numeric(df$`IT capital`)
df$`intangible expense`=as.numeric(df$`intangible expense`)
df$`Efficiency Ratio`=as.numeric(df$`Efficiency Ratio`)
df$`expense asset ratio`=as.numeric(df$`expense asset ratio`)
df$ROE=as.numeric(df$ROE)
df$ROA=as.numeric(df$ROA)
df$`Total asset`=as.numeric(df$`Total asset`)
df$`capital structure`=as.numeric(df$`capital structure`)
df$`Advertising intensity`=as.numeric(df$`Advertising intensity`)
df$`funding structure`=as.numeric(df$`funding structure`)
df$`organizational complexity`=as.numeric(df$`organizational complexity`)
df$`domestic asset size/total asset size` = as.numeric(df$`domestic asset size/total asset size`)
scaled.df <- scale(df[,-1])
# large banks
df_large_new = scaled.df[2:12,4:9][complete.cases(scaled.df[2:12,4:9]), ]
df_large = bind_rows(df_large, as.data.frame(df_large_new))
# med banks
df_med_new = scaled.df[14:27,4:9][complete.cases(scaled.df[14:27,4:9]), ]
df_med = bind_rows(df_med, as.data.frame(df_med_new))
# small banks
df_small_new = scaled.df[29:49,4:9][complete.cases(scaled.df[29:49,4:9]), ]
df_small = bind_rows(df_small, as.data.frame(df_small_new))
}
df_large = df_large[,-7][complete.cases(df_large[,-7]), ]
df_med = df_med[,-7][complete.cases(df_med[,-7]), ]
df_small = df_small[,-7][complete.cases(df_small[,-7]), ]
# efficiency ratio = non-interest expense/net operating revenue
# capital structure = total liabilities/revenue
# expense asset ratio = non-interest expense/Risk-weighted assets


efficiency_ratio_small = list()
expense_asset_ratio_small = list()
capital_structure_small = list()
funding_structure_small = list()
total_asset_small = list()
roa_small = list()
roe_small = list()
itexpense_small = list()
itcapital_small = list()

efficiency_ratio_medium = list()
expense_asset_ratio_medium = list()
capital_structure_medium = list()
funding_structure_medium = list()
total_asset_medium = list()
roa_medium = list()
roe_medium = list()
itexpense_medium = list()
itcapital_medium = list()

efficiency_ratio_large = list()
expense_asset_ratio_large = list()
capital_structure_large = list()
funding_structure_large = list()
total_asset_large = list()
roa_large = list()
roe_large = list()
itexpense_large = list()
itcapital_large = list()
# View(paste("df", i, sep = ""))
year_axis = 2008:2018
# 2-44 first big basket, 47-69, 72-101
# roe, capital, funding change over years
for(i in 1:11) { 
  #df <- paste("df", i, sep = "")
  #assign(df, read_excel("XBZ-Bank.xlsx", sheet = i))
  df = read_excel("XBZ-Bank.xlsx", sheet = i, na = " ")
  df <- df[-1, ]
  df$`IT capital`=as.numeric(df$`IT capital`)
  df$`hardware, software, IS salaries`=as.numeric(df$`hardware, software, IS salaries`)
  df$`Efficiency Ratio`=as.numeric(df$`Efficiency Ratio`)
  df$`expense asset ratio`=as.numeric(df$`expense asset ratio`)
  df$ROE=as.numeric(df$ROE)
  df$ROA=as.numeric(df$ROA)
  df$`Total asset`=as.numeric(df$`Total asset`)
  df$`capital structure`=as.numeric(df$`capital structure`)
  df$`Advertising intensity`=as.numeric(df$`Advertising intensity`)
  df$`funding structure`=as.numeric(df$`funding structure`)
  df$`organizational complexity`=as.numeric(df$`organizational complexity`)
  df$`domestic asset size/total asset size` = as.numeric(df$`domestic asset size/total asset size`)
  
  avg_efficiency_ratio_small = mean(df$`Efficiency Ratio`[29:49],na.rm = TRUE)
  efficiency_ratio_small <- c(efficiency_ratio_small, avg_efficiency_ratio_small)
  avg_expense_asset_ratio_small = mean(df$`expense asset ratio`[29:49],na.rm = TRUE)
  expense_asset_ratio_small <- c(expense_asset_ratio_small, avg_expense_asset_ratio_small)
  avg_capital_structure_small = mean(df$`capital structure`[29:49],na.rm = TRUE)
  capital_structure_small <- c(capital_structure_small, avg_capital_structure_small)
  avg_funding_structure_small = mean(df$`funding structure`[29:49],na.rm = TRUE)
  funding_structure_small <- c(funding_structure_small, avg_funding_structure_small)
  avg_total_asset_small = mean(df$`Total asset`[29:49],na.rm = TRUE)
  total_asset_small <- c(total_asset_small, avg_total_asset_small)
  avg_roa_small = mean(df$ROA[29:49],na.rm = TRUE)
  roa_small <- c(roa_small, avg_roa_small)
  avg_roe_small = mean(df$ROE[29:49],na.rm = TRUE)
  roe_small <- c(roe_small, avg_roe_small)
  avg_itexpense_small = mean(df$`hardware, software, IS salaries`[29:49],na.rm = TRUE)
  itexpense_small <- c(itexpense_small, avg_itexpense_small)
  avg_itcapital_small = mean(df$`IT capital`[29:49],na.rm = TRUE)
  itcapital_small <- c(itcapital_small, avg_itcapital_small)
  
  avg_efficiency_ratio_medium = mean(df$`Efficiency Ratio`[14:27],na.rm = TRUE)
  efficiency_ratio_medium <- c(efficiency_ratio_medium, avg_efficiency_ratio_medium)
  avg_expense_asset_ratio_medium = mean(df$`expense asset ratio`[14:27],na.rm = TRUE)
  expense_asset_ratio_medium <- c(expense_asset_ratio_medium, avg_expense_asset_ratio_medium)
  avg_capital_structure_medium = mean(df$`capital structure`[14:27],na.rm = TRUE)
  capital_structure_medium <- c(capital_structure_medium, avg_capital_structure_medium)
  avg_funding_structure_medium = mean(df$`funding structure`[14:27],na.rm = TRUE)
  funding_structure_medium <- c(funding_structure_medium, avg_funding_structure_medium)
  avg_total_asset_medium = mean(df$`Total asset`[14:27],na.rm = TRUE)
  total_asset_medium <- c(total_asset_medium, avg_total_asset_medium)
  avg_roa_medium = mean(df$ROA[14:27],na.rm = TRUE)
  roa_medium <- c(roa_medium, avg_roa_medium)
  avg_roe_medium = mean(df$ROE[14:27],na.rm = TRUE)
  roe_medium <- c(roe_medium, avg_roe_medium)
  avg_itexpense_medium = mean(df$`hardware, software, IS salaries`[14:27],na.rm = TRUE)
  itexpense_medium <- c(itexpense_medium, avg_itexpense_medium)
  avg_itcapital_medium = mean(df$`IT capital`[14:27],na.rm = TRUE)
  itcapital_medium <- c(itcapital_medium, avg_itcapital_medium)
  
  avg_efficiency_ratio_large = mean(df$`Efficiency Ratio`[2:12],na.rm = TRUE)
  efficiency_ratio_large <- c(efficiency_ratio_large, avg_efficiency_ratio_large)
  avg_expense_asset_ratio_large = mean(df$`expense asset ratio`[2:12],na.rm = TRUE)
  expense_asset_ratio_large <- c(expense_asset_ratio_large, avg_expense_asset_ratio_large)
  avg_capital_structure_large = mean(df$`capital structure`[2:12],na.rm = TRUE)
  capital_structure_large <- c(capital_structure_large, avg_capital_structure_large)
  avg_funding_structure_large = mean(df$`funding structure`[2:12],na.rm = TRUE)
  funding_structure_large <- c(funding_structure_large, avg_funding_structure_large)
  avg_total_asset_large = mean(df$`Total asset`[2:12],na.rm = TRUE)
  total_asset_large <- c(total_asset_large, avg_total_asset_large)
  avg_roa_large = mean(df$ROA[2:12],na.rm = TRUE)
  roa_large <- c(roa_large, avg_roa_large)
  avg_roe_large = mean(df$ROE[2:12],na.rm = TRUE)
  roe_large <- c(roe_large, avg_roe_large)
  avg_itexpense_large = mean(df$`hardware, software, IS salaries`[2:12],na.rm = TRUE)
  itexpense_large <- c(itexpense_large, avg_itexpense_large)
  avg_itcapital_large = mean(df$`IT capital`[2:12],na.rm = TRUE)
  itcapital_large <- c(itcapital_large, avg_itcapital_large)
}

library(ggplot2)
df11 = do.call(rbind, Map(data.frame, year = year_axis, ROE = roe_large, capital_structure = capital_structure_large, total_asset = total_asset_large))
library(plotrix)
par(mfrow=c(1,2))
twoord.plot(df11$year,df11$total_asset,df11$year,df11$capital_structure,xlab="Year",
            ylab="Total Asset(in k)",rylab="liability/revenue",lcol=4,
            main="large banks",
            do.first="plot_bg();grid(col=\"white\",lty=1)")
plot(df11$year, df11$ROE, main = "large banks ROE",
     xlab = "Year", ylab = "ROE",
     pch = 19, col = "blue", frame = FALSE)
lines(lowess(year_axis, df11$ROE), col = "blue")

df12 = do.call(rbind, Map(data.frame, year = year_axis, ROE = roe_medium, capital_structure = capital_structure_medium, total_asset = total_asset_medium))
par(mfrow=c(1,2))
twoord.plot(df12$year,df12$total_asset,df12$year,df12$capital_structure,xlab="Year",
            ylab="Total Asset(in k)",rylab="liability/revenue",lcol=4,
            main="medium banks",
            do.first="plot_bg();grid(col=\"white\",lty=1)")
plot(df12$year, df12$ROE, main = "medium banks ROE",
     xlab = "Year", ylab = "ROE",
     pch = 19, col = "blue", frame = FALSE)
lines(lowess(year_axis, df12$ROE), col = "blue")

df13 = do.call(rbind, Map(data.frame, year = year_axis, ROE = roe_small, capital_structure = capital_structure_small, total_asset = total_asset_small))
par(mfrow=c(1,2))
twoord.plot(df13$year,df13$total_asset,df13$year,df13$capital_structure,xlab="Year",
            ylab="Total Asset(in k)",rylab="liability/revenue",lcol=4,
            main="small banks",
            do.first="plot_bg();grid(col=\"white\",lty=1)")
plot(df13$year, df13$ROE, main = "small banks ROE",
     xlab = "Year", ylab = "ROE",
     pch = 19, col = "blue", frame = FALSE)
lines(lowess(year_axis, df13$ROE), col = "blue")

# efficiency, expense asset and total asset
df21 = do.call(rbind, Map(data.frame, year = year_axis, efficiency = efficiency_ratio_small, itexpense = itexpense_small, itcapital = itcapital_small))
par(mfrow=c(1,2))
twoord.plot(df21$year,df21$itexpense,df21$year,df21$efficiency,xlab="Year",
            ylab="IT expense (in k)",rylab="effiency ratio",lcol=4,
            main="small banks",
            do.first="plot_bg();grid(col=\"white\",lty=1)")
plot(df21$year, df21$itcapital, main = "small banks IT capital",
     xlab = "Year", ylab = "IT capital",
     pch = 19, col = "blue", frame = FALSE)
lines(lowess(year_axis, df21$itcapital), col = "blue")

df21 = do.call(rbind, Map(data.frame, year = year_axis, efficiency = efficiency_ratio_medium, itexpense = itexpense_medium, itcapital = itcapital_medium))
par(mfrow=c(1,2))
twoord.plot(df21$year,df21$itexpense,df21$year,df21$efficiency,xlab="Year",
            ylab="IT expense (in k)",rylab="effiency ratio",lcol=4,
            main="medium banks",
            do.first="plot_bg();grid(col=\"white\",lty=1)")
plot(df21$year, df21$itcapital, main = "medium banks IT capital",
     xlab = "Year", ylab = "IT capital",
     pch = 19, col = "blue", frame = FALSE)
lines(lowess(year_axis, df21$itcapital), col = "blue")

df21 = do.call(rbind, Map(data.frame, year = year_axis, efficiency = efficiency_ratio_large, itexpense = itexpense_large, itcapital = itcapital_large))
par(mfrow=c(1,2))
twoord.plot(df21$year,df21$itexpense,df21$year,df21$efficiency,xlab="Year",
            ylab="IT expense (in k)",rylab="effiency ratio",lcol=4,
            main="large banks",
            do.first="plot_bg();grid(col=\"white\",lty=1)")
plot(df21$year, df21$itcapital, main = "large banks IT capital",
     xlab = "Year", ylab = "IT capital",
     pch = 19, col = "blue", frame = FALSE)
lines(lowess(year_axis, df21$itcapital), col = "blue")



# scatter plot
# total asset as cluster, roa, it expense, it capital
# three different color for the three variables
par(mfrow=c(1,1))
x1 = cbind(year_axis,year_axis,year_axis)
y = cbind(total_asset_small,total_asset_medium,total_asset_large)
x2 = cbind(itexpense_small,itexpense_medium,itexpense_large)
x3 = cbind(itcapital_small,itcapital_medium,itcapital_large)
x4 = cbind(roe_small,roe_medium,roe_large)
x5 = cbind(efficiency_ratio_small,efficiency_ratio_medium,efficiency_ratio_large)
matplot(x1,y,type="p", ylab="asset size",xlab="year")
matplot(y,x2,type="p", xlab="asset size",ylab="IT_expense")
matplot(y,x3,type="p", xlab="asset size",ylab="IT_capital")
matplot(x2,x4,type="p", xlab="IT_expense",ylab="ROE")
matplot(x2,x5,type="p", ylim = 0:1, xlab="IT_expense",ylab="efficiency ratio")


# mean, median, sd and correlation table

