##Loading the data set ##
setwd("C:/Users/colruyt/Downloads/")
pig_df <- read.csv("Pigmeat_Prices.csv", header = TRUE)
pig_df

#Text Manipulation -- To remove special char "Ã‚"
pig_df$Price <- gsub(pattern = "â,¬", replacement = "", pig_df$Price)
View(pig_df)

#include class type E Pig meat of Belgium
library(dplyr)
pig_df_E <- filter(pig_df, Pig.Class == 'E', Member.State.Name == 'Belgium')
View(pig_df_E)
names(pig_df_E)

#include end date and price columns
pig_df_E <- pig_df_E[c(4,8)]
View(pig_df_E)

#Convert week end date variable to date variable
pig_df_E$year <- as.Date(pig_df_E$Week...End.Date, format = "%d/%m/%Y")

#get year from week end date
pig_df_E$year <- as.numeric(format(pig_df_E$year, format = "%Y"))
pig_df_E <- pig_df_E[c(2,3)]
pig_df_E$Price <- as.numeric(pig_df_E$Price)
View(pig_df_E)

#sum pig meat price per year
pig_sales_df <- aggregate(pig_df_E$Price, by=list(year=pig_df_E$year), FUN=sum)
View(pig_sales_df)

### rmse function#####
rmse<-function(error){
  sqrt(mean(error^2))
}
MAPE<-function(error,sales){
  mean(abs(error/sales))*100  
}


#Holtwinter's exponential smoothing

### Data Partition###
tsDatatraing.ts <- ts(pig_df_E[1:1536,c(1)], start = c(1991), frequency = 52)
tsDatatest.ts <- ts(pig_df_E[1501:1536,c(1)], start = c(2020), frequency = 12)

# Decomposition of additive time series
decompose_price = decompose(tsDatatraing.ts)
plot(decompose_price)

#Apply HW model
fit_holt_winter <- hw(y = tsDatatraining.ts, h=12)
summary(fit_holt_winter)
plot(forecast(fit_holt_winter))

#Validation
values <- fit_holt_winter$mean
error_holt_winter <- tsDatatest.ts-values
rmse(error_holt_winter)
MAPE(error_holt_winter,tsDatatest.ts)
cbind(tsDatatest.ts,values,error_holt_winter)
acf(fit_holt_winter$residuals)

