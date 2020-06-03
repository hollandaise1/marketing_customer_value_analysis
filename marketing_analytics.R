library(readr)
library(forecast)
library(corrplot)
library(RColorBrewer)
# Read in data
task1_data <- read_csv("Desktop/Case Study/task1_data.csv",  col_types = cols(Date = col_character(), Week_Num = col_integer()))
task1_data[4:9]

task1_data$tot_spend <- rowSums(task1_data[4:9])
head(task1_data)

task1_data$cumulative_kpi <- as.numeric(unlist(cumsum(task1_data[c('KPI')])))
task1_data$cumulative_spend <- as.numeric(unlist((cumsum(task1_data[c('tot_spend')]))))
task1_data$roi <- task1_data$cumulative_kpi/task1_data$cumulative_spend
plot(task1_data$roi,type="l", col='springgreen4', lwd=2)
summary(task1_data$roi)

# plot (x, y)
plot(task1_data$Week_Num, 
     task1_data$roi,
     type="l",
     col='springgreen4', 
     lwd=2,
     xlab="Week Number from 2017 to 2019",
     ylab="ROI (Spend in $ per KPI)", 
     main="Ads Spend Return of Investment (ROI) per KPI Across Channels (2017-2019)")

# lag

# tbats
x <- msts(task1_data$tot_spend, start=c(2017,1,7), seasonal.period=c(7,30.4,365.25))
fit <- tbats(x, use.box.cox=FALSE)
plot(fit)

# normalized by the base
# input variable x and return x_prime
# x_prime_1 is calculated as x1 indexed on x1
# x_prime_2 is calculated as x2 indexed on x1, x_prime_3 is x_3 indexed on x1, ..., x_prime_n is x_n indexed on x1

firstElement <- myList[1]
indexOnFirstElement <- function(x){
  for (i in 1:length(x)) {
    x[i] <- x[i]/firstElement
    print(x[i])
    return(x)
  }
}

indexedList <- sapply(myList, indexOnFirstElement)
indexedList

# smoothing
loess_kpi <- loess(task1_data$KPI ~ task1_data$Week_Num)
loess_tv <- loess(task1_data$TV ~ task1_data$Week_Num)
loess_social <- loess(task1_data$Social ~ task1_data$Week_Num)
loess_search <- loess(task1_data$Search ~ task1_data$Week_Num)
loess_video <- loess(task1_data$Video ~ task1_data$Week_Num)
loess_display <- loess(task1_data$Display ~ task1_data$Week_Num)
loess_radio <- loess(task1_data$Radio ~ task1_data$Week_Num)

# indexing
firstElement <- loess_kpi$fitted[1]
task1_data$kpi_loess_indexed <- sapply(loess_kpi$fitted, indexOnFirstElement)

firstElement <- loess_tv$fitted[1]
task1_data$tv_loess_indexed <- sapply(loess_tv$fitted, indexOnFirstElement)

firstElement <- loess_social$fitted[1]
task1_data$social_loess_indexed <- sapply(loess_social$fitted, indexOnFirstElement)

firstElement <- loess_search$fitted[1]
task1_data$search_loess_indexed <- sapply(loess_search$fitted, indexOnFirstElement)

firstElement <- loess_video$fitted[1]
task1_data$video_loess_indexed <- sapply(loess_video$fitted, indexOnFirstElement)

firstElement <- loess_display$fitted[1]
task1_data$display_loess_indexed <- sapply(loess_display$fitted, indexOnFirstElement)

firstElement <- loess_radio$fitted[1]
task1_data$radio_loess_indexed <- sapply(loess_radio$fitted, indexOnFirstElement)

# plotting
plot(task1_data$Week_Num, 
     task1_data$kpi_loess_indexed,  
     type="l", 
     col='springgreen4', 
     xlim = c(0, 160), 
     ylim = c(0.9, 2.8),
     lwd=3,
     xlab="Week Number from 2017 to 2019",
     ylab="Smoothed Spend($) Indexed Value", 
     main="Indexed Smoothed Ads Spend by Channel versus KPI (2017-2019)")

lines(task1_data$Week_Num, 
      task1_data$tv_loess_indexed,  type="l", col='snow4', lwd=2)
lines(task1_data$Week_Num, 
      task1_data$social_loess_indexed,  type="l", col='navy', lwd=2)
lines(task1_data$Week_Num, 
      task1_data$search_loess_indexed,  type="l", col='mediumpurple1', lwd=2)
lines(task1_data$Week_Num, 
      task1_data$video_loess_indexed,  type="l", col='skyblue1', lwd=2)
lines(task1_data$Week_Num, 
      task1_data$display_loess_indexed,  type="l", col='tomato2', lwd=2)
lines(task1_data$Week_Num, 
      task1_data$radio_loess_indexed,  type="l", col='orange3', lwd=2)
abline(h=c(1), col=c("black"), lty=c(1), lwd=c(1))
legend("top",
       legend=c("Video","Display", "Social", "Radio", "KPI", "TV", "Search"), 
       xpd = TRUE, horiz = TRUE, inset = c(0,0), 
       bty = "n", pch = 20, 
       col = c("skyblue1","tomato2", "navy","orange3", 
               "springgreen4",  "snow4", "mediumpurple1"), 
       cex = 0.8)


plot(task1_data$Week_Num, 
     task1_data$kpi_loess_indexed,  
     type="l", 
     col='springgreen4', 
     xlim = c(0, 160), 
     ylim = c(1, 1.4),
     lwd=3,
     xlab="Week Number from 2017 to 2019",
     ylab="Smoothed Spend($) Indexed Value", 
     main="Indexed Smoothed KPI (2017-2019)")

lines(task1_data$Week_Num, 
      task1_data$tv_loess_indexed,  type="l", col='snow4', lwd=2)
lines(task1_data$Week_Num, 
      task1_data$social_loess_indexed,  type="l", col='navy', lwd=2)
lines(task1_data$Week_Num, 
      task1_data$search_loess_indexed,  type="l", col='mediumpurple1', lwd=2)
#lines(task1_data$Week_Num, 
#      task1_data$video_loess_indexed,  type="l", col='skyblue1', lwd=2)
lines(task1_data$Week_Num, 
      task1_data$display_loess_indexed,  type="l", col='tomato2', lwd=2)
lines(task1_data$Week_Num, 
      task1_data$radio_loess_indexed,  type="l", col='orange3', lwd=2)
abline(h=c(1), col=c("black"), lty=c(1), lwd=c(1))



incl_columns_corr <- names(task1_data) %in% c("KPI", "TV", "Social",
                                              "Search", "Video", "Display",
                                              "Radio")
corr_data <- task1_data[incl_columns_corr]
corr_results <- cor(corr_data)
corrplot(corr_results, method="circle")

# not used
corrplot(corr_results, type="upper", order="hclust",
         col=c("black", "snow1"),
         bg="lightblue2")

loess_social_indexed <- loess(task1_data$Social_indexed ~ task1_data$Week_Num)
loess_search_indexed <- loess(task1_data$Search_indexed ~ task1_data$Week_Num)
loess_video_indexed <- loess(task1_data$Video_indexed ~ task1_data$Week_Num)
loess_display_indexed <- loess(task1_data$Display_indexed ~ task1_data$Week_Num)
loess_radio_indexed <- loess(task1_data$Radio_indexed ~ task1_data$Week_Num)


firstElement <- loess_tv$fitted[1]
task1_data$tv_loess_indexed <- sapply(loess_tv$fitted, indexOnFirstElement)


firstElement <- task1_data$Social[1]
task1_data$Social_indexed <- sapply(task1_data$Social, indexOnFirstElement)

firstElement <- task1_data$Search[1]
task1_data$Search_indexed <- sapply(task1_data$Search, indexOnFirstElement)

firstElement <- task1_data$Video[1]
task1_data$Video_indexed <- sapply(task1_data$Video, indexOnFirstElement)

firstElement <- task1_data$Display[1]
task1_data$Display_indexed <- sapply(task1_data$Display, indexOnFirstElement)

firstElement <- task1_data$Radio[1]
task1_data$Radio_indexed <- sapply(task1_data$Radio, indexOnFirstElement)






plot(task1_data$Week_Num, 
     loess_radio_indexed$fitted,  type="l", col='springgreen4', lwd=2)


lines(task1_data$Week_Num, 
      loess_display_indexed$fitted,  type="l", col='red', lwd=2)

# smoothed curves
loess_roi <- loess(task1_data$roi ~ task1_data$Week_Num)
plot(task1_data$Week_Num, 
      task1_data$Search,  type="l", col='green', lwd=2)

lines(task1_data$Week_Num, 
     task1_data$Video,  type="l", col='red', lwd=2)


summary(task1_data$roi)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.02617 0.02632 0.02685 0.02709 0.02741 0.02996


# diminish of return
plot(task1_data$cumulative_kpi, task1_data$per_unit_spend)

loess_dim_return <- loess(task1_data$per_unit_spend ~ task1_data$cumulative_kpi)
plot(task1_data$cumulative_kpi, 
     task1_data$per_unit_spend,
     type="l",
     col='springgreen4', 
     lwd=2,
     xlab="Cumulative KPI",
     ylab="Spend per Unit KPI in USD($)", 
     main="Per Unit Ads Spend Across Channels By Cumulative KPI (2017-2019)")

lines(predict(loess_dim_return), col='springgreen4', lwd=2)

plot(task1_data$cumulative_spend, task1_data$per_unit_spend)
plot(task1_data$Week_Num, task1_data$per_unit_spend)

plot(task1_data$Week_Num, task1_data$tot_spend)
plot(task1_data$Week_Num, task1_data$KPI)
plot(task1_data$Week_Num, task1_data$tot_spend)
length(task1_data$tot_spend)

head(task1_data)

# loess
lo <- loess(task1_data$KPI ~ task1_data$Week_Num)
plot(task1_data$Week_Num, task1_data$KPI)
lines(predict(lo), col='red', lwd=2)

lo0 <- loess(task1_data$tot_spend ~ task1_data$Week_Num)
plot(task1_data$Week_Num, task1_data$tot_spend)
lines(predict(lo0), col='navy', lwd=2)

lo1 <- loess(task1_data$TV ~ task1_data$Week_Num)
plot(task1_data$Week_Num, task1_data$TV)
lines(predict(lo1), col='orange', lwd=2)

lo2 <- loess(task1_data$Search ~ task1_data$Week_Num)
plot(task1_data$Week_Num, task1_data$Search)
lines(predict(lo2), col='green', lwd=2)

lo3 <- loess(task1_data$Social ~ task1_data$Week_Num)
plot(task1_data$Week_Num, task1_data$Social)
lines(predict(lo3), col='pink', lwd=2)

lo4 <- loess(task1_data$Video ~ task1_data$Week_Num)
plot(task1_data$Week_Num, task1_data$Video)
lines(predict(lo4), col='blue', lwd=2)

lo5 <- loess(task1_data$Display ~ task1_data$Week_Num)
plot(task1_data$Week_Num, task1_data$Display)
lines(predict(lo5), col='brown', lwd=2)

lo6 <- loess(task1_data$Radio ~ task1_data$Week_Num)
plot(task1_data$Week_Num, task1_data$Radio)
lines(predict(lo6), col='purple', lwd=2)


kpi_loess_line <- lo$fitted
tot_spend_loess_line <- lo0$fitted
tv_loess_line <- lo1$fitted
search_loess_line <- lo2$fitted
social_loess_line <- lo3$fitted
video_loess_line <- lo4$fitted
display_loess_line <- lo5$fitted
radio_loess_line <- lo6$fitted

loess_fitted <- data.frame(cbind(kpi_loess_line, tv_loess_line,
                                 search_loess_line, social_loess_line,
                                 video_loess_line, display_loess_line,
                                 radio_loess_line, tot_spend_loess_line))

write.table(loess_fitted, 'loess_fitted.csv', row.names = FALSE)

# first plot
scatter.smooth(x=task1_data$Week_Num, y=task1_data$KPI)
# add plot of second lowess line
lines(loess.smooth(x=task1_data$Week_Num, y=task1_data$Search))
lines(loess.smooth(x=task1_data$Week_Num, y=task1_data$Video), col="green")

library(ggplot2)
library(tidyr)

set.seed(123)
df <- data.frame("days"=1:25, "v1"=rnorm(25), "v2"=(rnorm(25)+0.1))

#Reshape data from wide to long
df2 <- gather(df,var,val,c(v1,v2))

ggplot(df2,aes(x = days, y = val)) +
  geom_point() +
  geom_smooth(aes(colour = var),se = F)

ggplot(df,aes(x = days, y = v1)) +
  geom_point() +  #Add scatter plot
  geom_smooth(aes(colour = 'v1'),se = F) +   #Add loess 1
  geom_smooth(aes(y = v2,colour = 'v2'),se = F) +  #Add loess 2... and so on
  scale_colour_discrete(name = 'Line',
                        breaks = c('v1','v2'),
                        labels = c('variable 1','variable 2'))    #Define legend

# tbats
kpi_tbats <- tbats(task1_data$KPI)
plot(kpi_tbats)
fc2 <- forecast(kpi_tbats, h=52)
plot(fc2, ylab="Number of KPI (subscription) per week")
decompose(kpi_tbats)

# observe x - y 
str(task1_data)
summary(task1_data)
plot(task1_data$KPI, task1_data$TV)
plot(task1_data$KPI, task1_data$Social)
plot(task1_data$KPI, task1_data$Search)
plot(task1_data$KPI, task1_data$Video)
plot(task1_data$KPI, task1_data$Display)
plot(task1_data$KPI, task1_data$Radio)
head(task1_data)
excl_columns_corr <- names(task1_data) %in% c("Date", "Week_Num")
corr_data <- task1_data[!excl_columns_corr]
head(corr_data)
cor(corr_data)

#Correlation:
Search > Video > Display > TV > Radio

# Cannibalization
# Radio is not good (why?) over the board negative
# Search and Social cannibalization -0.21691190
# Search and Display cannibalization -0.03171096

# Linear Regression:
model = lm(KPI ~ TV + Social + Search + Video + Display + Radio, data=task1_data)
summary(model)

# Linear Regression:
model1 = lm(KPI ~ TV + Social + Search + Video + Display, data=task1_data)
summary(model1)

# Linear Regression:
model2 = lm(KPI ~ Social + Search + Video + Display, data=task1_data)
summary(model2)

# Linear Regression:
model3 = lm(KPI ~ Search + Video, data=task1_data)
summary(model3)


# Read in data
wine <- read_csv("Desktop/wine.csv")
str(wine)
summary(wine)

plot(wine$AGST, wine$Price)
# Mean   :7.07
# Linear Regression (one variable)
model1 = lm(Price ~ AGST, data=wine)
summary(model1)

# y =  -3.4178 + 0.6351 AGST
# Multiple R-squared:  0.435,	Adjusted R-squared:  0.4105 

# Sum of Squared Errors
model1$residuals
SSE = sum(model1$residuals^2)
SSE # 5.734875

# Linear Regression (two variables)
model2 = lm(Price ~ AGST + HarvestRain, data=wine)
summary(model2)

# Multiple R-squared:  0.7074,	Adjusted R-squared:  0.6808 
# Sum of Squared Errors
SSE = sum(model2$residuals^2)
SSE # 2.970373

# Linear Regression (all variables)
model3 = lm(Price ~ AGST + HarvestRain + WinterRain + Age + FrancePop, data=wine)
summary(model3)

# Multiple R-squared:  0.8294,	Adjusted R-squared:  0.7845 
# Sum of Squared Errors
SSE = sum(model3$residuals^2)
SSE #1.732113


# Remove FrancePop
model4 = lm(Price ~ AGST + HarvestRain + WinterRain + Age, data=wine)
summary(model4)
# Multiple R-squared:  0.8286,	Adjusted R-squared:  0.7943 

# Correlations
cor(wine$WinterRain, wine$Price)
cor(wine$Age, wine$FrancePop)
plot(wine$Age, wine$FrancePop)
cor(wine)

# Remove Age and FrancePop
model5 = lm(Price ~ AGST + HarvestRain + WinterRain, data=wine)
summary(model5)


# Read in test set
wineTest = read.csv("Desktop/wine_test.csv")
str(wineTest)

# Make test set predictions
predictTest = predict(model4, newdata=wineTest)
predictTest

# Compute R-squared Out-of-Sample R^2
SSE = sum((wineTest$Price - predictTest)^2)
SST = sum((wineTest$Price - mean(wine$Price))^2)
1 - SSE/SST

