library(readxl)
NFL_DATA <- read_xlsx("NFL DATA.xlsx")
colnames(NFL_DATA)
Middle50 <- subset(NFL_DATA$APY,NFL_DATA$APY>1033648&NFL_DATA$APY<4000000)
hist(Middle50)


MiddleAPY<- subset(NFL_DATA$APY,NFL_DATA$APY>2000000&NFL_DATA$APY<4000000)
hist(MiddleAPY)

TG<-subset(NFL_DATA$TotalGuaranteed,NFL_DATA$TotalGuaranteed>0&NFL_DATA$TotalGuaranteed<230000000)
hist(TG)

middle50TG<-subset(NFL_DATA$TotalGuaranteed,NFL_DATA$TotalGuaranteed>115980&NFL_DATA$TotalGuaranteed<7000000)
hist(middle50TG)

quantile(NFL_DATA$APY,probs = 0.25)
quantile(NFL_DATA$APY,probs = 0.75)
quantile(NFL_DATA$APY,probs = 0.50)

fivepercent<-five<-subset(NFL_DATA$TotalGuaranteed,NFL_DATA$TotalGuaranteed>500000&NFL_DATA$TotalGuaranteed<5000000)
quantile(NFL_Data$TotalGuaranteed,probs=0.75)
quantile(NFL_Data$TotalGuaranteed,probs=0.50)


install.packages("ggplot2")
library(ggplot2)
install.packages("tidyverse")
library(tidyverse)

nfl_data1 <- NFL_DATA
#%>% rename(percent_gt = "%Guaranteed")

scatter_nfl <- ggplot(nfl_data1, aes(x = percent_gt, y = APY))
scatter_nfl + geom_line() + geom_point() + ylim(1033648,4e+06  ) 
#- mid 50 APY
scatter_nfl + geom_line() + geom_point() + ylim(0,4e+06  ) 
#- 75% APY
scatter_nfl + geom_point() + ylim(1033648,1377500  )
#- 25-50 APY

scatter_nfl + geom_point() + ylim(1033648,1377500  )+xlim(0,0.25)

scatter_nfl + geom_point() + ylim(0, 4e+06 )+xlim(0,1)


scatter_nfl + geom_point() + coord_cartesian(xlim = c(0.0, 0.40), ylim = c(0,7e+06))
scatter_nfl + geom_point() + coord_cartesian(xlim = c(0.0, 0.40), ylim = c(115980,7e+06))
scatter_nfl + geom_point() + coord_cartesian(xlim = c(0.0, 1), ylim = c(115980,7e+06)) + geom_smooth(method = lm, se = F)
scatter_nfl + geom_point() + coord_cartesian(xlim = c(0.0, 0.25), ylim = c(115980,7e+06))

