#3.1
library(readr)
Barley_price <- read_csv("~/Desktop/Barley_price.csv")
attach(Barley_price)
attach(Barley_production)
install.packages("tidyverse")
install.packages("dplyr")
library(dplyr)
#defining variables 
barleyp <- Barley_price %>% select(Year, "State", "State ANSI", Value)
barleyprice <-rename(barleyp, price = Value)

barleypr <- Barley_production %>% select(Year, "State", "State ANSI", Value)
barleyproduction<-rename(barleypr, production = Value)
finalbarley <- merge(barleyprice,barleyproduction, all.x = TRUE, all.y = TRUE)
finalbarley1 <- finalbarley%>% mutate(total = price * production)
library(dplyr)
newdata <- na.omit(finalbarley1)
finalb <- newdata %>% group_by(Year) %>% mutate(total_prod = sum(production)) %>% mutate(weight=total/total_prod)

library(plyr)
weight_mean <- ddply(finalb,.(State,Year),summarise, finalweight =weighted.mean(price,w = weight))

View(weight_mean)
library(lattice)
xyplot(finalweight ~ Year, groups = State, type = 'l', data = weight_mean, auto.key = list(columns = 5))

#3.2 Creating a summary table
newdata$State <- as.factor(newdata$State)

attach(newdata)
datastate <- rename(newdata$State)
states = c("IDAHO", "MINNESOTA", "MONTANA", "NORTH DAKOTA", "WYOMING")
newdata <- newdata %>% filter(State == states)
library(tidyr)
newdata %>% spread(Year,State) %>% select(Year == 1990:1999)
View(newdata)
meandata <- aggregate(x=newdata$production, by = list(newdata$State, newdata$Year), FUN=mean)
data_a <- subset(meandata, Group.2 >=1990, Group.2 < 2000)
mean_af <- aggregate(x=data_a$x/1000000, by = list(meandata$Group.1), FUN=mean)
names(mean_af)[2] <- "1990:1999"
names(mean_af)[1] <- "States"
data_b <- subset(meandata, Group.2 >=2000, Group.2 < 2010)
mean_bf <- aggregate(x=data_b$x/1000000, by = list(data_b$Group.1), FUN=mean)
names(mean_bf)[2] <- "2000:2009"
names(mean_bf)[1] <- "States"

data_c <- subset(meandata, Group.2 >=2010, Group.2 < 2017)
mean_cf <- aggregate(x=data_c$x/1000000, by = list(data_c$Group.1), FUN=mean)
names(mean_cf)[2] <- "2000:2017"
names(mean_cf)[1] <- "States"

table1 <- full_join(mean_af, mean_bf, by = c("States"))
final_table <- full_join(table1, mean_cf, by = c("States"))
View(final_table)
#4
#install.packages("plm")
library(plm)
#assigning state id #
paneldata <- na.omit(finalbarley)
paneldata$id<-as.numeric(factor(paneldata$State))
data.panel<-pdata.frame(paneldata, index = c("id", "year"))
pdim(data.panel)
unique(data.panel$year)
table(data.panel$year)
finalbarley
names(data.panel)
pdata1 <- aggregate(x=paneldata$production, by = list(paneldata$State, paneldata$Year), FUN=mean)
names(pdata1)[1] <- "States"
names(pdata1)[3] <- "Production"
names(pdata1)[2] <- "Year"
pdata2 <- aggregate(x=paneldata$price, by = list(paneldata$State, paneldata$Year), FUN=mean)
names(pdata2)[1] <- "States"
names(pdata2)[3] <- "Price"
names(pdata2)[2] <- "Year"
pdata <- merge(pdata1, pdata2, by = c("States", "Year"))
#install.packages("plm")
library(plm)
#assigning state id #
attach(pdata)
#assigning state id #
pdata$id<-as.numeric(factor(pdata$State))
data.panel<-pdata.frame(pdata, index = c("id", "Year"))
data.panel$logproduction = log(data.panel$Production)
fe<-plm(logproduction ~ Price, data = data.panel, index = c("Year"), model="within")
summary(fe)
fixef(fe)

write.csv(final_table, "summary.csv")
write.csv(data.panel, "data_panel.csv")
write.csv(weight_mean, "weight_data.csv")
