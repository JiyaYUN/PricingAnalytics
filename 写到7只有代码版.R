#loading packages
library(lfe)
library(data.table)
#Set working space
rm(list = ls());
setwd("D:/Simon.UR/Spring A/MKT440 Pricing Analytics")
#loading data
cardata=fread("cars.csv",stringsAsFactors = F)
irondata = fread("iron_ore.csv", stringsAsFactors = F)
summary(cardata)
#cleaning NA
cardata = na.omit(cardata)
#combining 2 data.table
cardata$ye = paste0('19', cardata$ye)
cardata$ye = as.integer(cardata$ye)
mergedata = merge(cardata, irondata, by.x = 'ye', by.y='year')

#4 Interpreting a log-log regression
reg=felm(log(qu)~log(eurpr), data=cardata)
summary(reg)

#5 Control Variables and Fixed Effects
#adding avexr, pop and ac as control variables, year, location and brand as fixed effects
bestmodel <- felm(log(qu) ~ log(eurpr)+avexr+pop+ac|factor(ye)+factor(loc)+factor(brand),data=cardata)
summary(bestmodel)


# 6 Instrumental Variables

ivmodel <- felm(log(qu) ~ avexr+pop+ac|
                       factor(ye)+factor(loc)+factor(brand)|
                       (log(eurpr)~unit_value_98+we+le+wi+he+cy)
                   , data=mergedata)
summary(ivmodel)

#(1) unit_value_98
summary(first_stage_unit_value_98 <- lm(log(eurpr) ~ unit_value_98, data=mergedata))
second_stage_unit_value_98=felm(log(qu)~1 | 0 | (log(eurpr)~unit_value_98), data=mergedata)
summary(second_stage_unit_value_98)
#(2) weight
first_stage_weight <- lm(log(eurpr) ~ we, data=mergedata)
summary(first_stage_weight)
second_stage_weight=felm(log(qu)~1 | 0 | (log(eurpr)~we), data=mergedata)
summary(second_stage_weight)
#(3) length，width, and height
first_stage_length <- lm(log(eurpr) ~ le, data=mergedata)
summary(first_stage_length)
second_stage_length=felm(log(qu)~1 | 0 | (log(eurpr)~le), data=mergedata)
summary(second_stage_length)

first_stage_width <- lm(log(eurpr) ~ wi, data=mergedata)
summary(first_stage_width)
second_stage_width=felm(log(qu)~1 | 0 | (log(eurpr)~wi), data=mergedata)
summary(second_stage_width)

first_stage_height <- lm(log(eurpr) ~ he, data=mergedata)
summary(first_stage_height)
second_stage_height=felm(log(qu)~1 | 0 | (log(eurpr)~he), data=mergedata)
summary(second_stage_height)
# (4) trying out cylinder volume
first_stage_cylinder <- lm(log(eurpr) ~ cy, data=mergedata)
summary(first_stage_cylinder)
second_stage_cylinder=felm(log(qu)~1 | 0 | (log(eurpr)~cy), data=mergedata)
summary(second_stage_cylinder)
#(5) trying out four price index
#avg consumer price index of destination country
first_stage_consumer_index_des <- lm(log(eurpr) ~ avdcpr, data=mergedata)
summary(first_stage_consumer_index_des)#positively correlated with price
second_stage_consumer_index_des=felm(log(qu)~1 | 0 | (log(eurpr)~avdcpr), data=mergedata)
summary(second_stage_consumer_index_des)#price-quantity relationship is negative

#avg producer price index of destination country
first_stage_producer_index_des <- lm(log(eurpr) ~ avdppr, data=mergedata)
summary(first_stage_producer_index_des)#positively correlated with price
second_stage_producer_index_des=felm(log(qu)~1 | 0 | (log(eurpr)~avdppr), data=mergedata)
summary(second_stage_producer_index_des)#price-quantity relationship is negative

#7. Cross-elasticities and competitive effects
#finalmodel including the prices of rival cars
finalmodel <- felm(log(qu) ~ avexr+pop+ac+log(avgurprrival)|
                       factor(ye)+factor(loc)+factor(brand)|
                       (log(eurpr)~unit_value_98+we+le+wi+he+cy)
                   , data=mergedata)
summary(finalmodel)

#8.Recovering Costs
#(1)use β1 = −0.2925 (your colleague’s estimate) to recover the costs
beta1 <- -0.2925 
mergedata$rivalcost = mergedata$eurpr*(1+beta1)/beta1
summary(mergedata)
mergedata$rivalcost

#(2)use β1 I obtained in the previous section to recover the costs which is -1.990
finalbeta1 <- -1.990
mergedata$finalrivalcost = mergedata$eurpr*(1+finalbeta1)/finalbeta1
summary(mergedata)
mergedata$finalrivalcost


