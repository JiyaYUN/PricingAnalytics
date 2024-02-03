library(lfe)
library(data.table)
#Set working space
rm(list = ls());
setwd("D:/Simon.UR/Spring A/MKT440 Pricing Analytics")
cardata=fread("cars.csv",stringsAsFactors = F)
irondata = fread("iron_ore.csv", stringsAsFactors = F)
summary(cardata)
cardata = na.omit(cardata)

cardata$ye = paste0('19', cardata$ye)
cardata$ye = as.integer(cardata$ye)
mergedata = merge(cardata, irondata, by.x = 'ye', by.y='year')
# 5 Control Variables and Fixed Effects
# Interpreting a log-log regression
summary(felm(formula = log(qu)~log(eurpr), data = cardata))
reg=felm(log(qu)~log(eurpr), data=cardata)
summary(reg)
bestmodel <- felm(log(qu) ~ log(eurpr)+cy+hp+wi+he+li+ac+avexr+avdcpr+
                       pop+avgwerival| 
                       factor(ma):factor(co)+factor(type):factor(model)+
                       factor(loc)+factor(frm), data=cardata)


bestmodel <- felm(log(qu) ~ log(eurpr)+avexr+pop+ac|factor(ye)+factor(loc)+factor(brand),data=cardata) #use this model
summary(bestmodel)

summary(bestmodel)
fe <- getfe(bestmodel)
feHondalengendLegend <- match('honda legend.legend',fe$idx)

coefbeta1=bestmodel$coefficients
pricespace=seq(0,50000,100)
fitted=(exp(feHondalengendLegend+feHondalengendLegend[1]*log(pricespace)+feHondalengendLegend[2]*mean(cardata$hp)))
plot(cardata$eurpr,cardata$qu, pch=1, main="Price vs Sales",
     xlab="Price", ylab="Sales", cex=2,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
lines(pricespace,fitted,col="blue",lwd=2)


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
regrival <- felm(log(qu) ~ log(avgurprrival) | factor(ye), data=cardata)
summary (regrival)


finalmodel <- felm(log(qu) ~ avexr+pop+ac+log(avgurprrival)|
                       factor(ye)+factor(loc)+factor(brand)|
                       (log(eurpr)~unit_value_98+we+le+wi+he)
                   , data=mergedata)
summary(finalmodel)

#1. The coefficient of log(avgeurprrival) represents the cross-price elasticity of demand.
#This coefficient measures the percentage change in the quantity of cars sold in response to each 1% change in average rival price.

#2. In the common business setting, this coefficient should be positive. Since the rival cars are substitutes for the target car,
#the increase in rival cars' prices will shift the demand towards more purchases of the target car.
#The estimated coefficient is positive, which resonates with our expectations.

#3. The estimated coefficient suggests a 1% change in average rival car price leads to a 1.8% change in the quantity sold of the target car,
#this represents a strong market elasticity, as the target car is considered a compelling substitute.
