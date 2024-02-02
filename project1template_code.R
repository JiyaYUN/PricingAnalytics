# Install the 'lfe' package
install.packages("lfe")
# Install the 'data.table' package
install.packages("data.table")
# Load the packages into the R session
library(lfe)
library(data.table)


#Set working space
rm(list = ls());
#gia's directory
setwd("D:/Simon.UR/Spring A/MKT440 Pricing Analytics")
#mengxi's directory
setwd("/Users/limengxi/Desktop/2024springA/pricing analytics/WK3/ae1")
cardata=fread("cars.csv",stringsAsFactors = F)
irondata = fread("iron_ore.csv", stringsAsFactors = F)
summary(cardata)
cardata = na.omit(cardata)

cardata$ye = paste0('19', cardata$ye)
cardata$ye = as.integer(cardata$ye)
mergedata = merge(cardata, irondata, by.x = 'ye', by.y='year')

# ------------

# Interpreting a log-log regression
summary(felm(formula = log(qu)~log(eurpr), data = cardata))
reg=felm(log(qu)~log(eurpr), data=cardata)
summary(reg)
bestmodel <- felm(log(qu) ~ log(eurpr)+cy+hp+wi+he+li+ac+avexr+avdcpr+
                  pop+avgwerival| 
                  factor(ma):factor(co)+factor(type):factor(model)+
                  factor(loc)+factor(frm), data=cardata)

bestmodel <- felm(log(qu) ~ log(eurpr)+hp+li+ac+avexr+avdcpr+
                      avdppr+pop+ngdp+rgdp+ergdp+engdpc+avgwerival| 
                      factor(type):factor(model)+
                      factor(loc), data=cardata)
summary(bestmodel)
fe <- getfe(bestmodel)
feHondalengendLegend <- match('honda legend.legend',fe$idx)

coefbeta1=bestmodel$coefficients
pricespace=seq(0,50000,100)
fitted=(exp(feHondalengendLegend+feHondalengendLegend[1]*log(pricespace)+feHondalengendLegend[2]*mean(cardata$hp)))
plot(cardata$eurpr,cardata$qu, pch=1, main="Price vs Sales",
     xlab="Price", ylab="Sales", cex=2,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
lines(pricespace,fitted,col="blue",lwd=2)

############################################################################################################## Gia

#trying IVs with two-stage least squares (2SLS) regression, seeing if two are correlated
#1st stage:
#find predicted values of the problematic variable (like price) using an instrument.
#2nd stage:
#to understand the impact of "log(eurpr)" on "log(qu)" after controlling for endogeneity via the IV


#1. trying out unit_value_98
summary(first_stage_unit_value_98 <- lm(log(eurpr) ~ unit_value_98, data=mergedata))
second_stage_unit_value_98=felm(log(qu)~1 | 0 | (log(eurpr)~unit_value_98), data=mergedata)
summary(second_stage_unit_value_98)
#good news! it's significant for both test with high F-statistics, unit_value_98 appears to be a 
#significant IV. However, there's a negative correlation between unit_value_98 and log(eurpr)
#meaning important control variables that influence both "Unit_Value_98" and "log(eurpr)" are 
#omitted from the model, we will add the control variables in the model later.


#2. trying out weight
first_stage_weight <- lm(log(eurpr) ~ we, data=mergedata)
summary(first_stage_weight)
second_stage_weight=felm(log(qu)~1 | 0 | (log(eurpr)~we), data=mergedata)
summary(second_stage_weight)
#good news! it's significant for both test with high F-statistics, first_stage_weight
#appears to be a significant IV.
#the weight of the car is related to its production cost and therefore its final 
#price, but does not directly affect the quantity demanded, except through the price.

#3. trying out length，width, and height, expecting the same dynamic as weight
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
#similar conclusion for four size-related measurements

#4. trying out cylinder volume
first_stage_cylinder <- lm(log(eurpr) ~ cy, data=mergedata)
summary(first_stage_cylinder)
second_stage_cylinder=felm(log(qu)~1 | 0 | (log(eurpr)~cy), data=mergedata)
summary(second_stage_cylinder)
#significant! 
#cars with larger cylinder volumes are generally more expensive, but the cylinder volume itself 
#does not directly influence how many cars are sold (the dependent variable), except through its 
#effect on the price.
#cylinder volume does not directly affect the demand for cars. This might be plausible if consumers 
#primarily consider factors like price, brand, or fuel efficiency, rather than cylinder volume, 
#when deciding to purchase a car.

#5. trying out four price index
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

#choosing destination related consumer and producer price index for IV because the quantity-price
#correlation coefficients are negative which are conventional, and the f-statistics are high.









############################################################################################################## Mengxi
#----------------------------------------------------------
#----------------------------------------------------------
#SECTION 1: Run regressions with control variables (X) and fixed effects using "felm" function

#  Standard syntax for the felm function is as follows.
#  felm(log(Q) ~ log(P) + X | factor(name of the categorical variables for fixed effects), data source)  

#With no fixed effects, the syntax is identical to the default "lm" function.
#Your colleague's regression
reg=felm(log(qu)~log(eurpr), data=cardata)
summary(reg)

#Example of adding controls: run a log-log regression with "year" as 
#fixed effect and "li" as a control variable
reg2=felm(log(qu) ~ log(eurpr)+li | factor(ye), data=cardata)
summary(reg2)

#We can add multiple FE separately, or interact them. This is how we add two separate fixed effects
#one for each year (same value across all car models) and the other for each car model (same value across all years)
reg3_1=felm(log(qu)~log(eurpr)+li | factor(ye)+factor(co), data=cardata)
summary(reg3_1)

#This is how we add interacting fixed effects for "each year-car model combination" - use ":" instead of "+".
#Note the difference between 3_1 and 3_2 (run both and check what FE is included using "getfe" below). Here
#each car model - year combination gets assigned a unique value of FE.
reg3_2=felm(log(qu)~log(eurpr)+li | factor(ye):factor(co), data=cardata)
summary(reg3_2)


#Note that unlike default "lm", "felm" function does not provide estimates of 
#the intercepts and fixed effects. To get those numbers, we use "getfe" function.

fe=getfe(reg3_2)

#getfe function produces all values for fixed effects as a list.

#Say we want to get the fixed effect value for year 1990. We need to
#find the right location of the list.

#Find where it is located - use "match" function.
idc <- match('90',fe$idx)
fe90=fe[idc,1]
#fe90 is the value of fixed effect for year 1990.

#If you included interactive fixed effects (reg3_2 above), you need
#to specify both the year and market to find the value of the corresponding
#fixed effect.
fe2=getfe(reg3_2)

#Say you want to find the value of FE for year 1990 in market 3.
idc <- match('90.3',fe2$idx)
fe90_3=fe[idc,1]

#Another example use of "getfe" function is available in topic 1 
#R code around line 200.


#For presenting results:
#Unfortunately, some shortcut functionalities to present results from the standard "lm" 
#won't work for the felm - like "predict" function won't work.
#Hence, drawing figures of felm outcome requires the use of generic plot functions. 

#An example:
#Take estimated coefficients from the regression above.
coef2=reg3_2$coefficients

#Take the space of prices to plot against the demand
pricespace=seq(0,50000,100)

#Calculate predicted demand at each price point, evaluated in year 1990 in market 3, for a 
#car with an average value of li.
fitted=(exp(fe90_3+coef2[1]*log(pricespace)+coef2[2]*mean(cardata$li)))

#Plot the line against the raw data.
plot(cardata$eurpr,cardata$qu, pch=1, main="Price vs Sales",
     xlab="Price", ylab="Sales", cex=2,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
lines(pricespace,fitted,col="blue",lwd=2)

#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------
#SECTION 2: Run an IV regression using "felm" function

#Examples of using an IV: 
# The syntax of felm function with an IV is as follows:

# felm(log(Q)~ X | factor(name of the categorical variable for FE) | (log(P)~ Z), data source) 

# "(log(P)~ Z)" part represents "we instrument log(P) with Z".
# Z is our IV for log(P). Importantly, if you instrument for log(P),
# we need to drop log(P) from the first part of the code.


#Run a log-log regression with "ye" as 
#fixed effect, "li" as a control variable and "we" as an
#IV for log(P). 
reg4=felm(log(qu)~li | factor(ye) | (log(eurpr)~we), data=cardata)
summary(reg4)

#Example of using an IV: if we don't include any X variable in
#an IV regression, simply place "1" in place of X.
reg5=felm(log(qu)~1 | factor(ye) | (log(eurpr)~we), data=cardata)
summary(reg5)

#Example of using an IV: if we don't include fixed effects in 
#an IV regression, place "0" in place of factor variable.
reg6=felm(log(qu)~li | 0 | (log(eurpr)~we), data=cardata)
summary(reg6)

#Example of using an IV: if we need to instrument for more than one
#variables, here's how.
#This is an example where we instrument "log(eurpr)" and "log(avgurprrival)" in
#our regression with instruments "we" and "do" (doesn't mean this is 
#the right combination). Note that if we want to
#instrument for two variables, we need at least two Z variables (generally, the
#number of Z variables needs to be equal to or larger than the number of variables
#that need to be instrumented).
reg7=felm(log(qu)~li | 0 | (log(eurpr)| log(avgurprrival) ~ we + do) ,data=cardata)
summary(reg7)

############################################################################################################## Vic
#----------------------------------------------------------
#----------------------------------------------------------
#Q7

regrival <- felm(log(qu) ~ log(avgurprrival) | factor(ye), data=cardata)
summary (regrival)

#1. The coefficient of log(avgeurprrival) represents the cross-price elasticity of demand.
#This coefficient measures the percentage change in the quantity of cars sold in response to each 1% change in average rival price.

#2. In the common business setting, this coefficient should be positive. Since the rival cars are substitutes for the target car,
#the increase in rival cars' prices will shift the demand towards more purchases of the target car.
#The estimated coefficient is positive, which resonates with our expectations.

#3. The estimated coefficient suggests a 1% change in average rival car price leads to a 3.4% change in the quantity sold of the target car,
#this represents a strong market elasticity, as the target car is considered a compelling substitute.

