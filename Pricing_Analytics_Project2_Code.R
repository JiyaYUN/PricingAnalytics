remove(list = ls())

setwd("D:/Simon.UR/Spring A/MKT440 Pricing Analytics/")

demo <- read.csv("demo_P2.csv")
kiwi <- read.csv("kiwi_bubbles_P2.csv")

#4. Logit model without segmentation

#Q1

#Q4.1
install.packages("gmnl")
library(gmnl)
install.packages("mlogit")
library(mlogit)

kiwi <- read.csv("kiwi_bubbles_data.csv")

# Drop rows with out-of-stock events
kiwi <- kiwi[!kiwi$price.MB == 99, ]

# Convert data to mlogit format
mlogitdata <- mlogit.data(kiwi, id = "id", varying = 4:7, choice = "choice", shape = "wide")

# Run MLE
mle <- gmnl(choice ~ price, data = mlogitdata)

# Summary of the estimated model
summary(mle)

# Interpretation of estimations
#Intercepts:
# We observe almost identical baseline popularity for all three products, 
# meaning all products are selected roughly at the same rate. It's because we are assuming homogeneous demand.
#KB as an example:
#The coefficient 4.25316 for the KB (intercept) in the logit model is a log-odds ratio. 
# To interpret its magnitude in a more intuitive way,we need to convert log-odds to odds, and then to probabilities
# The odds of choosing KB over not buying anything, when all other factors are held constant, are approximately 70.33 to 1.
# This means that the likelihood of a consumer choosing KB is 70.33 times higher than the likelihood of them not buying anything.
# The probability of a consumer choosing the product KB over the base category is approximately 98.6%. This suggests that, in the 
# absence of any other influences, there is a very high baseline probability that a consumer will choose KB compared to not purchasing at all 
#this applies to all other products. The probabilities of a consumer choosing one of the three products are all very high (over 95%).
#These high probabilities are relative to the base category which is the choice of not purchasing, the model suggests that once the 
# decision to purchase is made, there is a high probability that the purchase will be one of the three products.

# price:
# The coefficient for price is -3.73793, which is statistically significant (p < 0.001). This indicates that, across all products, 
# as the price increases, the log odds of choosing any given product decreases. In simpler terms, higher prices are associated with a 
# lower probability of any of the products being chosen. 

#Q2
# Function to calculate demand probability
demand <- function(price_KB, price_KR, price_MB, beta0_KB, beta0_KR, beta0_MB, beta1) {
    prob_KB <- exp(beta0_KB + beta1 * price_KB) /
        (1 + exp(beta0_KB + beta1 * price_KB) +
             exp(beta0_KR + beta1 * price_KR) +
             exp(beta0_MB + beta1 * price_MB))
    
    prob_KR <- exp(beta0_KR + beta1 * price_KR) /
        (1 + exp(beta0_KB + beta1 * price_KB) +
             exp(beta0_KR + beta1 * price_KR) +
             exp(beta0_MB + beta1 * price_MB))
    
    prob_MB <- exp(beta0_MB + beta1 * price_MB) /
        (1 + exp(beta0_KB + beta1 * price_KB) +
             exp(beta0_KR + beta1 * price_KR) +
             exp(beta0_MB + beta1 * price_MB))
    
    return(c(prob_KB, prob_KR, prob_MB))
}
# Average prices observed in the data
avg_price_KB <- mean(kiwi$price.KB)
avg_price_KR <- mean(kiwi$price.KR)
avg_price_MB <- mean(kiwi$price.MB)

# Estimated coefficients from the model
beta0_KB <- coef(mle)[1]
beta0_KR <- coef(mle)[2]
beta0_MB <- coef(mle)[3]
beta1 <- coef(mle)[4]

# Calculate own-price elasticities
own_elasticity_KB <- -beta1 * avg_price_KB * (1 - demand(avg_price_KB, avg_price_KR, avg_price_MB, beta0_KB, beta0_KR, beta0_MB, beta1)[1])
own_elasticity_KR <- -beta1 * avg_price_KR * (1 - demand(avg_price_KB, avg_price_KR, avg_price_MB, beta0_KB, beta0_KR, beta0_MB, beta1)[2])
own_elasticity_MB <- -beta1 * avg_price_MB * (1 - demand(avg_price_KB, avg_price_KR, avg_price_MB, beta0_KB, beta0_KR, beta0_MB, beta1)[3])
# Create a data frame with product names and own-price elasticities
own_elasticities <- data.frame(
    Product = c("KB", "KR", "MB"),
    Own_Price_Elasticity = c(own_elasticity_KB, own_elasticity_KR, own_elasticity_MB)
)

# Calculate cross-price elasticities
cross_elasticity_KB_KR <- -beta1 * avg_price_KR * demand(avg_price_KB, avg_price_KR, avg_price_MB, beta0_KB, beta0_KR, beta0_MB, beta1)[2]
cross_elasticity_KB_MB <- -beta1 * avg_price_MB * demand(avg_price_KB, avg_price_KR, avg_price_MB, beta0_KB, beta0_KR, beta0_MB, beta1)[3]
cross_elasticity_KR_KB <- -beta1 * avg_price_KB * demand(avg_price_KB, avg_price_KR, avg_price_MB, beta0_KB, beta0_KR, beta0_MB, beta1)[1]
cross_elasticity_KR_MB <- -beta1 * avg_price_MB * demand(avg_price_KB, avg_price_KR, avg_price_MB, beta0_KB, beta0_KR, beta0_MB, beta1)[3]
cross_elasticity_MB_KB <- -beta1 * avg_price_KB * demand(avg_price_KB, avg_price_KR, avg_price_MB, beta0_KB, beta0_KR, beta0_MB, beta1)[1]
cross_elasticity_MB_KR <- -beta1 * avg_price_KR * demand(avg_price_KB, avg_price_KR, avg_price_MB, beta0_KB, beta0_KR, beta0_MB, beta1)[2]
# Create a data frame for cross-price elasticities
cross_elasticities<- data.frame(
    Cross = c("KB_KR", "KB_MB", "KR_KB", "KR_MB", "MB_KB", "MB_KR"),
    Elasticity = c(
        cross_elasticity_KB_KR,
        cross_elasticity_KB_MB,
        cross_elasticity_KR_KB,
        cross_elasticity_KR_MB,
        cross_elasticity_MB_KB,
        cross_elasticity_MB_KR
    )
)

#Q3

install.packages("plotly")
library(plotly)

# Demand function incorporating KB, KR, and a fixed price for MB
demand <- function(priceKB, priceKR, priceMB_fixed, para) {
  numKB = exp(para[1] + para[3] * priceKB)
  numKR = exp(para[2] + para[3] * priceKR)
  numMB = exp(para[4] + para[3] * priceMB_fixed)
  denom = 1 + numKB + numKR + numMB
  probKB = numKB / denom
  probKR = numKR / denom
  return(c(probKB = probKB, probKR = probKR))
}

# Profit function for KB and KR, considering their prices and the fixed price of MB
profit <- function(priceKB, priceKR, priceMB_fixed, para) {
  unit_cost = 0.5
  market_size = 1000
  demands = demand(priceKB, priceKR, priceMB_fixed, para)
  profitKB = (priceKB - unit_cost) * demands['probKB'] * market_size
  profitKR = (priceKR - unit_cost) * demands['probKR'] * market_size
  return(profitKB + profitKR)
}

# Parameters including intercepts for KB, KR, price sensitivity, and intercept for MB
para = c(4.25316, 4.36240, -3.73793, 4.20440) # Adjust these based on your model's output
price_MB_fixed = 1.43

# Define a range of prices to examine for KB and KR
aux = seq(0.5, 3, by = 0.01)
pricespace = expand.grid(KB_Price = aux, KR_Price = aux)

# Compute profit for each combination of KB and KR prices
profitmat = numeric(nrow(pricespace))
for (i in 1:nrow(pricespace)) {
  profitmat[i] = profit(pricespace[i, "KB_Price"], pricespace[i, "KR_Price"], price_MB_fixed, para)
}

# Find the combination of prices that maximizes profit
max_profit_index = which.max(profitmat)
optimal_price_KB = pricespace[max_profit_index, "KB_Price"]
optimal_price_KR = pricespace[max_profit_index, "KR_Price"]
max_profit = max(profitmat)


# Visualize the results with a 3D scatter plot
p <- plot_ly(x = pricespace[,1], y = pricespace[,2], z = as.numeric(profitmat), type = "scatter3d", mode = "markers",
             marker = list(color = as.numeric(profitmat), colorscale = c('#FFE1A1', '#683531'), showscale = TRUE)) %>%
  layout(scene = list(xaxis = list(title = "P^KB"), yaxis = list(autorange = "reversed", title = "P^KR"), zaxis = list(title = "Profit"))) %>%
  config(mathjax = 'cdn')

# Display the plot
p




#5. Logit model with segmentation

#clustering customers into 10 groups and I will try different centers
demo_cluster = kmeans(x=demo[, 2:9], centers = 5, nstart = 1500)

#match cluster for each customer 
cluster_id = data.frame(id = demo$id)
cluster_id$cluster = demo_cluster$cluster
demo = merge(demo, cluster_id, by = "id", all.x = T)

#Data cleaning
demo = demo[!(demo$price.KB==99),]
demo = demo[!(demo$price.KR==99),]
demo = demo[!(demo$price.MB==99),]

#segment share
seg.share = c( table(demo_cluster$cluster),283 - sum(table(demo_cluster$cluster))) / 283	#N???

#multilogit model

#1.有KB时KR,MB的optimal price和profit
#2.排除kb时kr，mb的~~
#3.把mb=1.43代入demand function算kr的demand
#4.是不是跟前面的positioning投放在哪个segment一致

#6. Understanding strategic responses
