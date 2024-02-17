remove(list = ls())
setwd("D:/Simon.UR/Spring A/MKT440 Pricing Analytics/")

#4. Logit model without segmentation
library(gmnl)
library(mlogit)
#Q4.1
kiwi <- read.csv("kiwi_bubbles_P2.csv")
# Drop rows with out-of-stock events
kiwi <- kiwi[!kiwi$price.KB == 99, ]
kiwi <- kiwi[!kiwi$price.KR == 99, ]
kiwi <- kiwi[!kiwi$price.MB == 99, ]

# Convert data to mlogit format
mlogitdata <- mlogit.data(kiwi, id = "id", varying = 4:7, choice = "choice", shape = "wide")

# Run MLE
mle <- gmnl(choice ~ price, data = mlogitdata)

# Summary of the estimated model
summary(mle)

# Interpretation of estimations
# Intercepts:
# We observe almost identical baseline popularity for all three products, 
# meaning all products are selected roughly at the same rate. It's because we are assuming homogeneous demand.

# price:
# The coefficient for price is -3.73793, which is statistically significant (p < 0.001). This indicates that, across all products, 
# as the price increases, the log odds of choosing any given product decreases. In simpler terms, higher prices are associated with a 
# lower probability of any of the products being chosen. 

#Q4.2
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

#Q4.3
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
para = c(4.25316, 4.36240, -3.73793, 4.20440)
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
plot_ly(x = pricespace[,1], y = pricespace[,2], z = as.numeric(profitmat), type = "scatter3d", mode = "markers",
             marker = list(color = as.numeric(profitmat), colorscale = c('#FFE1A1', '#683531'), showscale = TRUE)) %>%
     layout(scene = list(xaxis = list(title = "P^KB"), yaxis = list(autorange = "reversed", title = "P^KR"), zaxis = list(title = "Profit"))) %>%
     config(mathjax = 'cdn')

#5. Logit model with segmentation
demo <- fread("demo_P2.csv",stringsAsFactors = F)

################
#Q5.1###########
################

#clustering customers into 8 groups
demo_cluster = kmeans(x=demo[, 2:18], centers = 8, nstart = 1500)

#create a dataframe storing cluster id and match for each customer 
cluster_id = data.frame(id = demo$id)
cluster_id$cluster = demo_cluster$cluster
kiwi = merge(kiwi, cluster_id, by = "id", all.x = TRUE)

kiwi$cluster[is.na(kiwi$cluster)] = 9 #check N/A in cluster
?table
#check the share and number of every segment
N = 359
seg.share = c( table(demo_cluster$cluster), N - sum(table(demo_cluster$cluster))) / N

#create a dataframe to store coeffcient
coef.est = data.frame(segment = 1:8, intercept.KB = NA, intercept.KR = NA, 
                      intercept.MB = NA, price.coef = NA) 

#building multi logit model and put coefficient in coef.est
for (seg in 1:8) {
     kiwi.sub = subset(kiwi, cluster == seg)
     mlogitdata = mlogit.data(kiwi.sub,id="id",varying=4:7,choice="choice",shape="wide")
     mle = gmnl(choice ~ price, data = mlogitdata)
     coef.est[seg, 2:5] = mle$coefficients
}

#################
#Q5.2############
#################

#build demand function for KR, KR and MB
demandKB <- function(priceKB, priceKR, priceMB, para) {
     prob = exp(para[1] + para[4] * priceKB) /
          (1 + exp(para[1] + para[4] * priceKB) + exp(para[2] + para[4] * priceKR) +
                exp(para[3] + para[4] * priceMB))
     return(prob)
}

demandKR <- function(priceKB, priceKR, priceMB, para) {
     prob = exp(para[2] + para[4] * priceKR) /
          (1 + exp(para[1] + para[4] * priceKB) + exp(para[2] + para[4] * priceKR) +
                exp(para[3] + para[4] * priceMB))
     return(prob)
}

demandMB <- function(priceKB, priceKR, priceMB, para) {
     prob = exp(para[3] + para[4] * priceMB) /
          (1 + exp(para[1] + para[4] * priceKB) + exp(para[2] + para[4] * priceKR) +
                exp(para[3] + para[4] * priceMB))
     return(prob)
}


kiwiPriceDF <- data.frame(cluster = 1:8,
                          probKB = NA,
                          probKR = NA,
                          probMB = NA,
                          sum_productKB  = NA,
                          sum_productKR  = NA,
                          sum_productMB  = NA,
                          sum_productKBKR = NA,
                          sum_productKBMB = NA,
                          sum_productKRMB = NA)

for (i in 1:8) {
     kiwi_cluster <- kiwi[kiwi$cluster == i, ]
     
     mean_price_KB <- mean(kiwi_cluster$price.KB, na.rm = TRUE)
     mean_price_KR <- mean(kiwi_cluster$price.KR, na.rm = TRUE)
     mean_price_MB <- mean(kiwi_cluster$price.MB, na.rm = TRUE)
     
     para <- coef.est[i, 2:5]
     
     prob_KB <- demandKB(mean_price_KB, mean_price_KR, mean_price_MB, para)
     prob_KR <- demandKR(mean_price_KB, mean_price_KR, mean_price_MB, para)  # Corrected
     prob_MB <- demandMB(mean_price_KB, mean_price_KR, mean_price_MB, para)  # Corrected
     
     kiwiPriceDF$probKB[i] <- prob_KB
     kiwiPriceDF$probKR[i] <- prob_KR
     kiwiPriceDF$probMB[i] <- prob_MB
}
kiwiPriceDF$probKB <- as.numeric(kiwiPriceDF$probKB)
kiwiPriceDF$probKR <- as.numeric(kiwiPriceDF$probKR)
kiwiPriceDF$probMB <- as.numeric(kiwiPriceDF$probMB)

kiwiPriceDF$aggrprobKB <- NA
kiwiPriceDF$aggrprobKR <- NA
kiwiPriceDF$aggrprobMB <- NA

for (i in 1:8) {
     kiwiPriceDF$aggrprobKB[i] <- 1000 * seg.share[i] * kiwiPriceDF$probKB[i]
     kiwiPriceDF$aggrprobKR[i] <- 1000 * seg.share[i] * kiwiPriceDF$probKR[i]
     kiwiPriceDF$aggrprobMB[i] <- 1000 * seg.share[i] * kiwiPriceDF$probMB[i]
}
aggrprobKB = sum(kiwiPriceDF$aggrprobKB)
aggrprobKR = sum(kiwiPriceDF$aggrprobKR)
aggrprobMB = sum(kiwiPriceDF$aggrprobMB)

for (i in 1:8){
     kiwiPriceDF$sum_productKB[i] <- seg.share[i] * para[4] * kiwiPriceDF$probKB[i] * (1 - kiwiPriceDF$probKB[i])
     kiwiPriceDF$sum_productKR[i] <- seg.share[i] * para[4] * kiwiPriceDF$probKR[i] * (1 - kiwiPriceDF$probKR[i])
     kiwiPriceDF$sum_productMB[i] <- seg.share[i] * para[4] * kiwiPriceDF$probMB[i] * (1 - kiwiPriceDF$probMB[i])
}


kiwiPriceDF$sum_productKB <- as.numeric(kiwiPriceDF$sum_productKB)
kiwiPriceDF$sum_productKR <- as.numeric(kiwiPriceDF$sum_productKR)
kiwiPriceDF$sum_productMB <- as.numeric(kiwiPriceDF$sum_productMB)

elasticity_k_KB <- -mean_price_KB / aggrprobKB * sum(kiwiPriceDF$sum_productKB)
elasticity_k_KR <- -mean_price_KR / aggrprobKR * sum(kiwiPriceDF$sum_productKR)
elasticity_k_MB <- -mean_price_MB / aggrprobMB * sum(kiwiPriceDF$sum_productMB)

for (i in 1:8){
     kiwiPriceDF$sum_productKBKR[i] <- seg.share[i] * para[4] * kiwiPriceDF$probKB[i] * kiwiPriceDF$probKR[i]
     kiwiPriceDF$sum_productKBMB[i] <- seg.share[i] * para[4] * kiwiPriceDF$probKB[i] * kiwiPriceDF$probMB[i]
     kiwiPriceDF$sum_productKRMB[i] <- seg.share[i] * para[4] * kiwiPriceDF$probKR[i] * kiwiPriceDF$probMB[i]
}

kiwiPriceDF$sum_productKBKR <- as.numeric(kiwiPriceDF$sum_productKBKR)
kiwiPriceDF$sum_productKBMB <- as.numeric(kiwiPriceDF$sum_productKBMB)
kiwiPriceDF$sum_productKRMB <- as.numeric(kiwiPriceDF$sum_productKRMB)

cross_elas_k_KBKR <- -mean_price_KR / aggrprobKB * sum(kiwiPriceDF$sum_productKBKR)
cross_elas_k_KBMB <- -mean_price_KB / aggrprobKB * sum(kiwiPriceDF$sum_productKBMB)
cross_elas_k_KRMB <- -mean_price_KR / aggrprobKR * sum(kiwiPriceDF$sum_productKRMB)

##############
#Q5.4#########
##############
#donot launching KB
uc=0.5
pricespace <- seq(from = 0.8, to = 5, by = 0.1) 
total_profitswithoutKB <- numeric(length(pricespace))
demandKRwithoutKB <- function(priceKR, priceMB, para) {
     prob = exp(para[2] + para[4] * priceKR) /
          (1 + exp(para[2] + para[4] * priceKR) +
                exp(para[3] + para[4] * priceMB))
     return(prob)
}
for (price_index in 1:length(pricespace)) {
     price <- pricespace[price_index]
     total_profit_at_price <- 0
     for (i in 1:8) {
          para <- coef.est[i, 2:5]
          demand_KR <- demandKRwithoutKB(price, 1.43, para)
          profit_at_price <- 1000 * seg.share[i] * demand_KR * (price - uc)
          total_profit_at_price <- total_profit_at_price + profit_at_price
     }
     total_profitswithoutKB[price_index] <- total_profit_at_price
}
optimal_price_index <- which.max(total_profitswithoutKB)
optimal_KR_pricewithoutKB <- pricespace[optimal_price_index]
optimal_KR_profitwithoutKB <- total_profitswithoutKB[optimal_price_index]

#launching KB
# Assuming pricespace_KB is defined similarly to pricespace for KR
pricespace_KB <- seq(0.8, 2.5, by = 0.1)

# Initialize a matrix or dataframe to store profits for each price combination
profit_matrix <- expand.grid(KR_Price = pricespace, KB_Price = pricespace_KB)
profit_matrix$Total_Profit <- 0

# Nested loops to iterate over price combinations
for (KR_index in 1:length(pricespace)) {
     for (KB_index in 1:length(pricespace_KB)) {
          KR_price <- pricespace[KR_index]
          KB_price <- pricespace_KB[KB_index]
          total_profit <- 0
          
          for (i in 1:8) {
               para <- coef.est[i, 2:5]
               demand_KR <- demandKR(KB_price, KR_price, 1.43, para)
               demand_KB <- demandKB(KB_price, KR_price, 1.43, para)
               profit_KR <- 1000 * seg.share[i] * demand_KR * (KR_price - uc)
               profit_KB <- 1000 * seg.share[i] * demand_KB * (KB_price - uc)
               total_profit <- total_profit + profit_KR + profit_KB
          }
          
          # Find the index in profit_matrix that matches the current KR and KB prices
          index <- which(profit_matrix$KR_Price == KR_price & profit_matrix$KB_Price == KB_price)
          profit_matrix$Total_Profit[index] <- total_profit
     }
}

# Find the optimal price combination
optimal_combination <- profit_matrix[which.max(profit_matrix$Total_Profit), ]
optimal_KR_price <- optimal_combination$KR_Price
optimal_KB_price <- optimal_combination$KB_Price

#6. Understanding strategic responses
#1. First, solve Mango’s optimal pricing problem, given that Kiwi’s price is the one you set from the previous section. What is the new price of MB?
# Assuming pricespace_MB is defined similarly to pricespace for KR
pricespace_MB <- seq(0.8, 2.5, by = 0.1)
pricespace <- seq(from = 0.8, to = 5, by = 0.01) 
total_profits <- numeric(length(pricespace))

for (price_index in 1:length(pricespace)) {
    price <- pricespace[price_index]
    total_profit_at_price <- 0
    for (i in 1:8) {
        para <- coef.est[i, 2:5]
        demand_MB <- demandMB(1.1, 1.2, price, para)
        profit_at_price <- 1000 * seg.share[i] * demand_MB * (price - uc)
        total_profit_at_price <- total_profit_at_price + profit_at_price
    }
    total_profits[price_index] <- total_profit_at_price
}
optimal_price_index <- which.max(total_profits)
optimal_MB_price0 <- pricespace[optimal_price_index]
optimal_MB_profit0 <- total_profits[optimal_price_index]
#The optimal_MB_price0 is 0.96,and the profit is 139.5803.

#2.Set prices for KR and KB to respond to the new price of Mango Bubble
# Assuming pricespace_KB is defined similarly to pricespace for KR
pricespace_KB <- seq(0.8, 2.5, by = 0.1)
# Initialize a matrix or dataframe to store profits for each price combination
profit_matrix <- expand.grid(KR_Price = pricespace, KB_Price = pricespace_KB)
profit_matrix$Total_Profit <- 0
# Nested loops to iterate over price combinations
for (KR_index in 1:length(pricespace)) {
    for (KB_index in 1:length(pricespace_KB)) {
        KR_price <- pricespace[KR_index]
        KB_price <- pricespace_KB[KB_index]
        total_profit <- 0
        
        for (i in 1:8) {
            para <- coef.est[i, 2:5]
            demand_KR <- demandKR(KB_price, KR_price, 0.96, para)
            demand_KB <- demandKB(KB_price, KR_price, 0.96, para)
            profit_KR <- 1000 * seg.share[i] * demand_KR * (KR_price - uc)
            profit_KB <- 1000 * seg.share[i] * demand_KB * (KB_price - uc)
            total_profit <- total_profit + profit_KR + profit_KB
        }
        
        # Find the index in profit_matrix that matches the current KR and KB prices
        index <- which(profit_matrix$KR_Price == KR_price & profit_matrix$KB_Price == KB_price)
        profit_matrix$Total_Profit[index] <- total_profit
    }
}

# Find the optimal price combination
optimal_combination0 <- profit_matrix[which.max(profit_matrix$Total_Profit), ]
optimal_KR_price0 <- optimal_combination0$KR_Price
optimal_KB_price0 <- optimal_combination0$KB_Price
#The KR_Price is 1.09, KB_Price is 1,Total Profit is 212.5706.

#3.Repeat the previous two steps iteratively, until neither Kiwi nor Mango has an incentive to set a different price 
#The first time
pricespace_MB <- seq(0.8, 2.5, by = 0.1)
pricespace <- seq(from = 0.8, to = 5, by = 0.01) 
total_profits <- numeric(length(pricespace))
for (price_index in 1:length(pricespace)) {
    price <- pricespace[price_index]
    total_profit_at_price <- 0
    for (i in 1:8) {
        para <- coef.est[i, 2:5]
        demand_MB <- demandMB(1, 1.09, price, para)
        profit_at_price <- 1000 * seg.share[i] * demand_MB * (price - uc)
        total_profit_at_price <- total_profit_at_price + profit_at_price
    }
    total_profits[price_index] <- total_profit_at_price
}
optimal_price_index <- which.max(total_profits)
optimal_MB_price1 <- pricespace[optimal_price_index]
optimal_MB_profit1 <- total_profits[optimal_price_index]
#The first time: Opt price for MB is 0.92,the profit is 116.8767

# Initialize a matrix or dataframe to store profits for each price combination
pricespace_KB <- seq(0.8, 2.5, by = 0.1)
profit_matrix <- expand.grid(KR_Price = pricespace, KB_Price = pricespace_KB)
profit_matrix$Total_Profit <- 0

# Nested loops to iterate over price combinations
for (KR_index in 1:length(pricespace)) {
    for (KB_index in 1:length(pricespace_KB)) {
        KR_price <- pricespace[KR_index]
        KB_price <- pricespace_KB[KB_index]
        total_profit <- 0
        
        for (i in 1:8) {
            para <- coef.est[i, 2:5]
            demand_KR <- demandKR(KB_price, KR_price, 0.92, para)
            demand_KB <- demandKB(KB_price, KR_price, 0.92, para)
            profit_KR <- 1000 * seg.share[i] * demand_KR * (KR_price - uc)
            profit_KB <- 1000 * seg.share[i] * demand_KB * (KB_price - uc)
            total_profit <- total_profit + profit_KR + profit_KB
        }
        
        # Find the index in profit_matrix that matches the current KR and KB prices
        index <- which(profit_matrix$KR_Price == KR_price & profit_matrix$KB_Price == KB_price)
        profit_matrix$Total_Profit[index] <- total_profit
    }
}

# Find the optimal price combination
optimal_combination1 <- profit_matrix[which.max(profit_matrix$Total_Profit), ]
optimal_KR_price1 <- optimal_combination1$KR_Price
optimal_KB_price1 <- optimal_combination1$KB_Price
#The first time KR_Price is 1.08, KB_Price is 1, Total_Profit is 202.0056

#The second time
pricespace_MB <- seq(0.8, 2.5, by = 0.01)
uc=0.5
pricespace <- seq(from = 0.8, to = 5, by = 0.01) 
total_profits <- numeric(length(pricespace))

for (price_index in 1:length(pricespace)) {
    price <- pricespace[price_index]
    total_profit_at_price <- 0
    for (i in 1:8) {
        para <- coef.est[i, 2:5]
        demand_MB <- demandMB(1, 1.08, price, para)
        profit_at_price <- 1000 * seg.share[i] * demand_MB * (price - uc)
        total_profit_at_price <- total_profit_at_price + profit_at_price
    }
    total_profits[price_index] <- total_profit_at_price
}
optimal_price_index <- which.max(total_profits)
optimal_MB_price2 <- pricespace[optimal_price_index]
optimal_MB_profit2 <- total_profits[optimal_price_index]
#The Second time: Opt price for MB is still 0.92, profit is 116.8767.
# Initialize a matrix or dataframe to store profits for each price combination
pricespace_KB <- seq(0.9, 1.1, by = 0.01)
profit_matrix <- expand.grid(KR_Price = pricespace, KB_Price = pricespace_KB)
profit_matrix$Total_Profit <- 0

# Nested loops to iterate over price combinations
for (KR_index in 1:length(pricespace)) {
    for (KB_index in 1:length(pricespace_KB)) {
        KR_price <- pricespace[KR_index]
        KB_price <- pricespace_KB[KB_index]
        total_profit <- 0
        
        for (i in 1:8) {
            para <- coef.est[i, 2:5]
            demand_KR <- demandKR(KB_price, KR_price, 0.92, para)
            demand_KB <- demandKB(KB_price, KR_price, 0.92, para)
            profit_KR <- 1000 * seg.share[i] * demand_KR * (KR_price - uc)
            profit_KB <- 1000 * seg.share[i] * demand_KB * (KB_price - uc)
            total_profit <- total_profit + profit_KR + profit_KB
        }
        
        # Find the index in profit_matrix that matches the current KR and KB prices
        index <- which(profit_matrix$KR_Price == KR_price & profit_matrix$KB_Price == KB_price)
        profit_matrix$Total_Profit[index] <- total_profit
    }
}

# Find the optimal price combination
optimal_combination2 <- profit_matrix[which.max(profit_matrix$Total_Profit), ]
optimal_KR_price2 <- optimal_combination2$KR_Price
optimal_KB_price2 <- optimal_combination2$KB_Price
#The Second time KR_Price is 1.08, KB_Price is 0.99 Total_Profit is 202.0099.

#The Third time
pricespace_MB <- seq(0.8, 2.5, by = 0.01)
uc=0.5
pricespace <- seq(from = 0.8, to = 5, by = 0.01) 
total_profits <- numeric(length(pricespace))

for (price_index in 1:length(pricespace)) {
    price <- pricespace[price_index]
    total_profit_at_price <- 0
    for (i in 1:8) {
        para <- coef.est[i, 2:5]
        demand_MB <- demandMB(0.99, 1.08, price, para)
        profit_at_price <- 1000 * seg.share[i] * demand_MB * (price - uc)
        total_profit_at_price <- total_profit_at_price + profit_at_price
    }
    total_profits[price_index] <- total_profit_at_price
}
optimal_price_index <- which.max(total_profits)
optimal_MB_price3 <- pricespace[optimal_price_index]
optimal_MB_profit3 <- total_profits[optimal_price_index]
#The Third time: Opt price for MB is still 0.92, profit is 115.6046.
pricespace_KB <- seq(0.9, 1.1, by = 0.01)
profit_matrix <- expand.grid(KR_Price = pricespace, KB_Price = pricespace_KB)
profit_matrix$Total_Profit <- 0

# Nested loops to iterate over price combinations
for (KR_index in 1:length(pricespace)) {
    for (KB_index in 1:length(pricespace_KB)) {
        KR_price <- pricespace[KR_index]
        KB_price <- pricespace_KB[KB_index]
        total_profit <- 0
        
        for (i in 1:8) {
            para <- coef.est[i, 2:5]
            demand_KR <- demandKR(KB_price, KR_price, 0.92, para)
            demand_KB <- demandKB(KB_price, KR_price, 0.92, para)
            profit_KR <- 1000 * seg.share[i] * demand_KR * (KR_price - uc)
            profit_KB <- 1000 * seg.share[i] * demand_KB * (KB_price - uc)
            total_profit <- total_profit + profit_KR + profit_KB
        }
        
        # Find the index in profit_matrix that matches the current KR and KB prices
        index <- which(profit_matrix$KR_Price == KR_price & profit_matrix$KB_Price == KB_price)
        profit_matrix$Total_Profit[index] <- total_profit
    }
}

# Find the optimal price combination
optimal_combination3 <- profit_matrix[which.max(profit_matrix$Total_Profit), ]
optimal_KR_price3 <- optimal_combination3$KR_Price
optimal_KB_price3 <- optimal_combination3$KB_Price
#The Third time KR_Price is 1.08, KB_Price is 0.99, Total_Profit is 202.0099.
#Now neither Kiwi nor Mango has an incentive to set a different price (you can be as accurate as one cent, but no need to be more accurate than that). 
#These prices are the new “equilibrium price”. 
#The KB price is 0.99,The KR price is 1.08.Total_Profit for Kiwi is 202.0099.
#The MB price is 0.92,profit for Mango is 115.6046.




