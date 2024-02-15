remove(list = ls())

setwd("D:/Simon.UR/Spring A/MKT440 Pricing Analytics/")

demo <- read.csv("demo_P2.csv")
kiwi <- read.csv("kiwi_bubbles_P2.csv")

#4. Logit model without segmentation

Q1
install.packages("gmnl")
library(gmnl)

kiwi <- read.csv("kiwi_bubbles_data.csv")

# Drop rows with out-of-stock events
kiwi <- kiwi[!kiwi$price.MB == 99, ]

# Convert data to mlogit format
mlogitdata <- mlogit.data(kiwi, id = "id", varying = 4:7, choice = "choice", shape = "wide")

# Run MLE
mle <- gmnl(choice ~ price, data = mlogitdata)

# Summary of the estimated model
summary(mle)

Q2
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
