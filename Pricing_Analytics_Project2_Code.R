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
