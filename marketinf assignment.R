## Assignment

library(readxl)    
library(tidyverse) 
library(cluster)     
library(openxlsx)    

Smart_Watch <-  read_excel(file.choose())
View(Smart_Watch)

is.null(Smart_Watch) ## check missing value

names(Smart_Watch) ## check columns names

## using Gower Distance becasue the dataset is mixed data ==> ordinal data and binomial data
Smart_Watch[, 8:10] <- lapply(Smart_Watch[, 8:10], as.factor)
df_gower <- daisy(Smart_Watch, metric = "gower")
gower_distance <- as.dist(df_gower)

# view(df_gower)

# CLUSTER DENDROGRAM
hc.w <- hclust(gower_distance, method = 'ward.D')
plot(hc.w, main = "Cluster Dendrogram", xlab = "Observations", ylab = "Height")

# DETERMINE THE OPTIMAL NUMBER OF CLUSTERS
x <- c(1:10)
sort_height <- sort(hc.w$height, decreasing = TRUE)
y <- sort_height[1:10]

plot(x, y, type = "b", main = "Elbow Plot", xlab = "Number of Clusters", ylab = "Height")

lines(x, y, col = "blue")
## 4 cluster

plot(hc.w, main = "Cluster Dendrogram", xlab = "Observations", ylab = "Height")
rect.hclust(hc.w, k = 4, border = 2:5) 

cluster <- cutree(hc.w, k = 4)
table(cluster)
# Add cluster assignments back to the original data
df_final <- cbind(Smart_Watch, cluster)
# Check the updated dataset
View(df_final)


# Q2


# CALCULATE SEGMENT SIZES
# Proportions of each cluster
proportions <- table(df_final$cluster) / nrow(df_final)
percentages <- proportions * 100
# Display segment sizes in percentages
print(percentages)


## need to convert back to numeric from factor to get mean
df_final[, c("AmznP", "Female", "Degree")] <- 
  lapply(df_final[, c("AmznP", "Female", "Degree")], function(x) as.numeric(as.character(x)))

segments<-
  df_final %>% 
  group_by(cluster) %>% 
  summarise(across(where(is.numeric), mean, .names = "{col}_mean"))
# Display the calculated means
segments

# write.xlsx(segments, 'segments.xlsx')

View(segments)

describe(Smart_Watch)

## Question 3
## ANOVA test

names(df_final)

df_final$cluster <- as.factor(df_final$cluster)

Constcom <- aov(ConstCom ~ cluster, data = df_final)
summary(Constcom)

TimelyInf <- aov(TimelyInf ~ cluster, data = df_final)
summary(TimelyInf)

TaskMgm <- aov(TaskMgm ~ cluster, data = df_final)
summary(TaskMgm)

DeviceSt <- aov(DeviceSt ~ cluster, data = df_final)
summary(DeviceSt)

Wellness <- aov(Wellness ~ cluster, data = df_final)
summary(Wellness)

Athlete <- aov(Athlete ~ cluster, data = df_final)
summary(Athlete)

Style <- aov(Style ~ cluster, data = df_final)
summary(Style)

AmznP <- aov(AmznP ~ cluster, data = df_final)
summary(AmznP)

Female <- aov(Female ~ cluster, data = df_final)
summary(Female)

Degree <- aov(Degree ~ cluster, data = df_final)
summary(Degree)

Income <- aov(Income ~ cluster, data = df_final)
summary(Income)

Age <- aov(Age ~ cluster, data = df_final)
summary(Age)

