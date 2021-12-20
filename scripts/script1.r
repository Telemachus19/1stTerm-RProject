# importing 
library(dplyr)
library(ggplot2)
# Getting data
dataPath <- readline("Enter the path to the data set : ")
grc <- as_tibble(read.csv("dataset/grc.csv", stringsAsFactors = FALSE))
# compare Cash and credit totals
ggplot(
  grc,
  aes(x = paymentType, y = total)
) +
  geom_boxplot(width = .2, 
               fill = "orange",
               outlier.color = "orange",
               outlier.size = 2)
# Compare each age and sum of total spending.
grc_age <- select(grc,age,total)
grc_age <- group_by(grc_age, age)
grc_age <- summarise(grc_age,totalSpending = sum(total))
print(grc_age)
ggplot(grc_age,aes(x = age, y = totalSpending)) + geom_col()
ggplot(grc_age, aes(x= age, y = totalSpending, group =1)) + geom_boxplot(outlier.size = 2)
