#' ---
#' title: "Project Report"
#' ---

## importing libraries
library(dplyr)
library(ggplot2)
library(arules)
library(hrbrthemes)
library(forcats)
## Getting the data
dataPath <- readline("Enter the path to the data set : ")
grc <- as_tibble(read.csv("D:/RProjects/1stTerm-RProject/dataset/grc.csv",stringsAsFactors = FALSE))
## displaying first 10 rows of our data
print(grc, n = 5,width = 120)

# Visualizing our Data
## Comparison between cash and credit total spending using box plot
ggplot(
  grc,
  aes(x = paymentType, y = total, fill = paymentType)
) +
  geom_boxplot(width = .2, 
               fill = "orange",
               outlier.color = "orange",
               outlier.size = 2)+
  theme_ipsum_rc() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Comparing cash and creadit total using box plot")
## Compare each age and sum of total spending.
grc_age <- select(grc,age,total)
grc_age <- group_by(grc_age, age)
grc_age <- summarise(grc_age,totalSpending = sum(total))
ggplot(
  grc_age,
  aes(x = age, y = totalSpending)) + 
  geom_line( color="gray") +
  geom_point(shape=21, color="black", fill="#69b3a2", size=6) +
  theme_ipsum()
grc_age <- mutate(grc_age,age = fct_reorder(as.factor(age),totalSpending))
ggplot(grc_age,aes(x = age, y = totalSpending)) + geom_col() + coord_flip() +theme_ipsum()

# Association Rules
## Preparing the data for generating the association rules
tdata<-strsplit(as.vector(grc$items), ',')
tdata <- transactions(tdata)
min_support <- as.numeric(readline("Enter the minimum support : "))
min_conf <- as.numeric(readline("Enter the conf support : "))
apriori_rules <- apriori(tdata, parameter = list(supp = min_support, conf = min_conf, minlen = 2))
inspect(apriori_rules)
