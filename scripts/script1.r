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
grc <- as_tibble(read.csv(dataPath,stringsAsFactors = FALSE))
## displaying first 10 rows of our data
print(grc, n = 10,width = 80)
## Preparing the data for k-means and apriori algorithms
grc_k <- grc %>%
  select(customer,age,total) %>%
  group_by(customer)%>%
  mutate(total = sum(total))%>% 
  unique()
grc_k <- data.frame(grc_k[,2:3],row.names = grc_k$customer)
tdata <- strsplit(as.vector(grc$items), ',')
tdata <- transactions(tdata)

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
grc_age <- grc_age %>% 
  group_by(age)%>%
  summarise(totalSpending = sum(total))
ggplot(
  grc_age,
  aes(x = age, y = totalSpending)) + 
  geom_line( color="gray") +
  geom_point(shape=21, color="black", fill="#69b3a2", size=6) +
  theme_ipsum() + 
  ggtitle("Comparing age and the total spending using line plot")
grc_age <- mutate(grc_age,age = fct_reorder(as.factor(age),totalSpending))
ggplot(
  grc_age,
  aes(x = age, y = totalSpending)) +
  geom_col() +
  coord_flip() +
  theme_ipsum() +
  ggtitle("Comparing age and the total spending using bar Plot")
## Show each city total spending and arrange it by total descending.

## Display the distribution of total spending.

# K-means
# Association Rules
## Getting the minimum support and minimum confidence from the user
min_support <- as.numeric(readline("Enter the minimum support : "))
min_conf <- as.numeric(readline("Enter the conf support : "))
## implementing the algorithm
apriori_rules <- apriori(tdata, parameter = list(supp = min_support, conf = min_conf, minlen = 2))
## displaying the result
as_tibble(DATAFRAME(apriori_rules,separate = TRUE, setStart = "", setEnd = "")) %>%
  print(n = 100, width = 90)
