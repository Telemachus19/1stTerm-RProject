#' ---
#' title: "Project Report"
#' ---

# importing libraries
library(dplyr)
library(ggplot2)
library(forcats)
library(arules)
library(hrbrthemes)
library(viridis)


## Getting the data
dataPath <- readline("Enter the path to the data set : ")
grc <- as_tibble(read.csv(dataPath,stringsAsFactors = FALSE))

### displaying first 10 rows of our data
print(grc,n = 10, width = 80)

### Preparing the data for k-means and apriori algorithms

#### creating a data framing containing customers, their ages and total spending
grc_customers <- grc %>%
  select(customer,age,total) %>%
  group_by(customer)%>%
  mutate(total = sum(total))%>% 
  unique()

#### making the data frame suitable for k-means
grc_kmeans <- data.frame(grc_customers[,2:3],row.names = grc_customers$customer)
No_of_clusters<-as.numeric(readline("Enter the number of clusters: "))
Kmeans_Algorithm<-kmeans(grc_kmeans,centers = No_of_clusters)
grc_kmeans<-mutate(grc_kmeans,Kmeans_Algorithm$cluster)
print(grc_kmeans)

#### splitting the items to suitable for apriori algorithms
tdata <- strsplit(as.vector(grc$items), ',')
tdata <- transactions(tdata)


# Visualizing our Data

## Comparison between cash and credit total spending using box plot
boxplot_cashCreadit <- ggplot(
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

boxplot_cashCreadit

## Compare each age and sum of total spending.

### Before Visualizing
table(grc_customers$age)
arrange(grc_customers,desc(total))
### Visualizing
customersBarPlot<- grc_customers %>%
  ggplot(
    aes(x = customer, y = total)
  ) + 
  geom_col() +
  coord_flip()+
  theme_ipsum()
grc_age <- select(grc,age,total)
grc_age <- grc_age %>% 
  group_by(age) %>%
  summarise(totalSpending = sum(total))
linePlotAgeSum <- ggplot(
  grc_age,
  aes(x = age, y = totalSpending)) + 
  geom_line( color="gray") +
  geom_point(shape=21, color="black", fill="#69b3a2", size=6) +
  theme_ipsum() + 
  theme(
    plot.title = element_text(size=11)
  )+
  ggtitle("Comparing age and the total spending using line plot")
grc_age <- mutate(grc_age,age = fct_reorder(as.factor(age),totalSpending))
barPlotAgeSum<-ggplot(
  grc_age,
  aes(x = age, y = totalSpending)) +
  geom_col() +
  coord_flip() +
  theme_ipsum() +
  theme(
    plot.title = element_text(size=11)
  )+
  ggtitle("Comparing age and the total spending using bar Plot")

customersBarPlot
linePlotAgeSum
barPlotAgeSum

## Show each city total spending and arrange it by total descending.

###Cleaning the data in order to be prepared for data visualization
CityVsTotalspending<-grc %>%
  select(city,total) %>%
  group_by(city) %>%
  mutate(total_spending=sum(total)) %>%
  unique()
CityVsTotalspending<-data.frame(CityVsTotalspending$city,CityVsTotalspending$total_spending)
CityVsTotalspending<-unique(CityVsTotalspending)
CityVsTotalspending<-arrange(CityVsTotalspending,desc(CityVsTotalspending$CityVsTotalspending.total_spending))
###Visualizing
CityandTotalspending<-ggplot(CityVsTotalspending,
                             mapping = aes(CityVsTotalspending.city,CityVsTotalspending.total_spending),
)+
  geom_point(size=10,color="#AA4371")+
  labs(x="Cities",y="Total spending",title = "Cities VS. Total Spending")+
  theme_gray()+
  coord_flip()+
  theme(
    plot.title = element_text(size=15))
print(CityandTotalspending)
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
