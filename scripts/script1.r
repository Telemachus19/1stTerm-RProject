#' ---
#' title: "Project Report"
#' ---

# importing libraries
library(dplyr)
library(ggplot2)
library(forcats)
library(arules)
library(hrbrthemes)


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

#### splitting the items to be suitable for apriori algorithms
tdata <- strsplit(as.vector(grc$items), ',')
tdata <- transactions(tdata)


# Visualizing our Data

## Comparison between cash and credit total spending using box plot
boxplot_cashCredit <- ggplot(
  grc,
  aes(x = paymentType, y = total, fill = paymentType)) +
  stat_boxplot(geom = "errorbar", width = .2) +
  geom_boxplot(width = .2,
               outlier.color = "orange",
               outlier.size = 2)+
  theme_ipsum_rc() +
  theme(
    plot.title = element_text(size=16)) +
  ggtitle("Comparing cash and credit total using box plot")
barplot_cashCredit <- ggplot(grc,
                             aes(x = paymentType,
                                 y = total,
                                 fill = paymentType)) +
  geom_col(width = .3) +
  scale_y_continuous(n.breaks = 12) +
  theme_ipsum() + 
  theme(
    plot.title = element_text(size = 16)) + 
  ggtitle("Comparing cash and credit total using bar plot")

print(boxplot_cashCredit)
print(barplot_cashCredit)

## Compare each age and sum of total spending.

### Before Visualizing
table(grc_customers$age)
arrange(grc_customers,desc(total))

### Visualizing
customersBarPlot<- grc_customers %>%
  ggplot(
    aes(x = customer, y = total)) + 
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
    plot.title = element_text(size=11))+
  ggtitle("Comparing age and the total spending using line plot")
grc_age <- mutate(grc_age,age = fct_reorder(as.factor(age),totalSpending))
barPlotAgeSum<-ggplot(
  grc_age,
  aes(x = age, y = totalSpending)) +
  geom_col() +
  coord_flip() +
  theme_ipsum() +
  theme(
    plot.title = element_text(size=16))+
  ggtitle("Comparing age and the total spending using bar Plot")

print(customersBarPlot)
print(barPlotAgeSum)

## Show each city total spending and arrange it by total descending.

###Cleaning the data in order to be prepared for data visualization
C_Vs_To <-grc %>%
  select(city,total) %>%
  group_by(city) %>%
  summarise(totalspending = sum(total))
C_Vs_To <- mutate(C_Vs_To, city = fct_reorder(as.factor(city),totalspending))
print(C_Vs_To)

###Visualizing
CityandTotalspending<- ggplot(C_Vs_To,aes(city,totalspending)) +
  geom_segment( aes(xend=city,y = 0,yend=totalspending)) +
  scale_y_continuous(n.breaks = 10) +
  geom_point( color="blue", size=9, alpha=.9) +
  theme_ipsum() +
  coord_flip() + 
  theme(
    plot.title = element_text(size=16))+
  ggtitle("Cities VS. Total Spending")

print(CityandTotalspending)

## Display the distribution of total spending.
Distribution_of_total_spending<-ggplot(grc_customers,aes(total)) +
  stat_boxplot(geom = "errorbar",width = .9) + 
  geom_boxplot(fill = "antiquewhite4",) +
  scale_x_continuous(n.breaks = 8) +
  theme_grey() +
  theme(
    plot.title = element_text(size=16),
    axis.text.y = element_blank()
    ) +
  xlab("Total Spending")  +
  ggtitle("Distribution of Total spending")
summary(grc_customers)
print(Distribution_of_total_spending)

# K-means

## Getting the number of clusters from the user
No_of_clusters<-as.numeric(readline("Enter the number of clusters: "))

## Implementing the algorithm using the bult-in function
Kmeans_Algorithm<-kmeans(grc_kmeans,centers = No_of_clusters)
grc_kmeans<-mutate(grc_kmeans,cluster=Kmeans_Algorithm$cluster)
print(grc_kmeans)

# Association Rules

## Getting the minimum support and minimum confidence from the user
min_support <- as.numeric(readline("Enter the minimum support : "))
min_conf <- as.numeric(readline("Enter the conf support : "))

## implementing the algorithm
apriori_rules <- apriori(tdata, parameter = list(supp = min_support, conf = min_conf, minlen = 2))

## displaying the result
if(length(size(apriori_rules)) == 0){
  print("No rules were generated")
}else{
  as_tibble(DATAFRAME(apriori_rules,separate = TRUE, setStart = "", setEnd = "")) %>%
    print(n = 100, width = 90)
}
