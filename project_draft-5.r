#Analysis of Sales Data
# Sunidhi Amatya (amat0017)
# Tasmia Bhuiyan (bhui0008)
# Ramya Karri (karr0015)
# See Lok Ashley Ho (ho0418)

# load necessary libraries
library(mongolite)
library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)

#### 1. Data Wrangling ####

## Connecting to MongoDB and loading the sales collection ##

# Connecting to the MongoDB database using the connection string
connection_string = 'mongodb+srv://k07aa5:k07aa5@comp8031.5es0wna.mongodb.net/?retryWrites=true&w=majority'

#Loading sales collection from sample_supplies dataset from the mongodb
  sales_collection <- mongo(collection="sales", db="sample_supplies", url=connection_string)
  sales_collection$count()
  
#Show the first item in the sales_collection to access and analyze the sales data
  sales_collection$iterate()$one()
  
## Converting the data into a tibble and dataframe ##
  
  sales_results <- sales_collection$find()
  sales_tb <- as.tibble(sales_results)
  head(sales_tb)

## Handling missing or incorrect data ##

# Check is there any missing data in the dataset 
  sum(is.na(sales_tb))
  
## Tidying the data ##
  
# Remove the row contain NA
  any(is.na(sales_tb))
  clean_tb <- drop_na(sales_tb)
  any(is.na(clean_tb))
  sum(is.na(clean_tb))
  clean_tb


#### 2. Data Transformation #### 
  names(clean_tb)
  
# Unnesting item array
  clean_tb <- clean_tb %>%
    unnest(items)
  names(clean_tb)
  
# Analysing the data
  summary(clean_tb)
  
## Data visualizations for customer age group and coupon used ##
  
# Converting categorical variable to numeric for couponUsed
  clean_tb <- clean_tb %>%
    mutate(coupon = as.numeric(couponUsed))
  
#creating age group
  clean_tb <- clean_tb %>%
    mutate(age_group = cut(customer$age, breaks = seq(0, 100, by = 10), right = FALSE))
  
#changing date format
  clean_tb$saleDate <- as.Date(clean_tb$saleDate)
  clean_tb

#### 3. Data Analysis ####
  
#Graph 1:bar plot to visualize the distribution of coupon usage among different age groups (Categorical vs continuous) #
  ggplot(clean_tb, aes(x = age_group, y = coupon)) +
           geom_bar(stat = "identity", fill = "steelblue") +
    labs(x = "Age Group", y = "Count", fill = "Coupon Used") +
    ggtitle("Coupon used by age group")
  

#Graph 2:bar chart to compare the distribution of coupon usage among different age groups 
  ggplot(clean_tb, aes(x = age_group, fill = couponUsed)) +
    geom_bar(position = "dodge") +
    labs(x = "Age Group", y = "Count", fill = "Coupon Used") +
    theme_minimal()

#Graph 3:stacked bar chart to visualize the distribution of customer satisfaction across different store locations #
  ggplot(clean_tb, aes(x = storeLocation, fill = as.factor(customer$satisfaction))) +
    geom_bar(position = "stack") +
    labs(x = "Store Location", y = "Count", fill = "Customer Satisfaction") +
    theme_minimal()

#Graph 4:bar chart to compare the proportion of different genders in each purchase method #
  ggplot(clean_tb, aes(x = factor(purchaseMethod), fill = factor(customer$gender))) +
    geom_bar(position = "fill") +
    labs(x = "Purchase method", y = "Proportion", title = "Gender vs Purchase method") +
    scale_fill_discrete(name = "Gender")

#Graph 5:scatter plot to visualize the relationship between price and quantity for laptops in the store location 'Austin'#
  names(clean_tb)
  subset_laptop<- filter(clean_tb, name == 'laptop')
  subset_austin<- filter(subset_laptop, storeLocation == 'Austin')

  ggplot(subset_austin, aes(x = price, y = quantity)) +
    geom_point(aes(color = factor(customer$gender))) +
    labs(x = "Quantity", y = "Price", title = "Scatter Plot of Price vs quantity")

#Graph 6:Box plot to compare the distribution of customer ages across different store locations.#
  ggplot(clean_tb, aes(x = factor(storeLocation), y = customer$age)) +
    geom_boxplot(fill = "blue", color = "black") +
    labs(x = "Store Location", y = "Customer age", title = "Age vs Store Location")
  
#Graph 7:Bar graph to visualize the distribution of items over different years.#
  # Extracting the year from the saleDate field #
  sales_yr <- clean_tb %>%
    mutate(year = year(saleDate))
  
  # Counting the occurrences of each item per year #
  item_counts <- sales_yr %>%
    group_by(name, year) %>%
    summarise(count = n())
  
  # Ploting the graph #
  ggplot(item_counts, aes(x = name, y = count, fill = as.factor(year))) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Item Year Distribution", x = "Item Name", y = "Count", fill = "Year") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
#Graph 8:Scatter plot to visualize the relationship between price and quantity for laptops, grouped by age group.#
  names(clean_tb)
  subset_laptop <- clean_tb %>%
    filter(name == "laptop")
  subset_laptop <- clean_tb %>% 
    mutate(age_group = cut(customer$age, breaks = c(0, 18, 30, 40, Inf), labels = c("Under 18", "18-30", "31-40", "Over 40")))
  
  ggplot(subset_laptop, aes(x = price, y = quantity)) +
    geom_point(size = 2) +
    labs(x = "Quantity of laptops", y = "Price", title = "Scatter Plot of Quantity of laptops vs Price")


#### 4. Data Modelling ####
  
#load necessary libraries
  library(caTools)

## Linear model ##
# create supervised machine learning models #
#split the data into train set and test set #
  set.seed(101)
  sample <- sample.split(sales_yr$year, SplitRatio = 0.7)
  train <- subset(sales_yr, sample == TRUE)
  test <- subset(sales_yr, sample == FALSE)
  
#Creating a machine learning model #
  model <- lm(price ~ year + coupon + customer$satisfaction + customer$age +quantity, train)
  summary(model)
  
  prediction <- predict(model, test)
  results_lm <- cbind(prediction, test$price)
  colnames(results_lm) <- c('prediction', 'actual')
  results_lm <- as.data.frame(results_lm)
  
#Validating the model #
  mse <- mean((results_lm$actual - results_lm$prediction)^2)
  rmse <- mse^0.5

  library(cluster)    # clustering algorithms
  library(factoextra) # clustering algorithms & visualization
  
  #Clustering 
  cluster_data <- clean_tb %>%
    select(price, quantity)
  
  # Set the number of clusters (k)
  k <- 3
  
  # Create the k-means clustering model
  kmeans_model <- kmeans(cluster_data, centers = k)
  
  # View the cluster centers
  cluster_centers <- kmeans_model$centers
  print(cluster_centers)
  
  # View the cluster assignments
  cluster_assignments <- kmeans_model$cluster
  print(cluster_assignments)
  
  # Calculate the within-cluster sum of squares (WCSS)
  wcss <- kmeans_model$tot.withinss
  print(wcss)
  
  # Calculate the total sum of squares (TSS)
  tss <- sum(apply(cluster_data, 2, function(x) sum((x - mean(x))^2)))
  print(tss)
  
  # Calculate the proportion of variance explained (PVE)
  pve <- (tss - wcss) / tss
  print(pve)
  