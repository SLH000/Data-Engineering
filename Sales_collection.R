library(mongolite)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(dplyr)

#Connect to the database 
connection_string = 'mongodb+srv://k07aa5:k07aa5@comp8031.5es0wna.mongodb.net/?retryWrites=true&w=majority'

#Loading the data from Mongodb
sales_collection <- mongo(collection="sales", db="sample_supplies", url=connection_string)
sales_collection$count()
sales_collection$iterate()$one()

sales_results <- sales_collection$find()
sales_tb <- as.tibble(sales_results)

#Data Transformation 
head(sales_tb)
sales_tb <- unnest_wider(sales_tb, items) %>% unnest(c(name, tags, price, quantity))
head(sales_tb)
#Check for missing data
sum(is.na(sales_tb))

#Add unit price column
sales_tb <- sales_tb %>% mutate(unitPrice = price/ quantity)

#Subset the data
subset_pens_Den <- filter(sales_tb, name == 'pens', storeLocation == 'Denver', customer$age <=30)
subset_binder <- filter(sales_tb, name == 'binder')
subset_envelopes <- filter(sales_tb, name == 'envelopes')
subset_notepad <- filter(sales_tb, name == 'notepad')
subset_laptop<- filter(sales_tb, name == 'laptop')
subset_backpack<- filter(sales_tb, name == 'backpack')

str(sales_tb)


library(corrgram)
plot_cor <- corrgram(sales_tb)
#Categorical vs categorical
plot_s1 <- ggplot(sales_tb, aes(x = factor(purchaseMethod), fill = factor(customer$gender))) +
  geom_bar(position = "fill") +
  labs(x = "Purchase method", y = "Proportion", title = "Gender vs Purchase method") +
  scale_fill_discrete(name = "Gender")

plot_s1a <- ggplot(sales_tb, aes(x = factor(name), fill = factor(customer$gender))) +
  geom_bar(position = "fill") +
  labs(x = "Items", y = "Proportion", title = "Items vs Gender") +
  scale_fill_discrete(name = "Gender")

plot_s2 <- ggplot(sales_tb, aes(x = factor(customer$gender), fill = factor(couponUsed))) +
  geom_bar(position = "fill") +
  labs(x = "Gender", y = "Proportion", title = "Gender vs Coupon Used") +
  scale_fill_discrete(name = "Coupon")

#Categorical vs numerical 
plot_s3 <- ggplot(sales_tb, aes(x = factor(purchaseMethod), y = customer$age)) +
  geom_boxplot(fill = "blue", color = "black") +
  labs(x = "Purchase method", y = "Age", title = "Age vs Purchase Method")

plot_s3a <- ggplot(sales_tb, aes(x=factor(name), y = customer$age)) +
  geom_boxplot(fill = "gray", color = "black")+
labs(x = "Item", y = "Age", title = "Age vs Item")

plot_s4 <- ggplot(sales_tb, aes(x = factor(storeLocation), y = customer$age)) +
  geom_boxplot(fill = "blue", color = "black") +
  labs(x = "Store Location", y = "Customer age", title = "Age vs Store Location")

plot_s5 <- ggplot(sales_tb, aes(x = customer$age, fill = factor(couponUsed) )) +
  geom_bar() + labs(x = "Age", y = "coupon used", title = "Age vs coupon used")

# Numerical vs Numerical
plot_s6 <- ggplot(subset_laptop, aes(x = quantity, y = price)) + geom_point() +
  labs(x = "Quantity", y = "Price", title = "Scatter Plot of Price vs quantity")

plot_s7 <- ggplot(subset_laptop, aes(x = customer$age, y = unitPrice)) + geom_point() +
  labs(x = "Age", y = "Unit Price", title = "Scatter Plot of Price vs quantity")
