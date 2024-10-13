# Install and run the required packages
install.packages("mongolite")
installed.packages("tidyverse")
install.packages("ggplot2")
library(mongolite)
library(tidyverse)
library(tidyr)
library(ggplot2)

#Connect to the database 
connection_string = 'mongodb+srv://k07aa5:k07aa5@comp8031.5es0wna.mongodb.net/?retryWrites=true&w=majority'

#collection movies from sample_mflix 

movies_collection <- mongo(collection="movies", db="sample_mflix", url=connection_string)
users_collection <- mongo(collection="users", db="sample_mflix", url=connection_string)
movies_collection$count()

#Show the first item in the movies_collection
movies_collection$iterate()$one()

users_collection$iterate()$one()

#Try to use the sort function to sort the collection 
run_time<-movies_collection$find(sort = '{"tomatoes.viewer.rating":-1}', limit =200,fields ='{"tomatoes.viewer.rating": true, "runtime":true}')
countries<-movies_collection$find(sort = '{"tomatoes.viewer.rating":-1}', limit =200,fields ='{"countries": true, "tomatoes.viewer.rating":true}')

#Put the data into a tibble
mc_tb <- as.tibble(movies_collection$find())
user_tb <- as.tibble(users_collection$find())


#Data Cleaning: Check and remove missing data
any(is.na(mc_tb))
clean_tb <- drop_na(mc_tb)
any(is.na(clean_tb))


#Filter the data from 2000-2015
year <- filter(clean_tb, year>=2000, na.rm = TRUE)

library(corrgram)
plot_corr <- corrgram(mc_tb)
str(mc_tb)
