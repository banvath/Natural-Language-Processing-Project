#setting directory
getwd()
setwd("C:/Users/banva/Desktop/capstone")
getwd()

#Importing Libraries
library(readr)
library(dplyr)

#Importing Data
tools <- read_csv("C:/Users/banva/Desktop/capstone/tool_data.csv")

#Look at the categories and their counts
categories_count = tools%>%
count(category)%>%
 arrange(desc(n))

##########################run code til##################################

#choose your category
cat_name= "Cordless Ratchets"
cat_name %in% unique(categories_count$category)


############################################################
cat_data = tools%>%
  filter(category==cat_name)

write.csv(cat_data, "cat_data.csv")
