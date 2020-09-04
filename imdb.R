#This is all my code for the Imdb database 

#Libraries I need 
library(tidyverse)

#Libraries for bar graph
install.packages("devtools")
library(devtools)
install_github("easyGgplot2", "kassambara")

#Read in data
imdb <- read.csv("./IMDBTrain.csv")
imdb.test <- read.csv("./IMDBTest.csv")

##
##Explorator Data Analysis 
##

##Overall summary of the data
summary(imdb)

##Scatterplot of Budget vs. Score
##budget in local currency. Need to convert to common currency 
ggplot(data = imdb, mapping = aes(x = budget, y = imdb_score)) + geom_point()

imdb %>% filter(budget > 1e9) %>% select(movie_title)
imdb %>% filter(budget > 1e8, country == "USA") %>% select(movie_title)

##Scatterplot of gross vs imdb
ggplot(data = imdb, mapping = aes(x = gross, y = imdb_score)) + geom_point()
with(imdb, cor(gross, imdb_score, use="complete.obs"))

#
#Look at missing values
#mean(apply(is.na(imdb), 1, sum)>0)
#dim(imdb)
#

#Look at next four columns
#Group five (column 20-24)
'imdb[1,20:24]
'sum(apply(is.na(imdb[,20:24]), 1, sum)<=0)
barplot(data = imdb$language)
ggplot(data = imdb, mapping = aes(x = title_year, y = imdb_score)) + geom_point()
ggplot(data = imdb, mapping = aes(x = title_year, y = imdb_score)) + geom_point()

