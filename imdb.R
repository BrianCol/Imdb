#This is all my code for the Imdb database 

#Libraries I need 
library(tidyverse)

#Libraries for bar graph
#install.packages("devtools")
#library(devtools)
#install_github("easyGgplot2", "kassambara")

#Read in data
imdb.train <- read.csv("./IMDBTrain.csv")
imdb.test <- read.csv("./IMDBTest.csv")

names(imdb.test)[names((imdb.test)=="Id")] <- "movie_title"
imdb <- bind_rows(train=imdb.train, test=imdb.test, id="Set")

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
imdb[1,20:24]
dim(imdb[,20:24])
sum(apply(is.na(imdb[,20:24]), 1, sum)<=0)
barplot(data = imdb$language)
ggplot(data = imdb, mapping = aes(x = title_year, y = imdb_score)) + geom_point()
ggplot(data = imdb, mapping = aes(x = title_year, y = imdb_score)) + geom_point()


#aspect ration, content_rating, face_number_in_poster, num_user_for_review, num_voted_user, title_year, movie_title
#stuff for new data
#
# IMDb Kaggle Competition
#

# Libraries
library(tidyverse)
library(DataExplorer)

# Read data
train <- read_csv("IMDBTrain.csv")
test <- read_csv("IMDBTest.csv")

#
# EDA
#

# Overall summary
summary(train)
glimpse(train)

# Missing data
plot_missing(movies)
mean(apply(is.na(movies), 1, sum)>0)

# Merge test and training
names(test)[names(test)=="Id"] <- "movie_title"
movies <- bind_rows(train=train, test=test, .id="Set")

# Gross vs score
ggplot(train, aes(x=gross, y=imdb_score)) + geom_point()
with(train, cor(gross,imdb_score, use="complete.obs"))

# Scatterplot of budget vs score
ggplot(train, aes(x=budget, y=imdb_score)) + geom_point()
# Need to convert budget to uniform currency
train %>% filter(budget>1e8, country=="USA") %>% select(movie_title)

# ***Many seem to be TV shows and not movies:
train[which(is.na(train$director_name)),c("movie_title","title_year","content_rating","duration")] %>% print(n=Inf)

#
# Look at assigned variables
#
movies.sub <- movies[,c("aspect_ratio","content_rating","facenumber_in_poster","num_user_for_reviews","num_voted_users","title_year","movie_title","imdb_score")]
attach(movies.sub)

# Aspect Ratio
movies.sub$aspect_ratio <- as.factor(aspect_ratio)
table(aspect_ratio)
sum(is.na(aspect_ratio))
ggplot(data=movies.sub, aes(x=aspect_ratio, y=imdb_score)) + geom_boxplot()
# DROP

# Content Rating
table(content_rating)
sum(is.na(content_rating))
View(movies[which(is.na(content_rating)),] %>% print(n=Inf))

## Change NA to Not Rated
movies$content_rating[which(is.na(content_rating))] <- "Not Rated"

# Faces in Poster
table(facenumber_in_poster)
ggplot(data=movies.sub, aes(x=facenumber_in_poster, y=imdb_score)) + geom_point()
cor(facenumber_in_poster,imdb_score, use="complete.obs")
# DROP

# Num User for Review
ggplot(data=movies.sub, aes(x=num_user_for_reviews, y=imdb_score)) + geom_point()
cor(num_user_for_reviews,imdb_score, use="complete.obs")
# Consider dropping: collinearity w/ voted users, more NA than voted users, lower cor than voted users
# DROP

# Num voted users
ggplot(data=movies.sub, aes(x=num_voted_users, y=imdb_score)) + geom_point()
cor(sqrt(num_voted_users),imdb_score, use="complete.obs")
cor(num_user_for_reviews, num_voted_users, use="complete.obs")
sum(is.na(num_voted_users))

# Year
sum(is.na(title_year))
movies.sub[title_year==1916,"movie_title"]
ggplot(data=movies.sub, aes(x=title_year, y=imdb_score)) + geom_point() + geom_vline(xintercept=c(1960,1995))
# Split years into 3 groups

# Movie Title
train.sub$movie_title <- str_trim(movie_title, "right")


