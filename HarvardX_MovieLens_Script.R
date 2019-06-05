#=======================================#
# Filename: HarvardX_MovieLens_Script.R #
# Author: Akash Kumar                   #
# Data modified: June 5, 2019           #
# R version 3.6.0                       #
#=======================================#


#===================================#
# Create edx set and validation set #
#===================================#

# Note: this process could take a couple of minutes

# install packages:
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                      col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)


#=====================================================#
# If unable to create the edx set and validation set, #
# load them from the rds files.                       #
#=====================================================#
# edx set:
edx <- readRDS("edx.rds")

# validation set:
validation <- readRDS("validation.rds")


#==============================#
# Load all necessary libraries #
#==============================#
library(tidyverse)
library(caret)


#============================#
# Clean and explore the data #
#============================#
head(edx)
tail(edx)
glimpse(edx)
summary(edx)

# Each movie title contains the year in which that movie was released.
# To make the data cleaner, separate the release year from the movie title.
edx <- edx %>%
  extract(title, c("title", "release_year"), 
          regex = "^(.*) \\((\\d*)\\)$", remove = TRUE)
edx$release_year <- as.numeric(edx$release_year)
# Add a column for the age (in years) of the movies.
edx <- edx %>% mutate(movie_age = 2019 - release_year)
edx <- edx[, c(1,2,3,4,6,8,5,7)] # Reorder the columns.
head(edx)    # Check out the new column.
summary(edx) # Make sure data remains consistent.


#================#
# Quiz Questions #
#================#
# Q1: How many rows and columns are there in the edx dataset?
dim(edx) # 9,000,055 rows and 8 columns
rating_count <- dim(edx)[1]
rating_count

# Q2: How many zeros were given as ratings in the edx dataset? 
edx %>% filter(rating == 0) %>% nrow() # No zeros were given as ratings.
# How many threes were given as ratings in the edx dataset?
edx %>% filter(rating == 3) %>% nrow() # 2,121,240 threes were given as ratings.

# Q3: How many different movies are in the edx dataset?
movie_count <- n_distinct(edx$movieId) # 10,677 movies.
movie_count

# Q4: How many different users are in the edx dataset?
user_count <- n_distinct(edx$userId) # 69,878 users.
user_count

# Q5: How many movie ratings are in each of the following genres in the edx dataset?
# Movies in Drama, Comedy, Thriller and Romance:
edx %>% filter(grepl("Drama", genres)) %>% nrow() # 3,910,127 movie ratings with Drama
edx %>% filter(grepl("Comedy", genres)) %>% nrow() # 3,540,930 movie ratings with Comedy
edx %>% filter(grepl("Thriller", genres)) %>% nrow() # 2,325,899 movie ratings with Thriller
edx %>% filter(grepl("Romance", genres)) %>% nrow() # 1,712,100 movie ratings with Romance

# Q6: Which movie has the greatest number of ratings? - Pulp Fiction (1994)
top_10_movies <- edx %>% 
  group_by(title) %>%
  summarize(count = n()) %>%
  top_n(10) %>%
  arrange(desc(count))
top_10_movies

# Q7: What are the five most given ratings in order from most to least?
# 4 > 3 > 5 > 3.5 > 2
edx %>% group_by(rating) %>% 
  summarize(count = n()) %>% 
  top_n(5) %>%
  arrange(desc(count))

# Q8: True or False: In general, half star ratings are 
# less common than whole star ratings. TRUE
rating_table <- edx %>% group_by(rating) %>% summarize(count = n())
rating_table <- as.data.frame(rating_table)
rating_table
sum(rating_table[seq(1,9,2),2])  # 1,843,170 half star ratings.
sum(rating_table[seq(2,10,2),2]) # 7,156,885 whole star ratings.

#==========================#
# Plots and Visualizations #
#==========================#
# Factors that might influence how a user rates a certain movie:
# 1. Distribution of movie ratings
# 2. Age of the movie
# 3. Movie genre
# 4. Popularity of the movie
# 5. Weighted rating of the movie
# 6. User's average rating

# 1. Rating distribution:
rating_table %>%
  ggplot(aes(x = rating, y = count)) +
  geom_bar(stat = "identity", fill = "red") +
  ggtitle("Rating Distribution")

# 2. Age of the movie:
youngest_movie <- min(edx$movie_age)
youngest_movie # The most recent movie is 11 years old.
oldest_movie <- max(edx$movie_age)
oldest_movie   # The oldest movie is 104 years old.
# Notice the peak around age 25.
edx %>%
  group_by(movie_age) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = movie_age, y = count)) +
  geom_line(color = "orange") +
  ggtitle("Movie Age Distribution")

# 3. Movie genre:
# Let's look at the popularity of the genres.
movie_genres <- edx %>% separate_rows(genres, sep = "\\|")
head(movie_genres)
# Drama is the most popular genre.
movie_genres %>%
  group_by(genres) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  ggplot(aes(x = reorder(genres,count), y = count)) +
  geom_bar(stat = "identity", fill = "violet") +
  coord_flip() +
  labs(x = "genre", y = "number of ratings") +
  ggtitle("Genre Popularity")

genre_avg_ratings <- movie_genres %>% group_by(genres) %>% 
  summarize(avg_rating = mean(rating))
# There is not much change in the ratings among genres.
ggplot(genre_avg_ratings, aes(x = reorder(genres, avg_rating), 
                       y = avg_rating, fill = avg_rating)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "genre", y = "average rating") +
  ggtitle("Movie Genre vs. Average Rating")

# 4. Movie Popularity:
# Popularity is based on the number of ratings.
# The more ratings a certain movie has, the more popular it is.
top_10_movies %>%
  ggplot(aes(x = reorder(title, count), y = count)) +
  geom_bar(stat = "identity", fill = "green") +
  coord_flip() +
  labs(x = "movie", y = "number of ratings") +
  ggtitle("Top 10 Most Popular Movies")

# 5. Weighted Rating:
# Popular movies are not necessarily good movies.
# Likewise, obscure movies are not necessarily bad movies.
# So we need to use weighted rating to identity the best 
# rated movies.

# To calculate the weighted rating, we will be using IMDb's
# weighted rating function.
# WR = (v/(v+m))*R + (m/(v+m))*C 
# R = average for the movie (mean) = (Rating)
# v = number of votes for the movie = (votes)
# m = minimum votes required to be listed in the Top 50 (currently 1000)
# C = the mean vote across the whole report (average of average ratings)
wr <- function(R, v, m, C) {
  return (v/(v+m))*R + (m/(v+m))*C
}
# Notice that this plot and the previous one contain the exact same movies.
edx %>%
  group_by(title) %>%
  summarize(num_ratings = n(), avg_rating = mean(rating),
            weighted_rating = wr(avg_rating, num_ratings, 1000, mean(avg_rating))) %>%
  top_n(10) %>%
  ggplot(aes(x = reorder(title, avg_rating), y = avg_rating)) +
  geom_bar(stat = "identity", fill = "gold") +
  coord_flip() +
  labs(x = "movie", y = "average rating") +
  ggtitle("Top 10 Best Rated Movies")

# 6. Average rating per user:
user_avg_rating <- edx %>% group_by(userId) %>% summarize(avg_rating = mean(rating))
ggplot(user_avg_rating, aes(userId, avg_rating)) +
  geom_point(alpha = 0.5, colour = "blue", shape = ".") +
  geom_smooth() +
  ggtitle("Average Rating per User")
mean(user_avg_rating$avg_rating) # 3.614 (Mean average rating)
mean(edx$rating)                 # 3.512 (Mean rating overall)
summary(user_avg_rating)


#=============================================#
# Residual Mean Squared Error (RMSE) function #
#=============================================#
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}


#=====================#
# 4 Prediction Models #
#=====================#
#Based on what we learned from our EDA, we decided to build and test 4 models:
#Model 1: Naive Baseline (mu)
#Model 2: Movie Effects (mu + b_i)
#Model 3: Movie Effects + User Effects (mu + b_i + b_u)
#Model 4: Movie Effects + User Effects + Regularization (mu + b_i + b_u + Regularization)


#===============================#
# Model 1: Naive Baseline (mu)  #
#===============================#
# Mean rating of the edx set:
mu_rating <- mean(edx$rating)
mu_rating # 3.512
# Calculate RMSE:
rmse_result_1 <- RMSE(edx$rating, mu_rating)
rmse_result_1
# Add RSME result to the results table.
rmse_results <- tibble(Method = "Naive Baseline", RMSE = rmse_result_1)
rmse_results %>% knitr::kable()


#===================================#
# Model 2: Movie Effects (mu + b_i) #
#===================================#
movie_avgs <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu_rating))
head(movie_avgs)
# Movie bias distribution:
movie_avgs %>%
  ggplot(aes(b_i)) + 
  geom_histogram(bins = 10, color = "black") +
  ggtitle("Movie Bias Distribution")
# b_i:
b_i <- edx %>% 
  left_join(movie_avgs, by = 'movieId') %>%
  pull(b_i)
# Prediction:
predicted_ratings <- mu_rating + b_i
# Calculate RMSE:
rmse_result_2 <- RMSE(edx$rating, predicted_ratings)
rmse_result_2
# Add RSME result to the results table.
rmse_results <- bind_rows(rmse_results, tibble(Method = "Movie Effects",
                                               RMSE = rmse_result_2))
rmse_results %>% knitr::kable()


#==========================================================#
# Model 3: Movie Effects and User Effects (mu + b_i + b_u) #
#==========================================================#
user_avgs <- edx %>%
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>% 
  summarize(b_u = mean(rating - mu_rating - b_i))
# User bias distribution:
user_avgs %>%
  ggplot(aes(b_u)) + 
  geom_histogram(color = "black") +
  ggtitle("User Bias Distribution")
# b_i: Same as Model 2.
# b_u:
b_u <- edx %>% 
  left_join(user_avgs, by = 'userId') %>%
  pull(b_u)
# Prediction:
predicted_ratings <- mu_rating + b_i + b_u
# Calculate RMSE:
rmse_result_3 <- RMSE(edx$rating, predicted_ratings)
rmse_result_3
# Add RSME result to the results table.
rmse_results <- bind_rows(rmse_results,
                          tibble(Method="Movie Effects + User Effects", 
                                     RMSE = rmse_result_3))
rmse_results %>% knitr::kable()


#=====================================================#
# Model 4: Movie and User Effects with Regularization #
#=====================================================#
# Penalized least squares method:
# Use cross-validation to pick the best lambda.
lambdas <- seq(0, 1, 0.05) # Sequence of lambdas.
# Compute the predictions on the validation set using
# different lambda values.
rmses <- sapply(lambdas, function(lambda) {
  # Movie bias:
  b_i <- edx %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu_rating) / (n() + lambda))
  # User bias:
  b_u <- edx %>%
    left_join(b_i, by = "movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu_rating) / (n() + lambda))
  # Prediction:
  predicted_ratings <- edx %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(prediction = mu_rating + b_i + b_u) %>% .$prediction
  return(RMSE(edx$rating, predicted_ratings))
})
# Plot:
qplot(lambdas, rmses)
# The lambda that results in the lowest RMSE:
lambda <- lambdas[which.min(rmses)]
lambda
# RMSE:
rmse_result_4 <- min(rmse_results$RMSE)
rmse_result_4
# Add RSME result to the results table.
rmse_results <- bind_rows(rmse_results,
                          tibble(Method="Movie Effects + User Effect + Regularization", 
                                     RMSE = min(rmses)))
rmse_results %>% knitr::kable()


#====================================#
# Test Model 4 on the validation set #
#====================================#
mu_rating <- mean(validation$rating)
lambda <- 0.5
# Movie bias:
b_i <- validation %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu_rating) / (n() + lambda))
# User bias:
b_u <- validation %>%
  left_join(b_i, by='movieId') %>% 
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu_rating) / (n() + lambda))
# Prediction:
predicted_ratings <- validation %>%
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  mutate(prediction = mu_rating + b_i + b_u) %>% .$prediction
# RMSE:
validation_rmse <- RMSE(validation$rating, predicted_ratings)
validation_rmse
