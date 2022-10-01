##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(ggthemes)
library(gridExtra)
library(caret)
library(lubridate)
library(psych)
library(latex2exp)
library(compiler)

##########################################################
# enabling the Just-in_time compiling speeds up code execution
setCompilerOptions(optimize=3)
enableJIT(3)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 3.6 or earlier:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))
# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
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

##########################################################
# load the data, if a previously generated movie_recommendation.RData - file exists
##########################################################

if (!exists("edx")) {
   load(paste(getwd(), "/movie_recommendation.RData", sep =""))
}

##########################################################
# a useful function for changing the formatting on ggplots' axes from github.com/tinyheero/tinutils/tree/master/R
##########################################################

fancy_scientific <- function(l) {
 l <- format(l, scientific = TRUE)
 l <- gsub("^(.*)e", "'\\1'e", l)
 l <- gsub("e", "%*%10^", l)
 parse(text=l)
}

##########################################################
# to show the structure of the movielens data
##########################################################

edx_example_tbl <- edx[1958:1961, ]
rownames(edx_example_tbl) <- NULL
edx_example_tbl

##########################################################
# to show a psooble error in the timestamp
##########################################################

as_datetime(edx[which(edx$userId==8010 & movieId==887), ] %>% pull(timestamp))

##########################################################
# to extract release date from the title field
##########################################################

validation <- validation %>% mutate("releaseDate"= (str_extract(.$title, "\\(\\d{4}\\)") %>% str_extract("\\d{4}") %>% as.numeric()))
edx        <- edx        %>% mutate("releaseDate"= (str_extract(.$title, "\\(\\d{4}\\)") %>% str_extract("\\d{4}") %>% as.numeric()))

##########################################################
# basic statistics of ratings in edy and in validation data sets
##########################################################

v <- validation %>% group_by(userId) %>% summarise(ratings_per_user = n(), mean_rating_per_user = mean(rating), sd_rating_per_user = sd(rating)) 
e <- edx %>% group_by(userId) %>% summarise(ratings_per_user = n(), mean_rating_per_user = mean(rating), sd_rating_per_user = sd(rating)) 
desc_v <- describe(v)
desc_e <- describe(e)
desc_v
desc_e

##########################################################
# insights into the structure of the data
##########################################################

g <- setDT(rbind.data.frame(edx, validation)) %>% group_by(movieId) %>% summarise(n=n(), mean_r=mean(rating)) 
g %>% ggplot(aes(y=n, x=mean_r)) + geom_point(size=0.5) + labs(x = "average rating of movie", y = "number of ratings of movie") + scale_y_continuous(labels=fancy_scientific)

g <- setDT(rbind.data.frame(edx, validation)) %>% group_by(movieId) %>% summarise(ratingsByMovie=n()) 
g %>% ggplot(aes(ratingsByMovie)) + geom_histogram(binwidth = 0.1, color = "black") + scale_x_log10() + annotation_logticks(sides="b") + labs(x = "number of ratings", y = "number of movies")

##########################################################
# insights into the structure of the data: genres
##########################################################

genres <- edx %>% pull(genres) %>% str_split(pattern="\\|") %>%  unlist() %>% unique() %>% sort()
n_genres_edx <- sapply(genres, function(g) { sum(str_detect(edx$genres, g)) })
n_genres_validation <- sapply(genres, function(g) { sum(str_detect(validation$genres, g)) })

genres_table <- data.frame(Genre = genres, edx = n_genres_edx, validation = n_genres_validation)
rownames(genres_table) <- NULL
genres_table <- genres_table[order(genres_table$edx, decreasing = TRUE),]
genres_table$edx <- sapply(genres_table$edx, function(x) { prettyNum(x, big.mark=",") })
genres_table$validation <- sapply(genres_table$validation, function(x) { prettyNum(x, big.mark=",") })
genres_table

# SetDT from the data.table package is used here and in the following to speed up data frame operations
all_movies <- setDT(rbind.data.frame(edx, validation))
list_genres <- all_movies %>% pull(genres) %>% str_split(pattern="\\|") %>% unlist() %>% unique() %>% sort() %>% .[-1]
for(i in seq_along(list_genres)){assign(tolower(gsub("\\-", "_", list_genres[i])), (
          all_movies %>% filter(str_detect(genres, list_genres[i])) %>% select(-title) %>% select(-timestamp) %>% select(-genres) %>% mutate("genre" = list_genres[i])))}
all_movies_by_genre <- do.call("rbind", lapply(tolower(gsub("\\-", "_", list_genres)),get))

genres_viol_plot <- all_movies_by_genre %>% group_by(movieId) %>% summarise(rating=mean(rating), movieId=movieId, genre=genre) %>% unique() %>% ggplot(aes(x=genre, y=rating)) + geom_violin() + stat_summary(fun = "mean", geom = "crossbar", width = 0.5, colour = "red") + scale_x_discrete(limits=rev) + coord_flip()

genres_viol_plot

film_noir <- all_movies %>% filter(grepl("Film-Noir", genres)) %>% group_by(movieId) %>% summarise(rating=mean(rating), "film_noir" = TRUE)
not_film_noir <- all_movies %>% filter(!grepl("Film-Noir", genres)) %>% group_by(movieId) %>% summarise(rating=mean(rating), "film_noir" = FALSE)
film_noir <- rbind(film_noir, not_film_noir)

horror <- all_movies %>% filter(grepl("Horror", genres)) %>% group_by(movieId) %>% summarise(rating=mean(rating), "horror"=TRUE)
not_horror <- all_movies %>% filter(!grepl("Horror", genres)) %>% group_by(movieId) %>% summarise(rating=mean(rating), "horror"=FALSE)
horror <- rbind(horror, not_horror)

grid.arrange(
  film_noir %>%  ggplot(aes(x = factor(film_noir), y = rating)) + geom_boxplot(notch = TRUE) + labs(x = "“Film-noir”: true/false"), 
  horror %>% ggplot(aes(x = factor(horror), y = rating)) + geom_boxplot(notch = TRUE) + labs(x = "“Horror”: true/false")
  , nrow = 1)

##########################################################
# insights into the structure of the data: top users
##########################################################

top_1 <- e[order(-e$ratings_per_user), ] %>% head(2) %>% pull(userId) %>% .[1]
top_2 <- e[order(-e$ratings_per_user), ] %>% head(2) %>% pull(userId) %>% .[2]
top_1_start_date <- edx %>% filter(userId == top_1) %>% summarize(t=timestamp) %>% describe() %>% .$min
top_1_end_date <- edx %>% filter(userId == top_1) %>% summarize(t=timestamp) %>% describe() %>% .$max
top_1_dates_diff <- difftime( as.Date(as_datetime(top_1_end_date)), as.Date(as_datetime(top_1_start_date)) ) 
top_1_dates_diff
sum_top1_ratings <- (edx %>% filter(userId == top_1) %>% count(userId) %>%  pull(n)) + (validation %>% filter(userId == top_1) %>% count(userId) %>%  pull(n))
sum_top2_ratings <- (edx %>% filter(userId == top_2) %>% count(userId) %>%  pull(n)) + (validation %>% filter(userId == top_2) %>% count(userId) %>%  pull(n))
sum_top1_ratings
sum_top2_ratings

g <- setDT(rbind.data.frame(edx, validation)) %>% group_by(userId) %>% summarise(ratingsByUser=n()) 
g %>% ggplot(aes(ratingsByUser)) + geom_histogram(binwidth = 0.05, color = "black") + scale_x_log10() + annotation_logticks(sides="b") + labs(x = "number of ratings by individual users", y = "number of users")

##########################################################
# insights into the structure of the data: ratings
##########################################################

g <- setDT(rbind.data.frame(edx, validation))
grid.arrange(
 g %>% filter(as_datetime(timestamp) < as.POSIXct("2003-02-18", format="%Y-%m-%d", tz="UTC")) %>% ggplot(aes(rating)) + geom_bar(width = 0.47) + ggtitle("before Feb. 18th, 2003") + theme(plot.title=element_text(size=14)) + scale_y_continuous(labels=fancy_scientific) ,
 g %>% filter(as_datetime(timestamp) >= as.POSIXct("2003-02-18", format="%Y-%m-%d", tz="UTC")) %>% ggplot(aes(rating)) + geom_bar() + ggtitle("from Feb. 18th 2003 onwards") + theme(plot.title=element_text(size=14)) + scale_y_continuous(labels=fancy_scientific) , nrow = 1)

g <- setDT(rbind.data.frame((edx %>% mutate("releaseDate"= (str_extract(.$title, "\\(\\d{4}\\)") %>% str_extract("\\d{4}") %>% as.numeric()))), (validation %>% mutate("releaseDate"= (str_extract(.$title, "\\(\\d{4}\\)") %>% str_extract("\\d{4}") %>% as.numeric())))))
g <- g %>% mutate("rating_year" = as.numeric(substr(as_datetime(timestamp),1,4))) %>% select(timestamp, userId, movieId, releaseDate, rating_year, rating) %>% mutate("ydiff"= rating_year - releaseDate)
g[which(g$ydiff < 0), ]$ydiff <- 0
g <- g %>% group_by(ydiff) %>% summarise(mean_r = mean(rating), ydiff) %>% unique()
gplot <- g %>% ggplot(aes(ydiff, mean_r)) + geom_point() + geom_smooth(method="gam", n=(max(g$ydiff)-min(g$ydiff))+1) + labs(x = "age of movie at the time of rating", y = "mean rating of movie")
ratings_by_movie_age <- (ggplot_build(gplot))$data[[2]] %>% select(x, y)
gplot

##########################################################
# calculation of mean values and predictions
##########################################################

# Calculate overall mean
mu <- mean(edx$rating)

# Calculate mean by movie
means_by_movie <- edx %>% group_by(movieId) %>% summarize(b_i = mean(rating - mu))

# Calculate mean by user 
means_by_user <- edx %>% left_join(means_by_movie, by = "movieId") %>% group_by(userId) %>% summarize(b_u = mean(rating - b_i - mu))

# Prepare calculation mean by genre: calculate means for single genres, and the average them for all genre combinations
list_genres <- edx %>% pull(genres) %>% str_split(pattern="\\|") %>% unlist() %>% unique() %>% sort() %>% as.data.frame() %>% mutate("b_g" = 0)
colnames(list_genres)[1] <- "genre"
for(i in 1:nrow(list_genres)) {
	g <- edx %>% filter(str_detect(genres, list_genres$genre[i])) %>% summarise(genre=list_genres$genre[i], b_g=mean(rating)) %>% unique()
	list_genres[which(list_genres$genre == g$genre),]$b_g = g$b_g
}
g <- edx$genres %>% unique() %>% as.data.frame()
colnames(g)[1] <- "genres"
g <- g %>% mutate(m_g = 0)
for (i in 1:nrow(g)) {
	n_genres <- length(unlist(strsplit(g[i, ]$genres, split="\\|")))
	s_genres <- 0
	for (j in 1:n_genres) { s_genres <- s_genres + list_genres[which(list_genres$genre == unlist(strsplit(g[i, ]$genres, split="\\|"))[j]),]$b_g }
	g[i,]$m_g <- s_genres / n_genres
}

# Calculate means by movie genre (two methods)
means_by_genres <- edx %>% left_join(means_by_movie, by = "movieId") %>% left_join(means_by_user, by = "userId") %>% left_join(g, by = "genres") %>% group_by(genres) %>% summarize(genres=genres, m_gcomb=m_g, mean_g=mean(rating), b_g = mean(rating - b_i - b_u - mu), b_gcomb = mean(m_g - b_i - b_u - mu) ) %>% unique()

# Calculate mean by movie-age at the time of rating
means_by_movie_age <- edx %>% mutate("releaseDate"= (str_extract(.$title, "\\(\\d{4}\\)") %>% str_extract("\\d{4}") %>% as.numeric()))%>% mutate("rating_year" = as.numeric(substr(as_datetime(timestamp),1,4))) %>% mutate("ydiff"= rating_year - releaseDate) %>% mutate(ydiff = ifelse(ydiff < 0, 0, ydiff)) %>% select(-c(releaseDate, rating_year, title)) %>% left_join(means_by_movie, by = "movieId") %>% left_join(means_by_user, by = "userId") %>% left_join(means_by_genres, by = "genres") %>% group_by(ydiff) %>% summarize(b_a = mean(rating - b_i - b_u - b_g - mu))

# Calculation of predictions in *train*-set
movie_effect_prediction      <- edx %>% left_join(means_by_movie, by = "movieId") %>% pull(b_i) + mu

movie_and_user_effect_train  <- edx %>% left_join(means_by_movie, by = "movieId") %>% left_join(means_by_user, by = "userId") %>% mutate(prediction = mu + b_i + b_u) %>% pull(prediction)

mov_usr_gen_eff_comb_train   <- edx %>% left_join(means_by_movie, by = "movieId") %>% left_join(means_by_user, by = "userId") %>% left_join(means_by_genres, by = "genres") %>% mutate(prediction = mu + b_i + b_u + b_gcomb) %>% pull(prediction)

mov_usr_gen_eff_train        <- edx %>% left_join(means_by_movie, by = "movieId") %>% left_join(means_by_user, by = "userId") %>% left_join(means_by_genres, by = "genres") %>% mutate(prediction = mu + b_i + b_u + b_g) %>% pull(prediction)

mov_usr_gen_eff_age_train    <- edx %>% left_join(means_by_movie, by = "movieId") %>% left_join(means_by_user, by = "userId") %>% left_join(means_by_genres, by = "genres") %>% mutate("releaseDate"= (str_extract(.$title, "\\(\\d{4}\\)") %>% str_extract("\\d{4}") %>% as.numeric()))%>% mutate("rating_year" = as.numeric(substr(as_datetime(timestamp),1,4))) %>% mutate("ydiff"= rating_year - releaseDate) %>% mutate(ydiff=replace(ydiff, ydiff < 0, 0)) %>% left_join(means_by_movie_age, by = "ydiff") %>% mutate(prediction = mu + b_i + b_u + b_g + b_a) %>% pull(prediction)

##########################################################
# plots of movie and user parameters 
##########################################################

qplot(means_by_movie$b_i, binwidth=0.25, color=I("black"), ylab="", xlab="movie effect")
edx %>% group_by(userId) %>% summarize(b_u = mean(rating)) %>% ggplot(aes(b_u)) + geom_histogram(binwidth=0.125, color=I("black")) + labs(x=unname(TeX("$b_u$")))

##########################################################
# plot of two methods of genres grouping asgainst each other
##########################################################
qplot(x=mean_g, y=m_gcomb, data=means_by_genres, ylab=unname(TeX("$m_{g(comb)}$")), xlab=unname(TeX("$m_g$")))+ geom_point(size=1.0)

##########################################################
#  Test set will be 20% of Edx MovieLens data
##########################################################
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.2, list = FALSE)
set.seed(1) 
train_set <- edx[test_index,]
temp <- edx[-test_index,]

##########################################################
# Make sure userId and movieId in test set are also in train set
##########################################################
test_set <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

##########################################################
# Add rows removed from test set back into train set
##########################################################
removed <- anti_join(temp, test_set)
train_set <- rbind(train_set, removed)

##########################################################
# Add ydiff-field to test set and train_set
##########################################################
train_set <- train_set %>% mutate("releaseDate"= (str_extract(.$title, "\\(\\d{4}\\)") %>% str_extract("\\d{4}") %>% as.numeric()))%>% mutate("rating_year" = as.numeric(substr(as_datetime(timestamp),1,4))) %>% mutate("ydiff"= rating_year - releaseDate) %>% mutate(ydiff = ifelse(ydiff < 0, 0, ydiff)) %>% select(-c(rating_year, releaseDate, title))
test_set  <- test_set %>% mutate("releaseDate"= (str_extract(.$title, "\\(\\d{4}\\)") %>% str_extract("\\d{4}") %>% as.numeric()))%>% mutate("rating_year" = as.numeric(substr(as_datetime(timestamp),1,4))) %>% mutate("ydiff"= rating_year - releaseDate) %>% mutate(ydiff = ifelse(ydiff < 0, 0, ydiff)) %>% select(-c(rating_year, releaseDate))

##########################################################
# possible use of regularization: calcuation and plot of lambda
##########################################################
lambdas <- c(seq(0, 4, 0.25), seq(4.1, 5.2, 0.1), seq(5.25, 10, 0.25))

mu <- mean(train_set$rating)
rmses <- sapply(lambdas, function(l){
	b_i <- train_set %>% group_by(movieId) %>% summarize(b_i = sum(rating - mu)/(n()+l))
	b_u <- train_set %>% left_join(b_i, by="movieId") %>% group_by(userId) %>% summarize(b_u = sum(rating - b_i - mu)/(n()+l))
	b_g <- train_set %>% left_join(b_i, by="movieId") %>% left_join(b_u, by="userId") %>% group_by(genres) %>% summarize(b_g = mean(rating - b_i - b_u - mu))
	b_a <- train_set %>% left_join(b_i, by="movieId") %>% left_join(b_u, by="userId") %>% left_join(b_g, by="genres") %>% group_by(ydiff) %>% summarize(b_a = mean(rating - b_i - b_u - b_g - mu))

	predicted_ratings <- test_set %>% 
	left_join(b_g, by = "genres") %>% left_join(b_i, by = "movieId") %>% left_join(b_u, by = "userId") %>% left_join(b_a, by = "ydiff") %>% 
	mutate("prediction" = mu + b_i + b_u + b_g + b_a) %>% .$prediction

	return(RMSE(predicted_ratings, test_set$rating, na.rm=TRUE))
})
lambda <- lambdas[which.min(rmses)]

qplot(x=lambdas, y=rmses, ylab="RSMEs", xlab=unname(TeX("$\\lambda$")))

##########################################################
# Calculation of the final model
##########################################################
# calculation of predictions
##########################################################
test_movie_effect_pred    <- valb_i <- edx %>% group_by(movieId) %>% summarize(b_i = sum(rating - mu)/(n()+lambda))
b_u <- edx %>% left_join(b_i, by="movieId") %>% group_by(userId) %>% summarize(b_u = sum(rating - b_i - mu)/(n()+lambda))
b_g <- edx %>% left_join(b_i, by="movieId") %>% left_join(b_u, by="userId") %>% group_by(genres) %>% summarize(b_g = mean(rating - b_i - b_u - mu))
b_a <- edx %>% left_join(b_i, by="movieId") %>% left_join(b_u, by="userId") %>% left_join(b_g, by="genres") %>% mutate("releaseDate"= (str_extract(.$title, "\\(\\d{4}\\)") %>% str_extract("\\d{4}") %>% as.numeric()))%>% mutate("rating_year" = as.numeric(substr(as_datetime(timestamp),1,4))) %>% mutate("ydiff"= rating_year - releaseDate) %>% mutate(ydiff=replace(ydiff, ydiff < 0, 0)) %>% group_by(ydiff) %>% summarize(b_a = mean(rating - b_i - b_u - b_g - mu))

test_movie_effect_pred    <- validation %>% left_join(means_by_movie, by = "movieId") %>% pull(b_i) + mu
test_movie_user_pred      <- validation %>% left_join(means_by_movie, by = "movieId") %>% left_join(means_by_user, by = "userId") %>% mutate(prediction = mu + b_i + b_u) %>% pull(prediction)
test_mov_usr_gen_pred     <- validation %>% left_join(means_by_movie, by = "movieId") %>% left_join(means_by_user, by = "userId") %>% left_join(means_by_genres, by = "genres") %>% mutate(prediction = mu + b_i + b_u + b_g) %>% pull(prediction)
test_mov_usr_gen_age_pred <- validation %>% left_join(means_by_movie, by = "movieId") %>% left_join(means_by_user, by = "userId") %>% left_join(means_by_genres, by = "genres") %>% mutate("releaseDate"= (str_extract(.$title, "\\(\\d{4}\\)") %>% str_extract("\\d{4}") %>% as.numeric()))%>% mutate("rating_year" = as.numeric(substr(as_datetime(timestamp),1,4))) %>% mutate("ydiff"= rating_year - releaseDate) %>% mutate(ydiff=replace(ydiff, ydiff < 0, 0)) %>% left_join(means_by_movie_age, by = "ydiff") %>% mutate(prediction = mu + b_i + b_u + b_g + b_a) %>% pull(prediction)
test_mov_usr_gen_age_p_reg <- validation %>% mutate("releaseDate"= (str_extract(.$title, "\\(\\d{4}\\)") %>% str_extract("\\d{4}") %>% as.numeric()))%>% mutate("rating_year" = as.numeric(substr(as_datetime(timestamp),1,4))) %>% mutate("ydiff"= rating_year - releaseDate) %>% mutate(ydiff=replace(ydiff, ydiff < 0, 0)) %>% left_join(b_g, by = "genres") %>% left_join(b_i, by = "movieId") %>% left_join(b_u, by = "userId") %>% left_join(b_a, by = "ydiff") %>% mutate("prediction" = mu + b_i + b_u + b_g + b_a) %>% .$prediction

##########################################################
# testing the predictions
##########################################################
test_pred_res <- data.frame(                 "prediction model"="overall mean",                               "RMSE" = formatC(RMSE(mean(edx$rating), validation$rating, na.rm=TRUE)         , format="f", digits=5)  )
test_pred_res <- rbind(test_pred_res,      c("prediction model"="movie effects",                              "RMSE" = formatC(RMSE(test_movie_effect_pred, validation$rating, na.rm=TRUE)   , format="f", digits=5) ))
test_pred_res <- rbind(test_pred_res,      c("prediction model"="movie and user effects",                     "RMSE" = formatC(RMSE(test_movie_user_pred, validation$rating, na.rm=TRUE)     , format="f", digits=5) ))
test_pred_res <- rbind(test_pred_res,      c("prediction model"="movie, user, and genres effects",            "RMSE" = formatC(RMSE(test_mov_usr_gen_pred, validation$rating, na.rm=TRUE)    , format="f", digits=5) ))
test_pred_res <- rbind(test_pred_res,      c("prediction model"="movie, user, genres, and movie age effects", "RMSE" = formatC(RMSE(test_mov_usr_gen_age_pred, validation$rating, na.rm=TRUE), format="f", digits=5) ))
test_pred_res <- rbind(test_pred_res,      c("prediction model"="reularized movie & user plus genres & movie age effects", "RMSE" = formatC(RMSE(test_mov_usr_gen_age_p_reg, validation$rating, na.rm=TRUE), format="f", digits=5) ))

##########################################################
# showing the results as a table
##########################################################
test_pred_res


