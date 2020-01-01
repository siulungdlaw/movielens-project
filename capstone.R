################################
# Create edx set, validation set
################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data

set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
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


#########################################################################################################

#################################################
####### codes to be submitted by learners #######
#################################################

#########################################################################################################

#preprocessing
edx2<-edx
edx2$movieId<-as.factor(edx2$movieId)
edx2$userId<-as.factor(edx2$userId)
edx2$genres<-as.factor(edx2$genres)


#calibrating mean
m<-mean(edx2$rating)

#calibaring movie bias
b_m<-edx2%>%group_by(movieId)%>% summarise(b_m=mean(rating)-m)


#calibrating user bias
b_u<-left_join(edx2, b_m)%>%group_by(userId)%>% summarise(b_u=mean(rating-m-b_m))




#calibrating genres bias
b_g<-left_join(left_join(edx2, b_m),b_u) %>% group_by(genres)%>% summarise(b_g=mean(rating-m-b_m-b_u))

#wrapper function
rating_p <- function(movies) {
  movies$movieId<-as.factor(movies$movieId) #change movieID from numeric to factor
  movies$userId<-as.factor(movies$userId) #change userID from numeric to factor
  movies$genres<-as.factor(movies$genres) #change genres from numeric to factor
  left_join(left_join(left_join(movies, b_g),b_u),b_m)%>% #combine files
    #calculate/cap/floor the results
    mutate(p0=m+b_m+b_u+b_g, p1=ifelse(p0>5, 5, p0), p=ifelse(p1<0, 0, p1))%>%  
    pull(p)
}

#creating predictions based on the edx file and calculate RMSE
p<-rating_p(edx) #calculate the predicted ratings
RMSE(p, edx$rating)


#creating predictions based on the validation file and calculate RMSE
p_val<-rating_p(validation)
RMSE(p_val, validation$rating)



