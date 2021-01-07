movies<-read.csv("C:\\Users\\anupo\\Downloads\\new_movies.csv", sep = ",", header=TRUE)
ratings<-read.csv("C:\\Users\\anupo\\Downloads\\new_ratings.csv", sep = ",", header=TRUE)

movies$genres[movies$genres=="(no genres listed)"] <- ""


#EDA
head(movies,10)

#most popular genres of movie released
#to calculate frequency of each genre type
values<-list()
for(i in 1:length(movies$genres))
{
  temp<-movies$genres[i]
  splits<-strsplit(temp,"|",fixed=TRUE)
  for(j in 1:length(splits))
  {
    if(splits[[1]][j] %in% names(values)){
      values[[splits[[1]][j]]] <- values[[splits[[1]][j]]] + as.integer(1)
    }
    else
    {
      values[[splits[[1]][j]]]<- as.integer(1)
    }
  }
}
barplot(unlist(values), xlab="genre", ylab="count", col=rainbow(20, start=0.1, end=0.9))

head(ratings, 10)


#Distribution of users rating
hist(ratings$rating, xlab="rating", main="Distribution of users ratings", col = "blue")


merge_ratings_movies <- merge(ratings, movies, by.x = "movieId", by.y = "movieId")
head(merge_ratings_movies, n=4L)

merge_ratings_movies <- subset(merge_ratings_movies, select=-c(timestamp, title, genres))


#Grouping the rating based on user

library(dplyr)
user_mean_df = aggregate(x=merge_ratings_movies, by=list(merge_ratings_movies$userId), FUN=mean)

userId_freq<-count(merge_ratings_movies, merge_ratings_movies$userId)
user_avg_ratings<-subset(merge(user_mean_df, userId_freq, by.x = "userId", by.y = "merge_ratings_movies$userId"),
                         select=c(userId, n, rating))
names(user_avg_ratings) <- c('userId', 'num_of_ratings', 'avg_rating')
head(user_avg_ratings)


library(ggplot2)
temp<-head(user_avg_ratings[order(user_avg_ratings$num_of_ratings, decreasing=TRUE),], 20)
temp$userId <- as.character(temp$userId)
p<-ggplot(temp, aes(userId, num_of_ratings, fill=userId))
p +geom_bar(stat = "identity") +theme(axis.text.x = element_text(angle=65, vjust=0.6))+ggtitle("Most active user")


movie_mean_df = subset(aggregate(x=merge_ratings_movies, by=list(merge_ratings_movies$movieId), FUN=mean),
                       select=c(movieId, rating))
head(movie_mean_df)
top_20_movies_df = head(movie_mean_df[order(movie_mean_df$rating, decreasing=TRUE),], 20)
top_20_movies_df$movieId<- as.character(top_20_movies_df$movieId)

#Movies with highest avg rating
p<-ggplot(top_20_movies_df, aes(movieId, rating))
p +geom_bar(stat = "identity") +theme(axis.text.x = element_text(angle=65, vjust=0.6))+ggtitle("Movies with highest avg rating")


#movies with low average rating
low_rated_movies_df <- head(filter(movie_mean_df, rating<1.5), 20)
low_rated_movies_df$movieId<- as.character(low_rated_movies_df$movieId)
p<-ggplot(low_rated_movies_df, aes(movieId, rating))
p +geom_bar(stat = "identity")+ggtitle("Movies with low average rating")

head(low_rated_movies_df, 20)



#install.packages("superml")
library("superml")
tfidf_movies_genres <- TfIdfVectorizer$new(max_features = 19, remove_stopwords = FALSE)
movies$genres[movies$genres=="(no genres listed)"] <- ""

tfidf_movies_genres_matrix<-tfidf_movies_genres$fit_transform(movies$genres)
#install.packages("e1071")
#library(e1071)
#svmfit = svm(tfidf_movies_genres_matrix, data = tfidf_movies_genres_matrix, kernel = "linear", cost = 10, scale = FALSE)

#install.packages("coop")
library(coop)

cosine_sim_movies<-cosine(t(tfidf_movies_genres_matrix))

#install.packages("itertools")
library(itertools)

get_recommendations_based_on_genres <- function(movie_title, cosine_sim_movies) {
  # Get the index of the movie that matches the title

  idx_movie<-movies[movies['title'] == movie_title]
  
  
  # Get the pairwsie similarity scores of all movies with that movie
  sim_scores_movies<-cosine_sim_movies[strtoi(idx_movie[1]), ]
  
  enumeration <- seq(1:length(sim_scores_movies))
  names(sim_scores_movies)<-enumeration
  
  
  movie_ids <- names(sort(sim_scores_movies, decreasing = TRUE))[2:3]
  return(movies[movies$movieId %in% movie_ids,])
  # Sort the movies based on the similarity scores
  
}

get_recommendations_based_on_genres("Father of the Bride Part II (1995)", cosine_sim_movies)


get_recommendation_content_model <- function(userId){
    
    recommended_movies <- NULL
  
    already_watched_movies <- ratings[ratings$userId ==userId,]
    
    for( i in seq(1:nrow(already_watched_movies))){
      result<-get_recommendations_based_on_genres(movies[movies$movieId == already_watched_movies[i,][1, "movieId"],][2][1, "title"], cosine_sim_movies)
      if(nrow(result)>0){
        for( j in seq(1:nrow(result))){
          if(any(already_watched_movies$movieId != result[j,][1, "movieId"])){
            recommended_movies<-rbind(recommended_movies, result[j,])
          }
        }
      }
    }
    return(recommended_movies)
}

h<-get_recommendation_content_model(1)


#KNN
library(class)

get_movie_label<- function(movie_id){
  return (knn(train=tfidf_movies_genres_matrix[1:8500,], test=tfidf_movies_genres_matrix[movie_id:movie_id,], cl=movies[1:8500,3:3], k=5))
}
true_count = 0
false_count = 0
evaluate_content_based_model<- function(movie_id){
  for( i in seq(1:nrow(movies))){
    movies_recommended_by_model = get_recommendations_based_on_genres(movies[i,][1, "title"], cosine_sim_movies)
    predicted_genres = get_movie_label(movies[i,][1, 'movieId'])
    predicted_genres <- as.vector(predicted_genres)
    for(var in 1:length(predicted_genres)){
      if(predicted_genres[var] == movies[i,][1, "genres"]){
        true_count <<- true_count + 1
      }
      else{
        #print("<<<<<<<<<<")
        #print(predicted_genres[var])
        #print(movies[i,][1, "genres"])
        #print(">>>>>>>>>>")
        false_count <<- false_count + 1
      }
    }
  }
}

evaluate_content_based_model()


total = false_count + true_count
hits = true_count/total
misses = false_count/total

print(paste0("hits: ", hits))
print(paste0("Misses ", misses))

