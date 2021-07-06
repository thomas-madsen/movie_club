setwd("~/Downloads/ml-latest")
ratings <- read.csv("ratings.csv")
ratings$userId <- factor(ratings$userId)
ratings$movieId <- factor(ratings$movieId)

library(dplyr)
mean_ratings <- ratings %>%
  group_by(movieId) %>%
  summarise_at(vars(rating), list(mean_rating=mean))

n_reviews = c(table(ratings$movieId))

ratings_reviews = cbind(n_reviews,mean_ratings[,2])
high_volume_movies = levels(ratings$movieId)[n_reviews > 50]
high_volume_movie_indices = which(n_reviews > 50)
ratings_reviews = ratings_reviews[high_volume_movie_indices,]

library(ggplot2)
ggplot(ratings_reviews, aes(y=mean_rating,x=n_reviews)) +
  geom_point() + 
  scale_x_continuous(trans='log10') + 
  geom_smooth(method='lm') + 
  labs(title = "Mean Rating vs. Number of Reviews", subtitle = "n = 13270 movies", caption = "For each tenfold increase in reviews, average rating increases by 0.135 (on a five-point scale)") + 
  ylab("Mean Rating")+
  xlab("Number of Reviews")

ratings_reviews_lm = lm(mean_rating ~ log10(n_reviews), ratings_reviews)
summary(ratings_reviews_lm)

ratings_subset = ratings[ratings$movieId %in% high_volume_movies,]

movie_1_ratings = rep(NA, nlevels(ratings_subset$userId))
movie_1_reviews = ratings_subset[ratings_subset$movieId == "89492",]
movie_1_ratings[as.numeric(movie_1_reviews$userId)] = movie_1_reviews$rating

n_comps = 5000

movie_1_comp_ratings = rep(NA,n_comps)
movie_1_corrs = rep(NA,n_comps)
movie_1_n_pairs = rep(NA,n_comps)

for(comp_id in 1:n_comps){
  movie_1_comp_ratings[comp_id] = ratings_reviews$mean_rating[comp_id + 1]
  
  movie_2_ratings = rep(NA, nlevels(ratings_subset$userId))
  movie_2_reviews = ratings_subset[ratings_subset$movieId == toString(comp_id + 1),]
  movie_2_ratings[as.numeric(movie_2_reviews$userId)] = movie_2_reviews$rating
  
  n_common_reviewers = sum(!is.na(movie_1_ratings) & !is.na(movie_2_ratings))
  movie_1_n_pairs[comp_id] = n_common_reviewers
  if(n_common_reviewers > 10){
    movie_1_corrs[comp_id] = cor(movie_1_ratings,movie_2_ratings,use="complete.obs")
  }
}

movie_1_comps = data.frame(movie_1_comp_ratings,movie_1_corrs,movie_1_n_pairs)
colnames(movie_1_comps) <- c("mean_rating","corr","n_common")

ggplot(movie_1_comps,aes(x=corr,y=mean_rating)) +
  geom_point(aes(size=n_common)) +
  ylab("Mean Rating") +
  xlab("Correlation with Rating for `Moneyball'") +
  labs(title = "Mean Rating vs. Correlation with `Moneyball' Rating", subtitle = "n = 13270 movies",size="In-Common \n Reviews", caption = "Movies with 11+ in-common reviewers")
  
