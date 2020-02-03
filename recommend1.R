rm(list=ls())
x1= rnorm(30)
x2 = rnorm(30)
dist(rbind(x1,x2),method="euclidean")
mtcars
cor(mtcars,method="pearson")
USArrests
library(cluster)
library(ggplot2)
data(iris)
iris$Species = as.numeric(iris$Species)
cost_df = data.frame()
for(i in 1:100){
  kmeans = kmeans(x=iris,center=i,iter.max = 50)
  cost_df = rbind(cost_df, cbind(i,kmeans$tot.withinss))
}
names(cost_df) = c("cluster","cost")
ggplot(data=cost_df, aes(x=cluster, y=cost, group=1))+
  theme_bw(base_family = "Garamond")+
  geom_line(colour = "darkgreen")+
  theme(text = element_text(size=20))+
  ggtitle("reduction cost\n")+
  xlab("\nclusters")+
  ylab("with cluster\n")


library(e1071)
sample = iris[sample(nrow(iris)),]
train = sample[1:105,]
test = sample[106:150,]
tune = tune(svm,Species~. , data=train, kernel = "radial",scale=FALSE,
            ranges = list(cost=c(0.001,0.01,0.1,1,5,10,100)))
tune$best.model
summary(tune)
model = svm(Species~.,data=train,kernel="radial",cost=10,scale=FALSE)
predict(model,test)




install.packages("recommenderlab")
library(recommenderlab)
set.seed(1)

data_package = data(package="recommenderlab")
data_package$results[,"Item"]
data(MovieLense)
methods(class=class(MovieLense))
similarity_users = similarity(MovieLense[1:4,],method="cosine",which="users")
as.matrix(similarity_users)
x11()
image(as.matrix(similarity_users))
similarity_items = similarity(MovieLense[,1:4],method = "cosine", which = "items")
as.matrix(similarity_items)


recommender_models = recommenderRegistry$get_entries(dataType = "realRatingMatrix")
names(recommender_models)
str(MovieLense)
dim(MovieLense)
vector_rating = as.vector(MovieLense@data)
unique(vector_rating)
vector_rating = vector_rating[vector_rating != 0]
table_rating = table(vector_rating)
vector_rating = as.factor(vector_rating)
qplot(vector_rating) + ggtitle("distribution")
views_per_movie = colCounts(MovieLense)
table_views = data.frame(movie = names(views_per_movie),views = views_per_movie)
table_views  = table_views[order(table_views$views,decreasing = TRUE),]
ggplot(table_views[1:6, ], aes(x=movie, y =views))+
  geom_bar(stat="identity")+ theme(axis.text.x = element_text(angle=45, hjust = 1))

average_rating = colMeans(MovieLense)

average_rating_relevant = average_rating[views_per_movie>100]
qplot(average_rating_relevant) + stat_bin(binwidth = 0.1)

image(MovieLense)

min_n_movies = quantile(rowCounts(MovieLense),0.99)
min_n_users = quantile(colCounts(MovieLense),0.99)
image(MovieLense[rowCounts(MovieLense)>min_n_movies,
                 colCounts(MovieLense)>min_n_users])


# 데이터 준비.
rating_movies = MovieLense[rowCounts(MovieLense)>50, colCounts(MovieLense)>100]
average_rating_per_user = rowMeans(rating_movies)
qplot(average_rating_per_user)+stat_bin(bicwidth=0.1)

rating_movies_norm = normalize(rating_movies)
sum(rowMeans(rating_movies_norm)>0.0001)


which_set = sample(x=1:5, size = nrow(rating_movies),
                     replace = TRUE)
for (i_model in 1:5){
  which_train = which_set == i_model
  recc_data_train = rating_movies[which_train,]
  recc_data_test = rating_movies[!which_train,]
}

recommender_models = recommenderRegistry$get_entries(dataType="realRatingMatrix")
recommender_models$IBCF_realRatingMatrix$parameters

recc_model = Recommender(data=recc_data_train, method = "IBCF",parameter=list(k=30))
models_details = getModel(recc_model)
models_details$description
dim(models_details$sim)
row_sums = rowSums(models_details$sim>0)
col_sums = colSums(models_details$sim>0)
qplot(col_sums)+stat_bin(binwidth = 1)


which_max = order(col_sums, decreasing = TRUE)[1:6]
rownames(models_details$sim)[which_max]
n_recommended = 6
recc_predicted = predict(object = recc_model, newdata=recc_data_test,
                         n = n_recommended)
recc_user1 = recc_predicted@items[[1]]
movie_user1 = recc_predicted@itemLabels[recc_user1]

recc_matrix = sapply(recc_predicted@items, function(x){colnames(rating_movies)[x]})
dim(recc_matrix)
recc_matrix[,1:4]

number_of_items = factor(table(recc_matrix))
chart_title =" distribution"
qplot(number_of_items)+ ggtitle(chart_title)

number_of_items_sort = sort(number_of_items, decreasing = TRUE)
number_of_items_top = head(number_of_items_sort,n=4)
table_top = data.frame(names(number_of_items_top),number_of_items_top)
