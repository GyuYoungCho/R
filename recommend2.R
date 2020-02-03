getwd()
rm(list = ls())
table_in = read.csv("anonymous-msweb.test.csv",header = FALSE)

library(data.table)
table_users = table_in[,1:2]
table_users = data.table(table_users)
setnames(table_users, 1:2, c("category","value"))
table_users = table_users[category %in% c("C","V")]

table_users[,chunk_user := cumsum(category == "C")]
table_long = table_users[,list(user=value[1],item=value[-1]),by = "chunk_user"]
table_long[,value :=1]
table_wide = reshape(data = table_long, direction = "wide", idvar = "user",
                     timevar = "item", v.names = "value")
vector_users = table_wide[,user]
table_wide[,user :=NULL]
table_wide[, chunk_user := NULL]
setnames(x = table_wide, old = names(table_wide), new = substring(names(table_wide),7))

matrix_wide = as.matrix(table_wide)
rownames(matrix_wide) = vector_users
matrix_wide[is.na(matrix_wide)] = 0


rating_matrix = as(matrix_wide, "binaryRatingMatrix")
rating_matrix
x11()
image(rating_matrix[1:50,1:50])


n_users = colCounts(rating_matrix)
qplot(n_users)+stat_bin(binwidth = 100)+ggtitle("distribution")
rating_matrix = rating_matrix[,colCounts(rating_matrix)>=5]
rating_matrix
sum(rowCounts(rating_matrix)==0)




table_in = data.table(table_in)
table_items = table_in[V1=="A"]
table_items = table_items[,c(2,4,5),with=FALSE]
setnames(table_items, 1:3 ,c("id","description","url"))
table_items = table_items[order(id)]

table_items[,category :="product"]

install.packages("countrycode_data")
library(countrycode_data)
countrycode
name_countries = c(codelist$country.name.en, "Taiwan",  
                   "UK", "Russia","Venezuela","Slovenija","Carbbean", "Netherlands (Holland)",
                   "Europe","Central America","MS North Africa")
table_items[description %in% name_countries, category := "region"]
table_items[grepl("Region",description),category := "region"]

table_items[,list(n_items =.N),by = category]

which_train = sample(x=c(TRUE,FALSE),size =nrow(rating_matrix),replace=TRUE, prob = c(0.8,0.2))
recc_data_train = rating_matrix[which_train,]
recc_data_test= rating_matrix[!which_train,]

recc_model = Recommender(data =recc_data_train, method="IBCF", parameter=list(method = "Jaccard"))
image(recc_model@model$sim)
dist_ratings = as(recc_model@model$sim,"matrix")
dist_category = table_items[,1-dist(category=="product")]
dist_category = as(dist_category,"matrix")
dim(dist_category)
rownames(dist_category) = table_items[,id]
colnames(dist_category) = table_items[,id]
vector_items = rownames(dist_ratings)
dist_category = dist_category[vector_items, vector_items]
image(dist_category)



weight_category = 0.25
dist_tot = dist_category * weight_category + dist_ratings*(1 - weight_category)
image(dist_tot)

recc_model@model$sim = as(dist_tot, "dgCMatrix")
n_recommend = 10
recc_predict = predict(object = recc_model, newdata = recc_data_test, n = n_recommend)
head(recc_predict@itemLabels)

table_labels = data.frame(id = recc_predict@itemLabels)
table_labels = merge(table_labels, table_items, by = "id", all.x=TRUE, all.y = FALSE, sort=FALSE)
descriptions  = as(table_labels$description,"character")

recc_user1 = recc_predict@items[[1]]
items_user1 = descriptions[recc_user1]
head(items_user1)

