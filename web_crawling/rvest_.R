install.packages("rvest")
library(rvest)
url <- 'https://www.imdb.com/search/title?count=100&release_date=2019,2019&title_type=feature'
webpage = read_html(url)
webpage
temp = html_nodes(webpage,'.lister-item-header a')
temp

html_text(temp)

temp2 = html_nodes(webpage,'.ratings-imdb-rating strong')
temp2
html_text(temp2)

temp3 = html_nodes(webpage,'.text-primary')
temp3
x = html_text(temp3);x
x = gsub("[.]","",x)
x

temp4 = html_nodes(webpage,'.text-muted.unbold')
temp4
x = html_text(temp4);x
x = as.numeric(gsub("[()]|([I]+)","",x))
x

dat = data.frame(title = html_text(temp), rating = html_text(temp2))
head(dat)


temp3 = html_nodes(webpage,'.sort-num_votes-visible span:nth-child(2)')
t3_text = html_text(temp3)
vote = as.numeric(gsub("[,]","",t3_text))
vote


dat = data.frame(title = html_text(temp), rating = html_text(temp2), votes = vote)
head(dat)


str_date ='2019-01-01'
end_date = '2019-12-31'
cnt = 100;str_num=1
url = paste0('https://www.imdb.com/search/title?title_type=feature&release_date=',str_date,",",end_date,"&count=",cnt,"&start=",str_num,"&ref_=adv_nxt")

page = read_html(url)
temp = html_node(page,".desc span:nth-child(1)")
html_text(temp)
strsplit(html_text(temp),"[ ]")[[1]][3]
num_tt = as.numeric(gsub(",","",strsplit(html_text(temp),"[ ]")[[1]][3]))
num_tt

num_page = ceiling(num_tt/cnt)
my_list = list()
for(i in 1:5){
    url = paste0('https://www.imdb.com/search/title?title_type=feature&release_date=',str_date,",",end_date,"&count=",cnt,"&start=",str_num,"&ref_=adv_nxt")
    page = read_html(url)
    temp = html_nodes(page,'.lister-item-header a')
    temp = html_text(temp)
    
    temp2 = html_nodes(page,'.ratings-imdb-rating strong')
    temp2 = html_text(temp2)
    
    my_list[[i]] = list(temp,temp2)
    Sys.sleep(runif(1,0.5,2))
    str_num = str_num + cnt
    cat('\n Page : ',i,' \ Total : ',num_page)
}
my_list


# https://movie.naver.com/movie/bi/mi/basic.nhn?code=136900

# iframe src="/movie/bi/mi/pointWriteFormList.nhn?code=136900&type=after&isActualPointWriteExecute=false&isMileageSubscriptionAlready=false&isMileageSubscriptionReject=false"

url = "https://movie.naver.com/movie/bi/mi/pointWriteFormList.nhn?code=136900&type=after&isActualPointWriteExecute=false&isMileageSubscriptionAlready=false&isMileageSubscriptionReject=false"
i = 1
url_temp = paste0(url,"&page=",i)
url_temp

my_webpage = read_html(url_temp)
tt_cnt_temp = html_nodes(my_webpage,".score_reple p")
html_text(tt_cnt_temp)


tt_cnt_temp = html_nodes(my_webpage,".total em")
y_count = html_text(tt_cnt_temp)
y_count = gsub("[,]","",y_count)
tt_cnt = as.numeric(y_count)    
tt_cnt

page_indx = 1:ceiling(tt_cnt/10)
length(page_indx)

rating = numeric()
u_name = character()
review = character()
for(i in 1:3){
    w_temp = read_html(paste0(url,"&page=",i))
    r_temp = html_nodes(w_temp, ".score_result .star_score em")
    rating = c(rating,as.numeric(html_text(r_temp)))
    u_temp = html_nodes(w_temp,".score_reple a span")
    u_txt = html_text(u_temp)
    u_txt = u_txt[u_txt!="BEST"]
    u_name = c(u_name,u_txt)
    t_temp = html_nodes(my_webpage,".score_reple p")
    t_txt = html_text(t_temp)
    t_txt = gsub("[\r\n\t]","",t_txt)
    review = c(review,t_txt)
    Sys.sleep(runif(1,1,3))
    cat("\n",i," page")
}

rating
data.frame(rating,u_name)
review


temp = gsub(")","",u_name)
temp_list = strsplit(temp,"[(]")
res = sapply(temp_list,function(x) x[length(x)])
head(res)

x11()
plot(table(rating),type="h")
