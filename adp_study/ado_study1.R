install.packages("caret")
library(dplyr)
library(ggplot2)
house = read.table("http://www.rossmanchance.com/iscam2/data/housing.txt", header = T, sep = "\t")

glimpse(house)
install.packages("sqldf")
library(sqldf)
sqldf("select * from iris")

df1 = data.frame(x=c(1,2), y=2:1)
df2 = data.frame(x=c(1,3), a=10)
sqldf("select * from df1 inner join df2 on df1.x=df2.x")

install.packages("gapminder")
library(gapminder)

head(gapminder)
# 정렬
gapminder[order(gapminder$year, gapminder$country),]

f2 = gapminder
names(f2)[6] = 'gdp_per_cap'

f2$total_gdp = f2$pop * f2$gdpPercap

median(gapminder$gdpPercap)
apply(gapminder[,4:6],2,mean)
summary(gapminder)

library(dplyr)
iris %>% head

iris %>% filter(Sepal.Length>5.4)
gapminder
arrange(gapminder, year)

gapminder %>% select(pop)
gapminder %>% mutate(total_gdp = pop * gdpPercap)
gapminder %>% summarise(n_obs = n(), n_country = n_distinct(country),
                        med_gdpc = median(gdpPercap))

gapminder %>% select(country) %>% distinct()
gapminder %>% filter(year==2007) %>% group_by(continent) %>% summarise(lifeExp = median(lifeExp)) %>% arrange(-lifeExp)
cor(gapminder$lifeExp, gapminder$gdpPercap)

x11()
opar = par(mfrow=c(2,2))
hist(gapminder$lifeExp)
hist(gapminder$gdpPercap)
hist(log10(gapminder$gdpPercap),nclass=50)
plot(log10(gapminder$gdpPercap), gapminder$lifeExp, cex=.5)
par(opar)
cor(gapminder$lifeExp, log10(gapminder$gdpPercap))


gapminder %>% ggplot(aes(x=lifeExp)) + geom_histogram()
gapminder %>% ggplot(aes(x=gdpPercap, y=lifeExp)) + geom_point() + scale_x_log10() + geom_smooth()

df = data.frame(gp = factor(rep(letters[1:3],each=10)), y=rnorm(30))
glimpse(df)
ds = df %>% group_by(gp) %>% summarise(mean = mean(y), sd = sd(y))

df %>% ggplot(aes(x=gp, y=y)) + geom_point() + geom_point(data = ds,aes(y = mean), colour = 'red', size=3)


gapminder %>% ggplot(aes(x=gdpPercap)) + geom_freqpoly() + scale_x_log10()
summary(gapminder)

table(diamonds$cut)
prop.table(table(diamonds$cut))

diamonds %>% ggplot(aes(cut)) + geom_bar()

diamonds %>%
    group_by(cut) %>%
    tally() %>%
    mutate(pct = round(n/sum(n)*100,1))

diamonds %>% ggplot(aes(carat, price)) + geom_point(alpha=0.1)

mpg %>% ggplot(aes(cyl, hwy)) + geom_jitter()

pairs(diamonds %>% sample_n(1000))

mpg %>% ggplot(aes(class, hwy)) + geom_boxplot()
mpg %>% ggplot(aes(class, hwy)) + geom_jitter(col='gray') + geom_boxplot(alpha=.5)

mpg %>% mutate(class=reorder(class,hwy, median)) %>% ggplot(aes(class, hwy)) + geom_jitter(col='gray') + geom_boxplot(alpha=.5)
mpg %>% mutate(class=reorder(class,hwy, median)) %>% ggplot(aes(class, hwy)) + geom_jitter(col='gray') + geom_boxplot(alpha=.5) + coord_flip()


glimpse(data.frame(Titanic))
xtabs(Freq ~ Class + Sex + Age + Survived, data.frame(Titanic))  # 고차원 행렬 나타내기

mosaicplot(Titanic, main="t")

apply(Titanic, c(3,4), sum)
prop.table(apply(Titanic, c(3,4), sum), margin = 1)

t2 = data.frame(Titanic)
t2 %>% group_by(Sex) %>% summarise(n = sum(Freq), survivors = sum(ifelse(Survived=='Yes',Freq,0))) %>% mutate(rate_survial=survivors/n)


gapminder %>% filter(year==2007) %>% ggplot(aes(gdpPercap, lifeExp)) +
    geom_point(aes(size=pop, col=continent)) + scale_x_log10() + ggtitle("gap 2007")

gapminder %>% ggplot(aes(year, lifeExp, group=country, col=continent)) + geom_line()

gapminder %>% ggplot(aes(year, lifeExp, group=country)) + geom_line() + facet_wrap(~ continent)
