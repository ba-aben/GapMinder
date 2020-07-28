gapminder <- read.csv(file = 'data//gapminder_data.csv')
gapminder <- read.csv("https://raw.githubusercontent.com/swcarpentry/r-novice-gapminder/gh-pages/_episodes_rmd/data/gapminder_data.csv")
dim(gapminder)
str(gapminder)
head(gapminder)

xlist <- list(a = "Software Carpentry", b = 1:10, data = head(iris))
xlist[['b']][2]

mod <- aov(pop ~ lifeExp, data=gapminder)
attributes(mod)
mod$df.residual

gapminder[gapminder$year == 1957,]
gapminder[,-c(1:4)]
gapminder[gapminder$lifeExp > 80,]
gapminder[1, c(4, 5)]
gapminder[gapminder$year == 2002 | gapminder$year == 2007,]

gapminder_small <- gapminder[c(c(1:9), c(19:23)),]

# Write a script that loops through the gapminder data by continent and prints 
# out whether the mean life expectancy is smaller or larger than 50 years.
for (c in unique(gapminder$continent)){
  gapcont <- gapminder[gapminder$continent==c,]
  if (mean(gapcont$lifeExp) > 50){
    cat(c, '= smaller than 50\n')
  } else {
    cat(c, '= smaller than 50\n')
  }
  rm(gapcont)
}

# Write a script that loops over each country in the gapminder dataset, tests 
# whether the country starts with a ‘B’, and graphs life expectancy against time 
# as a line graph if the mean life expectancy is under 50 years.
for (c in unique(gapminder$country)){
  if (substring(c, 1, 1) %in% c('b', 'B')){
    cat(c, 'starts with B\n')
  }
  gapcount <- gapminder[gapminder$country==c,]
  if (mean(gapcount$lifeExp) < 50){
    plot(x = gapcount$year, 
         y= gapcount$lifeExp,
         main = c)
  }
}
  
grep("^B", unique(gapminder$country), value=T)

library(ggplot2)
ggplot(data = gapminder, mapping = aes(x = gdpPercap, y = lifeExp)) +
  geom_point(alpha = 0.5) + 
  scale_x_log10() +
  geom_smooth(method="lm")


### Vectorization ###

ggplot(data = gapminder[gapminder$country %in% c('China', 'India', 'Indonesia'),], mapping = aes(x= year, y = pop / 1e6))+
  geom_point()

# Matrix mulitplication
m <- matrix(1:12, nrow=3, ncol=4)
m
m2 <- matrix(1:8, nrow=4, ncol=2)
m2
m %*% m2

### Functions ###
source("functions/fnxns.R")

calcGDP(gapminder, year=2007, country="Australia")
