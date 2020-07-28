### Splitting and Combining Data Frames with plyr ###

# http://swcarpentry.github.io/r-novice-gapminder/12-plyr/index.html
# Cheatsheet: https://raw.githubusercontent.com/rstudio/cheatsheets/master/data-transformation.pdf

library("plyr")

# The functions are named based on the data structure they expect as input, e.g.:
# [d]ata.frame --> [a]rray : daply
# Note here that plyr’s use of “array” is different to R’s, an array in ply can 
# include a vector or matrix.

# Takes a dataset and multiplies the population column
# with the GDP per capita column.
calcGDP <- function(dat, year=NULL, country=NULL) {
  if(!is.null(year)) {
    dat <- dat[dat$year %in% year, ]
  }
  if (!is.null(country)) {
    dat <- dat[dat$country %in% country,]
  }
  gdp <- dat$pop * dat$gdpPercap
  
  new <- cbind(dat, gdp=gdp)
  return(new)
}

# calculate the mean GDP per continent
gapminder <- read.csv("https://raw.githubusercontent.com/swcarpentry/r-novice-gapminder/gh-pages/_episodes_rmd/data/gapminder_data.csv")
class(gapminder) # data.frame
names(gapminder)

# General structure:
# xxply(
#   .data, 
#   .variables, # split on
#   .fun)

ddply(
  .data = calcGDP(gapminder),
  .variables = "continent",
  .fun = function(x) mean(x$gdp) # each split gets temp stored in x
)

# What if we want a different type of output data structure?:
dlply(
  .data = calcGDP(gapminder),
  .variables = "continent",
  .fun = function(x) mean(x$gdp) # each split gets temp stored in x
)

# Specify multiple columns to group by
df <- ddply(
  .data = calcGDP(gapminder),
  .variables = c("continent", "year"),
  .fun = function(x) mean(x$gdp) # each split gets temp stored in x
)
df

arr <- daply(
  .data = calcGDP(gapminder),
  .variables = c("continent", "year"),
  .fun = function(x) mean(x$gdp) # each split gets temp stored in x
)
arr
row.names(arr)
colnames(arr)

# Calculate the average life expectancy per continent and year. 
daply(
  .data = gapminder,
  .variables = c("continent", "year"),
  .fun = function(x) mean(x$lifeExp) # each split gets temp stored in x
)

# Alternative using pipes from dplyr
df2 <- gapminder %>% # use ctrl-shift-m for shortcut pipe
  group_by(continent, year) %>%
  summarize(mean_lifeExp = mean(lifeExp))
df2

gdp_pop_bycontinents_byyear <- gapminder %>%
  group_by(continent, year) %>%
  summarize(mean_gdpPercap = mean(gdpPercap),
            sd_gdpPercap = sd(gdpPercap),
            mean_pop = mean(pop),
            sd_pop = sd(pop))

# Which had the longest and shortest in 2007? 
library(dplyr)
df2 %>%
  filter(year == 2007) %>%
  arrange(desc(mean_lifeExp))

# Which had the greatest change in between 1952 and 2007?
df2 %>%
  filter(year %in% c(1952, 2007))

# select only a few of the variables
year_country_gdp <- gapminder %>% 
  select(year,country,gdpPercap)

year_country_gdp_euro <- gapminder %>%
  filter(continent == "Europe") %>%
  select(year, country, gdpPercap)

# Count
gapminder %>%
  filter(year == 2002) %>%
  count(continent, sort = TRUE)

# If we need to use the number of observations in calculations, the n() function 
# is useful. It will return the total number of observations in the current group 
# rather than counting the number of observations in each group within a specific 
# column.
gapminder %>%
  group_by(continent) %>%
  summarize(se_le = sd(lifeExp)/sqrt(n()))

# Create new variables prior to (or even after) summarizing information using 
# mutate()
gdp_pop_bycontinents_byyear <- gapminder %>%
  mutate(gdp_billion = gdpPercap*pop/10^9) %>%
  group_by(continent,year) %>%
  summarize(mean_gdpPercap = mean(gdpPercap),
            sd_gdpPercap = sd(gdpPercap),
            mean_pop = mean(pop),
            sd_pop = sd(pop),
            mean_gdp_billion = mean(gdp_billion),
            sd_gdp_billion = sd(gdp_billion))
  
## keeping all data but "filtering" after a certain condition
# calculate GDP only for people with a life expectation above 25
gdp_pop_bycontinents_byyear_above25 <- gapminder %>%
  mutate(gdp_billion = ifelse(lifeExp > 25, gdpPercap * pop / 10^9, NA)) %>%
  group_by(continent, year) %>%
  summarize(mean_gdpPercap = mean(gdpPercap),
            sd_gdpPercap = sd(gdpPercap),
            mean_pop = mean(pop),
            sd_pop = sd(pop),
            mean_gdp_billion = mean(gdp_billion),
            sd_gdp_billion = sd(gdp_billion))

## updating only if certain condition is fullfilled
# for life expectations above 40 years, the gpd to be expected in the future is scaled
gdp_future_bycontinents_byyear_high_lifeExp <- gapminder %>%
  mutate(gdp_futureExpectation = ifelse(lifeExp > 40, gdpPercap * 1.5, gdpPercap)) %>%
  group_by(continent, year) %>%
  summarize(mean_gdpPercap = mean(gdpPercap),
            mean_gdpPercap_expected = mean(gdp_futureExpectation))

### dplyr and ggplot ###

## The following...

# Get the start letter of each country
starts.with <- substr(gapminder$country, start = 1, stop = 1)
# Filter countries that start with "A" or "Z"
az.countries <- gapminder[starts.with %in% c("A", "Z"), ]
# Make the plot
ggplot(data = az.countries, aes(x = year, y = lifeExp, color = continent)) +
  geom_line() + facet_wrap( ~ country)

# ... can be transforemd with pipes and combined with ggplot into:

gapminder %>%
  # Get the start letter of each country
  mutate(startsWith = substr(country, start = 1, stop = 1)) %>%
  # Filter countries that start with "A" or "Z"
  filter(startsWith %in% c("A", "Z")) %>%
  # Make the plot
  ggplot(aes(x = year, y = lifeExp, color = continent)) +
  geom_line() +
  facet_wrap( ~ country)

# Calculate the average life expectancy in 2002 of 2 randomly selected countries 
# for each continent. Then arrange the continent names in reverse order. Hint: 
# Use the dplyr functions arrange() and sample_n(), they have similar syntax to 
# other dplyr functions.
gapminder %>%
  filter(year == 2002) %>% 
  group_by(continent) %>% 
  sample_n(size = 2) %>%
  summarize(meanLifeExp = mean(lifeExp)) %>% 
  arrange(desc(continent))




