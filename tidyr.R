# http://swcarpentry.github.io/r-novice-gapminder/14-tidyr/index.html

library(tidyr)
library(dplyr)

gapminder <- read.csv("https://raw.githubusercontent.com/swcarpentry/r-novice-gapminder/gh-pages/_episodes_rmd/data/gapminder_data.csv")
gapminder_wide <- read.csv('https://raw.githubusercontent.com/swcarpentry/r-novice-gapminder/gh-pages/_episodes_rmd/data/gapminder_wide.csv',
                           stringsAsFactors = FALSE)
str(gapminder_wide)

### from wide to long: pivot_longer() ###

gap_long <- gapminder_wide %>%
  pivot_longer(
    cols = c(starts_with('pop'), starts_with('lifeExp'), 
             starts_with('gdpPercap')), # vector of column names that will be pivoted into longer format
    names_to = "obstype_year", values_to = "obs_values"
  )
str(gap_long)

gap_long <- gapminder_wide %>%
  pivot_longer(
    cols = c(-continent, -country), # vector of column names that will NOT be pivoted into longer format
    names_to = "obstype_year", values_to = "obs_values"
  )
str(gap_long)

# Now obstype_year actually contains 2 pieces of information, the observation type 
# (pop,lifeExp, or gdpPercap) and the year. We can use the separate() function 
# to split the character strings into multiple variables
gap_long <- gap_long %>% 
  separate(obstype_year, into = c('obs_type', 'year'), sep = "_") %>% 
  mutate(year = as.integer(year))

# Using gap_long, calculate the mean life expectancy, population, and gdpPercap 
# for each continent. Hint: use the group_by() and summarize() functions we 
# learned in the dplyr lesson
gap_long %>% 
  group_by(continent, obs_type) %>% 
  summarize(
    meanObsValue = mean(obs_values)
  )

### from long to intermediate: pivot_wider() ###

gap_normal <- gap_long %>%
  pivot_wider(names_from = obs_type, values_from = obs_values)
str(gap_normal)
str(gapminder)

# Fix the order of the columns and check if gap_normal == gapminder
gap_normal <- gap_normal[, names(gapminder)]
all.equal(gap_normal, gapminder)

### from long to wide: pivot_wider() ###

# First we need to create appropriate labels for all our new variables (time*metric 
# combinations) and we also need to unify our ID variables to simplify the process 
# of defining gap_wide.

str(gap_long)
gap_temp <- gap_long %>%
  unite(ID_var, continent, country, sep = "_") %>%
  unite(var_names, obs_type, year, sep = "_") %>% 
  pivot_wider(names_from = var_names, values_from = obs_values) %>% 
  separate(ID_var, into = c('continent', 'country'), sep = "_")
str(gap_temp)

# Create a gap_ludicrously_wide format data by pivoting over countries, year and 
# the 3 metrics? Hint this new dataframe should only have 5 rows.
str(gap_long)
gap_temp <- gap_long %>%
  unite(var_names, obs_type, year, country, sep = "_") %>% 
  pivot_wider(names_from = var_names, values_from = obs_values)
str(gap_temp)
