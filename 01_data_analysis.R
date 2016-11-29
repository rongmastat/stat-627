library(plyr)
library(dplyr)
library(tidyr)
gap <- read.delim("gapminder.tsv", sep="\t")

#A Function Computing the GDP increasing rate for each country
gdp.rate <- function(x, year = 12){
  stopifnot(is.data.frame(x))
  stopifnot(is.numeric(x$gdpPercap))
  rate <- x %>% mutate(rate = (gdpPercap - lag(gdpPercap))/lag(gdpPercap)) %>% select(rate)
  return(rate[2:year,1])
}

#Generate extreme.rate.data showing maximum and minimum GDP increasing rate for each country.
gdp.rate.data <- ddply(gap, ~country, gdp.rate)
years <- gap %>% select(year) %>% unique()
year.index <- paste0(years[1:11,1], rep("/",11),years[2:12,1])
names(gdp.rate.data)[-1] <- year.index
max.rate <- gdp.rate.data %>% select(2:dim(years)[1]) %>% apply(1,max)
min.rate <- gdp.rate.data %>% select(2:dim(years)[1]) %>% apply(1,min)
continent <- (gap %>% select(country,continent) %>% unique())[,2]
extreme.rate.data <- gdp.rate.data %>% mutate(max.rate,min.rate,continent) %>% select(country, continent, max.rate, min.rate)
extreme.rate.data[,3:4] <- extreme.rate.data[,3:4] %>% round(digits = 3)

#write file
write.csv(extreme.rate.data, "extreme_rate_data.csv", row.names = FALSE)
