library(ggplot2)
library(gridExtra)

#Read csv file
extreme.rate.data <- read.csv("extreme_rate_data.csv", header = TRUE)

#Generate some figures

p <- ggplot(extreme.rate.data,aes(log(max.rate),min.rate)) 
p <- p + geom_point(aes(colour = continent))

ggsave("gdp_rate_plot.png", p)