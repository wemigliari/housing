library(dplyr)
library(readxl)

housing_index2 <- read_excel("/Users/wemigliari/Documents/R/tabelas/housing_price_rents_index_eu.xlsx",
                             sheet = "Housing Price Rents")

### Graph 2
library(tibble)

housing_index4 <- as.data.frame(housing_index2, na.rm=TRUE)
col.nam <- housing_index4[2:11]
housing_mean <- sapply(col.nam, mean, na.rm = TRUE)
housing_mean <- as.data.frame(housing_mean)

##housing_mean <- colMeans(housing_index4[sapply(housing_index4, is.numeric)])
##housing_mean <- data.frame(housing_mean)


housing_mean <- rownames_to_column(housing_mean, var = "Years") %>% as_tibble()

ggplot(housing_mean, aes(x=Years, y= housing_mean, group = 1)) +         ### https://stackoverflow.com/questions/27082601/ggplot2-line-chart-gives-geom-path-each-group-consist-of-only-one-observation
  geom_line(color="red") + 
  annotate("rect", xmin = "2020-Q1", xmax = "2021-Q4", ymin = 0, ymax = 150,
           alpha = .2) +
  xlab("") +
  theme_ipsum() +
  theme(plot.caption = element_text(size = 10)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +   ## Verticle x labels
  labs(title="Graphic 2. Annual and Quarterly Growth Rates (Average), Housing Prices, Eurozone, 2018-2020",
       y = "Index Levels (2015=100)", caption = "Source: Eurostat. Elaborated by Migliari, W. (2021).") +
  annotate(geom="text", x= "2020-Q1", y=100, label="SARS-CoV", 
           angle = 90,
           size=3,
           color = "#3E3E3E")+
  geom_vline(xintercept="2020-Q1", linetype="dotted", color = "red")


