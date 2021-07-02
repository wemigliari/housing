library(dplyr)
library(readxl)

### Graph 1

housing_index2 <- read_excel("Documents/R/tabelas/housing_price_rents_index_eu.xlsx",
                             sheet = "Housing Price Rents")

cols.num <- c(2:11)
housing_index2[cols.num] <- sapply(housing_index2[cols.num],as.numeric)

housing_index3 <- pivot_longer(housing_index2, cols=2:11, names_to = "Years", values_to = "Values")
housing_index3 <- data.frame(housing_index3)


ggplot(housing_index3, aes(x=Years)) +
  geom_line(aes(y = Values, size = 1, linetype = "dotted", colour="steelblue")) + 
  xlab("") +
  theme_ipsum() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title="Graphic 1. Annual and Quarterly Growth Rates (Variation), Housing Prices (EU), 2018-2020",
       y = "Index Levels (2015=100)", caption = "Source: Eurostat. Elaborated by Migliari, W. (2021).") +
  annotate(geom="text", x= "2020-Q1", y=125, label="SARS-Cov", 
           angle = 90,
           size=3,
           color = "white") +
  geom_vline(xintercept="2020-Q1", linetype="dotted", color = "black") +
  theme(legend.position = "none")

