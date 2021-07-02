library(dplyr)
library(readxl)

### Graph 3

inflation <- read_excel("Documents/R/tabelas/inflation_rates_eu.xlsx",
                        sheet = "Inflation Rate")

library(tibble)

inflation1 <- as.data.frame(inflation, na.rm=TRUE)

cols.num_inflation <- c(2:21)
inflation1[cols.num_inflation] <- sapply(inflation1[cols.num],as.numeric)

inflation2 <- pivot_longer(inflation1, cols=2:21, names_to = "Years", values_to = "Values")
inflation2 <- data.frame(inflation2)
class(inflation2)


ggplot(inflation2, aes(x=Years)) +
  geom_line(aes(y = Values, size = 1, linetype = "dotted", colour="steelblue")) + 
  xlab("") +
  theme_ipsum() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title="Graphic 3. Annual and Quarterly Inflation Rates (Variation), European Union, 2019-2021",
       y = "Index Levels (2015=100)", caption = "Source: Eurostat. Elaborated by Migliari, W. (2021).") +
  annotate(geom="text", x= "2020-01", y=-2, label="SARS-CoV", 
           angle = 90,
           size=3,
           color = "black") +
  geom_vline(xintercept="2020-01", linetype="dotted", color = "black") +
  theme(legend.position = "none")

