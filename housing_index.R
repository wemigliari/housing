library(ggplot2)
library(tibble)
library(RColorBrewer)
library(dplyr)
library(tidyr)
library(stringr)
library(readxl)
library(viridis)
library(hrbrthemes)
library(Ipaper)

housing_index <- read_excel("/Users/wemigliari/Documents/R/tabelas/housing_index_eu.xlsx",
                    sheet = "Housing Index")

housing_index1 <- pivot_longer(housing_index, cols=2:21, names_to = "Years", values_to = "Values")



### Boxplot           https://mgimond.github.io/ES218/Week03b.html

housing_index1 %>%
  ggplot(aes(x = Years, y=Values, fill=Years), alpha = 90) +
  geom_boxplot2(width = 0.8, width.errorbar = 0.5) +
  scale_fill_viridis(discrete = TRUE, alpha=0.4) +
  #geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("") +
  labs(title = "Boxplots. Annual and Quarterly Growth Rates (Variation), 2001-2020", y = "Deflated Housing Price Index, European Union", caption = "Source: Eurostat. Elaborated by Migliari, W. (2021).") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  annotate(geom="text", x="2010", y=20, label="Housing Price Index, 2010 = 100", 
           angle = 90,
           size=3,
           color = "#3E3E3E") +
  geom_vline(xintercept="2010", linetype="dotted", color = "orange")

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
  annotate(geom="text", x= "2020-01", y=-2, label="SARS-Cov", 
           angle = 90,
           size=3,
           color = "black") +
  geom_vline(xintercept="2020-01", linetype="dotted", color = "black") +
  theme(legend.position = "none")

### Graph 4 Household income

library(tibble)
library(RColorBrewer)
library(dplyr)
library(stringr)


household_income <- read_excel("Documents/R/tabelas/household_income_eu.xlsx",
                        sheet = "Household Income")

household_income <- as.data.frame(household_income, na.rm=TRUE)

cols.num_household <- c(2:11)
household_income[cols.num_household] <- sapply(household_income[cols.num],as.numeric)

household_income2<- pivot_longer(household_income, cols=2:11, names_to = "Years", values_to = "Values")
household_income2 <- data.frame(household_income2)
class(household_income2)


household_income3 <- household_income2 %>% 
  filter(str_detect(Countries, "European Union|Spain|Portugal|Greece|Belgium|Netherlands|France|Germany"))

household_income4 <- aggregate(household_income3[,3], list(household_income3$Countries), mean)
colnames(household_income4)[c(1:2)] <- c("Countries","Values")

#household_income4  <- household_income4 %>% arrange(Values)  O comando ordenou os valores, mas o ggplot não aceitou o comando.

household_income4$Countries <- factor(household_income4$Countries, levels = household_income4$Countries[order(household_income4$Values)])


area.color <- c("red", "red", "red", "orange", "orange","gold", "gold", "gold")

ggplot(household_income4, aes(x = Countries, y = Values)) + 
  theme_bw() + 
  labs(title = "Graphic 4. Annual Household Income Rates (Average), 2011-2020", 
       x = "", y = "Index Levels (2011=100)", caption = "Source: Eurostat. Elaborated by W. Migliari (2021).") +
  geom_bar(stat = "identity", fill = area.color) +
  geom_text(aes(label=Values), vjust=1.6, color="black", size=3.5)

