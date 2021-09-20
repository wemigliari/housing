library(ggplot2)
library(tibble)
library(RColorBrewer)
library(dplyr)
library(tidyr)
library(stringr)
library(readxl)

### Graph 4 Household income

household_income <- read_excel("/Users/wemigliari/Documents/R/tabelas/household_income_eu.xlsx",
                               sheet = "Household Income")

household_income <- as.data.frame(household_income, na.rm=TRUE)

cols.num_household <- c(2:11)
household_income[cols.num_household] <- sapply(household_income[cols.num_household],as.numeric)

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

