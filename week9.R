library(lattice)
library(ggplot2)
library(tidyverse)
library(AER)
library(reshape2)
library(FLCore)

data("Fatalities")
f <- Fatalities

# Since the data contains a lot of info, I decided to drop some of the columns I am not interested in.
f <- f %>% 
  dplyr::select(-c("breath", "jail", "service", "baptist", "mormon","unempus", "emppopus", "gsp", "sfatal")) %>% 
  mutate(fatal_ratio = (fatal/pop)*100,
         night_fatal_ratio = (nfatal/pop)*100, 
         fatal_young_drivers = (fatal1517/pop1517)*100 + (fatal1820/pop1820)*100 + (fatal2124/pop2124)*100,
         n_fatal_young_drivers = (nfatal1517/pop1517)*100 + (nfatal1820/pop1820)*100 + (nfatal2124/pop2124)*100,
         alcohol_fatal = (afatal/pop)*100
         ) %>%
  dplyr::select(-c("fatal", "nfatal", "fatal1517", "fatal1820", "nfatal1517", "nfatal1820", "nfatal2124", "fatal2124", "pop1517", "pop1820", "pop2124", "afatal", "pop", "emppop", "milestot"))


splom(f[c(9,11,12,13,14,15)],
      main="Vehicle Fatality Data")

attach(f)
# Create factored lists for
age.f <- factor(round(drinkage, digits = 0), levels = c(18, 19,20,21))
spirits.f <- factor(round(spirits, digits = 0))

# source:https://www.statmethods.net/advgraphs/trellis.html
cloud(alcohol_fatal~fatal_young_drivers * night_fatal_ratio|age.f, group = spirits.f,
      main="Different Types of Vehicle Fatality by Legal Drinking Age", ylab = "Night", xlab = "Young Driver", zlab ="Drunk", auto.key = list(
        title="State Alcohol Consumption Level"
        , columns = 2, cex.title = 1))

f_cor <- cor(f[sapply(f,is.numeric)])

f_cor_m <- melt(f_cor)

ggplot(f_cor_m, aes(x = Var1, y = Var2, fill = value))+
  geom_tile() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), axis.title.x = element_blank(), axis.title.y = element_blank()) + 
  scale_fill_gradient(low = "beige", high = "red") +
  ggtitle("Correlation Heatmap of Fatality Data")

#https://datascience.csuchico.edu/event/ggplot2_intro1/
ggplot(f, aes(x = income, y = fatal_young_drivers, color = factor(round(drinkage, digits = 0)))) + geom_point() + facet_grid(~round(drinkage, digits = 0)) + labs(title = "Vehicle Fatality Involving Young Drivers by Legal Drinking Age", subtitle = "Involving drivers from age 18 to 24.", x = "State Income Levels", y = "Fatality Involving Young Drivers", color = "Legal Drinking Age")
  
ggplot(f, aes(x = dry, y = fatal_young_drivers, color = paste(round(beertax, digits = 0), "%"))) + geom_point() + facet_grid(~paste(round(beertax, digits = 0), "%")) + labs(title = "Vehicle Fatality Involving Young Drivers by Alcohol Consumption and \nUnemployment Rate\n", x = "\nDry County Residency", y = "Young Drivers Fatality Rate\n", color = "Beertax\nPercentage")

       