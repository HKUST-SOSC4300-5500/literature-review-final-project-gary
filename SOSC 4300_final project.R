library(ggplot2)
library(cowplot)
library(randomForest)

## NOTE: The data used in this demo comes from 
## https://www.chinafile.com/infographics/visualizing-chinas-anti-corruption-campaign
## Specifically, this is data set for better understanding the massive campaign against corruption that China’s leader, Xi Jinping, launched shortly after he came to power in late 2012.


#####################################
##
## Reformat the data so that it is 
## 1) Easy to use (add nice column names)
## 2) Interpreted correctly by randomForest..
##
#####################################

# Import the data and look at the first six rows
data <- read.csv(file = '/Users/choyuching/Downloads/Corruption Viz - Data - CSV_7.31.2018_final copy2.csv')
head(data)
str(data)
