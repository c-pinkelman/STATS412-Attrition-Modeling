library(readr)
library(tidyverse)

## Read in data ##
ibm <- read_csv("Stats412 - Adv Regression/WA_Fn-UseC_-HR-Employee-Attrition.csv")
View(head(ibm))
colnames(ibm) <- tolower(colnames(ibm))

## Exploratory Anaylsis ##
str(ibm)
table(ibm$attrition)/nrow(ibm)

# attrition per department
View(ibm %>% group_by(department) %>%
       summarise(n = n(),
                 att_rate = length(which(attrition == "Yes"))/n))

# get summary statistics
ibm.num <- ibm[,-c(2,3,5,8,12,16,18,22,23)]
ibm.sum <- ibm.num %>%
  summarise_all(funs(min = min, 
                     q25 = quantile(., 0.25), 
                     median = median, 
                     q75 = quantile(., 0.75), 
                     max = max,
                     mean = mean, 
                     sd = sd))

ibm.stats.tidy <- ibm.sum %>%  gather(stat, val) %>%
  separate(stat, into = c("var", "stat"), sep = "_") %>%
  spread(stat, val) %>%
  select(var, min, q25, median, q75, max, mean, sd) # reorder columns
ibm.stats.tidy <- cbind(ibm.stats.tidy$var, round(ibm.stats.tidy[,2:8], 2))

# count missings
View(sapply(ibm, function(y) sum(length(which(is.na(y))))))

# contingency tables for categorical variables
cats <- colnames(ibm)[c(2,3,5,8,12,16,18,22,23)]
for (i in 1:length(cats)){
  col_i <- cats[i]
  print(col_i); print(i)
  print(table(ibm$attrition, ibm[[col_i]]))
  ibm[[col_i]] <- as.factor(ibm[[col_i]])
}

# remove job role variable
ibm <- colnames(ibm[,-c(16)])

# correlation anaylsis
ibm.cor <- ibm.num[,-c(5,6,18)]
colnames(ibm.cor)
cor(ibm.cor)
corrplot::corrplot(ibm.cor)






