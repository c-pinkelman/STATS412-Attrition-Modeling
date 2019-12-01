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
ibm <- ibm[,-c(16)]

# correlation anaylsis
ibm.cor <- as.matrix(ibm.num[,-c(5,6,18)])
colnames(ibm.cor)
cor(ibm.cor)
corrplot::corrplot(cor(ibm.cor), type = "upper")


## Hierarchical Logistic Model ##
# install library
#install.packages("lme4")
library(lme4)

# fit model with only department effect
fit <- glmer(attrition ~ (1 | department), family = binomial("logit"), data = ibm)
summary(fit)
fita <- glm(attrition ~ 1, data = ibm, family = binomial("logit")) 
logLik(fita)-logLik(fit)

fit <- glmer(attrition ~ (1 | overtime), family = binomial("logit"), data = ibm)
summary(fit)
fita <- glm(attrition ~ 1, data = ibm, family = binomial("logit")) 
logLik(fita)-logLik(fit)
# 'log Lik.' -3.979039e-12 (df=1)

(fit2 <- glmer(attrition ~ businesstravel + (1 | department), family = binomial("logit"), data
               = ibm)) 
