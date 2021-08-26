install.packages(c("readr", "tidyverse", "arules"))

#loAD Library to wrangle data and perform market basket analysis.
library(readr)
library(tidyverse)
library(arules)

#import the dataset
train <- read_csv("train.csv")
test <- read_csv("test.csv")

# Remove the survived column from training data and Combine all the rows
# into a single dataframe.
combined <- rbind(train %>% select( -Survived),test)

#
#use the mighty dplyr and engineer some new features
#
titanic.features <- combined %>%
  mutate(FirstClass = (Pclass == 1),
         SecondClass = (Pclass == 2),
         ThirdClass = (Pclass == 3),
         Female = (Sex == "female"),
         AgeMissing = is.na(Age),
         Child = (!is.na(Age) & Age < 18),
         Adult = (!is.na(Age) & Age > 17 & Age < 55),
         Elderly = (!is.na(Age) & Age >= 55))

#
# Use the Ticket feature as grouping a mechanism and engineer some features
#
titanic.groups <- combined %>%
  group_by(Ticket) %>%
  summarize(IsSolo = (n() == 1),
            IsCouple = (n() == 2),
            IsTriplet = (n() == 3),
            IsGroup = (n() > 3),
            HasChild = ifelse(sum(!is.na(Age)) > 0, min(Age, na.rm = TRUE) < 18, FALSE),
            HasElderly = ifelse(sum(!is.na(Age)) > 0, max(Age, na.rm = TRUE) >= 55, FALSE),
            NoAges =sum(!is.na(Age)) == 0)

#
# The apriori function of the arules package works with logical features,
#select only those add on the group features
#
titanic.binary <- titanic.features %>%
  inner_join(titanic.groups, by = "Ticket") %>%
  select(FirstClass,
         SecondClass,
         ThirdClass,
         Female,
         AgeMissing,
         Child,
         Adult,
         Elderly,
         IsSolo,
         IsCouple,
         IsTriplet,
         IsGroup,
         HasChild,
         HasElderly,
         NoAges)

#
# Split out the training data and add back the survival feature,
# transforming it into a logical
#
train.binary <- titanic.binary[1:891, ]
train.binary$Survived <- train$Survived == 1

# Market Basket Analysis
# transform the feature data frame to an arules transactions object
#
titanic.trans <- as(train.binary, "transactions")

#
# this line of code performs the work of MBA:
# 1 - Specifies a minimum support of 0.05 and confidence of 0.1
# 2 - Specifies a maximum of two "items".
# 3 - "Pins" the Survived "item" as the right-hand side (rhs)
#
# Net-result will be all the non-survived items on the lhs.
#
rules.one.feature <- apriori (data = titanic.trans,
                              parameter = list(supp = 0.05, conf = 0.1, minlen = 2, maxlen = 2),
                              appearance = list(default="lhs", rhs = "Survived"),
                              control = list(verbose = FALSE))

#
# sort and list top 20 rules in descending order
# by the lift metric.
#
rules.one.lift <- sort (rules.one.feature, by  = "lift", decreasing = TRUE)
inspect(head(rules.one.lift, n = 20))

#
# this line of code performs the work of MBA:
# 1 - Specifies a minimum support of 0.05 and confidence of 0.1
# 2 - Specifies a maximum of four "items".
# 3 - "Pins" the Survived "item" as the right-hand side (rhs)
#
# Net-result will be all the non-survived items on the lhs.
rules.three.features <- apriori(data = titanic.trans,
                                parameter = list(supp = 0.05, conf = 0.1, minlen = 2, maxlen = 4),
                                appearance = list(default = "lhs", rhs = "Survived"),
                                control = list(verbose = FALSE))

rules.three.lift <- sort(rules.three.features, by = "lift", decreasing = TRUE)
inspect(head(rules.three.lift, n = 20))