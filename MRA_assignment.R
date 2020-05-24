################################

# Market Basket Analysis

################################

install.packages("readxl") #For reading Excel file
install.packages("Rcpp")
#install.packages("dplyr") #For glimpse, etc.
#install.packages("plyr")
install.packages("magrittr") #For %>% Piping function
#install.packages("lubridate") #For Date & Time
install.packages("chron") #For Date & Time
#install.packages("ggplot2") #For graphical representation
#install.packages("cluster")
install.packages("arules")
install.packages("arulesViz", dependencies = TRUE)
#install.packages("datasets")

library(dplyr)
library(Rcpp)
library(magrittr)
library(lubridate)
library(chron)
library(readxl)
library(ggplot2)
library(cluster)
library(arules)
library(arulesViz)
library(datasets)


setwd("E:/r direct/MRA/Assignment")

#Load the data
trans <- read.transactions('groceries.csv', format = 'basket', sep=',')

trans
inspect(trans[1:5])
summary(trans)
itemFrequencyPlot(trans, topN=20, type='absolute') #Frequency plot - Display Result


# Create Association Rules
# support should be small , confidence =.8 (for smaller dataset)

rules <- apriori(trans, parameter = list(supp=0.001, conf=0.8)) # -- 410  rules
### NAVEEN CODE ADDITION #### 
rules <- sort(rules, by='confidence', decreasing = TRUE)
rules <- rules[is.redundant(rules)] ### 18 redundent rules 
inspect(rules[1:18])

rules <- sort(rules, by='confidence', decreasing = TRUE)
rules <- rules[!is.redundant(rules)] ### 392 non redudent rules
inspect(rules[1:50])

plot(rules, method="graph", layout=igraph::in_circle())

rules <- apriori(trans, parameter = list(supp=0.001, conf=0.8 , maxlen=3)) # --   29 rules
inspect(rules[1:20])

rules <- sort(rules, by='confidence', decreasing = TRUE)
rules <- rules[is.redundant(rules)] ### 0 redundent rules 

plot(rules, method="graph", layout=igraph::in_circle())

rules <- apriori(trans, parameter = list(supp=0.001, conf=0.8 , maxlen=3)) # --  29 rules

rules <- sort(rules, by='confidence', decreasing = TRUE)
rules <- rules[!is.redundant(rules)] ### 29 non redudent rules
inspect(rules[1:29])

plot(rules, method="graph", layout=igraph::in_circle())
### END OF NAVEEN ADDITION###

# rules <- apriori(trans, parameter = list(supp=0.0015, conf=0.8)) # -- 60 rules
# rules <- apriori(trans, parameter = list(supp=0.0018, conf=0.8)) # --  20 rules
# rules <- apriori(trans, parameter = list(supp=0.002, conf=0.8))  # -- 11 rules
# rules <- apriori(trans, parameter = list(supp=0.0025, conf=0.8))  # -- 3 rules
# rules <- apriori(trans, parameter = list(supp=0.003, conf=0.8))   # -- 1 rule
 

# sorting the rules according to confidence 
rules <- sort(rules, by='confidence', decreasing = TRUE)

summary(rules) #Display Result

#-----------------------------

inspect(rules[1:20]) # Display top " " rules
top20 <- rules[1:20]
plot(top20, method="graph") # Graphically display top rules


# scatter plot
plot (rules, measure=c("support", "lift"), shading="confidence")


#-----------------------------------------------------------------

# To find out what customers had purchased before buying 'Whole Milk'. 
# This will help you understand the patterns that led to the purchase of 'whole milk'.

rules1 <- apriori (trans, parameter=list (supp=0.001,conf = 0.08),
                  appearance = list (default="lhs",rhs="whole milk"), control = list (verbose=F))

rules1 <- sort(rules1, by='confidence', decreasing = TRUE)
inspect(rules1[1:10])


# In this case: the Customers who bought 'Whole Milk' also bought.
# In the equation, 'whole milk' is in LHS (left hand side).

rules2 <- apriori (trans, parameter=list (supp=0.001,conf = 0.15,minlen=2), 
                    appearance = list (default="rhs",lhs="whole milk"), control = list (verbose=F))

# 1:7 --out of bounds
inspect(rules2[1:6])




