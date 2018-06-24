# Panel data: lags & within means - a working example
#
require(plm)
require(dplyr)
#
#
# Balanced panel, add 1st lags and within mean
data("Cigar", package = "plm")
?Cigar
#
Cigar2 <- Cigar %>%  
          group_by(state) %>%
          mutate(price_1 = lag(price, k = 1), sales_1 = lag(sales, k = 1), sales_w = mean(sales, na.rm = T))
#
#
# Non-balanced panel, add 1st lags and within mean
# .. please note that there is no difference in calclating 
#    lags and within means for balanced/non-balanced panels
data("EmplUK", package = "plm")
?EmplUK
#
EmplUK2 <- EmplUK %>%  
  group_by(firm) %>%
  mutate(wage_1 = lag(wage, k = 1), capital_1 = lag(capital, k = 1), capital_w = mean(capital, na.rm = T)) 
