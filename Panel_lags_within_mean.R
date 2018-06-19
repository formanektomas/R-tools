# Panel data: lags & within means - a working example
#
require(plm)
library(dplyr)
#
#
# balanced panel
data("Cigar", package = "plm")
?Cigar
#
Cigar2 <- Cigar %>%  
          group_by(state) %>%
          mutate(price_1 = lag(price), sales_1 = lag(sales), sales_w = mean(sales))
#
#
# non-balanced panel
data("EmplUK", package = "plm")
?EmplUK
#
EmplUK2 <- EmplUK %>%  
  group_by(firm) %>%
  mutate(wage_1 = lag(wage), capital_1 = lag(capital), capital_w = mean(capital))
