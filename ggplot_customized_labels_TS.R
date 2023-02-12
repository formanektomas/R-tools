library(ggplot2)
library(Ecdat)
# data
CRSPmon <- Ecdat::CRSPmon
library(tidyr)
time <- as.Date(CRSPmon, format = "%Y-%m")
CRSPmon <- as.data.frame(CRSPmon, row.names = NULL)
CRSPmon$time <- time
CRSPmon <- CRSPmon[50:80,]
CRSPmon <- pivot_longer(CRSPmon, cols = 1:4, names_to = "stock", values_to = "ret")
# plot with label tweaking 
ggplot(CRSPmon, aes(x=time,y=ret,color=stock))+
  geom_line()+
  scale_x_date(date_breaks = "1 month", date_labels = "%m – %Y") +
  labs(title = "Stock returns",
       x = "Mont and year",
       y = "return",       
       color = "Stock identification") +
  theme(axis.text.x = element_text(angle = 90))
