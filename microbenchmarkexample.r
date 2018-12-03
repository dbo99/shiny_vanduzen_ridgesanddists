date_time <- rep(seq(from=as.POSIXct("2018-12-01", tz="GMT"), 
                     to=as.POSIXct("2018-12-05", tz="GMT"), by="1 day"),3)
value <- c(1,2,NA,NA,5, NA,NA,NA,4,5,7,NA,NA,NA, 8)
class <- c(rep("a", 5), rep("b", 5), rep("c", 5))
df <- data.frame(date_time, value, class)

df[!is.na(date_time),]
df[complete.cases(value), ]


library(microbenchmark)
library(ggplot2)
library(dplyr)

df1 <- df
for(i in 1:10) df1 <- rbind(df1, df1)


mb <- microbenchmark(
  index = df[!is.na(df$value), ],
  na.omit = na.omit(df),
  dplyr = df %>% filter(is.na(value))
)

mb1 <- microbenchmark(
  index = df1[!is.na(df1$value), ],
  na.omit = na.omit(df1),
  dplyr = df1 %>% filter(is.na(value))
)
mb1

ap <- autoplot(mb)
ap1 <- autoplot(mb1)
cowplot::plot_grid(ap, ap1)
