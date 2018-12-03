Sys.setenv(TZ="America/Los_Angeles")
date_time <- rep(seq(from=as.POSIXct("2018-12-01", tz="GMT"), 
                     to=as.POSIXct("2018-12-05", tz="GMT"), by="1 day"),3)
value <- c(1,2,NA,NA,5, NA,NA,NA,4,5,7,NA,NA,NA, 8)
class <- c(rep("a", 5), rep("b", 5), rep("c", 5))
df <- data.frame(date_time, value, class)




library(microbenchmark)
library(ggplot2)
library(dplyr)

fun <- function(DF){
  DF2 <- DF[!is.na(DF$value), ]
  u <- unique(DF2$class)
  sp <- split(DF2, DF2$date_time)
  inx <- sapply(sp, function(d){
    all(u %in% d$class)
  })
  DF2 <- do.call(rbind, sp[inx])
  row.names(DF2) <- NULL
  DF2
}

df1 <- df
for(i in 1:10) df1 <- rbind(df1, df1)


mb <- microbenchmark(
  ruifun = fun(df),
  
  dplyr = df %>% group_by(date_time) %>%
    filter(all(!is.na(value)))
)

mb1 <- microbenchmark(
  ruifun = fun(df1),

  dplyr = df1 %>% group_by(date_time) %>%
    filter(all(!is.na(value)))
)
mb1

ap <- autoplot(mb)
ap1 <- autoplot(mb1)
cowplot::plot_grid(ap, ap1)
