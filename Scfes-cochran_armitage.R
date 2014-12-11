library(dplyr)

#Data
story <- read.csv("storyevent.csv")
scorematch <- read.csv("scorematchevent.csv")
temp <- merge(story, scorematch, all=T)
temp <- data.frame(temp, start=as.Date(temp$startdate))
scfesall <-arrange(temp, start)

#Cochran-Armitage
scfes <- scfesall %>% select(userd, get1, start) %>% subset(!is.na(get1))
colnames(scfes) <- c("double", "all", "start")
prop.trend.test(scfes$double, scfes$all, scfes$start)

#Cochran-Armitage
taso <- scfesall %>% filter(character=="kotori") %>% select(userd, get1, start)
colnames(taso) <- c("double", "all", "start")
prop.trend.test(taso$double, taso$all, taso$start)

#plot
library(ggplot2)
scfes2 <- mutate(scfes, double, all, start, rate=double/all)
taso2 <- mutate(taso, double, all, start, rate=double/all)

graph1 <- ggplot(scfes2, aes(x=start, y=rate))
graph1 + geom_line()
ggsave("graph1.png")

graph2 <- ggplot(taso2, aes(x=start, y=rate))
graph2 + geom_line()
ggsave("graph2.png")