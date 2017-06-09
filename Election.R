library(ggplot2)
library(data.table)
library(tidyverse)
library(fitdistrplus)

master.data <- data.table(read.csv("pivottablefull.csv"))
levels(master.data$Party) <- c("CON", "LAB", "LIB", "OTH", "OTH", "OTH")
master.data <- master.data %>%
  group_by(Constituency, Year, Party) %>%
  mutate(Vote = as.integer(sum(Vote))) %>%
  unique %>%
  data.table %>%
  mutate(Vote2 = ifelse(Vote == 0, 1, Vote)) %>%
  group_by(Constituency, Year) %>%
  mutate(vote.share.adj = (Vote2)/sum(Vote2)) %>%
  data.table

year.party <- expand.grid(unique(master.data$Party), unique(master.data$Year))

betas <- list()
for(i in 1:nrow(year.party)){
  x <- master.data$vote.share.adj[master.data$Party == year.party$Var1[i] &
                                  master.data$Year == year.party$Var2[i]]
  betas[[i]] <- fitdist(x, "beta")
}

for(i in 1:nrow(year.party))
  plot(betas[[i]])

