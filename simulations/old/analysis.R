require(dplyr)
require(ggplot2)
require(lme4)
require(lmerTest)
require(ordPens)

theme_update(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(colour = "black"),
             axis.text=element_text(size=20, colour = "black"), axis.title=element_text(size=18, face = "bold"), axis.title.x = element_text(vjust = 0),
             legend.title = element_text(size = 24, face = "bold"), legend.text = element_text(size = 20), plot.title = element_text(size = 26, face = "bold", vjust = 1))

setwd("~/Me/Psychology/Projects/choicesets/with_sam/simulations")

df <- read.csv('results/testing_cs.csv')
df$Choice <- as.factor(df$Choice)
df$MBrank <- factor(df$MBrank, ordered = T)
nTrials = length(unique(df$Trial))
nSubj = length(unique(df$Subj))

trial = 1;
df.filt = df #%>% filter(Trial == trial)

# Show that it's choice sets
df.rank = read.csv('results/MBrank.csv')
df.rank$MBrank <- factor(df.rank$MBrank, ordered = T)

ranks = unique(df.rank$MBrank)
numOfRank = NULL
probRank = NULL
rankCount_adj = NULL
for (i in 1:length(ranks)) {
  numOfRank[i] = sum(df.rank$MBrank == ranks[i])
  rankCount_adj[i] = sum(as.numeric(as.character(df$MBrank)) == as.numeric(as.character(ranks[i]))) / numOfRank[i]
  probRank[i] = rankCount_adj[i] / (nTrials * nSubj)
}
df.mbrank = data.frame(ranks = ranks, numOfRank = numOfRank, probRank = probRank)
df.mbrank$ranks <- factor(ranks, ordered = T)
df.mbrank$ranks_numeric <- as.numeric(as.character(df.mbrank$ranks))
df.mbrank$xsq <- df.mbrank$ranks_numeric ^ 2;

base = ggplot(data = df.mbrank, aes(x = ranks_numeric, y = probRank)) +
  geom_point(colour = "black", group = 1)
base + geom_smooth(method = 'lm', formula = y ~ choose(x - 1, 4))

mexp = lm(probRank ~ exp(.1 * ranks_numeric), data = df.mbrank)
mchoose = lm(probRank ~ choose(ranks_numeric - 1, 4), data = df.mbrank)

summary(mexp)
summary(mchoose)
AIC(mexp)
AIC(mchoose)

df.mb = as.data.frame(table(df.filt$MBrank) / ) %>%
  rename(choice = Var1, MBrank = Var2, count = Freq)

df.counts.rank = as.data.frame(table(df.filt$Choice, df.filt$MF_top, df.filt$MBrank)) %>%
  rename(choice = Var1, MFrank = Var2, MBrank = Var3, count = Freq)

df.counts.rank.col = df.counts.rank %>% group_by(MFrank, MBrank) %>% summarize(count = sum(count))
p1 = ggplot(df.counts.rank.col, aes(x = MBrank, y = count, colour = MFrank, group = MFrank)) +
  geom_line(aes(), size = 1) +
  geom_point(aes(), size = 5) +
  labs(x = "MB", y = "Number of times chosen") 

df.counts.rank = as.data.frame(table(df.filt$Choice, df.filt$MFrank, df.filt$MB_top)) %>%
  rename(choice = Var1, MFrank = Var2, MBrank = Var3, count = Freq)

df.counts.rank.col = df.counts.rank %>% group_by(MFrank, MBrank) %>% summarize(count = sum(count))
p2 = ggplot(df.counts.rank.col, aes(x = MFrank, y = count, colour = MBrank, group = MBrank)) +
  geom_line(aes(), size = 1) +
  geom_point(aes(), size = 5) +
  labs(x = "MF", y = "Number of times chosen") 

grid.arrange(p1, p2, ncol = 2)



k <- length(levels(df$Choice))
I <- diag(k-1)
J <- matrix(rep(1, (k-1)^2), c(k-1, k-1))

# Have to really learn this if I'm gonna do it
m1 <- MCMCglmm(Choice ~ -1 + trait + MFval * MBval,
               random = ~ idh(trait):Subj + idh(1+MFval * MBval):Subj,
               rcov = ~us(trait):units,
               # prior = list(R = list(fix = 1, V = .5 * (I + J), n = 4),
               #              G=list(G1 = list(V = diag(k), n = k),
               #                     G2 = list(V = diag(k), n = k))),
               family = c("categorical"), data = df)

npmlt(df$Choice ~ df$MFval, random = ~1, id = df$Subj, k = 2, link = "blogit", EB = FALSE)

data(schizo)
schizo$y <- as.factor(schizo$y)
npmlt(schizo$y~1+schizo$trt,random=~1,id=schizo$id,k=2,EB=FALSE,link="blogit")
