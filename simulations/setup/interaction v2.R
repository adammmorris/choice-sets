require(dplyr)
require(ggplot2)
require(mlogit)
require(ggstatsplot)
require(rje)
require(plotly)
library(mvtnorm)

J = 12
bmean = 4.5
bstd = .5
N = 400
K = 1
numMenus = 1

ps.cs = matrix(nrow = numMenus, ncol = 2)
ps.mix = matrix(nrow = numMenus, ncol = 2)

coefs.cs = matrix(nrow = numMenus, ncol = 2)
coefs.mix = matrix(nrow = numMenus, ncol = 2)

s1s = matrix(nrow = N, ncol = J)
s2s = matrix(nrow = N, ncol = J)

for (menu in 1:numMenus) {
  
  df = data.frame(Subj = NULL, Trial = NULL, Trial_unique = NULL, OptionID = NULL, Choice = NULL, X1 = NULL, X2 = NULL, X3 = NULL)
  #rho <- -0.75
  #Cor <- array(c(1, rho, rho, 1), dim=c(2,2))
  
  s1 = c(1, 1, 3, 3, 3, 4, 6, 6, 7, 10, 14, 15)
  s2 = c(12, 22, 21, 13, 14, 18, 15, 9, 11, 7, 5, 6)
  # s1 = c(2, 2, 2,  4,  6,  6,  7,  9, 10, 10, 11, 12)
  # s2 = c(24, 21, 19, 18, 16, 13, 12, 11,  8,  6,  4, 2)
  
  for (subj in 1:N) {
    # # Y = rmvnorm(12, sigma=Cor)
    # #s1 = round(pnorm(Y[,1]) * 14 + 1)
    # #s2 = round(pnorm(Y[,2]) * 24 + 1)
    s1s[subj,] = s1
    s2s[subj,] = s2
    
    b1 = rgamma(1, 0, bstd)
    b2 = rgamma(1, 10, bstd)
    b3 = 0

    betas = c(b1, b2, b3)

    recall = as.logical(round(runif(J))) #+ .1 * (s1 > median(s1)) + .1 * (s2 > median(s2))))
    recall[sample(J, J)] = T
    #recall[which.max(s1)] = F
    recalled.words = which(recall)
    num.recalled = sum(recall)
    
    s1_temp = s1
    cond = 'normal'
    if (runif(1) < .5) {
       s1_temp = max(s1_temp) + 1 - s1_temp
       cond = 'reversed'
    }
    
    x1 = s1_temp[recall]
    x2 = s2[recall]
    x3 = x1 * x2
    
    SS = sample(2:5,1)
    if (SS > num.recalled) {
      SS = num.recalled
    }
    
    for (trial in 1:K) {
      attributes = matrix(c(x1 / max(x1),x2 / max(x2),x3 / max(x3)), nrow = 3, ncol = num.recalled, byrow = T)

      probs = exp(betas %*% attributes) / sum(exp(betas %*% attributes))
      choice.mix = sample(recalled.words, 1, F, probs)

      # (1) get sample!
      probs1 = exp(b1 * x1 / max(x1)) / sum(exp(b1 * x1 / max(x1)))
      s = sample(num.recalled, SS, F, probs1)

      # (2) choose
      probs2 = exp(b2 * x2[s] / max(x2[s])) / sum(exp(b2 * x2[s] / max(x2[s])))
      choice.cs = recalled.words[sample(s, 1, T, probs2)]
      #choice = s[which.max(x2[s])]

      for (alt in 1:num.recalled) {
        x1_rank = rank(x1, ties.method = 'max')
        df = rbind(df, data.frame(Subj = subj, Trial_unique = paste(subj, trial, sep="_"),
                                  OptionID = alt, Choice.cs = choice.cs == recalled.words[alt], Choice.mix = choice.mix == recalled.words[alt],
                                  MFval = x1[alt], MBval = x2[alt], MFhigh = factor(x1[alt] > median(s1_temp), c(T,F), c('high','low')),
                                  MFrank = x1_rank[alt],
                                  Cond = factor(cond, c('normal', 'reversed'), c('normal', 'reversed'))))
      }
    }
  }

  df = df %>% mutate(Total = 1.5 * MFval / max(s1) + 10 * MBval / max(s2), #Cond = as.numeric(Cond),
                     MBval.exp = MBval^2, Total.exp = exp(Total))
  df.cs = mlogit.data(df, choice = "Choice.cs", shape = "long", alt.var = "OptionID", chid.var = "Trial_unique")
  df.mix = mlogit.data(df, choice = "Choice.mix", shape = "long", alt.var = "OptionID", chid.var = "Trial_unique")
  
  m.cs = mlogit(Choice.cs ~ MFval + MBval | -1, df.cs)
  summary(m.cs)
  m.mix = mlogit(Choice.mix ~ MFval + MBval | -1, df.mix)
  summary(m.mix)
  
  ps.cs[menu,1] = summary(m.cs)$CoefTable[3,4]
  ps.mix[menu,1] = summary(m.mix)$CoefTable[3,4]
  
  coefs.cs[menu,1] = m.cs$coefficients[3]
  coefs.mix[menu,1] = m.mix$coefficients[3]
  
  df.sum = df %>% mutate(MFval = MFhigh) %>%
    group_by(Cond, MFval,MBval,Subj) %>% summarize(Choice = any(Choice.cs)) %>%
    group_by(Cond, MFval,MBval) %>%
    summarize(Choice.mean = mean(Choice), Choice.se = sqrt(Choice.mean * (1 - Choice.mean) / n()))
  ggplot(data = df.sum, aes(x = MBval, y = Choice.mean, colour = MFval, group = MFval)) +
    geom_point(aes(size = 2)) + geom_line() +
    geom_errorbar(aes(ymin=Choice.mean - Choice.se, ymax = Choice.mean + Choice.se), width = .2) +
    guides(size = FALSE) + facet_wrap(~Cond)
  # 
  # ggplot(data = df.sum, aes(x = MBval, y = Choice.log, colour = MFval, group = MFval)) +
  #   geom_point(aes(size = 2)) + geom_line() +
  #   geom_errorbar(aes(ymin=Choice.log - Choice.se, ymax = Choice.log + Choice.se), width = .2) +
  #   guides(size = FALSE)
  
  df.total = df %>% mutate(Total = MBval) %>%
    group_by(Cond, Total, Subj) %>%
    summarize(Choice.cs = any(Choice.cs), Choice.mix = any(Choice.mix)) %>%
    group_by(Cond, Total) %>%
    summarize(Choice.cs.m = mean(Choice.cs), Choice.mix.m = mean(Choice.mix))
  ggplot(data = df.total, aes(x = Total, y = Choice.cs.m)) +
    geom_point(aes(size = 2)) + geom_line() +
    #geom_errorbar(aes(ymin=Choice.cs - Choice.se, ymax = Choice.mean + Choice.se), width = .2) +
    guides(size = FALSE) + facet_wrap(~Cond)
  
  m.cs.total = summary(lm(Choice.cs.m ~ exp(MBval) * Cond, data = df.total))
  coefs.cs[menu,2] = m.cs.total$coefficients[4,1]
  ps.cs[menu,2] = m.cs.total$coefficients[4,4]
  m.cs.total = summary(mlogit(Choice.cs ~ MBval | -1, df.cs))
  # coefs.cs[menu,2] = m.cs.total$coefficients[4]
  # ps.cs[menu,2] = m.cs.total$CoefTable[4,4]
  
  m.mix.total = summary(lm(Choice.mix.m ~ exp(MBval) * Cond, data = df.total))
  coefs.mix[menu,2] = m.mix.total$coefficients[4,1]
  ps.mix[menu,2] = m.mix.total$coefficients[4,4]
  m.mix.total = summary(mlogit(Choice.mix ~ MBval | -1, df.mix))
  # coefs.mix[menu,2] = m.mix.total$coefficients[4]
  # ps.mix[menu,2] = m.mix.total$CoefTable[4,4]
}

save.image('powertest_curvature.rdata')

#which(coefs.cs.bin == max(coefs.cs.bin[coefs.mix.bin < .1 & !(coefs.cs.bin %in% coefs.cs.bin[c(23)])]))
  

# analytic ----------------------------------------------------------------
 
getProb = function(word_ind, x1, b1, x2, b2, k) {
  probs1 = exp(b1 * x1 / max(x1)) / sum(exp(b1 * x1 / max(x1)))
  
  words = 1:length(probs1)
  sets = t(combn(words[-word_ind], k-1))
  sets = cbind(sets, rep(word_ind, nrow(sets)))
  
  prob = 0
  
  for (set_ind in 1:nrow(sets)) {
    set = sets[set_ind,]
    pset = powerSet(set)
    
    complement = sum(probs1[setdiff(words, set)])
    
    temp_prob = vector(mode = "numeric", length = length(pset))
    for (subset_ind in 1:length(pset)) {
      subset = pset[[subset_ind]]
      temp_prob[subset_ind] = (-1) ^ length(subset) / (1 + sum(probs1[subset]) / complement)
    }
    
    set_prob = sum(temp_prob)
    probs2 = exp(b2 * x2[set] / max(x2[set])) / sum(exp(b2 * x2[set] / max(x2[set])))
    prob = prob + probs2[k] * set_prob
  }
  
  return(prob)
}
getProb.mix = function(word_ind, x1, b1, x2, b2) {
  probs = exp(b1 * x1 / max(x1) + b2 * x2 / max(x2)) / sum(exp(b1 * x1 / max(x1) + b2 * x2 / max(x2)))
  return(probs[word_ind])
}

b1 = 1
b2 = 10
k = 4
J = 12

# s1 = c(1, 1, 3, 3, 3, 4, 6, 6, 7, 10, 14, 15)
# s2 = c(12, 22, 21, 13, 14, 18, 15, 9, 11, 7, 5, 6)
s1 = c(2, 2, 2,  4,  6,  6,  7,  9, 10, 10, 11, 12)
s2 = c(24, 21, 19, 18, 16, 13, 12, 11,  8,  6,  4, 2)
s1_rev = max(s1) + 1 - s1


df.graph2 = data.frame()
for (word_ind in 1:J) {
  df.graph2 = rbind(df.graph2, data.frame(MFval = s1[word_ind], MFhigh = factor(s1[word_ind] > median(s1)),
                                          MBval = s2[word_ind],
                                    Choice.cs = getProb(word_ind, s1, b1, s2, b2, k), Choice.mix = getProb.mix(word_ind, s1, b1, s2, b2),
                                    Cond = 'normal'))
  df.graph2 = rbind(df.graph2, data.frame(MFval = s1_rev[word_ind], MFhigh = factor(s1_rev[word_ind] > median(s1_rev)), MBval = s2[word_ind],
                                    Choice.cs = getProb(word_ind, s1_rev, b1, s2, b2, k), Choice.mix = getProb.mix(word_ind, s1_rev, b1, s2, b2),
                                    Cond = 'reversed'))
}

df.graph2 = df.graph2 %>% mutate(Total = b1 * MFval / max(s1) + b2 * MBval / max(s2))
                                 #Choice.cs = log(Choice.cs+1e-100), Choice.mix = log(Choice.mix+1e-100))

ggplot(data = df.graph2, aes(x = MBval)) +
  #geom_point(aes(y = Choice.cs, size = 2)) + geom_line(aes(y = Choice.cs)) +
  geom_point(aes(y = Choice.mix, size = 2)) + geom_line(aes(y = Choice.mix)) +
  guides(size = FALSE) + facet_wrap(~ Cond)
  #geom_smooth(method = 'lm', formula = log(y) ~ x)
  #geom_smooth()

test.linear = lm(log(Choice.mix) ~ poly(MBval, 1), df.graph2 %>% filter(Cond == 'normal'))
test.nonlinear = lm(log(Choice.mix) ~ poly(MBval, 2), df.graph2 %>% filter(Cond == 'normal'))
anova(test.linear, test.nonlinear)

test.linear = lm(Choice.cs ~ poly(MBval, 1), df.graph2 %>% filter(Cond == 'normal'))
test.nonlinear = lm(Choice.cs ~ poly(MBval, 2), df.graph2 %>% filter(Cond == 'normal'))
anova(test.linear, test.nonlinear)

ggplot(data = df.graph2, aes(x = Total)) +
  geom_point(aes(y = Choice.cs), size = 5, color = 'blue') +
  geom_point(aes(y = Choice.mix), size = 5, color = 'red')

summary(lm(Choice.mix ~ Diff * Total, data = df.graph2))

plot_ly(df.graph2, x = ~Total, y = ~Diff, z = ~Choice.cs, marker = list(color = ~Choice.cs))

test2 = loess(Choice.mix ~ Total * Diff, data = df.graph2, span = 1)
test2.pred = predict(test2, newdata = expand.grid(list(Total = 5:35, Diff = 0:20)))
persp(test2.pred)

#test = surf.ls(np = 1, x = df.graph2$MBval, y = df.graph2$MFval, z = df.graph2$Choice.mix)
#test2 = trmat(test, xl = min(s1), xu = max(s1), yl = min(s2), yu = max(s2), n = 20)
#contour(test2)

test = lm(Choice.cs ~ poly(MFval, MBval, degree = 3), data = df.graph2)
contour(test, MBval ~ MFval)

SurfMod <- contour(test, MBval ~ MFval)

# extract list values from rsm Surface Model 
Xvals <- SurfMod$`MFval ~ MBval`[1]
Yvals <- SurfMod$`MFval ~ MBval`[2]
Zvals <- SurfMod$`MFval ~ MBval`[3]

# Construct matrix with col and row names 
SurfMatrix <- Zvals$z
colnames(SurfMatrix) <- Yvals$y
rownames(SurfMatrix) <- Xvals$x

# Convert matrix to data frame
library(reshape2)
SurfDF <- melt(SurfMatrix)

ggplot(data = SurfDF) +
  geom_tile(data = SurfDF, aes(Var1, Var2,z = value, fill = value)) +
  stat_contour(data = SurfDF, aes(Var1, Var2, z = value, color = ..level..)) +
  scale_colour_gradient(low = "green", high = "red") +
  geom_point(data = df.graph2, aes(MFval, MBval, z = Choice.cs, color = Choice.cs))
  
# analysis 2

s1 = 1:20
s2 = 1:20

df.graph2 = data.frame()
for (word_ind in 1:20) {
  for (word_ind2 in 1:20) {
    df.graph2 = rbind(df.graph2, data.frame(MFval = s1[word_ind], MFhigh = factor(s1[word_ind] > median(s1)), MBval = s2[word_ind2],
                                            Choice.cs = getProb(word_ind, word_ind2, s1, b1, s2, b2, k), Choice.mix = getProb.mix(word_ind, word_ind2, s1, b1, s2, b2)))
    df.graph2 = rbind(df.graph2, data.frame(MFval = s1_rev[word_ind], MFhigh = factor(s1_rev[word_ind] > median(s1_rev)), MBval = s2[word_ind2],
                                            Choice.cs = getProb(word_ind, word_ind2, s1_rev, b1, s2, b2, k), Choice.mix = getProb.mix(word_ind, word_ind2, s1_rev, b1, s2, b2)))
  }
}


# bootstrapping -----------------------------------------------------------


# num.bs = 500
# bs.sample = N
# bs.coefs.cs = numeric(num.bs)
# bs.pcts = matrix(nrow = num.bs, ncol = 4)
# bs.coefs.mix = numeric(num.bs)
# 
# for (i in 1:num.bs) {
#   which.subjs = sample(N, bs.sample, T)
#   df.bs = data.frame()
#   for (j in 1:length(which.subjs)) {
#     which.rows = df.cs$Subj == which.subjs[j]
#     df.bs = rbind(df.bs, df.cs[which.rows, c(1,2,4,5,6,10,11)])
#     df.bs$Subj[(nrow(df.bs) - sum(which.rows)) : nrow(df.bs)] = j
#   }
# 
#   df.bs.logit = mlogit.data(df.bs, choice = "Choice.cs", shape = "long", alt.var = "OptionID")
# 
#   m.bs = mlogit(Choice.cs ~ MFhigh * MBhigh | -1, df.bs.logit)
#   bs.coefs.cs[i] = m.bs$coefficients[3]
#   
#   df.bs.coll = df.bs %>% mutate(MFhigh = factor(MFhigh), MBhigh = factor(MBhigh)) %>%
#     group_by(MFhigh,MBhigh,Subj) %>% summarize(Choice = any(Choice.cs)) %>%
#     group_by(MFhigh,MBhigh) %>%
#     summarize(Choice.mean = mean(Choice))
#   bs.pcts[i,] = df.bs.coll$Choice.mean
# }
