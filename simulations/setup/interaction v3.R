# prelims -----------------------------------------------------------------

require(dplyr)
require(ggplot2)
require(mlogit)
require(ggstatsplot)
require(rje)
require(plotly)
library(mvtnorm)

getProb = function(word_ind, x1, b1, x2, b2, k) {
  probs1 = exp(b1 * x1 / max(x1)) / sum(exp(b1 * x1 / max(x1)))
  
  words = 1:length(probs1)
  if (k-1 == 1) {
    sets = array(words[-word_ind])
  } else {
    sets = t(combn(words[-word_ind], k-1))
  }
  sets = cbind(sets, rep(word_ind, nrow(sets)))
  
  prob = 0
  
  for (set_ind in 1:nrow(sets)) {
    set = sets[set_ind,]
    
    if (length(setdiff(words,set)) == 0) {
      set_prob = 1
    } else {
      pset = powerSet(set)
      
      complement = sum(probs1[setdiff(words, set)])
      
      temp_prob = vector(mode = "numeric", length = length(pset))
      for (subset_ind in 1:length(pset)) {
        subset = pset[[subset_ind]]
        temp_prob[subset_ind] = (-1) ^ length(subset) / (1 + sum(probs1[subset]) / complement)
      }
      
      set_prob = sum(temp_prob)
    }
    
    probs2 = exp(b2 * x2[set] / max(x2[set])) / sum(exp(b2 * x2[set] / max(x2[set])))
    prob = prob + probs2[k] * set_prob
  }
  
  return(prob)
}
getProb.mix = function(word_ind, x1, b1, x2, b2) {
  probs = exp(b1 * x1 / max(x1) + b2 * x2 / max(x2)) / sum(exp(b1 * x1 / max(x1) + b2 * x2 / max(x2)))
  return(probs[word_ind])
}

sse.cs = function(x, df, k) {
  ss = 0
  for (i in 1:nrow(df)) {
    ss = ss + (df$Choice.cs[i] - getProb(i,df$MFval, x[1], df$MBval, x[2], k)) ^ 2
  }
  return(ss)
}
sse.mix = function(x, df) {
  ss = 0
  denom = sum(exp(x[1] * df$MFval / max(df$MFval) + x[2] * df$MBval / max(df$MBval)))
  for (i in 1:nrow(df)) {
    ss = ss + (df$Choice.mix[i] - (exp(x[1] * df$MFval[i] / max(df$MFval) + x[2] * df$MBval[i] / max(df$MBval)) / denom)) ^ 2
  }
  return(ss)
}

fit.cs = function(x, df, k) {
  ll = 0
  df.subj = df %>% group_by(Subj) %>% summarize(Choice = which(Choice.cs))
  for (i in 1:nrow(df.subj)) {
    subj.name = df.subj$Subj[i]
    ll = ll + log(getProb(df.subj$Choice[i], df$MFval[df$Subj == subj.name], x[1], df$MBval[df$Subj == subj.name], x[2], k))
  }
  return(-ll)
}
fit.mix = function(x, df) {
  ll = 0
  df.subj = df %>% group_by(Subj) %>% summarize(Choice = which(Choice.cs))
  for (i in 1:nrow(df.subj)) {
    subj.name = df.subj$Subj[i]
    s1 = df$MFval[df$Subj == subj.name]
    s2 = df$MBval[df$Subj == subj.name]
    choice = df.subj$Choice[i]
    ll = ll + log((exp(x[1] * s1[choice] / max(s1) + x[2] * s2[choice] / max(s2)) /
                     sum(exp(x[1] * s1 / max(s1) + x[2] * s2 / max(s2)))))
  }
  return(-ll)
}

se = function(x,n) {sqrt(x * (1 - x) / length(x))}

# sims ------------------------------------------------------------------

bs = c(0,10)
bstd = .5
N = 500
K = 1
numMenus = 1

#s1_1 = c(10,0)
#s2_1 = c(10,20)
s1_1 = c(10,0,0,0,0,0,0,0,0,0,0,0)+1
s2_1 = c(10,20,0,0,0,0,0,0,0,0,0,0)+1
 
# s1_1 = c(1:9, 15, 20, 25, 30)
# s2_1 = c(4,2,20,3,4,3,3,4,3,2,10,2,1)
# s1_2 = c(1,2,30,4:9,15,20,25,3)
# s2_2 = c(4,2,20,3,4,3,3,4,3,1,1,10,1)
# s1_3 = c(1,5,9,13,16,18,19,2,9,3,9,1,3)
# s2_3 = c(19,18,16,13,9,5,1,4,3,1,1,10,1)

J = length(s1_1)

for (menu in 1:numMenus) {
  
  df = data.frame(Subj = NULL, Trial = NULL, Trial_unique = NULL, OptionID = NULL, Choice = NULL, X1 = NULL, X2 = NULL, X3 = NULL)
  
  for (subj in 1:N) {
    b1 = rgamma(1, bs[1], bstd)
    b2 = rgamma(1, bs[2], bstd)
    b3 = 0

    betas = c(b1, b2, b3)

    recall = as.logical(round(runif(J))) #+ .1 * (s1 > median(s1)) + .1 * (s2 > median(s2))))
    recall[sample(J, J)] = T
    recalled.words = which(recall)
    num.recalled = sum(recall)
    
    cond = sample(3,1)
    if (cond == 1) {
      s1 = s1_1
      s2 = s2_1
    } else if (cond == 2) {
      s1 = s1_2
      s2 = s2_2
    } else {
      s1 = s1_3
      s2 = s2_3
    }
    
    x1 = s1[recall]
    x2 = s2[recall]
    
    SS = sample(2:5,1)
    if (SS > num.recalled) {
      SS = num.recalled
    }
    
    for (trial in 1:K) {
      probs = exp(b1 * x1 / max(x1) + b2 * x2 / max(x2)) / sum(b1 * x1 / max(x1) + b2 * x2 / max(x2))
      choice.mix = sample(recalled.words, 1, F, probs)

      # (1) get sample!
      probs1 = exp(b1 * x1 / max(x1)) / sum(exp(b1 * x1 / max(x1)))
      s = sample(num.recalled, SS, F, probs1)

      # (2) choose
      probs2 = exp(b2 * x2[s] / max(x2[s])) / sum(exp(b2 * x2[s] / max(x2[s])))
      choice.cs = recalled.words[sample(s, 1, T, probs2)]

      for (alt in 1:num.recalled) {
        x1_rank = rank(x1, ties.method = 'max')
        df = rbind(df, data.frame(Subj = subj, Trial_unique = paste(subj, trial, sep="_"),
                                  OptionID = alt,
                                  Choice.cs = choice.cs == recalled.words[alt],
                                  Choice.mix = choice.mix == recalled.words[alt],
                                  MFval = x1[alt], MBval = x2[alt],
                                  MFhigh = factor(x1[alt] > median(s1), c(T,F), c('high','low')),
                                  Cond = factor(cond),
                                  Max.s1 = max(s1), Max.s2 = max(s2)))
      }
    }
  }

  df.graph = df %>% mutate(MFval = MFval) %>% group_by(Cond, MFval, MBval, Subj) %>%
    summarize(Choice.cs = any(Choice.cs), Choice.mix = any(Choice.mix)) %>%
    group_by(Cond, MFval, MBval) %>%
    summarize(Choice.cs = mean(Choice.cs), Choice.mix = mean(Choice.mix),
              Choice.cs.se = se(Choice.cs, n()), Choice.mix.se = se(Choice.mix, n()))
  
  # plot
  ggplot(df.graph, aes(x = MBval, y = Choice.cs, color = MFval, group = MFval)) + geom_point() + facet_wrap(~Cond)
  
  # find best fitting weights, at item level
  k_fit = 4
  # optim.cs = optim(c(1,1),
  #       function(x) {
  #           sse.cs(x, df.graph %>% filter(Cond == 1), k_fit) +
  #           sse.cs(x, df.graph %>% filter(Cond == 2), k_fit) +
  #           sse.cs(x, df.graph %>% filter(Cond == 3), k_fit)}#,
  #       #method = 'L-BFGS-B'#, lower = c(0,0), upper = c(10,10)
  #       )
  # optim.mix = optim(c(1,1),
  #                  function(x) {
  #                    sse.mix(x, df.graph %>% filter(Cond == 1)) +
  #                      sse.mix(x, df.graph %>% filter(Cond == 2)) +
  #                      sse.mix(x, df.graph %>% filter(Cond == 3))}#,
  #                  #method = 'L-BFGS-B'#, lower = c(0,0), upper = c(10,10)
  # )
  
  #optim.cs = optim(c(1,1), function(x) {fit.cs(x, df, k_fit)})
  #optim.mix = optim(c(1,1), function(x) {fit.mix(x, df)})
  
  df = df %>% mutate(Total.cs = optim.cs$par[1] * MFval / Max.s1 + optim.cs$par[2] * MBval / Max.s2,
                     Total.mix = optim.mix$par[1] * MFval / Max.s1 + optim.mix$par[2] * MBval / Max.s2)
  
  # total plot
  df.graph.total = df %>% mutate(Total = Total.mix, Choice = Choice.mix) %>%
    group_by(Cond, Total, Subj) %>%
    summarize(Choice = any(Choice)) %>%
    group_by(Cond, Total) %>%
    summarize(Choice = mean(Choice), Choice.se = se(Choice, n()))
  
  ggplot(df.graph.total, aes(x = Total, y = Choice)) + geom_point() + facet_wrap(~Cond)
  
  #df.cs = mlogit.data(df, choice = "Choice.cs", shape = "long", alt.var = "OptionID", chid.var = "Trial_unique")
  #df.mix = mlogit.data(df, choice = "Choice.mix", shape = "long", alt.var = "OptionID", chid.var = "Trial_unique")
}

# analytic ----------------------------------------------------------------

# (1)
#s1_1 = c(10,0)
#s2_1 = c(10,20)
s1_1 = c(10,0,0,0,0,0,0,0,0,0,0,0)+1
s2_1 = c(10,20,0,0,0,0,0,0,0,0,0,0)+1

# (2)
# s1_1 = c(1:9, 15, 20, 25, 30)
# s2_1 = c(4,2,20,3,4,3,3,4,3,2,10,2,1)
# s1_2 = c(1,2,30,4:9,15,20,25,3)
# s2_2 = c(4,2,20,3,4,3,3,4,3,1,1,10,1)
# s1_3 = c(1,5,9,13,16,18,19,2,9,3,9,1,3)
# s2_3 = c(19,18,16,13,9,5,1,4,3,1,1,10,1)

b1 = 0
b2 = 10
k = 2
J = length(s1_1)

df.graph2 = data.frame()
for (word_ind in 1:J) {
  s1 = s1_1
  s2 = s2_1
  df.graph2 = rbind(df.graph2, data.frame(MFval = s1[word_ind], MFhigh = factor(s1[word_ind] > median(s1)), MBval = s2[word_ind],
                                          Choice.cs = getProb(word_ind, s1, b1, s2, b2, k),
                                          Choice.mix = getProb.mix(word_ind, s1, b1, s2, b2),
                                          Cond = '1'))
  # s1 = s1_2
  # s2 = s2_2
  # df.graph2 = rbind(df.graph2, data.frame(MFval = s1[word_ind], MFhigh = factor(s1[word_ind] > median(s1)), MBval = s2[word_ind],
  #                                         Choice.cs = getProb(word_ind, s1, b1, s2, b2, k),
  #                                         Choice.mix = getProb.mix(word_ind, s1, b1, s2, b2),
  #                                         Cond = '2'))
  # s1 = s1_3
  # s2 = s2_3
  # df.graph2 = rbind(df.graph2, data.frame(MFval = s1[word_ind], MFhigh = factor(s1[word_ind] > median(s1)), MBval = s2[word_ind],
  #                                         Choice.cs = getProb(word_ind, s1, b1, s2, b2, k),
  #                                         Choice.mix = getProb.mix(word_ind, s1, b1, s2, b2),
  #                                         Cond = '3'))
}

df.graph2 = df.graph2 %>% mutate(Total = b1 * MFval / max(MFval) + b2 * MBval / max(MBval))

# Total graph
ggplot(df.graph2, aes(x = Total, y = Choice.mix)) + geom_point() + facet_wrap(~Cond)
