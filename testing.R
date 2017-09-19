J = 14
bmean = 3
bstd = 1
N = 100
K = 8 # # of trials
SS = 4 # sample size

x2_total = NULL

for (i in 1:K) {
  #x2_total = c(x2_total, sample(rep(0:1, each = 7)))
  x2_total = c(x2_total, sample(1:14))
}

x2_total = matrix(x2_total, nrow = K, ncol = J, byrow = T)

df = data.frame(Subj = NULL, Trial = NULL, Trial_unique = NULL, OptionID = NULL, Choice = NULL, X1 = NULL, X2 = NULL, X3 = NULL)

for (subj in 1:N) {
  b1 = rnorm(1, bmean, bstd)
  b2 = rnorm(1, bmean, bstd)
  #b3 = rnorm(1, bmean, bstd)
  b3 = 0
  
  betas = c(b1, b2, b3)
  
  x1 = sample(rep(0:1, each = 7))
  
  for (trial in 1:K) {
    
    x2 = x2_total[trial,]
    x3 = x1 * x2
    attributes = matrix(c(x1,x2,x3), nrow = 3, ncol = J, byrow = T)
    
    probs = exp(betas %*% attributes) / sum(exp(betas %*% attributes))
    choice = sample(J, 1, F, probs)
    
    # (1) get sample!
    probs1 = exp(b1 * x1) / sum(exp(b1 * x1))
    s = sample(J, SS, F, probs1)
    
    # (2) choose
    #probs2 = exp(b1 * x1[s] + b2 * x2[s] + b3 * x3[s]) / sum(exp(b1 * x1[s] + b2 * x2[s] + b3 * x3[s]))
    probs2 = exp(b2 * x2[s]) / sum(exp(b2 * x2[s]))
    #choice = sample(s, 1, T, probs2)
    #choice = s[which.max(x2[s])]
    
    for (alt in 1:J) {
      df = rbind(df, data.frame(Subj = subj, Trial = trial, Trial_unique = paste(subj, trial, sep="_"),
                                OptionID = alt, Choice = choice == alt, 
                                X1 = x1[alt], X2 = x2[alt], X3 = x3[alt]))
    }
  }
}

df = df %>% mutate(X1 = X1 - mean(X1), X2 = X2 - mean(X2))
df.m = mlogit.data(df, choice = "Choice", shape = "long", id.var = "Subj", alt.var = "OptionID", chid.var = "Trial_unique")

m = mlogit(Choice ~ X1 + X2 + X3 | -1, df.m, panel = T,
           rpar = c(X1 = "n", X2 = "n", X3 = "n"), correlation = F, halton = NA, R = 1000, tol = .001)
summary(m)

#se = function(x) {return(sd(x) / sqrt(length(x)))}
#dodge <- position_dodge(width=0.9)

#df.subj = df %>% group_by(Subj, OptionID) %>% summarize(Choice = mean(Choice), X1 = mean(X1), X2 = mean(X2))
#df.sum = df.subj %>% group_by(OptionID) %>% summarize(Choice.mean = mean(Choice), Choice.se = se(Choice),
#                                                      X1 = mean(X1), X2 = mean(X2))

#ggplot(data = df.sum, aes(x = X2, y = Choice.mean, group = X1, colour = X1)) +
#  geom_point(aes(size = 2)) + geom_line() +
#  geom_errorbar(aes(ymax = Choice.mean + Choice.se, ymin = Choice.mean - Choice.se), width = .1)

df.group = df %>% mutate(X2_type = ifelse(X2 == max(X2), 1, ifelse(X2 == min(X2), -1, 0))) %>%
  group_by(X1,X2_type) %>% filter(X2_type != 0) %>% summarize(Choice = mean(Choice))
#df.group = df %>% group_by(X1,X2) %>% summarize(Choice = mean(Choice))
ggplot(data = df.group, aes(x = X2_type, y = Choice, group = X1, colour = X1)) +
  geom_point(aes(size = 2)) + geom_line()

#df.subj = df %>% group_by(Subj, X1, X2) %>% summarize(Choice = mean(Choice), Choice.log = log(Choice))
#m.2 = lm(Choice.log ~ X1 * X2, data = df.subj)
#summary(m.2)

#m.2 = lmer(Choice ~ X1 * X2 + (1 | Subj) + (0 + X1 | Subj) + (0 + X2 | Subj) + (0 + X1:X2 | Subj) + (1 | OptionID), data = df.subj)
#summary(m.2)

#m.null = mlogit(Choice ~ X1 + X2 | -1, df.m, panel = F,
#             rpar = c(X1 = "n", X2 = "n"), correlation = F, halton = NA, R = 1000, tol = .001)
#1-pchisq(2*(m$logLik[1] - m.null$logLik[1]),1)
