
# power analysis ----------------------------------------------------------


## Bootstrapping power analysis
nBS = 50
nSubj = 200
subjlist.logit = unique(df.logit$Subj)

ps = numeric(nBS)
for (bs in 1:nBS) {
  df.bs = data.frame(Subj = NULL, Trial = NULL, OptionID = NULL, Choice = NULL, MFval = NULL, MBval = NULL, nExposures = NULL, Recalled = NULL)
  
  set.seed(Sys.time())
  for (i in 1:nSubj) {
    # choose random
    subj = sample(subjlist.logit, 1)
    df.bs = rbind(df.bs, df.logit %>% filter(Subj == subj) %>% mutate(Subj = i))
  }
  
  df.bs = df.bs %>% mutate(MFcent = MFval - mean(MFval), MBcent = MBval - mean(MBval), Int = MFcent * MBcent)
  m.bs = runLogit(df.bs)
  ps[bs] = summary(m.bs)$CoefTable[3,4]
}

mean(ps < .1)





# simulations -------------------------------------------------------------


## Simulations
df.sim.cs = read.csv('simulations/results/wg_v8/cs-mf-mb.csv') %>% mutate(MFhigh = MFval, MBhigh = ifelse(MBval > 7, 1, 0), MFcent = MFval - mean(MFval), MBcent = MBval - mean(MBval), Int = MFcent * MBcent)
df.sum = df.sim.cs %>% group_by(MFhigh,MBhigh) %>% summarize(Choice.mean = mean(Choice))
ggplot(data = df.sum, aes(x = MBhigh, y = Choice.mean, group = MFhigh, colour = MFhigh)) +
  geom_point(aes(size = 2)) + geom_line()

m.sim.cs = runLogit(df.sim.cs)
summary(m.sim.cs)

m.sim.cs = runLogit_b(df.sim.cs)
summary(m.sim.cs)
estbetas = apply(m.sim.cs$betadraw, c(1,2), mean)
hist(estbetas[,3])
t.test(estbetas[,3])

df.sim.null = read.csv('simulations/results/wg_v3/mixture-mf-mb.csv') %>% mutate(MFcent = MFval - mean(MFval), MBcent = MBval - mean(MBval), Int = MFcent * MBcent)
m.sim.null = runLogit(df.sim.null)
summary(m.sim.null)

# Graph
df.rank.sim.cs = read.csv('simulations/results/wg_v3/cs-mf-mb-s2.csv')
nrows = nrow(df.rank.sim.cs)
df.rank.sim.cs = df.rank.sim.cs %>%
  mutate(Stage1_value = factor(s1_value > 5, c(F,T), c('Low', 'High')), Stage2_value = factor(rank_value > 7, c(F,T), c('Low', 'High'))) %>%
  group_by(Stage2_value, Stage1_value) %>%
  summarize(numChosen_S2 = n(), prob_rank = numChosen_S2 / nrows) %>%
  mutate(log_prob_rank = log(prob_rank))

base = ggplot(data = df.rank.sim.cs, aes(x = Stage2_value, y = prob_rank, group = Stage1_value, colour = Stage1_value)) +
  geom_point(aes(size = 2))
base + guides(size  = F) + ylab('') +
  xlab('') + geom_smooth(method='lm',formula=y~x)
# mlogit & interaction ------------------------------------------------------------------

# Do logit
runLogit = function(df) {
  df$Choice = as.logical(df$Choice)
  df$OptionID = factor(df$OptionID)
  df = df %>% mutate(Trial_unique = paste(Subj, Trial, sep="_"))
  df$Trial = factor(df$Trial)
  df$Trial_unique = factor(df$Trial_unique)
  df$Subj = factor(df$Subj)
  df.m = mlogit.data(df, choice = "Choice", shape = "long", id.var = "Subj", alt.var = "OptionID", chid.var = "Trial_unique")
  
  m = mlogit(Choice ~ MFcent + MBcent + Int | -1, df.m, panel = T,
             rpar = c(MFcent = "n", MBcent = "n", Int = 'n'), correlation = F, halton = NA, R = 1000, tol = .001)
  return(m)
}

## Prepare for mlogit
numRealQuestions = numQuestions - 1
df.logit = data.frame(Subj = NULL, Trial = NULL, OptionID = NULL, Choice = NULL, MFval = NULL, MBval = NULL, nExposures = NULL, Recalled = NULL, Question = NULL)


for (subj in 1:nrow(df.demo)) {
  subj.name = df.demo$subject[subj]
  recalled.temp = recalled_ever[subj, ]
  #recalled.temp = !logical(numWords)
  num.recalled.temp = sum(recalled.temp)
  
  df.words.temp = df.words %>% filter(subject == subj.name)
  df.s2.temp = df.s2 %>% filter(subject == subj.name) %>% arrange(question_order)
  
  nAnswered = sum(!is.na(df.s2.temp$choice_real_ind))
  
  if (nAnswered > 0 & subj.name %in% include_names) {
    Subj.col = rep(subj, num.recalled.temp * nAnswered)
    
    MFval.col = rep(df.words.temp$value[recalled.temp], nAnswered)
    MFhigh.col = rep(df.words.temp$high_val[recalled.temp] * 1, nAnswered)
    nExposures.col = rep(df.words.temp$exposures[recalled.temp], nAnswered)
    Recalled.col = rep(df.words.temp$recall.ever[recalled.temp] * 1, nAnswered)
    numChosen.col = rep(df.words.temp$numChosen_high[recalled.temp], nAnswered)
    #OptionID.col = rep(which(recalled.temp), nAnswered)
    OptionID.col = rep(1:num.recalled.temp, nAnswered)
    Trial.col = rep(1:nAnswered, each = num.recalled.temp)
    Question.col = rep(df.s2.temp$question_ind[!is.na(df.s2.temp$choice_real_ind)], each = num.recalled.temp)
    
    temp.mbval = matrix(0, nrow = nAnswered, ncol = num.recalled.temp)
    temp.mbhigh = matrix(0, nrow = nAnswered, ncol = num.recalled.temp)
    temp.choice = matrix(0, nrow = nAnswered, ncol = num.recalled.temp)
    temp.choice2 = matrix(0, nrow = nAnswered, ncol = num.recalled.temp)
    ind = 1
    for (q in 1:numRealQuestions) {
      if (!is.na(df.s2.temp$choice_real_ind[q])) {
        all_vals = as.numeric.vector(df.s2.temp$all_values[q])
        mbvals = rank(all_vals, ties.method = 'max')
        #mbvals = all_vals
        temp.mbval[ind,] = mbvals[recalled.temp]
        temp.mbhigh[ind,] = mbvals[recalled.temp] > 13
        
        choice = logical(num.recalled.temp)
        choice[which(df.s2.temp$choice_real_ind[q] == which(recalled.temp))] = TRUE
        temp.choice[ind,] = choice
        
        choice2 = vector(mode = 'numeric', num.recalled.temp)
        #choice2[1] = which(df.s2.temp$choice_real_ind[q] == which(recalled.temp))
        choice2[1] = OptionID.col[1:num.recalled.temp][choice]
        temp.choice2[ind,] = choice2
        
        ind = ind + 1
      }
    }
    
    MBval.col = as.vector(t(temp.mbval))
    MBhigh.col = as.vector(t(temp.mbhigh))
    Choice.col = as.vector(t(temp.choice))
    Choice2.col = as.vector(t(temp.choice2))
    
    df.logit = rbind(df.logit,
                     data.frame(Subj = Subj.col, Trial = Trial.col, OptionID = OptionID.col, Choice = Choice.col,
                                MFval = MFval.col, MBval = MBval.col, MFhigh = MFhigh.col, MBhigh = MBhigh.col, Choice2 = Choice2.col, nExposures = nExposures.col,
                                Recall = Recalled.col, nChosen = numChosen.col, Question = Question.col))
    
  }
}

df.logit = df.logit %>% mutate(MFcent = MFhigh - mean(MFhigh), MBcent = MBhigh - mean(MBhigh), Int = MFcent * MBcent,
                               nChosen_cent = nChosen - mean(nChosen))

m.real = runLogit(df.logit)
summary(m.real)


## Graph
df.sum = df.logit %>% group_by(MFhigh,MBhigh) %>% summarize(Choice.mean = mean(Choice)) #%>% mutate(Choice.mean = Choice.mean * ifelse(MFval %in% c(0,10), 2/3, 1))

ggplot(data = df.sum, aes(x = MBhigh, y = Choice.mean, group = MFhigh, colour = MFhigh)) +
  geom_point(aes(size = 2)) + geom_line()

