# setup -------------------------------------------------------------------

require(dplyr)
require(ggplot2)
require(lme4)
require(lmerTest)
require(mlogit)
require(lattice)
require(stringdist)
require(ggstatsplot)
require(plotly)
require(rsm)
require(rje)

theme_update(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(colour = "black"),
             axis.text=element_text(size=20, colour = "black"), axis.title=element_text(size=18, face = "bold"), axis.title.x = element_text(vjust = 0),
             legend.title = element_text(size = 24, face = "bold"), legend.text = element_text(size = 20), plot.title = element_text(size = 26, face = "bold", vjust = 1))


setwd("~/Me/Psychology/Projects/choicesets/git")
#setwd("C:/Users/Jphil/Dropbox/choiceSets/choice-sets")

getIndex = function(x, list) {
  y = numeric(length(x))
  for (j in 1:length(x)) {
    if (any(list %in% x[j])) {
      y[j] = which(list %in% x[j])
    } else {
      y[j] = NA
    }
  }
  return(y)
}

as.string.vector = function(x) {
  temp = strsplit(substr(x,2,nchar(x)-1), split=",")[[1]]
  return(substr(temp, 2, nchar(temp) - 1))
}

as.numeric.vector = function(x) {
  return(as.numeric(strsplit(substr(x,2,nchar(x)-1), split=",")[[1]]))
}

se = function(x) {return(sd(x, na.rm = T) / sqrt(length(x)))}
dodge <- position_dodge(width=0.9)


# import data -------------------------------------------------------------

numWords = 12;
numTrials = 96;
minNAs = 1;
path = 'data/value/v4/real2/'
pointsPerCent_s1 = 10;
pointsPerCent_s2 = 1;
pointsPerWord = 3; # for memory condition
allBonus = 25;
numRealQuestions = 1;
type = 0;
maxRepeats = 2;
numQuestions = 2;

# Load data
df.demo = read.csv(paste0(path, 'demo.csv'), stringsAsFactors = F) %>% arrange(subject) %>% mutate(total_time_real = total_time / 60000)
df.words.raw = read.csv(paste0(path, 'words.csv'), stringsAsFactors = F) %>% arrange(subject, word_ind)
df.s1.raw = read.csv(paste0(path, 's1.csv'), stringsAsFactors = F) %>% arrange(subject);
df.s2.raw = read.csv(paste0(path, 's2.csv'), stringsAsFactors = F) %>% arrange(subject, question_order)

subjlist = df.demo$subject

# words
df.words = df.words.raw %>% filter(subject %in% subjlist) %>%
  mutate(repeated = word_ind == lead(word_ind)) %>% 
  filter(!repeated)

# s2
df.s2 = df.s2.raw %>% filter(subject %in% subjlist)
df.s2$choice = toupper(df.s2$choice)
df.s2$scratch = gsub("[.]", ",", toupper(as.character(df.s2$scratch)))
df.s2$all_values = as.character(df.s2$all_values)
df.s2$comp_check_pass = as.numeric(df.s2$comp_check_pass)

for (i in 1:nrow(df.s2)) {
  subj.name = df.s2$subject[i]
  wordlist = (df.words %>% filter(subject == subj.name))$word
  c = gsub("\n.*","",df.s2$choice[i])
  creal = wordlist[amatch(c, wordlist, maxDist = 2)]
  cind = getIndex(creal, wordlist)
  
  if (is.na(cind)) {
    # try scratch
    c = gsub("\n.*","",df.s2$scratch[i])
    creal = wordlist[amatch(c, wordlist, maxDist = 2)]
    cind = getIndex(creal, wordlist)
  }
  
  df.s2$choice_real[i] = creal
  df.s2$choice_real_ind[i] = cind
}

# s1
df.s1 = df.s1.raw %>% filter(subject %in% subjlist) %>%
  mutate(correct_word = ain(toupper(resp), word, maxDist = 2), correct_val = resp2 == value, word_chosen = ifelse(choice, alt, word),
         correct_choice = choice == ifelse(value > alt, 0, 1))
df.s1.subj = df.s1 %>% group_by(subject) %>%
  summarize(pctCorrect_words = mean(correct_word, na.rm = T), pctCorrect_val = mean(correct_val, na.rm = T),
            pctCorrect_choice = mean(correct_choice, na.rm = T), numTrials = n())

# compute exclusion -------------------------------------------------------

df.s2.excl = df.s2 %>% filter(subject %in% subjlist) %>%
  group_by(subject) %>%
  summarize(comp_check_pass = mean(comp_check_pass[question_order == 0]),
            numNAs = sum(is.na(choice_real)),
            numRepeats = sum(choice_real == lag(choice_real), na.rm = T),
            numTrials = n())

recalled = matrix(F, nrow = nrow(df.s2.excl), ncol = numWords)
recalled_ever = matrix(F, nrow = nrow(df.s2.excl), ncol = numWords)
recalled_val = matrix(F, nrow = nrow(df.s2.excl), ncol = numWords)
df.words$recall = NULL
df.words$recall.ever = NULL
df.words$order = NULL

for (i in 1:nrow(df.s2.excl)) {
  subj.name = df.s2.excl$subject[i]
  df.words.temp = df.words %>% filter(subject == subj.name)
  df.s2.temp = df.s2 %>% filter(subject == subj.name)
  df.s2.temp.mem = df.s2.temp %>% filter(question == 'Memory')
  
  words_temp = trimws(as.string.vector(df.s2.temp.mem$choice))
  val_temp = as.numeric(trimws(as.string.vector(df.s2.temp.mem$scratch)))
  val_temp[is.na(val_temp)] = -99
  
  wordlist = df.words.temp$word
  
  if (length(wordlist) == numWords) {
    for (j in 1:numWords) {
      which_word = amatch(wordlist[j], words_temp, maxDist = 2, nomatch = 0)
      recalled[i,j] = which_word > 0
      
      df.words$recall[df.words$subject == subj.name & df.words$word == wordlist[j]] = recalled[i,j]
      
      if (recalled[i,j]) {
        true_val = df.words.temp$s1_value[df.words.temp$word_ind == (j - 1)]
        recalled_val[i,j] = abs(val_temp[which_word] - true_val) <= 1
      }
      
      recalled_ever[i,j] = recalled[i,j] | any(na.omit(df.s2.temp$choice_real_ind) == j)
      df.words$recall.ever[df.words$subject == subj.name & df.words$word == wordlist[j]] = recalled_ever[i,j]
      
      df.words$order[df.words$subject == subj.name & df.words$word == wordlist[j]] = which_word
    }
  }
}


include_rows = NULL
include_names = NULL

for (subj in 1:length(subjlist)) {
  subj.name = subjlist[subj]
  df.s1.subj.temp = df.s1.subj %>% filter(subject == subj.name)
  df.s2.subj.temp = df.s2.excl %>% filter(subject == subj.name)
  df.demo.temp = df.demo %>% filter(subject == subj.name)
  
  exclude = df.demo.temp$write_down == 'Yes' || #df.s2.subj.temp$comp_check_pass < 1 ||
    df.s2.subj.temp$numNAs > minNAs || sum(recalled[subj,]) < 5 || df.s2.subj.temp$numTrials != numQuestions ||
    df.s1.subj.temp$numTrials != numTrials || df.s1.subj.temp$pctCorrect_words < .75 || df.s1.subj.temp$pctCorrect_val < .75 #||
    #df.s1.subj.temp$pctCorrect_choice < .75 #|| df.s2$cond[df.s2$subject == subj.name] == 'normal'
  if (exclude) {
    include_rows[subj] = FALSE
  } else {
    include_rows[subj] = TRUE
    include_names = c(include_names, subj.name)
  }
}

# data manip --------------------------------------------------------------

## words

for (i in 1:nrow(df.words)) {
  subj = df.words$subject[i]
  
  s1_valuelist = (df.words %>% filter(subject == subj))$s1_value
  s1_valuelist_rank = rank(s1_valuelist, ties.method = 'max')
  s1_valuelist_indiv = (df.words %>% filter(subject == subj & recall.ever == 1))$s1_value
  df.words$high_s1value[i] = df.words$s1_value[i] > median(s1_valuelist)
  df.words$rank_s1value[i] = s1_valuelist_rank[df.words$word_ind[i] + 1]
  df.words$high_s1value_indiv[i] = df.words$s1_value[i] > median(s1_valuelist_indiv)
  
  s2_valuelist = (df.words %>% filter(subject == subj))$s2_value
  s2_valuelist_rank = rank(s2_valuelist, ties.method = 'max')
  s2_valuelist_indiv = (df.words %>% filter(subject == subj & recall.ever == 1))$s2_value
  df.words$high_s2value[i] = df.words$s2_value[i] > median(s2_valuelist)
  df.words$rank_s2value[i] = s2_valuelist_rank[df.words$word_ind[i] + 1]
  df.words$high_s2value_indiv[i] = df.words$s2_value[i] > median(s2_valuelist_indiv)
}

## s2
for (i in 1:nrow(df.s2)) {
  subj.name = df.s2$subject[i]
  wordlist = (df.words %>% filter(subject == subj.name))$word
  
  cind = df.s2$choice_real_ind[i]
  creal = df.s2$choice_real[i]
  
  word_rows = subj.name == df.words$subject & creal == df.words$word
  
  df.s2$s1_value[i] = ifelse(is.na(cind), NA, df.words$s1_value[word_rows])
  df.s2$high_s1value[i] = ifelse(is.na(cind), NA, df.words$high_s1value[word_rows])
  df.s2$high_s1value_indiv[i] = ifelse(is.na(cind), NA, df.words$high_s1value_indiv[word_rows])
  df.s2$rank_s1value[i] = ifelse(is.na(cind), NA, df.words$rank_s1value[word_rows])
  
  df.s2$s2_value[i] = ifelse(is.na(cind), NA, df.words$s2_value[word_rows])
  df.s2$high_s2value[i] = ifelse(is.na(cind), NA, df.words$high_s2value[word_rows])
  df.s2$high_s2value_indiv[i] = ifelse(is.na(cind), NA, df.words$high_s2value_indiv[word_rows])
  df.s2$rank_s2value[i] = ifelse(is.na(cind), NA, df.words$rank_s2value[word_rows])
  
  s2_valuelist = (df.words %>% filter(subject == subj.name))$s2_value
  df.s2$median_value[i] = median(s2_valuelist)
}

df.s2 = df.s2 %>% mutate(s2_subj_ind = as.numeric(as.factor(subject)), # just for modeling
                         bonus_value = ifelse(is.na(choice_real_ind), 0, s2_value))

df.s2.filt = df.s2 %>% filter(subject %in% include_names & question_order == 0)
df.words.filt = df.words %>% filter(subject %in% include_names)

# check out data ----------------------------------------------------------

# s2 rank value
ggpiestats(df.s2.filt, high_s2value)
ggpiestats(df.s2.filt, high_s2value_indiv)
gghistostats(df.s2.filt, rank_s2value, test.value = 6.5, centrality.para = 'mean', type = 'np')

# s1 high value
ggpiestats(data = df.s2.filt, main = high_s1value)
ggpiestats(df.s2.filt, high_s1value_indiv)
gghistostats(df.s2.filt, rank_s1value, test.value = 6.5, centrality.para = 'median', type = 'p')
grouped_ggpiestats(cond, data = df.s2.filt, main = high_s1value)

wilcox.test(df.s2.filt$rank_s1value, mu = 7)

# logit test
df.logit = data.frame(Subj = NULL, Trial = NULL, OptionID = NULL, Choice = NULL, MFval = NULL, MBval = NULL, nExposures = NULL, Recalled = NULL, Question = NULL)

for (subj in 1:nrow(df.demo)) {
  subj.name = df.demo$subject[subj]
  recalled.temp = recalled_ever[subj, ]
  #recalled.temp = !logical(numWords)
  num.recalled.temp = sum(recalled.temp)
  
  df.words.temp = df.words.filt %>% filter(subject == subj.name)
  df.s2.temp = df.s2.filt %>% filter(subject == subj.name) %>% arrange(question_order)
  
  nAnswered = sum(!is.na(df.s2.temp$choice_real_ind))
  
  if (nAnswered > 0 & subj.name %in% include_names) {
    Subj.col = rep(subj, num.recalled.temp * nAnswered)
    Condition.col = rep(df.s2.temp$cond, num.recalled.temp * nAnswered)

    MFval.col = rep(df.words.temp$s1_value[recalled.temp], nAnswered)
    MFhigh.col = rep(df.words.temp$high_s1value[recalled.temp] * 1, nAnswered)
    MFrank.col = rep(df.words.temp$rank_s1value[recalled.temp] * 1, nAnswered)
    MBval.col = rep(df.words.temp$s2_value[recalled.temp], nAnswered)
    MBhigh.col = rep(df.words.temp$high_s2value[recalled.temp] * 1, nAnswered)
    MBrank.col = rep(df.words.temp$rank_s2value[recalled.temp] * 1, nAnswered)
    nExposures.col = rep(df.words.temp$exposures[recalled.temp], nAnswered)
    Recalled.col = rep(df.words.temp$recall.ever[recalled.temp] * 1, nAnswered)
    numChosen.col = rep(df.words.temp$numChosen_high[recalled.temp], nAnswered)
    OptionID_real.col = rep(which(recalled.temp), nAnswered)
    OptionID.col = rep(1:num.recalled.temp, nAnswered)
    Trial.col = rep(1:nAnswered, each = num.recalled.temp)
    Question.col = rep(df.s2.temp$question_ind[!is.na(df.s2.temp$choice_real_ind)], each = num.recalled.temp)
    
    temp.choice = matrix(0, nrow = nAnswered, ncol = num.recalled.temp)
    ind = 1
    for (q in 1:numRealQuestions) {
      if (!is.na(df.s2.temp$choice_real_ind[q])) {
        choice = logical(num.recalled.temp)
        choice[which(df.s2.temp$choice_real_ind[q] == which(recalled.temp))] = TRUE
        temp.choice[ind,] = choice
        
        ind = ind + 1
      }
    }
    
    Choice.col = as.vector(t(temp.choice))
    
    df.logit = rbind(df.logit,
                     data.frame(Subj = Subj.col, Trial = Trial.col, OptionID = OptionID.col, Choice = Choice.col,
                                MFval = MFval.col, MBval = MBval.col, MFhigh = MFhigh.col, MBhigh = MBhigh.col, 
                                MFrank = MFrank.col, MBrank = MBrank.col, Condition = Condition.col))
    #Recall = Recalled.col, Question = Question.col, OptionID_real = OptionID_real.col))
    
  }
}

for (i in 1:nrow(df.logit)) {
  if (df.logit$MFval[i] == 1 && df.logit$MBval[i] == 11) {
    df.logit$MBval[i] = 12
  }
  if (df.logit$MFval[i] == 3 & df.logit$MBval[i] == 12) {
    df.logit$MBval[i] = 13
  }
  if (df.logit$MFval[i] == 6 & df.logit$MBval[i] == 14) {
    df.logit$MBval[i] = 15
  }
  if (df.logit$MFval[i] == 15 & df.logit$MBval[i] == 11) {
    df.logit$MBval[i] = 12
  }
  if (df.logit$MFval[i] == 13 & df.logit$MBval[i] == 12) {
    df.logit$MBval[i] = 13
  }
  if (df.logit$MFval[i] == 10 & df.logit$MBval[i] == 14) {
    df.logit$MBval[i] = 15
  }
}

df.logit = df.logit %>% mutate(Trial_unique = paste(Subj, Trial, sep="_"),
                               Total = 1.5 * MFval / 15 + 10 * MBval / 22,
                               MFhigh = factor(MFhigh, c(1,0), c('high', 'low')),
                               Condition = factor(Condition, c('normal', 'reversed'), c('normal', 'reversed')))

df.logit2 = mlogit.data(df.logit, choice = "Choice", shape = "long", id.var = "Subj", alt.var = "OptionID", chid.var = "Trial_unique")

m = mlogit(Choice ~ MFval * MBval | -1, df.logit2)#, panel = T,
#rpar = c(MFcent = "n", MBcent = "n", Int = "n"), correlation = F, halton = NA, R = 1000, tol = .001)
summary(m)

m2 = mlogit(Choice ~ Total | -1, df.logit2)
summary(m2)

# interaction graph

df.graph = df.logit %>% mutate(MFhigh = MFhigh, MBhigh = MBval) %>%
  group_by(Condition, MFhigh,MBhigh,Subj) %>% summarize(Choice = any(Choice)) %>%
  group_by(Condition, MFhigh,MBhigh) %>%
  summarize(Choice.mean = mean(Choice), Choice.se = sqrt(Choice.mean * (1 - Choice.mean) / n()))
ggplot(data = df.graph, aes(x = MBhigh, y = Choice.mean, group = MFhigh, colour = MFhigh)) +
  geom_point(aes(size = 2)) + geom_line() +
  #geom_smooth(method='lm', formula = y ~ poly(x,3)) +
  #geom_smooth() +
  geom_errorbar(aes(ymin=Choice.mean - Choice.se, ymax = Choice.mean + Choice.se), width = .2) +
  guides(size = FALSE) #+ facet_wrap(~ Condition)

plot_ly(df.graph %>% filter(Condition == 'reversed'), x = ~MFhigh, y = ~MBhigh, z = ~Choice.mean, marker = list(color = ~Choice.mean))

df.graph.all = df.logit %>%
  group_by(MFval,MBval,Subj) %>% summarize(Choice = any(Choice)) %>%
  group_by(MFval,MBval) %>%
  summarize(Choice.mean = mean(Choice), Choice.se = sqrt(Choice.mean * (1 - Choice.mean) / n())) %>%
  mutate(Choice.mean = round(Choice.mean, 2))

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
# s1 = c(1,  1,  3,  3,  3,  4,  6,  6,  7,  10, 14, 15)
# s2 = c(12, 22, 21, 12, 14, 18, 15, 9, 11,  7,  5,  6)

# s1 = c(3, 10, 6, 15, 7, 14, 3, 4, 3, 1, 1, 6)
# s2 = c(21, 7, 15, 6, 11, 5, 13, 18, 14, 12, 22, 9)
# 
s1 = c(1, 1, 3, 3, 3, 4, 6, 6, 7, 10, 14, 15)
s2 = c(12, 22, 21, 13, 14, 18, 15, 9, 11, 7, 5, 6)

s1_rev = max(s1) + 1 - s1

df.real = df.logit %>%
  group_by(Condition, MFval,MBval,Subj) %>% summarize(Choice = any(Choice)) %>%
  group_by(Condition, MFval,MBval) %>%
  summarize(Choice.mean = mean(Choice), Choice.se = sqrt(Choice.mean * (1 - Choice.mean) / n()))

df.graph2 = data.frame()
for (word_ind in 1:numWords) {
  df.graph2 = rbind(df.graph2, data.frame(MFval = s1[word_ind], MFhigh = factor(s1[word_ind] > median(s1), c(T,F), c('high', 'low')), MBval = s2[word_ind],
                                          Choice.cs = getProb(word_ind, s1, b1, s2, b2, k), Choice.mix = getProb.mix(word_ind, s1, b1, s2, b2),
                                          Real = df.real$Choice.mean[df.real$MFval == s1[word_ind] & df.real$MBval == s2[word_ind]],
                                          Real.se = df.real$Choice.se[df.real$MFval == s1[word_ind] & df.real$MBval == s2[word_ind]],
                                          Cond = df.real$Condition[df.real$MFval == s1[word_ind] & df.real$MBval == s2[word_ind]]))
  df.graph2 = rbind(df.graph2, data.frame(MFval = s1_rev[word_ind], MFhigh = factor(s1_rev[word_ind] > median(s1_rev), c(T,F), c('high', 'low')), MBval = s2[word_ind],
                                           Choice.cs = getProb(word_ind, s1_rev, b1, s2, b2, k), Choice.mix = getProb.mix(word_ind, s1_rev, b1, s2, b2),
                                           Real = df.real$Choice.mean[df.real$MFval == s1_rev[word_ind] & df.real$MBval == s2[word_ind]],
                                          Real.se = df.real$Choice.se[df.real$MFval == s1_rev[word_ind] & df.real$MBval == s2[word_ind]],
                                          Cond = df.real$Condition[df.real$MFval == s1_rev[word_ind] & df.real$MBval == s2[word_ind]]))
}

df.graph2 = df.graph2 %>% mutate(Total = b1 * MFval / max(s1) + b2 * MBval / max(s2))

ggplot(data = df.graph2 %>% mutate(Cond = factor(Cond)), aes(x = MBval)) +
  #geom_point(aes(y = Choice.mix, size = 2), shape = 16) + geom_line(aes(y = Choice.mix)) +
  #geom_point(aes(y = Choice.cs, size = 2), shape = 16) + geom_line(aes(y = Choice.cs)) +
  geom_point(aes(y = Real, size = 2), shape = 16) + geom_line(aes(y = Real)) +
  geom_errorbar(aes(ymin=Real - Real.se, ymax = Real + Real.se), width = .2) +
  #geom_smooth(method='lm', aes(y = Real), formula = y ~ exp(x)) +
  guides(size = FALSE) + facet_wrap(~ Cond)

#df.test = df.logit %>% group_by(Condition, Total, Subj) %>% summarize(Choice = any(Choice)) %>% group_by(Condition, Total) %>% summarize(Choice = mean(Choice))
test.cs = lm(Choice.mix ~ exp(Total) * Cond, data = df.graph2)
summary(test.cs)

# df.test = df.s2.filt %>% mutate(Total = s1_value / 15 + 10 * s2_value / 22)
# grouped_gghistostats(cond, data = df.test, x = Total)
# t.test(df.test$Total[df.test$cond == 'normal'], df.test$Total[df.test$cond == 'reversed'])

test.cs = lm(Real ~ exp(MBval), data = df.graph2 %>% filter(Cond == 'reversed'))
#test.cs = lm(Real ~ poly(MBval,2) * Cond, data = df.graph2)
summary(test.cs)

fn = function(formula, data, indices) {
  return(summary(lm(formula, data = data[indices,]))$r.squared)
}

bs = boot(df.graph2 %>% filter(Cond == 'reversed'), fn, R = 100, formula = Real ~ exp(Total))
cs1 = boot.ci(bs, type = 'bca') # .3066, .7606 vs #.811, .96

grouped_ggscatterstats(Cond, data = df.graph2, x = Choice.cs, y = Real)


fn = function(x, df) {
  ss = 0
  denom = sum(exp(x[1] * df$MFval / 15 + x[2] * df$MBval / 22))
  for (i in 1:nrow(df)) {
    ss = ss + (df$Real[i] - (exp(x[1] * df$MFval[i] / 15 + x[2] * df$MBval[i] / 22) / denom)) ^ 2
  }
  return(ss)
}
fn.cs = function(x, df) {
  ss = 0
  for (i in 1:nrow(df)) {
    ss = ss + (df$Real[i] - getProb(i,df$MFval, x[1], df$MBval, x[2], 4)) ^ 2
  }
  return(ss)
}
optim(c(1,1), function(x) {fn.cs(x, df.graph2 %>% filter(Cond == 'normal')) + fn.cs(x, df.graph2 %>% filter(Cond == 'reversed'))},
      method = 'L-BFGS-B', lower = c(0,0), upper = c(10,10))

#ggplot(data = df.graph2, aes(x = Total, y = Choice.cs)) + geom_point(size = 2) + geom_line()
#geom_smooth(aes(y = Choice.cs)) + geom_smooth(aes(y = Choice.mix))
#summary(lm(Real ~ Diff * Total, data = df.graph2))

# test = lm(Choice.cs ~ poly(MFval, MBval, degree = 3), data = df.graph2)
# persp(test, MBval ~ MFval)
# contour(test, MBval ~ MFval)
# 
# test2 = loess(Choice.cs ~ MFval * MBval, data = df.graph2, span = .75)
# test2.pred = predict(test2, newdata = expand.grid(list(MFval = 1:15, MBval = 5:22)))
# persp(test2.pred)
# 
# plot_ly(df.graph2 %>% filter(Cond == 'normal'), x = ~MFval, y = ~MBval, z = ~Real, marker = list(color = ~Real))
# 
# df.graph2 = df.graph2 %>% mutate(color1 = ifelse(MFhigh, '#002bff', '#bbc7ff'), color2 = ifelse(MFhigh, '#ff0040', '#ffacc1'),
#                                  color3 = ifelse(MFhigh, '#00a303', '#84ce85'))
# 
# plot_ly(df.graph2, x = ~MBval, y = ~MFval, z = ~Choice.mix, marker = list(color = ~Choice.mix)) #%>% layout(scene = list(zaxis = list(range=c(0,.6))))

# df.graph.collapsed = df.graph2 %>% summarize(RMSE.cs = sqrt(mean((Choice.cs - Real) ^ 2)), RMSE.mix = sqrt(mean((Choice.mix - Real) ^ 2)))

# recall effects ----------------------------------------------------------


nrecall = rowSums(recalled[include_rows,])
mean(nrecall)

# plot split by value
df.words.byvalue = df.words.filt %>% group_by(high_s1value_indiv, high_s2value_indiv, subject) %>% summarize(recall = mean(recall, na.rm = T)) %>%
  group_by(high_s1value_indiv, high_s2value_indiv) %>% summarize(recall.mean = mean(recall, na.rm = T), recall.se = se(recall))
ggplot(df.words.byvalue, aes(x = high_s1value_indiv, y = recall.mean, color = high_s2value_indiv, group = high_s2value_indiv)) +
  geom_bar(stat = "identity", position = dodge) +
  geom_errorbar(aes(ymax = recall.mean + recall.se, ymin = recall.mean - recall.se), width = .5, position = dodge) +
  xlab('') + ylab('') + guides(fill = F)

# did value influence recall?
#m.recall = glmer(recall ~ high_value + (0 + high_value | subject) + (1 | subject) + (1 | word),
#                 data = df.words %>% filter(subject %in% include_names), family = binomial)
m.recall = glmer(recall ~ high_value + (1 | word),
                 data = df.words.filt, family = binomial)
summary(m.recall)

## order effects
#subj.notfirst = (df.s2.filt %>% filter(rank_s2value != 12))$subject

histogram(~ order | s1_value, df.words.filt[df.words.filt$recall == T, ])
m.order = lmer(order ~ s1_value + s2_value + (s1_value + s2_value || subject),
               data = df.words.filt %>% filter(recall) %>% mutate(subject = factor(subject))) #& df.words.filt$subject %in% subj.notfirst, ])
summary(m.order)
#df.s2.subj.filt$order_weights = coef(m.order)$subject$high_valueTRUE

slopes = numeric(length(include_names))
cond = logical(length(include_names))
for (i in 1:length(include_names))
{
  subj = include_names[i]
  df.words.temp = df.words.filt %>% filter(subject == subj & recall)
  m.order.subj = lm(order ~ s1_value + s2_value, df.words.temp)
  slopes[i] = m.order.subj$coefficients[2]
  cond[i] = df.words.temp$cond == 'normal'
}

m.order.corr = lm(df.s2.filt$s1_value ~ slopes)
summary(m.order.corr)
ggscatterstats(x = slopes, y = df.s2.filt$s1_value)

ggplot(df.words.filt %>% filter(recall) %>% mutate(subject = factor(subject)), aes(x = s1_value, y = order, group = subject, color = subject)) +
  geom_jitter() +
  guides(color = F) +
  geom_smooth(method = 'lm', se = F)
  

# bonuses, modeling -----------------------------------------------------------------

## save for modeling
df.test = df.s2 %>% group_by(subject) %>% summarize(anyGood = any(!is.na(choice_real_ind)))

rewards_tr = matrix(0, nrow = sum(include_rows), ncol = numWords)
ind = 1
for (subj in 1:nrow(df.demo)) {
  subj.name = df.demo$subject[subj]
  
  if (subj.name %in% include_names & df.test$anyGood[df.test$subject == subj.name]) {
    df.words.temp = df.words %>% filter(subject == subj.name)
    
    for (word in 1:numWords) {
      rewards_tr[ind, word] = df.words.temp$s1_value[word]
    }
    ind = ind + 1
  }
}

write.csv(rewards_tr, paste0(path, 'rewards_s1.csv'), row.names = F)
write.csv(recalled_ever[include_rows & df.test$anyGood, ] * 1, paste0(path, 'recalled.csv'), row.names = F)

df.modeling = df.s2 %>% filter(subject %in% include_names & !is.na(choice_real_ind)) %>%
  mutate(all_values_nocomma = gsub(",", " ", all_values)) %>% 
  dplyr::select(s2_subj_ind, choice_real_ind, all_values_nocomma)
write.table(df.modeling, paste0(path, 'choices.csv'), row.names = F, col.names = F, sep=",")

## bonuses
recalled_total = recalled & recalled_val
nrecall_bonus = rowSums(recalled_total)
df.s2.subj = df.s2 %>% filter(question_order == 0) %>%
  mutate(s2_bonus = ifelse(is.na(s2_value), 0, s2_value),
         mem_bonus = nrecall_bonus * pointsPerWord + allBonus * (nrecall_bonus == numWords))
df.demo = df.demo %>% mutate(s2_bonus = I(df.s2.subj$s2_bonus), mem_bonus = I(df.s2.subj$mem_bonus),
                             bonus = round((s1_bonus / pointsPerCent_s1 + s2_bonus / pointsPerCent_s2  + mem_bonus) / 100, 2))
write.table(df.demo %>% dplyr::select(WorkerID = subject, Bonus = bonus),
            paste0(path, 'Bonuses.csv'), row.names = FALSE, col.names = FALSE, sep = ",")

## save
save.image(paste0(path, 'analysis.rdata'))

# jphil’s stuff -----------------------------------------------------------



## word order raster plot:
cor.test(df.words.filt$order[df.words.filt$recall==TRUE],df.words.filt$s1_value[df.words.filt$recall==TRUE])
plot <- df.words.filt %>%
  filter(recall & subject %in% subj.notfirst) %>%
  mutate(value = s1_value, high_value = factor(c("Low Past Value","High Past Value")[as.factor(high_s1value)])) %>%
  group_by(order,value) %>%
  summarise(count = table(value)[1]) %>%
  ggplot(aes(x=order,y=value,fill=count)) +
  geom_tile()+
  scale_fill_continuous(low = 'white',high = 'red') +
  #facet_wrap(~high_value, scales="free_y",ncol=1) +
  theme_bw() +
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,legend.title=element_blank()
    #,legend.position=c(.1,.9)
    #,legend.text=element_text(size=rel(1.4))
    ,axis.text.y=element_text(size=rel(1.5))
    ,axis.text.x=element_text(size=rel(1.5))
    ,axis.title.y=element_text(vjust=.9)
    ,axis.ticks = element_blank()
    ,strip.text=element_text(size=rel(1.5))
    ,axis.title=element_text(size=rel(1.5))    
  )
plot  


## word order mean position plot

orderD <- df.words %>%
  filter(recall) %>%
  mutate(value = s1_value, high_val = factor(c("Low Past Value","High Past Value")[as.factor(high_s1value)])) %>%
  group_by(value,high_val,subject) %>%
  summarise(meanOrders = mean(order,na.rm=T)) %>%
  group_by(value,high_val) %>%
  summarise(meanOrder = mean(meanOrders,na.rm=T),
            seOrder = se(meanOrders),
            minOrder = meanOrder - seOrder,
            maxOrder = meanOrder + seOrder) %>%
  arrange(order(high_val))


# plot2 <- ggplot(orderD,aes(x=meanOrder,y=value)) +
#   geom_errorbarh(xmin=orderD$minOrder, xmax=orderD$maxOrder, height=.2) +
#   geom_point(size=3,color="Red") +
#   coord_cartesian(xlim=c(0,15)) +
#   facet_wrap(~high_val, scales="free_y",ncol=1) +
#   theme_bw() +
#   theme(
#     plot.background = element_blank()
#     ,panel.grid.major = element_blank()
#     ,panel.grid.minor = element_blank()
#     ,legend.title=element_blank()
#     #,legend.position=c(.1,.9)
#     #,legend.text=element_text(size=rel(1.4))
#     ,axis.text.y=element_text(size=rel(1.5))
#     ,axis.text.x=element_text(size=rel(1.5))
#     ,axis.title.y=element_text(vjust=.9)
#     ,axis.ticks = element_blank()
#     ,strip.text=element_text(size=rel(1.5))
#     ,axis.title=element_text(size=rel(1.5))    
#   )
# plot2

## Graph for simulations - here the data are set so that there are 500 words, and a choice set size of 10, meaning 2% of the 

# jphilPalette <- c("darkorange3","lightblue","darkgreen","azure4")
# 
# d.sims = read.csv("data/modelSim.csv") %>% gather(model,earnings,-R) %>%
#   mutate(model = recode(model, CS ="Choice Set", MB = "Full Planning", MF = "No Planning"),
#          model = factor(model, levels=c("No Planning","Choice Set","Full Planning")),
#          R = factor(R),
#          earnings = (earnings/max(earnings))*100) %>%
#   filter(R!=1) %>%
#   ggplot(aes(x=R,y=earnings,fill=model)) +
#   geom_bar(position="dodge",stat="identity") +
#   #geom_line(aes(color=model)) +
#   scale_fill_manual(values=grey.colors(3,start=.9,end=.3)) +
#   theme_bw() +
#   theme(
#     plot.background = element_blank()
#     ,panel.grid.major = element_blank()
#     ,panel.grid.minor = element_blank()
#     ,legend.title=element_blank()
#     ,legend.text=element_text(size=rel(1.4))
#     ,axis.title=element_blank()
#     ,axis.text.y=element_text(size=rel(1.5))
#     ,axis.text.x=element_text(size=rel(1.5))
#     ,axis.ticks = element_blank()
#   )
# d.sims
