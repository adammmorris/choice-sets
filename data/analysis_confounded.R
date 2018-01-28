# setup -------------------------------------------------------------------

require(dplyr)
require(ggplot2)
require(lme4)
require(lmerTest)
require(mlogit)
require(lattice)
require(stringdist)

theme_update(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(colour = "black"),
             axis.text=element_text(size=20, colour = "black"), axis.title=element_text(size=18, face = "bold"), axis.title.x = element_text(vjust = 0),
             legend.title = element_text(size = 24, face = "bold"), legend.text = element_text(size = 20), plot.title = element_text(size = 26, face = "bold", vjust = 1))

setwd("~/Me/Psychology/Projects/choicesets/with_sam")


## Setup

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

se = function(x) {return(sd(x) / sqrt(length(x)))}
dodge <- position_dodge(width=0.9)


# import data -------------------------------------------------------------


numWords = 14;
pointsPerCent = 10;
pointsPerWord = 10; # for memory condition
path = 'data/cs_wg_v8/real1/'

# Load data
df.demo = read.csv(paste0(path, 'demo.csv'), stringsAsFactors = F) %>% arrange(subject) %>% mutate(total_time_real = total_time / 60000)
df.words.raw = read.csv(paste0(path, 'words.csv'), stringsAsFactors = F) %>% arrange(subject, word_ind)
df.s1.raw = read.csv(paste0(path, 's1.csv'), stringsAsFactors = F) %>% arrange(subject)
df.s2.raw = read.csv(paste0(path, 's2.csv'), stringsAsFactors = F) %>% arrange(subject, question_order)

#df.words.subj = df.words.raw %>% group_by(subject) %>% summarize(num = n()) %>% filter(num == 14)
subjlist = df.demo$subject

## words

df.words = df.words.raw %>% mutate(doubled = ifelse(is.na(lead(word)), FALSE, word == lead(word) & subject == lead(subject))) %>%
  filter(doubled == FALSE & subject %in% subjlist) %>%
  mutate(high_val = value > 5, numChosen = 0)

## s1

df.s1 = df.s1.raw %>% filter(subject %in% subjlist) %>%
  mutate(correct_word = ain(toupper(resp), word, maxDist = 2), correct_val = resp2 == value, word_chosen = ifelse(choice, alt, word),
         high_val = value > 5)

for (i in 1:nrow(df.s1)) {
  subj = df.s1$subject[i]
  word = df.s1$word[i]
  alt = df.s1$alt[i]
  choice = df.s1$choice[i]
  df.s1$word_value[i] = df.words$value[df.words$subject == subj & df.words$word == word]
  df.s1$alt_value[i] = df.words$value[df.words$subject == subj & df.words$word == alt]
  df.s1$word_better[i] = df.s1$word_value[i] > df.s1$alt_value[i]
  df.s1$chose_better[i] = ifelse(df.s1$word_better[i], !choice, choice)
}

# get numChosen & cors
df.s1.subjword = df.s1 %>% group_by(subject, word_chosen) %>% summarize(numChosen = n())
for (i in 1:nrow(df.words)) {
  subjword_rows = df.s1.subjword$subject == df.words$subject[i] & df.s1.subjword$word_chosen == df.words$word[i]
  df.words$numChosen[i] = ifelse(any(subjword_rows), df.s1.subjword$numChosen[subjword_rows], NA)
  df.words$numChosen_high[i] = df.words$numChosen[i] > 6
}
df.words.subj = df.words %>% group_by(subject) %>% summarize(cor = cor(value, numChosen)) 

# get pctCorrects
df.s1.subj = df.s1 %>% group_by(subject) %>%
  summarize(pctCorrect_words = mean(correct_word, na.rm = T), pctCorrect_val = mean(correct_val, na.rm = T), numTrials = n(),
            pctHighVal = mean(high_val), pctChoseBetter = mean(chose_better, na.rm = T))
df.s1.trial = df.s1 %>% group_by(trial) %>% summarize(pctCorrectChoice = mean(chose_better))
plot(df.s1.trial$pctCorrectChoice)

## s2
df.s2 = df.s2.raw %>% filter(subject %in% subjlist)

# Mutate df.s2
df.s2$choice = toupper(df.s2$choice)
df.s2$scratch = gsub("[.]", ",", toupper(as.character(df.s2$scratch)))
df.s2$all_values = as.character(df.s2$all_values)

df.s2$rank_value = NULL
df.s2$num_ties = NULL
for (i in 1:nrow(df.s2)) {
  subj.name = df.s2$subject[i]
  wordlist = (df.words %>% filter(subject == subj.name))$word
  c = df.s2$choice[i]
  creal = wordlist[amatch(c, wordlist, maxDist = 2)]
  cind = getIndex(creal, wordlist)
  
  all_vals = as.numeric.vector(df.s2$all_values[i])
  #all_vals = rewards_te[qvec[df.s2$question_ind[i] + 1], ] * mult[df.s2$question_ind[i] + 1]
  #df.s2$all_values[i] = paste0('[', toString(all_vals), ']')
  
  all_vals_rank = rank(all_vals, ties.method = 'max')
  s2_val = ifelse(is.na(cind), NA, all_vals[cind])
  word_rows = subj.name == df.words$subject & creal == df.words$word
  
  df.s2$choice_real[i] = creal
  df.s2$choice_real_ind[i] = cind
  df.s2$s2_value[i] = s2_val
  df.s2$rank_value[i] = ifelse(is.na(cind), NA, all_vals_rank[cind])
  df.s2$s1_value[i] = ifelse(is.na(cind), NA, df.words$value[word_rows])
  df.s2$s1_exposures[i] = ifelse(is.na(cind), NA, df.words$exposures[word_rows])
  df.s2$s1_chosen[i] = ifelse(is.na(cind), NA, df.words$numChosen[word_rows])
}

df.s2 = df.s2 %>% mutate(s2_subj_ind = as.numeric(as.factor(subject)), # don't use that ind for anything serious
                         doubled = ifelse(is.na(choice_real_ind), NA, ifelse(is.na(lead(choice_real_ind)), F, choice_real_ind == lead(choice_real_ind)) |
                                                  ifelse(is.na(lag(choice_real_ind)), F, choice_real_ind == lag(choice_real_ind))),
                         bonus_value = ifelse(is.na(choice_real_ind), 0, ifelse(doubled, 0, s2_value)),
                         high_val = s1_value > 5)

df.mem = df.s2 %>% filter(question == 'Memory')

df.s2.subj = df.s2 %>% filter(subject %in% df.demo$subject) %>%
  group_by(subject) %>%
  summarize(s2_bonus = sum(bonus_value), rt = mean(rt) / 1000,
            comp_check_pass = mean(comp_check_pass),
            comp_check_rt = mean(comp_check_rt) / 1000,
            numNAs = sum(is.na(choice_real)),
            numRepeats = sum(choice_real == lag(choice_real), na.rm = T),
            high_val = mean(high_val, na.rm = T),
            s2_val = mean(rank_value, na.rm = T))


## Compute recalled
recalled = matrix(F, nrow = nrow(df.mem), ncol = numWords)
recalled_ever = matrix(F, nrow = nrow(df.mem), ncol = numWords)
recalled_val = matrix(F, nrow = nrow(df.mem), ncol = numWords)
df.words$recall = NULL
df.words$recall.ever = NULL
df.words$order = NULL

for (i in 1:nrow(df.mem)) {
  subj.name = df.mem$subject[i]
  df.words.temp = df.words %>% filter(subject == subj.name)
  df.s2.temp = df.s2 %>% filter(subject == subj.name)
  
  words_temp = trimws(as.string.vector(df.mem$choice[i]))
  val_temp = as.numeric(trimws(as.string.vector(df.mem$scratch[i])))
  
  wordlist = df.words.temp$word
  
  if (length(wordlist) == numWords) {
    for (j in 1:numWords) {
      which_word = amatch(wordlist[j], words_temp, maxDist = 2, nomatch = 0)
      recalled[i,j] = which_word > 0
      
      if (recalled[i,j]) {
        true_val = df.words.temp$value[df.words.temp$word_ind  == (j - 1)]
        recalled_val[i,j] = abs(val_temp[which_word] - true_val) <= 2
      }
      df.words$recall[df.words$subject == subj.name & df.words$word == wordlist[j]] = recalled[i,j]
      
      recalled_ever[i,j] = recalled[i,j] | any(na.omit(df.s2.temp$choice_real_ind) == j)
      df.words$recall.ever[df.words$subject == subj.name & df.words$word == wordlist[j]] = recalled_ever[i,j]
      
      df.words$order[df.words$subject == subj.name & df.words$word == wordlist[j]] = which_word
    }
  }
}

## Compute exclusion
# Exclude if any of these: cor in s1 < .75, comp_check_pass < .5, pctCorrect_words < .75, pctCorrect_pts < .75, numNAs > 3, numRepeats > 2, numRecalled < 5
include_rows = NULL
include_names = NULL

for (subj in 1:length(subjlist)) {
  subj.name = subjlist[subj]
  df.s1.subj.temp = df.s1.subj %>% filter(subject == subj.name)
  df.s2.subj.temp = df.s2.subj %>% filter(subject == subj.name)

  if (df.s2.subj.temp$comp_check_pass <= .5 || df.s1.subj.temp$pctCorrect_words < .75 || df.s1.subj.temp$pctCorrect_val < .75 || df.s1.subj.temp$numTrials != 91 || df.s1.subj.temp$pctChoseBetter < .6 ||
      df.s2.subj.temp$numRepeats > 2 || sum(recalled[subj,]) < 5 || df.s2.subj.temp$numNAs > 4) { # either 2 or 4
    include_rows[subj] = FALSE
  } else {
    include_rows[subj] = TRUE
    include_names = c(include_names, subj.name)
  }
}

#good_v3 = c(2,3,4,7,8,9,10,13,15,17,18,21,23,24,28,30,31,32,36,39,40,42,44,45,46,49,51,52,54,56,57,59,62,64,66,67,68,74,75,76,77,78,81,82,83,84,86,89,91,94,95,98,99,100,106,107,108,109,110,113,114,115,118,119,120)
#good_v5 = c(3,5,10,11,13,14,16,20,23,25,26,28,31,32,36,37,41,42,43,45,46,50,52,53,54,55,56,58,59,60,62,68,70,71,73,74,75,76,77,81,84,86,87,88,89,92,94,96,98,99,101,103,105,107,108,109,111,115,117,118,119,121,125,126,127,129,130,131,133,134,136,141,143,145,146,147,153,157,158,160,161,164,165,170,171,173,176,177,181,182,184,185,186,187,190,192,193,194,195,196,197,198,200,201,202,204,205,206,208,209,210,213,214,216,218,221,224,225,226,227,228,231,233,234,235,237,238,244,246,248,251,252,253,255)
#include_names_good = include_names[good_v5]


# check out data ----------------------------------------------------------


## Check out data
nrecall = rowSums(recalled[include_rows,])
nrecall_val = rowSums(recalled[include_rows,])
mean(nrecall)
mean(nrecall_val)

df.words.coll = df.words %>% filter(subject %in% include_names) %>% group_by(high_val, subject) %>% summarize(recall = mean(recall, na.rm = T)) %>%
  group_by(high_val) %>% summarize(recall.mean = mean(recall, na.rm = T), recall.se = se(recall))
ggplot(df.words.coll, aes(x = high_val, y = recall.mean)) +
  geom_bar(stat = "identity", position = dodge) +
  geom_errorbar(aes(ymax = recall.mean + recall.se, ymin = recall.mean - recall.se), width = .5, position = dodge) +
  xlab('') + ylab('') + guides(fill = F)

# Test what affected recall
m.recall = glmer(recall ~ high_val + (0 + high_val | subject) + (1 | subject) + (1 | word),
                 data = df.words %>% filter(subject %in% include_names), family = binomial)
summary(m.recall)


## Check out df.s2 stats
hist(df.s2[df.s2$subject %in% include_names, ]$rank_value, breaks = 15, main = "S2 ranks of words chosen in S2", xlab = "S2 rank")
mean(df.s2[df.s2$subject %in% include_names, ]$rank_value, na.rm = T)

hist(as.numeric(df.s2[df.s2$subject %in% include_names, ]$high_val), breaks = 15, main = "S1 values of words chosen in S2", xlab = "S1 value")
mean(df.s2[df.s2$subject %in% include_names, ]$high_val, na.rm = T)

df.s2.subj.filt = df.s2.subj %>% filter(subject %in% include_names)

ggplot(df.s2.subj.filt, aes(x = high_val)) + geom_histogram(col = 'black', fill = 'blue')
ggplot(df.s2.subj.filt, aes(x = s2_val)) + geom_histogram(col = 'black', fill = 'blue') + xlim(c(1,14))
t.test(df.s2.subj.filt$high_val - .5)
t.test(df.s2.subj.filt$s2_val - 7)


# order effects -----------------------------------------------------------


# Test order
histogram(~ order | value, df.words[df.words$subject %in% include_names & df.words$recall == T, ])
m.order = lmer(order ~ high_val + (high_val | subject) + (high_val | word),
                 data = df.words[df.words$subject %in% include_names & df.words$recall == T, ])
summary(m.order)

coefs = coef(m.order)$subject$high_valTRUE
weights_v8 = c(6.01e-01,2.56e-01,3.72e-01,3.10e-01,2.10e-01,5.41e-08,1.08e-08,3.03e-01,2.15e-01,1.49e-08,2.65e-08,2.17e-06,1.88e-01,1.13e-08,4.72e-08,4.62e-02,1.62e-01,1.60e-01,7.77e-02,2.49e-01,1.87e-08,1.33e-01,1.80e-08,1.21e-01,4.56e-01,1.24e-07,2.04e-01,1.30e-07,2.03e-03,6.07e-01,4.46e-07,7.41e-01,1.21e-01,2.68e-07,1.84e-01,9.94e-01,5.68e-01,7.53e-01,4.95e-07,9.45e-08,6.10e-08,2.74e-08,1.23e-07,5.15e-08,3.44e-01,5.73e-07,5.88e-01,2.65e-01,3.24e-08,7.84e-01,2.13e-01,9.03e-08,7.08e-01,7.47e-01,2.30e-01,4.69e-01,3.20e-01,1.37e-07,1.94e-07,2.50e-08,5.05e-01,1.77e-01,1.05e-07,2.09e-01,3.54e-08,01,4.47e-01,4.35e-01,4.30e-08,3.83e-01,6.80e-01,4.75e-01,1.55e-01,3.82e-01,4.46e-01,5.44e-01,01,4.42e-06,3.37e-01,5.01e-01,3.51e-01,1.74e-01,1.85e-01,1.15e-05,1.21e-01,8.85e-08,1.50e-07,2.03e-01,3.29e-01,3.33e-01,3.48e-01,2.89e-07,2.44e-01,01,9.50e-09,9.14e-08,4.96e-02,3.68e-01,4.69e-01,1.79e-06,6.75e-01,2.18e-01,1.21e-01,7.06e-01,2.13e-01,2.49e-01,7.83e-01,2.85e-01,4.43e-01,5.93e-08,6.45e-01,2.86e-07,1.60e-08,5.30e-02,1.83e-07,4.96e-01,2.09e-07,4.51e-08,3.51e-01,4.04e-01,3.19e-01,5.59e-01,5.82e-01,5.40e-07,3.97e-08,7.36e-03,5.21e-08,2.93e-01,2.78e-07,5.07e-01,1.45e-01,1.14e-06,01,4.02e-01,6.53e-09,2.57e-01,1.64e-01,1.99e-01,1.97e-07,2.61e-07,01,2.04e-07,9.94e-02,3.17e-08,2.38e-08,3.29e-01,3.22e-01,2.42e-01,1.61e-07,1.19e-07,1.61e-07,2.81e-01,5.76e-08,2.34e-01,3.25e-08,2.10e-02,3.79e-01,3.51e-01,5.01e-01,6.04e-08,1.23e-07,7.24e-01,2.11e-01,7.10e-08,1.00e+00,2.66e-01,1.76e-06,1.65e-01,2.99e-07,9.67e-01,1.94e-07,2.58e-02,2.69e-07,1.68e-07,4.15e-01,2.44e-01,7.50e-08,1.60e-07,2.75e-01,1.03e-02,1.44e-01,7.14e-01,3.41e-02,3.07e-07)
cor.test(coefs, weights_v8)


# mlogit ------------------------------------------------------------------
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


# bonuses, modeling -----------------------------------------------------------------


## Get bonuses
nrecall_bonus = rowSums(recalled & recalled_val)
df.s2.subj = df.s2.subj %>% mutate(mem_bonus = nrecall_bonus[df.mem$subject == subject] * pointsPerWord)
df.demo = df.demo %>% mutate(s2_bonus = I(df.s2.subj$s2_bonus), mem_bonus = I(df.s2.subj$mem_bonus),
                             bonus = round((s1_bonus + s2_bonus + mem_bonus) / (pointsPerCent * 100), 2))
write.table(df.demo %>% filter(id >= 150) %>% select(WorkerID = subject, Bonus = bonus),
            paste0(path, 'Bonuses.csv'), row.names = FALSE, col.names = FALSE, sep = ",")

save.image(paste0(path, 'analysis.rdata'))


## Save for modeling

rewards_tr = matrix(0, nrow = sum(include_rows), ncol = numWords)
ind = 1
for (subj in 1:nrow(df.demo)) {
  subj.name = df.demo$subject[subj]
  
  if (subj.name %in% include_names) {
    df.words.temp = df.words %>% filter(subject == subj.name)
    
    for (word in 1:numWords) {
      rewards_tr[ind, word] = df.words.temp$value[word]
      #exposures_tr[ind, word] = df.s1.temp$exposures[word]
      #chosen_tr[ind, word] = df.s1.temp$numChosen[word]
    }
    ind = ind + 1
  }
}

write.csv(rewards_tr, paste0(path, 'rewards_tr.csv'), row.names = F)
write.csv(recalled_ever[include_rows, ] * 1, paste0(path, 'recalled.csv'), row.names = F)

df.modeling = df.s2 %>% filter(subject %in% include_names & !is.na(choice_real_ind)) %>%
  mutate(all_values_nocomma = gsub(",", " ", all_values)) %>% 
  select(s2_subj_ind, choice_real_ind, all_values_nocomma)
write.table(df.modeling, paste0(path, 'choices.csv'), row.names = F, col.names = F, sep=",");








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