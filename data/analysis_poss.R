require(dplyr)
require(ggplot2)
require(lme4)
require(lmerTest)
require(mlogit)
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

numWords = 15;
pointsPerCent = 10;
path = 'data/cs_wg_v3_poss/pilot1/'

# Load data
df.demo = read.csv(paste0(path, 'demo.csv'), stringsAsFactors = F) %>% arrange(subject) %>% mutate(total_time_real = total_time / 60000)
df.words.raw = read.csv(paste0(path, 'words.csv'), stringsAsFactors = F) %>% arrange(subject, word_ind)
df.s1.raw = read.csv(paste0(path, 's1.csv'), stringsAsFactors = F) %>% arrange(subject)
df.s2.raw = read.csv(paste0(path, 's2.csv'), stringsAsFactors = F) %>% arrange(subject)

subjlist = df.demo$subject

## Fix DFs

# drop anyone who didn't finish
df.s1 = df.s1.raw %>% filter(subject %in% subjlist) %>% mutate(correct_word = ain(toupper(resp), word, maxDist = 2), correct_val = resp2 == value)
df.s2 = df.s2.raw %>% filter(subject %in% subjlist)
df.words = df.words.raw %>% mutate(doubled = ifelse(is.na(lead(word)), FALSE, word == lead(word) & subject == lead(subject))) %>%
  filter(doubled == FALSE & subject %in% subjlist) %>%
  mutate(high_val = value > 5, numChosen = 0)

# get numChosen & cors
df.s1.subjword = df.s1 %>% group_by(subject, word) %>% summarize(numChosen = sum(choice == 0))
for (i in 1:nrow(df.words)) {
  subjword_rows = df.s1.subjword$subject == df.words$subject[i] & df.s1.subjword$word == df.words$word[i]
  df.words$numChosen[i] = ifelse(any(subjword_rows), df.s1.subjword$numChosen[subjword_rows], NA)
}
df.cors = df.words %>% group_by(subject) %>% summarize(cors = cor(numChosen, value))

# get pctCorrects
df.s1.subj = df.s1 %>% group_by(subject) %>% summarize(pctCorrect_words = mean(correct_word), pctCorrect_val = mean(correct_val), numTrials = n())

# Mutate df.s2
df.s2$choice = toupper(df.s2$choice)
df.s2$scratch = gsub("[.]", ",", toupper(as.character(df.s2$scratch)))
df.s2$all_values = as.character(df.s2$all_values)

rewards_te = matrix(c(3, 1, 2, 1, 5, 4, 4, 0, 3, 2, 2, 2, 2, 3,
              1, 4, 0, 4, 4, 0, 3, 2, 6, 3, 3, 3, 2, 2,
              3, 2, 4, 4, 0, 4, 1, 5, 1, 2, 4, 2, 4, 1,
              2, 3, 3, 4, 3, 2, 3, 4, 5, 2, 4, 4, 3, 2,
              23, 12, 23, 14, 12, 25, 12, 12, 15, 26, 9, 24, 2, 8,
              4, 19, 0, 14, 14, 12, 14, 14, 4, 17, 19, 0, 4, 0,
              5, 12, 1, 8, 8, 13, 18, 5, 6, 0, 18, 1, 13, 6,
              5, 2, 13, 7, 11, 7, 1, 22, 12, 3, 17, 5, 14, 15,
              2, 1, 2, 4, 2, 3, 1, 0, 2, 0, 8, 2, 2, 2,
              6, 5, 5, 9, 7, 8, 5, 3, 9, 5, 5, 8, 6, 4,
              6,  3,  5,  3,  7,  6,  6,  1, 11,  8,  5,  7,  4,  5), nrow = 11, ncol = numWords, byrow = T);
qvec = c(1, 2, 5, 6, 7, 8, 10, 11)
mult = c(10, 10, 5, 5, 5, 10, 10, 10)

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
  df.s2$num_ties[i] = ifelse(is.na(cind), NA, sum(s2_val == all_vals))
  df.s2$s1_value[i] = ifelse(is.na(cind), NA, df.words$value[word_rows])
  df.s2$s1_exposures[i] = ifelse(is.na(cind), NA, df.words$exposures[word_rows])
  df.s2$s1_chosen[i] = ifelse(is.na(cind), NA, df.words$numChosen[word_rows])
  
  df.s2$numWords_s1val[i] = ifelse(is.na(cind), NA, ifelse(df.s2$s1_value[i] %in% c(0,10), 3, 2))
}

df.s2 = df.s2 %>% mutate(s2_subj_ind = as.numeric(as.factor(subject)), # don't use that ind for anything serious
                         doubled = ifelse(is.na(choice_real_ind), NA, ifelse(is.na(lead(choice_real_ind)), F, choice_real_ind == lead(choice_real_ind)) |
                                                  ifelse(is.na(lag(choice_real_ind)), F, choice_real_ind == lag(choice_real_ind))),
                         bonus_value = ifelse(is.na(choice_real_ind), 0, ifelse(doubled, 0, s2_value)),
                         high_val = s1_value > 5)

df.mem = df.s2 %>% filter(question == 'Memory')

df.s2.subj = df.s2 %>% filter(subject %in% df.demo$subject) %>% group_by(subject) %>%
  summarize(s2_bonus = sum(bonus_value), rt = mean(rt) / 1000,
            comp_check_pass = mean(comp_check_pass),
            comp_check_rt = mean(comp_check_rt) / 1000,
            numNAs = sum(is.na(choice_real)),
            numRepeats = sum(choice_real == lag(choice_real), na.rm = T))

## Compute recalled
recalled = matrix(F, nrow = nrow(df.mem), ncol = numWords)
recalled_ever = matrix(F, nrow = nrow(df.mem), ncol = numWords)
recalled_val = matrix(F, nrow = nrow(df.mem), ncol = numWords)
df.words$recall = NULL

for (i in 1:nrow(df.mem)) {
  subj.name = df.mem$subject[i]
  df.words.temp = df.words %>% filter(subject == subj.name)
  df.s2.temp = df.s2 %>% filter(subject == subj.name)
  
  words_temp = trimws(as.string.vector(df.mem$choice[i]))
  val_temp = as.numeric(trimws(as.string.vector(df.mem$scratch[i])))
  
  wordlist = df.words.temp$word
  
  for (j in 1:numWords) {
    which_word = amatch(wordlist[j], words_temp, maxDist = 2, nomatch = 0)
    recalled[i,j] = which_word > 0
    
    if (recalled[i,j]) {
      true_val = df.words.temp$value[df.words.temp$word_ind  == (j - 1)]
      recalled_val[i,j] = abs(val_temp[which_word] - true_val) <= 2
    }
    df.words$recall[df.words$subject == subj.name & df.words$word == wordlist[j]] = recalled[i,j]
    
    recalled_ever[i,j] = recalled[i,j] | any(na.omit(df.s2.temp$choice_real_ind) == j)
  }
}

## Compute exclusion
# Exclude if any of these: cor in s1 < .75, comp_check_pass < .5, pctCorrect_words < .75, pctCorrect_pts < .75, numNAs > 3, numRepeats > 2, numRecalled < 5
include_rows = NULL
include_names = NULL

for (subj in 1:nrow(df.demo)) {
  subj.name = df.demo$subject[subj]
  df.s1.subj.temp = df.s1.subj %>% filter(subject == subj.name)
  df.s2.subj.temp = df.s2.subj %>% filter(subject == subj.name)
  df.cors.temp = df.cors %>% filter(subject == subj.name)
  
  if (df.s1.subj.temp$pctCorrect_words < .75 || df.s1.subj.temp$pctCorrect_val < .75 || df.s2.subj.temp$comp_check_pass < .5 || df.s2.subj.temp$numNAs > 3 ||
      df.s2.subj.temp$numRepeats > 2 || df.cors.temp$cors < .75 || sum(recalled[subj,]) < 5 || df.s1.subj.temp$numTrials != 112) {
    include_rows[subj] = FALSE
  } else {
    include_rows[subj] = TRUE
    include_names = c(include_names, subj.name)
  }
}

## Check out data
nrecall = rowSums(recalled[include_rows,])
nrecall_val = rowSums(recalled[include_rows,])
mean(nrecall)
mean(nrecall_val)

# Test what affected recall
m.recall = glmer(recall ~ value + (0 + value | subject) + (1 | subject) + (1 | word),
                 data = df.words[df.words$subject %in% include_names, ], family = binomial)
summary(m.recall)

## Check out df.s2 stats
hist(df.s2[df.s2$subject %in% include_names, ]$rank_value, breaks = 15, main = "S2 ranks of words chosen in S2", xlab = "S2 rank")
mean(df.s2[df.s2$subject %in% include_names, ]$rank_value, na.rm = T)

hist(df.s2[df.s2$subject %in% include_names, ]$s1_value, breaks = 15, main = "S1 values of words chosen in S2", xlab = "S1 value")
mean(df.s2[df.s2$subject %in% include_names, ]$s1_value, na.rm = T)

#hist(df.s1[df.s1$subject %in% include_names, ]$numChosen, breaks = 15)
#mean(df.s1[df.s1$subject %in% include_names, ]$numChosen, na.rm = T)
#hist(df.s2[!(df.s2$subject %in% exclude_names), ]$s1_chosen, breaks = 15)
#mean(df.s2[!(df.s2$subject %in% exclude_names), ]$s1_chosen, na.rm = T)

## Prepare for mlogit
numRealQuestions = numQuestions - 1
df.logit = data.frame(Subj = NULL, Trial = NULL, OptionID = NULL, Choice = NULL, MFval = NULL, MBval = NULL, nExposures = NULL, Recalled = NULL)

for (subj in 1:nrow(df.demo)) {
  subj.name = df.demo$subject[subj]
  #recalled.temp = recalled_ever[subj, ]
  recalled.temp = !logical(numWords)
  num.recalled.temp = sum(recalled.temp)
  
  df.words.temp = df.words %>% filter(subject == subj.name)
  df.s2.temp = df.s2 %>% filter(subject == subj.name) %>% arrange(question_order)
  
  nAnswered = sum(!is.na(df.s2.temp$choice_real_ind))

  if (nAnswered > 0 & subj.name %in% include_names) {
    Subj.col = rep(subj, num.recalled.temp * nAnswered)
    
    MFval.col = rep(df.words.temp$high_val[recalled.temp] * 1, nAnswered)
    nExposures.col = rep(df.words.temp$exposures[recalled.temp], nAnswered)
    Recalled.col = rep(df.words.temp$recall[recalled.temp] * 1, nAnswered)
    numChosen.col = rep(df.words.temp$numChosen[recalled.temp], nAnswered)
    OptionID.col = rep(which(recalled.temp), nAnswered)
    Trial.col = rep(1:nAnswered, each = num.recalled.temp)
    
    temp.mbval = matrix(0, nrow = nAnswered, ncol = num.recalled.temp)
    temp.choice = matrix(0, nrow = nAnswered, ncol = num.recalled.temp)
    ind = 1
    for (q in 1:numRealQuestions) {
      if (!is.na(df.s2.temp$choice_real_ind[q])) {
        all_vals = as.numeric.vector(df.s2.temp$all_values[q])
        #all_vals = rewards_te[qvec[df.s2.temp$question_ind[q] + 1], ]
        mbvals = rank(all_vals, ties.method = 'max')
        temp.mbval[ind,] = mbvals[recalled.temp]
        
        choice = logical(num.recalled.temp)
        choice[which(df.s2.temp$choice_real_ind[q] == which(recalled.temp))] = TRUE
        temp.choice[ind,] = choice
        
        ind = ind + 1
      }
    }
    
    MBval.col = as.vector(t(temp.mbval))
    Choice.col = as.vector(t(temp.choice))
    
    df.logit = rbind(df.logit,
                     data.frame(Subj = Subj.col, Trial = Trial.col, OptionID = OptionID.col, Choice = Choice.col,
                                MFval = MFval.col, MBval = MBval.col, nExposures = nExposures.col,
                                Recall = Recalled.col, nChosen = numChosen.col))

  }
}

# Do logit
runLogit = function(df) {
  df$Choice = as.logical(df$Choice)
  df$OptionID = factor(df$OptionID)
  df = df %>% mutate(Trial_unique = paste(Subj, Trial, sep="_"))
  df$Trial = factor(df$Trial)
  df$Trial_unique = factor(df$Trial_unique)
  df$Subj = factor(df$Subj)
  df = df %>% mutate(MFcent = MFval - mean(MFval), MBcent = MBval - mean(MBval), Int = MFcent * MBcent)
  df.m = mlogit.data(df, choice = "Choice", shape = "long", id.var = "Subj", alt.var = "OptionID", chid.var = "Trial_unique")
  
  m = mlogit(Choice ~ MFcent + MBcent + Int + Recall | -1, df.m, panel = T,
             rpar = c(MFcent = "n", MBcent = "n", Int = "n"), correlation = F, halton = NA, R = 1000, tol = .001)
  return(m)
}

m.real = runLogit(df.logit)
summary(m.real)

## Save for modeling

rewards_tr = matrix(0, nrow = sum(include_rows), ncol = numWords)
#exposures_tr = matrix(0, nrow = sum(!exclude), ncol = numWords)
#chosen_tr = matrix(0, nrow = sum(!exclude), ncol = numWords)
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

## Graph
df.rank = df.s2 %>% filter(subject %in% include_names & !is.na(rank_value))
nrows = nrow(df.rank)
df.rank = df.rank %>%
  mutate(pos = s1_value > 5) %>%
  group_by(rank_value, pos) %>%
  summarize(numChosen_S2 = n(), numOpps_S2 = mean(num_ties), prob_rank = numChosen_S2 / numOpps_S2 / nrows) %>%
  mutate(log_prob_rank = log(prob_rank))
#df.rank$prob_rank[df.rank$five == FALSE] = df.rank$prob_rank[df.rank$five == FALSE] / 4

base = ggplot(data = df.rank, aes(x = rank_value, y = prob_rank, group = pos, colour = pos)) +
  geom_point()
base
base + geom_smooth(method = 'lm', formula = y ~ exp(1 * x))
base + geom_smooth(method = 'lm', formula = y ~ choose(x - 1, 5))
#base + geom_smooth(method = 'lm', formula = y ~ x)
#base + stat_function(fun = function(x) {.0091 * exp(.1 * x) - .0266}) +
#  stat_function(fun = function(x) {7.22e-06 * choose(x - 1, 4) - 6.447e-04})

#for (j in 1:nrow(df.demo)) {
#  subj = df.demo$subject[j]
#  vals = df.s1$pos[as.character(df.s1$subject) == as.character(subj)]
#  choices = as.numeric.vector(as.character(df.demo$tr_choices[j]))
#  words = as.string.vector(as.character(df.demo$tr_resp_correct[j]))
#  word_vals = vals[amatch(toupper(words), wordlist, maxDist = 2)]
#  choices[word_vals]
#}

## Get bonuses
nrecall_bonus = rowSums(recalled & recalled_val)
df.s2.subj = df.s2.subj %>% mutate(mem_bonus = nrecall_bonus[df.mem$subject == subject] * pointsPerWord)
df.demo = df.demo %>% mutate(s2_bonus = I(df.s2.subj$s2_bonus), mem_bonus = I(df.s2.subj$mem_bonus),
                             bonus = round((s1_bonus + s2_bonus + mem_bonus) / (pointsPerCent * 100), 2))
write.table(df.demo %>% filter(id >= 150) %>% select(WorkerID = subject, Bonus = bonus),
            paste0(path, 'Bonuses - cs_wg_v3_real2.csv'), row.names = FALSE, col.names = FALSE, sep = ",")

save.image(paste0(path, 'analysis.rdata'))

### SIMULATIONS

df.sim = read.csv('simulations/results/wg_v2/mixture-mf-mb.csv')
m.sim = runLogit(df.sim)
summary(m.sim)









