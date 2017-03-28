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

numWords = 12;
numQuestions = 9; # including memory
pointsPerCent = 10;
pointsPerWord = 10; # for memory condition
path = 'data/cs_wg_v3/pilot1/'
included_words = c(1,  3,  5,  6,  8, 10, 11, 12, 15, 16, 18, 19) + 1

# Load data
df.demo = read.csv(paste0(path, 'demo.csv')) %>% arrange(subject)
df.s1.raw = read.csv(paste0(path, 's1.csv')) %>% arrange(subject, word_ind)
df.s2 = read.csv(paste0(path, 's2.csv')) %>% arrange(subject)

## Fix DFs

# Mutate df.demo
df.demo = df.demo %>% mutate(total_time_real = total_time / 60000)

# Get wordlist
df.words = df.s1.raw %>% top_n(numWords) %>% select(word, word_ind) %>% arrange(word_ind)
wordlist = as.character(df.words$word)
subjlist = unique(as.character(df.s2$subject))

# Mutate df.s1
# If doubling problem..
df.s1 = df.s1.raw %>% mutate(doubled = ifelse(is.na(lead(word)), FALSE, word == lead(word) & subject == lead(subject))) %>% filter(doubled == FALSE) %>%
  mutate(abs_value = abs(value), pos = value > 5, numChosen = 0)

for (subj in 1:nrow(df.demo)) {
  df.demo.temp = df.demo[subj, ]
  subj.name = df.demo.temp$subject
  s1.words = toupper(as.string.vector(as.character(df.demo.temp$tr_resp_correct)))
  s1.choices = as.numeric.vector(as.character(df.demo.temp$tr_choices))
  
  df.s1.trials = data.frame(s1.words, s1.choices) %>% group_by(s1.words) %>% summarize(numChosen = sum(s1.choices == 0))
  
  for (word in 1:nrow(df.s1.trials)) {
    df.s1$numChosen[which(as.character(df.s1$subject) == as.character(subj.name) & df.s1$word == df.s1.trials$s1.words[word])] = df.s1.trials$numChosen[word]
  }
}

df.s1.subj = df.s1 %>% group_by(subject) %>% summarize(cors = cor(value, numChosen)) %>% filter(!is.na(cors))

# Mutate df.s2
df.s2$choice = toupper(df.s2$choice)
df.s2$scratch = gsub("[.]", ",", toupper(as.character(df.s2$scratch)))
df.s2$all_values = as.character(df.s2$all_values)

#df.s2 = df.s2 %>%
#  mutate(choice_real = wordlist[amatch(choice, wordlist, maxDist = 2)],
#             choice_real_ind = getIndex(choice_real, wordlist),
#             s2_value = ifelse(is.na(choice_real_ind), 0, as.numeric.vector(all_values)[included_words][choice_real_ind]),
#             s2_subj_ind = as.numeric(subject), # don't use that ind for anything serious
#             doubled = ifelse(is.na(choice_real_ind), NA, ifelse(is.na(lead(choice_real_ind)), F, choice_real_ind == lead(choice_real_ind)) |
#                         ifelse(is.na(lag(choice_real_ind)), F, choice_real_ind == lag(choice_real_ind))),
#             bonus_value = ifelse(is.na(choice_real_ind), 0, ifelse(doubled, 0, s2_value)))

df.s2$rank_value = NULL
df.s2$num_ties = NULL
for (i in 1:nrow(df.s2)) {
  subj.name = df.s2$subject[i]
  c = df.s2$choice[i]
  creal = wordlist[amatch(c, wordlist, maxDist = 2)]
  cind = getIndex(creal, wordlist)
  all_vals = as.numeric.vector(df.s2$all_values[i])[included_words]
  all_vals_rank = rank(all_vals, ties.method = 'max')
  s2_val = ifelse(is.na(cind), 0, all_vals[cind])
  s1_rows = as.character(subj.name) == as.character(df.s1$subject) & as.character(c) == as.character(df.s1$word)
  
  df.s2$choice_real[i] = creal
  df.s2$choice_real_ind[i] = cind
  df.s2$s2_value[i] = s2_val
  df.s2$rank_value[i] = ifelse(is.na(cind), NA, all_vals_rank[cind])
  df.s2$num_ties[i] = ifelse(is.na(cind), NA, sum(s2_val == all_vals))
  df.s2$s1_value[i] = ifelse(is.na(cind), NA, df.s1$value[s1_rows])
  df.s2$s1_exposures[i] = ifelse(is.na(cind), NA, df.s1$exposures[s1_rows])
  df.s2$s1_chosen[i] = ifelse(is.na(cind), NA, df.s1$numChosen[s1_rows])
}

df.s2 = df.s2 %>% mutate(s2_subj_ind = as.numeric(subject), # don't use that ind for anything serious
                         doubled = ifelse(is.na(choice_real_ind), NA, ifelse(is.na(lead(choice_real_ind)), F, choice_real_ind == lead(choice_real_ind)) |
                                                  ifelse(is.na(lag(choice_real_ind)), F, choice_real_ind == lag(choice_real_ind))),
                         bonus_value = ifelse(is.na(choice_real_ind), 0, ifelse(doubled, 0, s2_value)),
                         pos = s1_value > 0,
                         abs_s1_value = abs(s1_value))

df.mem = df.s2 %>% filter(question == 'Memory')

df.s2.subj = df.s2 %>% filter(subject %in% df.demo$subject) %>% group_by(subject) %>%
  summarize(s2_bonus = sum(bonus_value), rt = mean(rt) / 1000,
            comp_check_pass = mean(comp_check_pass),
            numNAs = sum(is.na(choice_real)),
            numRepeats = sum(choice_real == lag(choice_real), na.rm = T))

## Compute recalled

# exclude if number of NAs in df.s2$choice_real > 2
# exclude if number of repeats > 1
exclude = NULL
exclude_names = NULL
include_names = NULL

# For memory condition
pctCorrect_words = NULL
pctCorrect_pts = NULL
recalled = matrix(F, nrow = nrow(df.mem), ncol = numWords)
recalled_ever = matrix(F, nrow = nrow(df.mem), ncol = numWords)
recalled_val = matrix(F, nrow = nrow(df.mem), ncol = numWords)
df.s1$recall = NULL

for (i in 1:nrow(df.mem)) {
  subj.name = df.mem$subject[i]
  df.s1.temp = df.s1 %>% filter(as.character(subj.name) == subject)
  df.s2.temp = df.s2 %>% filter(as.character(subj.name) == subject)
  
  words_temp = trimws(as.string.vector(df.mem$choice[i]))
  val_temp = as.numeric(trimws(as.string.vector(df.mem$scratch[i])))
  
  for (j in 1:numWords) {
    which_word = amatch(wordlist[j], words_temp, maxDist = 2, nomatch = 0)
    recalled[i,j] = which_word > 0
    
    if (recalled[i,j]) {
      true_val = df.s1.temp$value[df.s1.temp$word_ind  == (j - 1)]
      recalled_val[i,j] = abs(val_temp[which_word] - true_val) <= 2
    }
    df.s1$recall[as.character(df.s1$subject) == as.character(df.mem$subject[i]) & as.character(df.s1$word) == wordlist[j]] = recalled[i,j]
    
    recalled_ever[i,j] = recalled[i,j] | any(na.omit(df.s2.temp$choice_real_ind) == j)
  }
  
  resp_words = trimws(toupper(gsub("[[:punct:]]", "", strsplit(as.character(df.demo$tr_resp[i]), ",")[[1]])))
  resp_words_c = trimws(toupper(gsub("[[:punct:]]", "", strsplit(as.character(df.demo$tr_resp_correct[i]), ",")[[1]])))
  resp_pts = trimws(toupper(gsub("[[:punct:]]", "", strsplit(as.character(df.demo$tr_resp2[i]), ",")[[1]])))
  resp_pts_c = trimws(toupper(gsub("[[:punct:]]", "", strsplit(as.character(df.demo$tr_resp2_correct[i]), ",")[[1]])))
  pctCorrect_words[i] = mean(resp_words == resp_words_c)
  pctCorrect_pts[i] = mean(resp_pts == resp_pts_c)
  if (pctCorrect_words[i] < .75 || pctCorrect_pts[i] < .75 || df.s2.subj$comp_check_pass[i] <= .5 || df.s2.subj$numNAs[i] > 2 ||
      df.s2.subj$numRepeats[i] > 1) {
    exclude[i] = T
    exclude_names = c(exclude_names, as.character(subj.name))
  } else {
    exclude[i] = F
    include_names = c(include_names, as.character(subj.name))
  }
}
nrecall = rowSums(recalled[!exclude,])
nrecall_val = rowSums(recalled[!exclude,])
mean(nrecall)
mean(nrecall_val)
nrecall_bonus = rowSums(recalled & recalled_val)

df.s2.subj = df.s2.subj %>% mutate(mem_bonus = nrecall_bonus[df.mem$subject == subject] * pointsPerWord)

# Test what affected recall
m.recall = glmer(recall ~ value + numChosen + (0 + value + numChosen | subject) + (1 | subject) + (1 | word), data = df.s1[!exclude, ], family = binomial)
summary(m.recall)

# Graph relationship to recall
#df.s1.collapsed = df.s1 %>% group_by(exposures) %>% summarize(recall = mean(recall, na.rm = T))
#plot(df.s1.collapsed$recall ~ df.s1.collapsed$exposures)

## Check out df.s2 stats
hist(df.s2[df.s2$subject %in% include_names & df.s2$question_ind == 1, ]$rank_value, breaks = 15, main = "S2 ranks of words chosen in S2", xlab = "S2 rank")
mean(df.s2[df.s2$subject %in% include_names, ]$rank_value, na.rm = T)

hist(df.s2[df.s2$subject %in% include_names & df.s2$question_ind == 6, ]$s1_value, breaks = 15, main = "S1 values of words chosen in S2", xlab = "S1 value")
mean(df.s2[df.s2$subject %in% include_names, ]$s1_value, na.rm = T)

#hist(df.s2[!(df.s2$subject %in% exclude_names), ]$s1_exposures, breaks = 15)
#mean(df.s2[!(df.s2$subject %in% exclude_names), ]$s1_exposures, na.rm = T)

#hist(df.s2[!(df.s2$subject %in% exclude_names), ]$abs_s1_value, breaks = 15)
#mean(df.s2[!(df.s2$subject %in% exclude_names), ]$abs_s1_value, na.rm = T)

hist(df.s1[df.s1$subject %in% include_names, ]$numChosen, breaks = 15)
mean(df.s1[df.s1$subject %in% include_names, ]$numChosen, na.rm = T)
hist(df.s2[!(df.s2$subject %in% exclude_names), ]$s1_chosen, breaks = 15)
mean(df.s2[!(df.s2$subject %in% exclude_names), ]$s1_chosen, na.rm = T)

## Get bonuses
df.demo = df.demo %>% mutate(s2_bonus = I(df.s2.subj$s2_bonus), mem_bonus = I(df.s2.subj$mem_bonus),
                             bonus = round((s1_bonus + s2_bonus + mem_bonus) / (pointsPerCent * 100), 2))
write.table(df.demo %>% filter(id >= 150) %>% select(WorkerID = subject, Bonus = bonus),
          paste0(path, 'Bonuses - cs_wg_v3_pilot1.csv'), row.names = FALSE, col.names = FALSE, sep = ",")

## Prepare for mlogit
numRealQuestions = numQuestions - 1
df.logit = data.frame(Subj = NULL, Trial = NULL, OptionID = NULL, Choice = NULL, MFval = NULL, MBval = NULL, nExposures = NULL, Recalled = NULL)

for (subj in 1:nrow(df.demo)) {
  subj.name = df.demo$subject[subj]
  #recalled.temp = recalled_ever[subj, ]
  recalled.temp = !logical(numWords)
  num.recalled.temp = sum(recalled.temp)
  df.s1.temp = df.s1 %>% filter(as.character(subj.name) == subject) %>% arrange(word_ind)
  #df.s1.temp = df.s1.temp[!duplicated(df.s1.temp$word), ]
  df.s2.temp = df.s2 %>% filter(as.character(subj.name) == subject) %>% arrange(question_order)
  nAnswered = sum(!is.na(df.s2.temp$choice_real_ind))

  if (nAnswered > 0 & subj.name %in% include_names) {
    Subj.col = rep(subj, num.recalled.temp * nAnswered)
    
    MFval.col = rep(df.s1.temp$value[recalled.temp], nAnswered)
    nExposures.col = rep(df.s1.temp$exposures[recalled.temp], nAnswered)
    Recalled.col = rep(df.s1.temp$recall[recalled.temp] * 1, nAnswered)
    numChosen.col = rep(df.s1.temp$numChosen[recalled.temp], nAnswered)
    OptionID.col = rep(which(recalled.temp), nAnswered)
    Trial.col = rep(1:nAnswered, each = num.recalled.temp)
    
    temp.mbval = matrix(0, nrow = nAnswered, ncol = num.recalled.temp)
    temp.choice = matrix(0, nrow = nAnswered, ncol = num.recalled.temp)
    ind = 1
    for (q in 1:numRealQuestions) {
      if (!is.na(df.s2.temp$choice_real_ind[q])) {
        mbvals = rank(as.numeric.vector(df.s2.temp$all_values[q])[included_words], ties.method = 'max')
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
  df = df %>% mutate(MFcent = nChosen - mean(nChosen), MBcent = MBval - mean(MBval), Int = MFcent * MBcent)
  df.m = mlogit.data(df, choice = "Choice", shape = "long", id.var = "Subj", alt.var = "OptionID", chid.var = "Trial_unique")
  
  m = mlogit(Choice ~ MFcent + MBcent + Int | -1, df.m, panel = T,
             rpar = c(MFcent = "n", MBcent = "n", Int = "n"), correlation = F, halton = NA, R = 1000, tol = .001)
  return(m)
}

m.real = runLogit(df.logit)
summary(m.real)

## Save for modeling
rewards_tr = matrix(0, nrow = sum(!exclude), ncol = numWords)
exposures_tr = matrix(0, nrow = sum(!exclude), ncol = numWords)
chosen_tr = matrix(0, nrow = sum(!exclude), ncol = numWords)
ind = 1
for (subj in 1:nrow(df.demo)) {
  if (!exclude[subj]) {
    subj.name = df.demo$subject[subj]
    df.s1.temp = df.s1 %>% filter(as.character(subj.name) == subject) %>% arrange(word_ind)
    
    for (word in 1:numWords) {
      rewards_tr[ind, word] = df.s1.temp$value[word]
      exposures_tr[ind, word] = df.s1.temp$exposures[word]
      chosen_tr[ind, word] = df.s1.temp$numChosen[word]
    }
    ind = ind + 1
  }
}

write.csv(exposures_tr, paste0(path, 'exposures_tr.csv'), row.names = F)
write.csv(chosen_tr, paste0(path, 'chosen_tr.csv'), row.names = F)
write.csv(rewards_tr, paste0(path, 'rewards_tr.csv'), row.names = F)
write.csv(recalled_ever[!exclude, ] * 1, paste0(path, 'recalled.csv'), row.names = F)

df.modeling = df.s2 %>% filter(!(subject %in% exclude_names) & !is.na(choice_real_ind) & subject %in% df.demo$subject) %>%
  mutate(all_values_nocomma = gsub(",", " ", all_values)) %>% 
  select(s2_subj_ind, choice_real_ind, all_values_nocomma)
write.table(df.modeling, paste0(path, 'choices.csv'), row.names = F, col.names = F, sep=",");

## Graph
df.rank = df.s2 %>% filter(subject %in% include_names & !is.na(rank_value)) %>%
  mutate(pos = s1_value > 5) %>%
  group_by(rank_value, pos) %>%
  summarize(numChosen_S2 = n(), numOpps_S2 = mean(num_ties), prob_rank = numChosen_S2 / numOpps_S2)
#df.rank$prob_rank[df.rank$five == FALSE] = df.rank$prob_rank[df.rank$five == FALSE] / 4

base = ggplot(data = df.rank, aes(x = rank_value, y = prob_rank, group = pos, colour = pos)) +
  geom_point()
base
base + geom_smooth(method = 'lm', formula = y ~ exp(1 * x))
base + geom_smooth(method = 'lm', formula = y ~ choose(x - 1, 10))
#base + stat_function(fun = function(x) {.0091 * exp(.1 * x) - .0266}) +
#  stat_function(fun = function(x) {7.22e-06 * choose(x - 1, 4) - 6.447e-04})

for (j in 1:nrow(df.demo)) {
  subj = df.demo$subject[j]
  vals = df.s1$pos[as.character(df.s1$subject) == as.character(subj)]
  choices = as.numeric.vector(as.character(df.demo$tr_choices[j]))
  words = as.string.vector(as.character(df.demo$tr_resp_correct[j]))
  word_vals = vals[amatch(toupper(words), wordlist, maxDist = 2)]
  choices[word_vals]
}

### SIMULATIONS

df.sim = read.csv('simulations/results/wg_v2/mixture-mf-mb.csv')
m.sim = runLogit(df.sim)
summary(m.sim)









