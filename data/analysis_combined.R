# preliminaries -----------------------------------------------------------

require(dplyr)
require(ggplot2)
require(lme4)
require(lmerTest)
require(mlogit)
require(stringdist)
require(ez)
require(lattice)

theme_update(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(colour = "black"),
             axis.text=element_text(size=20, colour = "black"), axis.title=element_text(size=18, face = "bold"), axis.title.x = element_text(vjust = 0),
             legend.title = element_text(size = 24, face = "bold"), legend.text = element_text(size = 20), plot.title = element_text(size = 26, face = "bold", vjust = 1))

setwd("~/Me/Psychology/Projects/choicesets/with_sam")

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


se = function(x) {return(sd(x) / sqrt(length(x)))}
dodge <- position_dodge(width=0.9)



# get data ----------------------------------------------------------------


numWords = 14; # words in stage 1
pointsPerCent = 10;
nTrials = 112;
path = 'data/cs_wg_v10/real2/'

# Load data
df.demo = read.csv(paste0(path, 'demo.csv'), stringsAsFactors = F) %>% arrange(subject) %>% mutate(total_time_real = total_time / 60000)
df.words.raw = read.csv(paste0(path, 'words.csv'), stringsAsFactors = F) %>% arrange(subject, word_ind)
df.s1.raw = read.csv(paste0(path, 's1.csv'), stringsAsFactors = F) %>% arrange(subject)
df.s2.raw = read.csv(paste0(path, 's2.csv'), stringsAsFactors = F) %>% arrange(subject)
df.poss.raw = read.csv(paste0(path, 'poss.csv'), stringsAsFactors = F) %>% arrange(subject)

subjlist = df.demo$subject

#df.words.subj = df.words.raw %>% group_by(subject) %>% summarize(num = n()) %>% filter(num == 14)
#subjlist = intersect(df.demo$subject, unique(df.words.subj$subject))

## words

df.words = df.words.raw %>% mutate(doubled = ifelse(is.na(lead(word)), FALSE, word == lead(word) & subject == lead(subject))) %>%
  filter(doubled == FALSE & subject %in% subjlist) %>%
  mutate(s1_val_high = value > 5, numChosen = 0)

## s1

df.s1 = df.s1.raw %>% filter(subject %in% subjlist) %>%
  mutate(correct_word = ain(toupper(resp), word, maxDist = 2), correct_val = resp2 == value, word_chosen = ifelse(choice, alt, word),
         s1_val_high = value > 5)

for (i in 1:nrow(df.s1)) {
  subj = df.s1$subject[i]
  word = df.s1$word[i]
  alt = df.s1$alt[i]
  choice = df.s1$choice[i]
  df.s1$word_value[i] = df.words$value[df.words$subject == subj & df.words$word == word]
  df.s1$alt_value[i] = alt
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
            pctHighVal = mean(s1_val_high), pctChoseBetter = mean(chose_better))

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
                         s1_val_high = s1_value > 5)

df.mem = df.s2 %>% filter(question == 'Memory')

df.s2.subj = df.s2 %>% filter(subject %in% df.demo$subject) %>%
  group_by(subject) %>%
  summarize(s2_bonus = sum(bonus_value), rt = mean(rt) / 1000,
            comp_check_pass = mean(comp_check_pass),
            comp_check_rt = mean(comp_check_rt) / 1000,
            numNAs = sum(is.na(choice_real)),
            numRepeats = sum(choice_real == lag(choice_real), na.rm = T),
            s1_val_high = mean(s1_val_high, na.rm = T),
            s2_val = mean(rank_value, na.rm = T))

## poss
df.poss = df.poss.raw %>% filter(subject %in% subjlist)
df.poss.prac = df.poss %>% filter(practice == 1)
df.poss = df.poss %>% filter(practice == 0)

df.poss$prompt = toupper(df.poss$prompt)

for (i in 1:nrow(df.poss)) {
  subj.name = df.poss$subject[i]
  wordlist = (df.words %>% filter(subject == subj.name))$word
  c = df.poss$prompt[i]
  creal = wordlist[amatch(c, wordlist, maxDist = 2)]
  cind = getIndex(creal, wordlist)
  word_rows = subj.name == df.words$subject & creal == df.words$word
  s1_val = ifelse(is.na(cind), -1, df.words$value[word_rows])
  
  df.poss$s1_value[i] = s1_val
  df.poss$s1_exposures[i] = ifelse(is.na(cind) | s1_val == -1, NA, df.words$exposures[word_rows])
  df.poss$s1_chosen[i] = ifelse(is.na(cind) | s1_val == -1, NA, df.words$numChosen[word_rows])
}

df.poss$s1_value = factor(df.poss$s1_value, levels = c(-1, 5, 0, 1, 9, 10), labels = c('absent', 'grey', 'low1', 'low2', 'high1', 'high2'))
levels(df.poss$s1_value) = list(low=c('low1','low2'), high=c('high1','high2'), absent='absent')

df.poss.prac.subj = df.poss.prac %>% group_by(subject) %>%
  summarize(correct = mean(correct))
df.poss.subj = df.poss %>% group_by(subject) %>%
  summarize(pctNA = mean(choice == -1), numCompleted = n())

df.poss.collapsed = df.poss %>%
  filter(choice != -1) %>%
  mutate(choice = ifelse(choice == 1, 0, 1)) %>%
  group_by(subject, cond, s1_value) %>%
  summarize(choice = mean(choice))

df.poss.bycond = df.poss.collapsed %>% group_by(subject) %>% summarize(numTypes = n())

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
include_rows = NULL
include_names = NULL

for (subj in 1:nrow(df.demo)) {
  subj.name = df.demo$subject[subj]
  df.s1.subj.temp = df.s1.subj %>% filter(subject == subj.name)
  df.s2.subj.temp = df.s2.subj %>% filter(subject == subj.name)
  df.poss.subj.temp = df.poss.subj %>% filter(subject == subj.name)
  df.poss.bycond.temp = df.poss.bycond %>% filter(subject == subj.name)
  
  if (df.s1.subj.temp$pctCorrect_words < .75 || df.s1.subj.temp$pctChoseBetter < .6 || df.s1.subj.temp$numTrials != nTrials || df.demo$write_down[subj] == 'Yes' ||
      df.s2.subj.temp$comp_check_pass < .5 || df.s2.subj.temp$numRepeats > 2 || sum(recalled[subj,]) < 5 || df.s2.subj.temp$numNAs > 4 || df.poss.subj.temp$numCompleted != 21 || df.poss.bycond.temp$numTypes != 3) {
    include_rows[subj] = FALSE
  } else {
    include_rows[subj] = TRUE
    include_names = c(include_names, subj.name)
  }
}


# check out data ----------------------------------------------------------
df.poss.filt = df.poss %>% filter(subject %in% include_names)
df.poss.collapsed.filt = df.poss.collapsed %>% filter(subject %in% include_names)
df.s2.filt = df.s2 %>% filter(subject %in% include_names)
df.s1.filt = df.s1 %>% filter(subject %in% include_names)
df.words.filt = df.words %>% filter(subject %in% include_names)

# what affected recall?
nrecall = rowSums(recalled[include_rows,])
mean(nrecall)

m.recall = glmer(recall ~ s1_val_high + (1 + s1_val_high | subject) + (1 | word),
                 data = df.words.filt, family = binomial)
summary(m.recall)

# possibility judgments
df.poss.graph = df.poss.collapsed.filt %>% group_by(cond, s1_value) %>%
  summarize(choice.mean = mean(choice), choice.se = se(choice))

ggplot(df.poss.graph, aes(x = s1_value, y = choice.mean, group = cond, fill = cond)) +
  geom_bar(stat = "identity", position = dodge) +
  geom_errorbar(aes(ymax = choice.mean + choice.se, ymin = choice.mean - choice.se), width = .5, position = dodge) +
  xlab('') + ylab('')

df.poss.subj.filt = df.poss.collapsed.filt %>% group_by(subject, cond) %>% summarize(val = choice[s1_value == 'low'] - choice[s1_value == 'high'])
histogram(~ df.poss.subj.filt$val | df.poss.subj.filt$cond)

ezANOVA(data.frame(df.poss.collapsed.filt %>% filter(s1_value %in% c('low', 'high'))), choice, wid = subject, within = s1_value, between = cond)
summary(lmer(choice ~ s1_value * cond + (1 | subject), data = df.poss.collapsed %>% filter(s1_value %in% c('low', 'high'))))

# decisions
hist(df.s2.filt$rank_value, breaks = 15, main = "S2 ranks of words chosen in S2", xlab = "S2 rank")
mean(df.s2.filt$rank_value, na.rm = T)

hist(as.numeric(df.s2.filt$s1_val_high), breaks = 15, main = "S1 values of words chosen in S2", xlab = "S1 value")
mean(df.s2.filt$s1_val_high, na.rm = T)

df.s2.subj.filt = df.s2.subj %>% filter(subject %in% include_names)

ggplot(df.s2.subj.filt, aes(x = s1_val_high)) + geom_histogram(col = 'black', fill = 'blue')
ggplot(df.s2.subj.filt, aes(x = s2_val)) + geom_histogram(col = 'black', fill = 'blue') + xlim(c(1,14))
t.test(df.s2.subj.filt$s1_val_high - .5)
t.test(df.s2.subj.filt$s2_val - 7)

# correlation
numGoodSubj = length(include_names)
df.cor = data.frame(poss_measure = numeric(numGoodSubj), s2_measure = numeric(numGoodSubj), time_cond = factor(numGoodSubj))
df.cor$poss_measure = df.poss.subj.filt$val
df.cor$s2_measure = df.s2.subj.filt$s1_val_high
df.cor$time_cond = df.poss.subj.filt$cond

m.cor = lm(s2_measure ~ poss_measure * time_cond, data = df.cor)
summary(m.cor)

df.cor.pressure = df.cor %>% filter(time_cond == 'pressure')
cor.test(df.cor.pressure$s2_measure, df.cor.pressure$poss_measure)


# order
histogram(~ order | s1_val_high, df.words.filt[df.words.filt$recall == T, ])
m.order = lmer(order ~ s1_val_high + (1 | subject) + (0 + s1_val_high | subject) + (s1_val_high | word),
               data = df.words.filt[df.words.filt$recall == T, ])
summary(m.order)

coefs = coef(m.order)$subject$s1_val_highTRUE
cor.test(coefs, df.cor$s2_measure) # people who chose high s1 words more also put high-val words earlier
cor.test(coefs, df.cor$poss_measure) # people who said low-val words were more impossible more also put high-val words earlier

#weights_v8 = c(6.01e-01,2.56e-01,3.72e-01,3.10e-01,2.10e-01,5.41e-08,1.08e-08,3.03e-01,2.15e-01,1.49e-08,2.65e-08,2.17e-06,1.88e-01,1.13e-08,4.72e-08,4.62e-02,1.62e-01,1.60e-01,7.77e-02,2.49e-01,1.87e-08,1.33e-01,1.80e-08,1.21e-01,4.56e-01,1.24e-07,2.04e-01,1.30e-07,2.03e-03,6.07e-01,4.46e-07,7.41e-01,1.21e-01,2.68e-07,1.84e-01,9.94e-01,5.68e-01,7.53e-01,4.95e-07,9.45e-08,6.10e-08,2.74e-08,1.23e-07,5.15e-08,3.44e-01,5.73e-07,5.88e-01,2.65e-01,3.24e-08,7.84e-01,2.13e-01,9.03e-08,7.08e-01,7.47e-01,2.30e-01,4.69e-01,3.20e-01,1.37e-07,1.94e-07,2.50e-08,5.05e-01,1.77e-01,1.05e-07,2.09e-01,3.54e-08,01,4.47e-01,4.35e-01,4.30e-08,3.83e-01,6.80e-01,4.75e-01,1.55e-01,3.82e-01,4.46e-01,5.44e-01,01,4.42e-06,3.37e-01,5.01e-01,3.51e-01,1.74e-01,1.85e-01,1.15e-05,1.21e-01,8.85e-08,1.50e-07,2.03e-01,3.29e-01,3.33e-01,3.48e-01,2.89e-07,2.44e-01,01,9.50e-09,9.14e-08,4.96e-02,3.68e-01,4.69e-01,1.79e-06,6.75e-01,2.18e-01,1.21e-01,7.06e-01,2.13e-01,2.49e-01,7.83e-01,2.85e-01,4.43e-01,5.93e-08,6.45e-01,2.86e-07,1.60e-08,5.30e-02,1.83e-07,4.96e-01,2.09e-07,4.51e-08,3.51e-01,4.04e-01,3.19e-01,5.59e-01,5.82e-01,5.40e-07,3.97e-08,7.36e-03,5.21e-08,2.93e-01,2.78e-07,5.07e-01,1.45e-01,1.14e-06,01,4.02e-01,6.53e-09,2.57e-01,1.64e-01,1.99e-01,1.97e-07,2.61e-07,01,2.04e-07,9.94e-02,3.17e-08,2.38e-08,3.29e-01,3.22e-01,2.42e-01,1.61e-07,1.19e-07,1.61e-07,2.81e-01,5.76e-08,2.34e-01,3.25e-08,2.10e-02,3.79e-01,3.51e-01,5.01e-01,6.04e-08,1.23e-07,7.24e-01,2.11e-01,7.10e-08,1.00e+00,2.66e-01,1.76e-06,1.65e-01,2.99e-07,9.67e-01,1.94e-07,2.58e-02,2.69e-07,1.68e-07,4.15e-01,2.44e-01,7.50e-08,1.60e-07,2.75e-01,1.03e-02,1.44e-01,7.14e-01,3.41e-02,3.07e-07)
#cor.test(coefs, weights_v8)

# clean-up ----------------------------------------------------------------



# Get bonuses
df.demo = df.demo %>% mutate(bonus = round(s1_bonus / (pointsPerCent * 100), 2))
write.table(df.demo %>% select(WorkerID = subject, Bonus = bonus),
            paste0(path, 'Bonuses - cs_wg_v10_real2.csv'), row.names = FALSE, col.names = FALSE, sep = ",")

# Save analysis
save.image(paste0(path, 'analysis.rdata'))

# Save for modeling
rewards_tr = matrix(0, nrow = sum(include_rows), ncol = numWords)
ind = 1
for (subj in 1:nrow(df.demo)) {
  subj.name = df.demo$subject[subj]
  
  if (subj.name %in% include_names) {
    df.words.temp = df.words %>% filter(subject == subj.name)
    
    for (word in 1:numWords) {
      rewards_tr[ind, word] = df.words.temp$value[word]
    }
    ind = ind + 1
  }
}

write.csv(rewards_tr, paste0(path, 'rewards_tr.csv'), row.names = F)
write.csv(recalled_ever[include_rows, ] * 1, paste0(path, 'recalled.csv'), row.names = F)

df.modeling = df.s2 %>% filter(subject %in% include_names & !is.na(choice_real_ind)) %>%
  mutate(all_values_nocomma = gsub(",", " ", all_values)) %>% 
  select(s2_subj_ind, choice_real_ind, all_values_nocomma)
write.table(df.modeling, paste0(path, 'choices.csv'), row.names = F, col.names = F, sep=",")