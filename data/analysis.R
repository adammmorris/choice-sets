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

setwd("~/Me/Psychology/Projects/choicesets/git")

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


# import data -------------------------------------------------------------

versions = c('value1', 'value2', 'freq', 'confounded', 'stripped')
version = versions[2]

if (version == 'value1') {
  numWords = 14;
  numTrials = 112;
  minNAs = 4;
  path = 'data/value/v1/real2/'
  pointsPerCent = 10;
  pointsPerWord = 10; # for memory condition
  type = 0; # 0 is value, 1 is freq
} else if (version == 'value2') {
  numWords = 14;
  numTrials = 112;
  minNAs = 2;
  path = 'data/value/v2/real1/'
  pointsPerCent = 10;
  pointsPerWord = 10; # for memory condition
  type = 0;
} else if (version == 'freq') {
  numWords = 14;
  numTrials = 112;
  minNAs = 4;  
  path = 'data/frequency/v1/real1/'
  type = 1;
} else if (version == 'confounded') {
  numWords = 14;
  numTrials = 91;
  minNAs = 4;  
  path = 'data/confounded/v1/real1/'
  type = 0;
} else if (version == 'stripped') {
  numWords = 14;
  numTrials = 0;
  minNAs = 1;  
  path = 'data/value/v3/real2/'
  type = 2;
}


# Load data
df.demo = read.csv(paste0(path, 'demo.csv'), stringsAsFactors = F) %>% arrange(subject) %>% mutate(total_time_real = total_time / 60000)
df.words.raw = read.csv(paste0(path, 'words.csv'), stringsAsFactors = F) %>% arrange(subject, word_ind)
if (type != 2) {
  df.s1.raw = read.csv(paste0(path, 's1.csv'), stringsAsFactors = F) %>% arrange(subject);
} else {
  df.s1.raw = data.frame(subject = numeric(), resp = numeric(), word = numeric(), resp2 = numeric(), value = numeric(), alt = numeric(),
                         choice = numeric());
}
df.s2.raw = read.csv(paste0(path, 's2.csv'), stringsAsFactors = F) %>% arrange(subject, question_order)

subjlist = df.demo$subject

## words
df.words = df.words.raw %>%
  mutate(doubled = ifelse(is.na(lead(word)), FALSE, word == lead(word) & subject == lead(subject))) %>%
  filter(doubled == FALSE & subject %in% subjlist)

for (i in 1:nrow(df.words)) {
  df.words$high_value[i] = ifelse(type == 1, df.words$exposures[i] > 8, df.words$value[i] > 5)
  if (type == 2) { # stripped-down version
    valuelist = (df.words %>% filter(subject == df.words$subject[i]))$value
    df.words$high_value[i] = df.words$value[i] > median(valuelist)
  }
}

## s1
df.s1 = df.s1.raw %>% filter(subject %in% subjlist) %>%
  mutate(correct_word = ain(toupper(resp), word, maxDist = 2), correct_val = resp2 == value, word_chosen = ifelse(choice, alt, word))
df.s1.subj = df.s1 %>% group_by(subject) %>%
  summarize(pctCorrect_words = mean(correct_word, na.rm = T), pctCorrect_val = ifelse(type == 1, 1, mean(correct_val, na.rm = T)),
            numTrials = n())

## s2
df.s2 = df.s2.raw %>% filter(subject %in% subjlist)
df.s2$choice = toupper(df.s2$choice)
df.s2$scratch = gsub("[.]", ",", toupper(as.character(df.s2$scratch)))
df.s2$all_values = as.character(df.s2$all_values)

df.s2$rank_value = NULL
for (i in 1:nrow(df.s2)) {
  subj.name = df.s2$subject[i]
  wordlist = (df.words %>% filter(subject == subj.name))$word
  c = df.s2$choice[i]
  creal = wordlist[amatch(c, wordlist, maxDist = 2)]
  cind = getIndex(creal, wordlist)
  
  all_vals = as.numeric.vector(df.s2$all_values[i])
  all_vals_rank = rank(all_vals, ties.method = 'max')
  s2_val = ifelse(is.na(cind), NA, all_vals[cind])
  word_rows = subj.name == df.words$subject & creal == df.words$word
  
  df.s2$choice_real[i] = creal
  df.s2$choice_real_ind[i] = cind
  df.s2$s2_value[i] = s2_val
  df.s2$rank_value[i] = ifelse(is.na(cind), NA, all_vals_rank[cind])
  df.s2$s1_value[i] = ifelse(is.na(cind), NA, df.words$value[word_rows])
  df.s2$s1_exposures[i] = ifelse(is.na(cind), NA, df.words$exposures[word_rows])
  df.s2$high_value[i] = ifelse(type == 1, df.s2$s1_exposures[i] > 8, df.s2$s1_value[i] > 5)
}

df.s2 = df.s2 %>% mutate(s2_subj_ind = as.numeric(as.factor(subject)), # just for modeling
                         doubled = ifelse(is.na(choice_real_ind), NA, ifelse(is.na(lead(choice_real_ind)), F, choice_real_ind == lead(choice_real_ind)) |
                                                  ifelse(is.na(lag(choice_real_ind)), F, choice_real_ind == lag(choice_real_ind))),
                         bonus_value = ifelse(is.na(choice_real_ind), 0, ifelse(doubled, 0, s2_value)))

df.s2.subj = df.s2 %>% filter(subject %in% df.demo$subject) %>%
  group_by(subject) %>%
  summarize(s2_bonus = sum(bonus_value), rt = mean(rt) / 1000,
            comp_check_pass = mean(comp_check_pass),
            comp_check_rt = mean(comp_check_rt) / 1000,
            numNAs = sum(is.na(choice_real)),
            numRepeats = sum(choice_real == lag(choice_real), na.rm = T),
            high_value = mean(high_value, na.rm = T),
            rank_value = mean(rank_value, na.rm = T))

df.s2.subj$mem_words = NULL
df.s2.subj$mem_vals = NULL
for (i in 1:nrow(df.s2.subj)) {
  s2.filt = df.s2 %>% filter(subject == df.s2.subj$subject[i] & question == 'Memory')
  df.s2.subj$mem_words[i] = ifelse(length(s2.filt$choice) == 0, NA, s2.filt$choice)
  df.s2.subj$mem_vals[i] = ifelse(length(s2.filt$scratch) == 0, NA, s2.filt$scratch)
}


## Compute recalled
recalled = matrix(F, nrow = nrow(df.s2.subj), ncol = numWords)
recalled_ever = matrix(F, nrow = nrow(df.s2.subj), ncol = numWords)
recalled_val = matrix(F, nrow = nrow(df.s2.subj), ncol = numWords)
df.words$recall = NULL
df.words$recall.ever = NULL
df.words$order = NULL

for (i in 1:nrow(df.s2.subj)) {
  subj.name = df.s2.subj$subject[i]
  df.words.temp = df.words %>% filter(subject == subj.name)
  df.s2.temp = df.s2 %>% filter(subject == subj.name)
  
  words_temp = trimws(as.string.vector(df.s2.subj$mem_words[i]))
  val_temp = as.numeric(trimws(as.string.vector(df.s2.subj$mem_vals[i])))
  
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


# exclusion ---------------------------------------------------------------

# Exclude if any of these: cor in s1 < .75, comp_check_pass < .5, pctCorrect_words < .75, pctCorrect_pts < .75, numNAs > 3, numRepeats > 2, numRecalled < 5
include_rows = NULL
include_names = NULL

for (subj in 1:length(subjlist)) {
  subj.name = subjlist[subj]
  df.s1.subj.temp = df.s1.subj %>% filter(subject == subj.name)
  df.s2.subj.temp = df.s2.subj %>% filter(subject == subj.name)
  df.demo.temp = df.demo %>% filter(subject == subj.name)

  exclude = df.demo.temp$write_down == 'Yes' || df.s2.subj.temp$comp_check_pass < .5 || df.s2.subj.temp$numRepeats > 2 ||
    df.s2.subj.temp$numNAs > minNAs || df.s2.subj.temp$numNAs > minNAs || sum(recalled[subj,]) < 5
  if (type != 2) {
    exclude = exclude || df.s1.subj.temp$numTrials != numTrials || df.s1.subj.temp$pctCorrect_words < .75 ||
      df.s1.subj.temp$pctCorrect_val < .75
  }
  if (exclude) {
    include_rows[subj] = FALSE
  } else {
    include_rows[subj] = TRUE
    include_names = c(include_names, subj.name)
  }
}

# check out data ----------------------------------------------------------

## stage 2 choices
df.s2.filt = df.s2 %>% filter(subject %in% include_names)
df.s2.subj.filt = df.s2.subj %>% filter(subject %in% include_names)

# s2 rank value
hist(df.s2.filt$rank_value, breaks = 15, main = "S2 ranks of words chosen in S2", xlab = "S2 rank")
mean(df.s2.filt$rank_value, na.rm = T)
ggplot(df.s2.subj.filt, aes(x = rank_value)) + geom_histogram(col = 'black', fill = 'blue') + xlim(c(1,14))
t.test(df.s2.subj.filt$rank_value - 7)

# s1 high value
hist(as.numeric(df.s2.filt$high_value), breaks = 15, main = "S1 values of words chosen in S2", xlab = "S1 value")
mean(df.s2.filt$high_value, na.rm = T)
ggplot(df.s2.subj.filt, aes(x = high_value)) + geom_histogram(col = 'black', fill = 'blue')
t.test(df.s2.subj.filt$high_value - .5)

## recall
nrecall = rowSums(recalled[include_rows,])
mean(nrecall)

# plot split by value
df.words.byvalue = df.words %>% filter(subject %in% include_names) %>% group_by(high_value, subject) %>% summarize(recall = mean(recall, na.rm = T)) %>%
  group_by(high_value) %>% summarize(recall.mean = mean(recall, na.rm = T), recall.se = se(recall))
ggplot(df.words.byvalue, aes(x = high_value, y = recall.mean)) +
  geom_bar(stat = "identity", position = dodge) +
  geom_errorbar(aes(ymax = recall.mean + recall.se, ymin = recall.mean - recall.se), width = .5, position = dodge) +
  xlab('') + ylab('') + guides(fill = F)

# did value influence recall?
m.recall = glmer(recall ~ high_value + (0 + high_value | subject) + (1 | subject) + (1 | word),
                 data = df.words %>% filter(subject %in% include_names), family = binomial)
summary(m.recall)

## order effects
df.words.filt = df.words %>% filter(subject %in% include_names)
histogram(~ order | value, df.words[df.words$subject %in% include_names & df.words$recall == T, ])
m.order = lmer(order ~ high_value + (high_value | subject) + (high_value | word),
                 data = df.words.filt[df.words.filt$recall == T, ])
summary(m.order)

# bonuses, modeling -----------------------------------------------------------------

## save for modeling

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

write.csv(rewards_tr, paste0(path, 'rewards_s1.csv'), row.names = F)
write.csv(recalled_ever[include_rows, ] * 1, paste0(path, 'recalled.csv'), row.names = F)

df.modeling = df.s2 %>% filter(subject %in% include_names & !is.na(choice_real_ind)) %>%
  mutate(all_values_nocomma = gsub(",", " ", all_values)) %>% 
  select(s2_subj_ind, choice_real_ind, all_values_nocomma)
write.table(df.modeling, paste0(path, 'choices.csv'), row.names = F, col.names = F, sep=",")

## bonuses
nrecall_bonus = rowSums(recalled & recalled_val)
df.s2.subj = df.s2.subj %>% mutate(mem_bonus = nrecall_bonus * pointsPerWord)
df.demo = df.demo %>% mutate(s2_bonus = I(df.s2.subj$s2_bonus), mem_bonus = I(df.s2.subj$mem_bonus),
                             bonus = round((s1_bonus + s2_bonus + mem_bonus) / (pointsPerCent * 100), 2))
write.table(df.demo %>% filter(id >= 150) %>% select(WorkerID = subject, Bonus = bonus),
            paste0(path, 'Bonuses.csv'), row.names = FALSE, col.names = FALSE, sep = ",")

## save
save.image(paste0(path, 'analysis.rdata'))