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

runLogit = function(df) {
  df$Choice = as.logical(df$Choice)
  df = df %>% mutate(Trial_unique = paste(Subj, Trial, sep="_"))
  df.m = mlogit.data(df, choice = "Choice", shape = "long", id.var = "Subj", alt.var = "OptionID", chid.var = "Trial_unique")
  
  m = mlogit(Choice ~ MFcent + MBcent + Int | -1, df.m, panel = T,
             rpar = c(MFcent = "n", MBcent = "n", Int = "n"), correlation = F, halton = NA, R = 1000, tol = .001)
  return(m)
}

se = function(x) {return(sd(x, na.rm = T) / sqrt(length(x)))}
dodge <- position_dodge(width=0.9)


# import data -------------------------------------------------------------

versions = c('value1', 'value2', 'freq', 'confounded', 'stripped')
version = versions[5]

if (version == 'value1') {
  numWords = 14;
  numTrials = 112;
  minNAs = 4;
  path = 'data/value/v1/real2/'
  pointsPerCent = 10;
  pointsPerWord = 10; # for memory condition
  numRealQuestions = 9
  type = 0; # 0 is value, 1 is freq, 2 is stripped
} else if (version == 'value2') {
  numWords = 14;
  numTrials = 112;
  minNAs = 2;
  path = 'data/value/v2/real1/'
  pointsPerCent = 10;
  pointsPerWord = 10; # for memory condition
  numRealQuestions = 5
  type = 0;
} else if (version == 'freq') {
  numWords = 14;
  numTrials = 112;
  minNAs = 4;  
  path = 'data/frequency/v1/real1/'
  numRealQuestions = 9
  type = 1;
} else if (version == 'confounded') {
  numWords = 14;
  numTrials = 91;
  minNAs = 4;  
  path = 'data/confounded/v1/real1/'
  numRealQuestions = 9
  type = 0;
} else if (version == 'stripped') {
  numWords = 14;
  numTrials = 0;
  minNAs = 1;
  path = 'data/value/v3/real3/'
  type = 2;
  numRealQuestions = 1;
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
    df.words$high_value[i] = ifelse(df.words$value[i] == median(valuelist), as.logical(runif(1) > .5), df.words$value[i] > median(valuelist))
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
  df.s2$high_value[i] = ifelse(is.na(cind), NA, df.words$high_value[word_rows])#ifelse(type == 1, df.s2$s1_exposures[i] > 8, df.s2$s1_value[i] > 5)
  df.s2$high_rank[i] = ifelse(is.na(cind), NA, df.s2$rank_value[i] > 7)
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
            s1_value = mean(s1_value, na.rm = T),
            high_value = mean(high_value, na.rm = T),
            rank_value = mean(rank_value, na.rm = T),
            high_rank = mean(high_rank, na.rm = T))

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
    df.s2.subj.temp$numNAs > minNAs || sum(recalled[subj,]) < 5
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


if (type == 2) {
  df.s2 = df.s2 %>% filter(question_order > 0)
  df.s2.subj = df.s2 %>%
    group_by(subject) %>%
    summarize(s1_value = mean(s1_value, na.rm = T),
              high_value = mean(high_value, na.rm = T),
              rank_value = mean(rank_value, na.rm = T),
              high_rank = mean(high_rank, na.rm = T))
}


## stage 2 choices
df.s2.filt = df.s2 %>% filter(subject %in% include_names)
df.s2.subj.filt = df.s2.subj %>% filter(subject %in% include_names)

# s2 rank value
ggplot(df.s2.subj.filt, aes(x = rank_value)) + geom_histogram(col = 'black', fill = 'blue') + xlim(c(1,14))
t.test(df.s2.subj.filt$rank_value - 7)
ggplot(df.s2.subj.filt, aes(x = high_rank)) + geom_histogram(col = 'black', fill = 'blue') + ylim(c(0,40)) + xlim(c(0,1)) +
  xlab('') + ylab('')
t.test(df.s2.subj.filt$high_rank - .5)

# s1 high value
ggplot(df.s2.subj.filt, aes(x = high_value)) + geom_histogram(col = 'black', fill = 'blue')
t.test(df.s2.subj.filt$high_value - .5)
ggplot(df.s2.subj.filt, aes(x = s1_value)) + geom_histogram(col = 'black', fill = 'blue')
t.test(df.s2.subj.filt$s1_value - 5)

# logit test
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
    MFhigh.col = rep(df.words.temp$high_value[recalled.temp] * 1, nAnswered)
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
        temp.mbhigh[ind,] = mbvals[recalled.temp] > 9
        
        choice = logical(num.recalled.temp)
        choice[which(df.s2.temp$choice_real_ind[q] == which(recalled.temp))] = TRUE
        temp.choice[ind,] = choice
        
        #choice2 = vector(mode = 'numeric', num.recalled.temp)
        #choice2[1] = which(df.s2.temp$choice_real_ind[q] == which(recalled.temp))
        #choice2[1] = OptionID.col[1:num.recalled.temp][choice]
        #temp.choice2[ind,] = choice2
        
        ind = ind + 1
      }
    }
    
    MBval.col = as.vector(t(temp.mbval))
    MBhigh.col = as.vector(t(temp.mbhigh))
    Choice.col = as.vector(t(temp.choice))
    #Choice2.col = as.vector(t(temp.choice2))
    
    df.logit = rbind(df.logit,
                     data.frame(Subj = Subj.col, Trial = Trial.col, OptionID = OptionID.col, Choice = Choice.col,
                                MFval = MFval.col, MBval = MBval.col, MFhigh = MFhigh.col, MBhigh = MBhigh.col,
                                Recall = Recalled.col, Question = Question.col))
    
  }
}

df.logit = df.logit %>% mutate(MFcent = MFhigh - mean(MFhigh), MBcent = MBval - mean(MBval), Int = MFcent * MBcent)

m.real = runLogit(df.logit)
summary(m.real)

# interaction graph

df.sum = df.logit %>%
  group_by(MFhigh,MBval) %>% summarize(Choice.mean = mean(Choice)) #%>% mutate(Choice.mean = Choice.mean * ifelse(MFval %in% c(0,10), 2/3, 1))

ggplot(data = df.sum, aes(x = MBval, y = Choice.mean, group = MFhigh, colour = MFhigh)) +
  geom_point(aes(size = 2)) + geom_line()

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
df.s2.subj.filt$order_weights = coef(m.order)$subject$high_valueTRUE

## weights
if (version == "value1") {
  df.s2.subj.filt$weights = c(7.79e-02,3.15e-01,2.04e-01,1.17e-01,6.19e-08,1.94e-07,1.68e-01,1.27e-01,9.58e-01,2.50e-01,5.62e-07,1.60e-07,5.84e-01,3.52e-07,2.96e-01,2.94e-07,4.19e-01,2.74e-01,3.86e-08,2.79e-08,1.16e-01,4.21e-07,4.02e-01,2.92e-01,2.48e-02,1.51e-08,4.69e-07,2.69e-01,1.15e-08,6.13e-01,2.51e-01,2.06e-01,5.69e-02,1.86e-07,2.90e-07,4.00e-01,8.97e-08,1.42e-08,2.32e-01,4.33e-01,8.15e-03,1.97e-01,1.41e-07,6.35e-01,2.34e-01,2.36e-01,3.88e-08,3.74e-08,2.56e-08,2.60e-01,1.15e-06,2.34e-01,8.60e-01,5.42e-08,1.74e-01,1.76e-01,2.46e-01,3.32e-01,3.93e-07,4.69e-01,3.03e-02,7.43e-07,2.14e-01,1.11e-07,2.41e-01,5.26e-02,2.10e-01,4.74e-01,1.55e-01,1.00e+00,1.67e-07,4.37e-08,3.19e-07,9.62e-02,2.91e-08,2.62e-01,2.40e-01,2.03e-01,2.76e-01,1.98e-01,8.71e-08,1.04e-06,1.62e-01,1.54e-01,1.16e-01,2.42e-01,1.64e-07,7.93e-08,4.14e-01,9.77e-09,1.22e-01,3.96e-01,2.70e-08,3.83e-01,8.94e-08,2.67e-06,1.62e-01,7.43e-01,3.02e-02,1.72e-08,2.37e-01,2.33e-01,3.64e-01,1.99e-08,1.53e-07,3.34e-03,7.11e-08,1.19e-08,3.58e-01,9.93e-01,1.37e-01,3.07e-01,4.64e-07,8.93e-09,1.00e+00,2.75e-07,1.34e-01,2.76e-01,1.37e-02,5.08e-09,3.69e-01,1.00e+00,1.70e-01)
}

ggplot(df.s2.subj.filt, aes(high_value, weights)) + geom_point() + geom_smooth(method = 'lm')
ggplot(df.s2.subj.filt, aes(order_weights, weights)) + geom_point() + geom_smooth(method = 'lm')

# bonuses, modeling -----------------------------------------------------------------

## save for modeling
qlist = c(2,4,5)
df.test = df.s2 %>% filter(question_ind %in% qlist) %>% group_by(subject) %>% summarize(anyGood = any(!is.na(choice_real_ind)))

rewards_tr = matrix(0, nrow = sum(include_rows), ncol = numWords)
ind = 1
for (subj in 1:nrow(df.demo)) {
  subj.name = df.demo$subject[subj]
  
  if (subj.name %in% include_names & df.test$anyGood[df.test$subject == subj.name]) {
    df.words.temp = df.words %>% filter(subject == subj.name)
    
    for (word in 1:numWords) {
        rewards_tr[ind, word] = ifelse(type == 1, df.words.temp$exposures[word], df.words.temp$value[word])
    }
    ind = ind + 1
  }
}

write.csv(rewards_tr, paste0(path, 'rewards_s1.csv'), row.names = F)
write.csv(recalled_ever[include_rows & df.test$anyGood, ] * 1, paste0(path, 'recalled.csv'), row.names = F)

df.modeling = df.s2 %>% filter(subject %in% include_names & !is.na(choice_real_ind) & question_ind %in% qlist) %>%
  mutate(all_values_nocomma = gsub(",", " ", all_values)) %>% 
  dplyr::select(s2_subj_ind, choice_real_ind, all_values_nocomma)
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

# jphil’s stuff -----------------------------------------------------------



## word order raster plot:
cor.test(df.words$order[df.words$recall==TRUE],df.words$value[df.words$recall==TRUE])
plot <- df.words %>%
        filter(recall) %>%
        mutate(high_val = factor(c("Low Past Value","High Past Value")[as.factor(high_val)])) %>%
        group_by(order,value,high_val) %>%
        summarise(count = table(value)[1]) %>%
        ggplot(aes(x=order,y=value,fill=count)) +
          geom_tile()+
          scale_fill_continuous(low = 'white',high = 'red') +
          facet_wrap(~high_val, scales="free_y",ncol=1) +
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
  mutate(high_val = factor(c("Low Past Value","High Past Value")[as.factor(high_val)])) %>%
  group_by(value,high_val,subject) %>%
  summarise(meanOrders = mean(order,na.rm=T)) %>%
  group_by(value,high_val) %>%
  summarise(meanOrder = mean(meanOrders,na.rm=T),
            seOrder = se(meanOrders),
            minOrder = meanOrder - seOrder,
            maxOrder = meanOrder + seOrder) %>%
  arrange(order(high_val))
  

plot2 <- ggplot(orderD,aes(x=meanOrder,y=value)) +
          geom_errorbarh(xmin=orderD$minOrder, xmax=orderD$maxOrder, height=.2) +
          geom_point(size=3,color="Red") +
          coord_cartesian(xlim=c(0,15)) +
          facet_wrap(~high_val, scales="free_y",ncol=1) +
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
plot2

## Graph for simulations - here the data are set so that there are 500 words, and a choice set size of 10, meaning 2% of the 

jphilPalette <- c("darkorange3","lightblue","darkgreen","azure4")

d.sims = read.csv("data/modelSim.csv") %>% gather(model,earnings,-R) %>%
          mutate(model = recode(model, CS ="Choice Set", MB = "Full Planning", MF = "No Planning"),
                 model = factor(model, levels=c("No Planning","Choice Set","Full Planning")),
                 R = factor(R),
                 earnings = (earnings/max(earnings))*100) %>%
          filter(R!=1) %>%
          ggplot(aes(x=R,y=earnings,fill=model)) +
                 geom_bar(position="dodge",stat="identity") +
                 #geom_line(aes(color=model)) +
                 scale_fill_manual(values=grey.colors(3,start=.9,end=.3)) +
                 theme_bw() +
                 theme(
                    plot.background = element_blank()
                   ,panel.grid.major = element_blank()
                   ,panel.grid.minor = element_blank()
                   ,legend.title=element_blank()
                   ,legend.text=element_text(size=rel(1.4))
                   ,axis.title=element_blank()
                   ,axis.text.y=element_text(size=rel(1.5))
                   ,axis.text.x=element_text(size=rel(1.5))
                   ,axis.ticks = element_blank()
                 )
d.sims