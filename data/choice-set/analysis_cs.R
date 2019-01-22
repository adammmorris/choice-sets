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


setwd("~/Me/Psychology/Projects/choicesets/git/data/choice-set")

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

as.string.vector.noquotes = function(x) {
  temp = strsplit(substr(x,2,nchar(x)-1), split=",")[[1]]
  return(temp)
}

as.numeric.vector = function(x) {
  return(as.numeric(strsplit(substr(x,2,nchar(x)-1), split=",")[[1]]))
}

se = function(x) {return(sd(x, na.rm = T) / sqrt(length(x)))}
dodge <- position_dodge(width=0.9)


# import data -------------------------------------------------------------

path = 'months_confounded/real1/'
cs.flipped = F

numWords = 12;
numTrials = 132;
minNAs = 1;
pointsPerCent_s1 = 5;
pointsPerCent_s2 = 1;
numRealQuestions = 1;
type = 0;
maxRepeats = 2;
numQuestions = 2;

# Load data
df.demo = read.csv(paste0(path, 'demo.csv'), stringsAsFactors = F) %>% arrange(subject) %>% mutate(total_time_real = total_time / 60000)
df.words.raw = read.csv(paste0(path, 'words.csv'), stringsAsFactors = F) %>% arrange(subject, word_ind)
df.s1.raw = read.csv(paste0(path, 's1.csv'), stringsAsFactors = F) %>% arrange(subject)
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

# df.s2.excl

df.s2.excl = df.s2 %>% filter(subject %in% subjlist) %>%
  group_by(subject) %>%
  summarize(comp_check_pass = mean(comp_check_pass[question_order == 0]),
            numNAs = sum(is.na(choice_real)),
            numRepeats = sum(choice_real == lag(choice_real), na.rm = T),
            numTrials = n(),
            cond = cond[1])

# df.words

for (i in 1:nrow(df.words)) {
  subj = df.words$subject[i]
  choice = (df.s2 %>% filter(subject == subj, question_order == 0))$choice_real
  df.s2.temp = df.s2 %>% filter(subject == subj)
  
  
  # which words were in cs
  cs = (df.s2.temp %>% filter(question == 'choice-set'))$choice
  cs.order = (df.s2.temp %>% filter(question == 'choice-set'))$scratch
  if (length(cs) > 0) {
    cs = as.string.vector.noquotes(gsub('\"', '', cs))
    cs.order = as.string.vector.noquotes(gsub('\"', '', cs.order))
    for (j in 1:length(cs)) {
      cs.split = strsplit(cs[j], ":")[[1]]
      word = cs.split[1]
      val = cs.split[2]
      word_rows = df.words$word == word & df.words$subject == subj
      
      # in later versions, "1" means no and "0" means yes
      val = ifelse(cs.flipped, ifelse(val == "1", "0", ifelse(val == "0", "1", val)), val)
      
      cs.order.split = strsplit(cs.order[j], ":")[[1]]
      order.val = as.numeric(ifelse(val == "1", cs.order.split[2], NA))
      
      if (length(choice) > 0) {
        df.words$in.cs[word_rows] = ifelse(val == "1" | word == choice, T, ifelse(val == "0", F, NA))
        df.words$cs.order[word_rows] = order.val
      } else {
        df.words$in.cs[word_rows] = NA
      }
    }
  } else {
    df.words$in.cs[word_rows] = NA
  }
}

# s1
df.s1 = df.s1.raw %>% filter(subject %in% subjlist) %>% mutate(choice = as.numeric(choice))
#newSubj = T

for (i in 1:nrow(df.s1)) {
  subj = df.s1$subject[i]
  
  #if (i > 1 && subj != df.s1$subject[i-1]) {
  #  newSubj = T
  #}
  
  #if (newSubj) {
  #  bonusTrial = sample(1:numTrials, 1)
  #}
  
  v1 = df.words$s1_value[df.words$subject == subj & df.words$word == df.s1$word[i]]
  v2 = df.words$s1_value[df.words$subject == subj & df.words$word == df.s1$alt[i]]
  choice = df.s1$choice[i]
  
  df.s1$word_chosen[i] = ifelse(choice, df.s1$alt[i], df.s1$word[i])
  
  #df.s1$correct_word[i] = ain(toupper(df.s1$resp[i]), df.s1$word[i], maxDist = 2) & ain(toupper(df.s1$resp3[i]), df.s1$alt[i], maxDist = 2)
  #df.s1$correct_val[i] = df.s1$resp2[i] == df.s1$value[i] & df.s1$resp4[i] == df.s1$value2[i]
  df.s1$correct_word[i] = df.s1$resp[i]
  df.s1$correct_choice[i] = ifelse(length(v2) > 0, ifelse(v1 == v2, 1, ifelse(v1 > v2, choice == 0, choice == 1)), -1)
  
  #df.s1$bonusTrial[i] = bonusTrial
  #df.s1$bonusMoney[i] = ifelse(df.s1$choice[bonusTrial] == 0, df.s1$value[bonusTrial], df.s1$value2[bonusTrial])
  
  #newSubj = F
  
  #df.demo$s1_bonus[df.demo$subject == subj] = df.s1$bonusMoney[i]
}

df.s1.subj = df.s1 %>% group_by(subject) %>%
  summarize(pctCorrect_words = mean(correct_word, na.rm = T),
            pctCorrect_choice = mean(correct_choice, na.rm = T), numTrials = n())

df.s1.last = df.s1 %>% filter(trial > 66) %>% group_by(subject) %>% summarize(pctCorrect_choice = mean(correct_choice, na.rm = T),
                                                                              pctCorrect_words = mean(correct_word, na.rm = T))

# compute exclusion -------------------------------------------------------

include_rows = NULL
include_names = NULL

for (subj in 1:length(subjlist)) {
  subj.name = subjlist[subj]
  df.s1.subj.temp = df.s1.subj %>% filter(subject == subj.name)
  df.s1.temp = df.s1.subj %>% filter(subject == subj.name)
  df.s2.subj.temp = df.s2.excl %>% filter(subject == subj.name)
  df.demo.temp = df.demo %>% filter(subject == subj.name)
  df.words.temp = df.words %>% filter(subject == subj.name)
  
  exclude = df.demo.temp$write_down == 'Yes' || df.s2.subj.temp$comp_check_pass < 1 ||
    df.s2.subj.temp$numNAs > minNAs || df.s2.subj.temp$numTrials != numQuestions ||
    df.s1.subj.temp$numTrials != numTrials ||
    df.s1.temp$pctCorrect_choice < .7
  if (exclude) {
    include_rows[subj] = FALSE
  } else {
    include_rows[subj] = TRUE
    include_names = c(include_names, subj.name)
  }
}

# data manip --------------------------------------------------------------

## words

#month_list = c('JANUARY', 'FEBRUARY', 'MARCH', 'APRIL', 'MAY', 'JUNE', 'JULY', 'AUGUST', 'SEPTEMBER', 'OCTOBER', 'NOVEMBER', 'DECEMBER')

for (i in 1:nrow(df.words)) {
  subj = df.words$subject[i]
  word = df.words$word[i]
  
  s1_valuelist = (df.words %>% filter(subject == subj))$s1_value
  s1_valuelist_rank = rank(s1_valuelist, ties.method = 'max')
  df.words$high_s1value[i] = df.words$s1_value[i] > median(s1_valuelist)
  df.words$rank_s1value[i] = s1_valuelist_rank[df.words$word_ind[i] + 1]

  s2_valuelist = (df.words %>% filter(subject == subj))$s2_value
  s2_valuelist_rank = rank(s2_valuelist, ties.method = 'max')
  s2_valuelist_indiv = (df.words %>% filter(subject == subj & in.cs == 1))$s2_value
  s2_valuelist_indiv_rank = rank(-s2_valuelist_indiv, ties.method = 'max')
  df.words$high_s2value[i] = df.words$s2_value[i] > median(s2_valuelist)
  df.words$rank_s2value[i] = s2_valuelist_rank[df.words$word_ind[i] + 1]
  df.words$rank_s2value_indiv[i] = ifelse(df.words$in.cs[i], s2_valuelist_indiv_rank[df.words$s2_value[i] == s2_valuelist_indiv], NA)
  df.words$rank_s2value_indiv_mean[i] = mean(s2_valuelist_indiv_rank)
  
  # df.s1.temp = df.s1 %>% filter(subject == subj & trial > 128)
  # df.words$last_seen3[i] = word %in% df.s1.temp$word | word %in% df.s1.temp$alt
  # df.s1.temp = df.s1 %>% filter(subject == subj & trial > 130)
  # df.words$last_seen1[i] = word %in% df.s1.temp$word | word %in% df.s1.temp$alt
  
  df.s1.temp = df.s1 %>% filter(subject == subj)
  df.words$last_seen[i] = max(c(which(word == df.s1.temp$word), which(word == df.s1.temp$alt)))
  df.words$last_chosen[i] = max(which(word == df.s1.temp$word_chosen))
  df.words$num_chosen[i] = sum(word == df.s1.temp$word_chosen)
}

df.words = df.words %>% mutate(chosen = ifelse(in.cs, 0, NA), chosen_noNA = 0)

## s2
for (i in 1:nrow(df.s2)) {
  subj.name = df.s2$subject[i]
  wordlist = (df.words %>% filter(subject == subj.name))$word
  
  cind = df.s2$choice_real_ind[i]
  creal = df.s2$choice_real[i]
  
  word_rows = subj.name == df.words$subject & creal == df.words$word
  
  df.s2$s1_value[i] = ifelse(is.na(cind), NA, df.words$s1_value[word_rows])
  df.s2$high_s1value[i] = ifelse(is.na(cind), NA, df.words$high_s1value[word_rows])
  df.s2$rank_s1value[i] = ifelse(is.na(cind), NA, df.words$rank_s1value[word_rows])
  
  df.s2$s2_value[i] = ifelse(is.na(cind), NA, df.words$s2_value[word_rows])
  df.s2$high_s2value[i] = ifelse(is.na(cind), NA, df.words$high_s2value[word_rows])
  df.s2$rank_s2value[i] = ifelse(is.na(cind), NA, df.words$rank_s2value[word_rows])
  df.s2$rank_s2value_indiv[i] = ifelse(is.na(cind), NA, df.words$rank_s2value_indiv[word_rows])
  df.s2$rank_s2value_indiv_mean[i] = ifelse(is.na(cind), NA, df.words$rank_s2value_indiv_mean[word_rows])
  
  s2_valuelist = (df.words %>% filter(subject == subj.name))$s2_value
  df.s2$median_value[i] = median(s2_valuelist)
  
  df.words$chosen[word_rows] = 1
  df.words$chosen_noNA[word_rows] = 1
}

df.s2 = df.s2 %>% mutate(s2_subj_ind = as.numeric(as.factor(subject)), # just for modeling
                         bonus_value = ifelse(is.na(choice_real_ind), 0, s2_value))

df.s2.filt = df.s2 %>% filter(subject %in% include_names & question_order == 0)
df.words.filt = df.words %>% filter(subject %in% include_names)
df.demo.filt = df.demo %>% filter(subject %in% include_names)

# check out data ----------------------------------------------------------

## plots!
# effect of stage 1 value on...
df.graph.s1 = df.words.filt %>% mutate(high_s1value = factor(high_s1value, c(F,T), c('Low', 'High')),
                                      often = factor(num_chosen > 10, c(F,T), c('Rare', 'Often'))) %>%
  group_by(high_s1value) %>% #filter(s1_value >= 4) %>%
  summarize(in.cs = mean(in.cs, na.rm = T), in.cs.se = sqrt(in.cs * (1-in.cs) / n()),
            cs.order.m = mean(cs.order, na.rm = T), cs.order.se = sd(cs.order, na.rm = T) / sqrt(length(cs.order[!is.na(cs.order)])),
            chosen = mean(chosen, na.rm = T), chosen.se = sqrt(chosen * (1-chosen) / n()))
# choice sets
ggplot(df.graph.s1, aes(x = high_s1value, y = in.cs)) +
  geom_point(size = 5) + #geom_line() +
  geom_errorbar(aes(ymin = in.cs - in.cs.se, ymax = in.cs+in.cs.se), width = .2) +
  #geom_smooth(method='lm') +
  xlab('Stage 1 value') + ylab('Prob. in\nconsideration set') +
  scale_y_continuous(breaks = c(.3,.4), limits = c(.3,.41)) + 
  #scale_x_continuous(breaks = c(1,12)) +
  theme(axis.title = element_text(size = 24))# +
  #ylim(.3, .4)
  #facet_wrap(~cond)
# selection out of choice set
ggplot(df.graph.s1, aes(x = s1_value, y = chosen)) +
  geom_point(size = 5) + geom_line() +
  geom_errorbar(aes(ymin = chosen - chosen.se, ymax = chosen+chosen.se), width = .2) +
  geom_smooth(method='lm')+
  xlab('Stage 1 value') + ylab('Prob. chosen out of choice set')
  #scale_y_continuous(breaks = c(0,1), limits = c(0,1)) +
  #scale_x_continuous(breaks = c(1,12))# +
  #facet_wrap(~cond)

# 2x2
df.graph.s1 = df.words.filt %>% #mutate(#high_s1value = factor(high_s1value, c(F,T), c('Low', 'High')),
  #often = factor(num_chosen > 10, c(F,T), c('Rare', 'Often'))) %>%
  mutate(s1value.fac = cut(s1_value, 2, c('Low', 'High')), chosen.fac = cut(num_chosen, 12, 1:12)) %>%
  group_by(s1value.fac, chosen.fac) %>% #filter(s1_value >= 4) %>%
  summarize(in.cs = mean(in.cs, na.rm = T), in.cs.se = sqrt(in.cs * (1-in.cs) / n()),
            cs.order.m = mean(cs.order, na.rm = T), cs.order.se = sd(cs.order, na.rm = T) / sqrt(length(cs.order[!is.na(cs.order)])),
            chosen = mean(chosen, na.rm = T), chosen.se = sqrt(chosen * (1-chosen) / n()))
ggplot(df.graph.s1, aes(x = chosen.fac, y = in.cs, group = s1value.fac, color = s1value.fac)) +
  geom_point(size = 5) + geom_line() +
  geom_errorbar(aes(ymin = in.cs - in.cs.se, ymax = in.cs+in.cs.se), width = .2) +
  #geom_smooth(method='lm') +
  #xlab('Stage 1 value') + ylab('Prob. in\nconsideration set') +
  #scale_y_continuous(breaks = c(.3,.4), limits = c(.3,.41)) + 
  #scale_x_continuous(breaks = c(1,12)) +
  theme(axis.title = element_text(size = 24))


# effect of stage 2 value on...
df.graph.s2 = df.words.filt %>% group_by(cond, word_ind, s2_value) %>%
  summarize(in.cs = mean(in.cs, na.rm = T), in.cs.se = sqrt(in.cs * (1-in.cs) / n()),
            chosen = mean(chosen, na.rm = T), chosen.se = sqrt(chosen * (1-chosen) / n()))
# choice sets
ggplot(df.graph.s2, aes(x = s2_value, y = in.cs)) +
  geom_point(size = 5) + geom_line() +
  geom_errorbar(aes(ymin = in.cs - in.cs.se, ymax = in.cs+in.cs.se), width = .2) +
  geom_smooth(method='lm', color = 'black')
  #xlab('Stage 2 value') + ylab('Prob. in choice set') +
  #scale_y_continuous(breaks = c(0,1), limits = c(0,1)) +
  #scale_x_continuous(breaks = c(1,26))# +
# selection out of choice set
ggplot(df.graph.s2, aes(x = s2_value, y = chosen)) +
  geom_point(size = 5) + geom_line() +
  geom_errorbar(aes(ymin = chosen - chosen.se, ymax = chosen+chosen.se), width = .2) +
  geom_smooth(method='lm') #+
  #xlab('Stage 2 value') + ylab('Prob. chosen out of choice set') +
  #scale_y_continuous(breaks = c(0,1), limits = c(0,1)) +
  #scale_x_continuous(breaks = c(1,12)) +
  #facet_wrap(~cond)


## stats!

m.s1.cs = glmer(in.cs~s1_value*num_chosen+(1+s1_value*num_chosen||subject)+(1+s1_value*num_chosen||word_ind),
                data = df.words.filt %>% mutate(subject = factor(subject)),
                family='binomial')
summary(m.s1.cs)

# s1 -> cs
m.s1.cs = glmer(in.cs~s1_value+(1+s1_value||subject),
                data = df.words.filt,
                family='binomial')
summary(m.s1.cs)

m.s1.cs = lmer(cs.order~s1_value+(1+s1_value||subject),
                data = df.words.filt)
summary(m.s1.cs)

# m.s1 = glmer(in.cs~s1_value+I(s1_value^2)+(s1_value+I(s1_value^2)|subject),#(1|subject)+(0+s1_value|subject)+(0+I(s1_value^2)|subject),
#              data = df.words.filt %>% filter(!(s1_value %in% c(1,12))) %>% mutate(s1_value = s1_value - mean(s1_value)),
#              family='binomial')
# summary(m.s1)

# s2 -> cs
m.s2.cs = glmer(in.cs~s2_value+(s2_value|subject),
                data = df.words.filt,
                family='binomial')
summary(m.s2.cs)

# for selection tests, we need logit
df.logit = data.frame(Subj = NULL, Trial = NULL, OptionID = NULL, Choice = NULL, MFval = NULL, MBval = NULL, nExposures = NULL, Recalled = NULL, Question = NULL)

for (subj in 1:nrow(df.demo.filt)) {
  subj.name = df.demo$subject[subj]
  recalled.temp = df.words.filt$in.cs[df.words.filt$subject == subj.name]
  #recalled.temp = !logical(numWords)
  num.recalled.temp = sum(recalled.temp)
  
  df.words.temp = df.words.filt %>% filter(subject == subj.name)
  df.s2.temp = df.s2.filt %>% filter(subject == subj.name) %>% arrange(question_order)
  
  nAnswered = sum(!is.na(df.s2.temp$choice_real_ind))
  
  if (nAnswered > 0) {
    Subj.col = rep(subj, num.recalled.temp * nAnswered)
    Condition.col = rep(df.s2.temp$cond, num.recalled.temp * nAnswered)
    
    MFval.col = rep(df.words.temp$s1_value[recalled.temp], nAnswered)
    MFhigh.col = rep(df.words.temp$high_s1value[recalled.temp] * 1, nAnswered)
    MBval.col = rep(df.words.temp$s2_value[recalled.temp], nAnswered)
    MBhigh.col = rep(df.words.temp$high_s2value[recalled.temp] * 1, nAnswered)
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
                                MFval = MFval.col, MBval = MBval.col, MFhigh = MFhigh.col, MBhigh = MBhigh.col))
  }
}
df.logit = df.logit %>% mutate(Trial_unique = paste(Subj, Trial, sep="_"))
df.logit2 = mlogit.data(df.logit, choice = "Choice", shape = "long", id.var = "Subj", alt.var = "OptionID", chid.var = "Trial_unique")

m.selection = mlogit(Choice ~ MFval + MBval | -1, df.logit2, panel = T,
  rpar = c(MFval = "n", MBval = "n"), correlation = F, halton = NA, R = 1000, tol = .001)
summary(m.selection)

# # s2 rank value
# ggpiestats(df.s2.filt, high_s2value)
# ggpiestats(df.s2.filt, high_s2value_indiv)
# gghistostats(df.s2.filt, rank_s2value, test.value = 6.5, centrality.para = 'mean', type = 'np')
# 
# # s1 high value
# ggpiestats(data = df.s2.filt, main = high_s1value)
# ggpiestats(df.s2.filt, high_s1value_indiv)
gghistostats(df.s2.filt, rank_s1value, test.value = 6.5, centrality.para = 'median', type = 'p')
# grouped_ggpiestats(cond, data = df.s2.filt, main = high_s1value)
# 
# wilcox.test(df.s2.filt$rank_s1value, mu = 7)
# 
# df.cs = df.words.filt %>% group_by(subject) %>% summarize(cs.size = sum(in.cs))

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
test = df.s2 %>% group_by(subject) %>% summarize(num0 = sum(question_order == 0), num1 = sum(question_order == 2))
df.s2.subj = df.s2 %>% filter(question_order == 0) %>%
  mutate(s2_bonus = ifelse(is.na(s2_value), 0, s2_value),
         mem_bonus = 0)
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
df.demo = df.demo %>% mutate(s2_bonus = I(df.s2.subj$s2_bonus), mem_bonus = I(df.s2.subj$mem_bonus),
                             bonus = round((s1_bonus / pointsPerCent_s1 + s2_bonus / pointsPerCent_s2  + mem_bonus) / 100, 2))
df.demo = df.demo %>% mutate(s2_bonus = I(df.s2.subj$s2_bonus), mem_bonus = I(df.s2.subj$mem_bonus),
                             w1_bonus = match(substr(tr_resp2,1,1), letters), w1_bonus = ifelse(is.na(w1_bonus), 0, w1_bonus),
                             w2_bonus = match(substrRight(tr_resp_correct,1), letters), w2_bonus = ifelse(is.na(w2_bonus), 0, w2_bonus),
                             bonus = round((s1_bonus / pointsPerCent_s1 + s2_bonus / pointsPerCent_s2 + w1_bonus + w2_bonus + mem_bonus) / 100, 2))
write.table(df.demo %>% dplyr::select(WorkerID = subject, Bonus = bonus),
            paste0(path, 'Bonuses.csv'), row.names = FALSE, col.names = FALSE, sep = ",")

## save
save.image(paste0(path, 'analysis.rdata'))
