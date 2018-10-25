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

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

se = function(x) {return(sd(x, na.rm = T) / sqrt(length(x)))}
dodge <- position_dodge(width=0.9)


# import data -------------------------------------------------------------

numWords = 12;
numTrials = 132;
minNAs = 1;
path = 'months_notr_v2/real1/'
pointsPerCent_s1 = 5;
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

for (i in 1:nrow(df.words)) {
  subj = df.words$subject[i]
  choice = (df.s2 %>% filter(subject == subj, question_order == 0))$choice_real
  df.s2.temp = df.s2 %>% filter(subject == subj)
  
  if (T) {
    # which words were in cs
    cs = (df.s2.temp %>% filter(question == 'choice-set'))$choice
    if (length(cs) > 0) {
      cs = as.string.vector.noquotes(gsub('\"', '', cs))
      for (j in 1:length(cs)) {
        cs.split = strsplit(cs[j], ":")[[1]]
        word = cs.split[1]
        val = cs.split[2]
        
        if (length(choice) > 0) {
          df.words$in.cs[df.words$word == word & df.words$subject == subj] = ifelse(val == "1" | word == choice, T, ifelse(val == "0", F, NA))
        } else {
          df.words$in.cs[df.words$word == word & df.words$subject == subj] = NA
        }
      }
    } else {
      df.words$in.cs[df.words$word == word & df.words$subject == subj] = NA
    }
  } else {
    df.words$in.cs[i] = df.words$recall.ever[i]
  }
}

# compute exclusion -------------------------------------------------------

include_rows = NULL
include_names = NULL

for (subj in 1:length(subjlist)) {
  subj.name = subjlist[subj]
  df.s2.subj.temp = df.s2.excl %>% filter(subject == subj.name)
  df.demo.temp = df.demo %>% filter(subject == subj.name)
  df.words.temp = df.words %>% filter(subject == subj.name)
  
  exclude = df.demo.temp$write_down == 'Yes' || df.s2.subj.temp$comp_check_pass < 1 ||
    df.s2.subj.temp$numNAs > minNAs || df.s2.subj.temp$numTrials != numQuestions #|| sum(df.words.temp$in.cs) < 2 || #df.demo.temp$use_s1 == 'Yes'
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
}

df.words = df.words %>% mutate(chosen = ifelse(in.cs, 0, NA))

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
  
  if (df.s2$cond[i] == 'late') {
    df.s2$cond[i] = df.s2$cond[df.s2$question_ind == 2 & df.s2$subject == subj.name]
  }
}

df.s2 = df.s2 %>% mutate(s2_subj_ind = as.numeric(as.factor(subject)), # just for modeling
                         bonus_value = ifelse(is.na(choice_real_ind), 0, s2_value))

df.s2.filt = df.s2 %>% filter(subject %in% include_names & question_order == 0)
df.words.filt = df.words %>% filter(subject %in% include_names)
df.demo.filt = df.demo %>% filter(subject %in% include_names)

# check out data ----------------------------------------------------------

df.cs = df.words.filt %>% group_by(cond, subject) %>% summarize(cs.size = sum(in.cs)) %>%
  group_by(cond) %>% summarize(cs.size.m = mean(cs.size), cs.size.se = se(cs.size))

ggplot(df.cs, aes(x = cond, y = cs.size.m)) +
  geom_point(size = 5) + geom_smooth(method = 'lm') +
  geom_errorbar(aes(ymin = cs.size.m - cs.size.se, ymax = cs.size.m + cs.size.se), width = .2)

# plot all months
df.months = df.words.filt %>% filter(cond == '5') %>% group_by(word_ind) %>% summarize(in.cs = mean(in.cs))
ggplot(df.months, aes(x = word_ind, y = in.cs)) +
  geom_point(size = 5)

# plot payoff vs cond
df.bonus = df.s2.filt %>% group_by(cond) %>% summarize(bonus.mean = mean(s2_value), bonus.se = se(s2_value))
df.cs$bonus.mean = df.bonus$bonus.mean
df.cs$bonus.se = df.bonus$bonus.se

ggplot(df.cs, aes(x = cond, y = bonus.mean)) +
  geom_point(size = 5) + #geom_smooth(method = 'lm') +
  geom_errorbar(aes(ymin = bonus.mean - bonus.se, ymax = bonus.mean + bonus.se), width = .2)

# plot payoff vs cs-size
df.cs.subj = df.words.filt %>% group_by(subject) %>% summarize(cs.size = sum(in.cs)) %>%
  group_by(cs.size)
df.cs.subj$bonus = df.s2.filt$s2_value
df.cs2 = df.cs.subj %>% group_by(cs.size) %>% summarize(bonus.mean = mean(bonus), bonus.se = se(bonus))
ggplot(df.cs2, aes(x = cs.size, y = bonus.mean)) +
  geom_point(size = 5) + geom_smooth(method = 'lm') #+
  #geom_errorbar(aes(ymin = bonus.mean - bonus.se, ymax = bonus.mean + bonus.se), width = .2)

# bonuses, modeling -----------------------------------------------------------------

## bonuses
test = df.s2 %>% group_by(subject) %>% summarize(num0 = sum(question_order == 0), num1 = sum(question_order == 2))
df.s2.subj = df.s2 %>% filter(question_order == 0) %>%
  mutate(s2_bonus = ifelse(is.na(s2_value), 0, s2_value),
         mem_bonus = 0)
df.demo = df.demo %>% mutate(s2_bonus = I(df.s2.subj$s2_bonus), mem_bonus = I(df.s2.subj$mem_bonus),
                             w1_bonus = match(substr(tr_resp2,1,1), letters), w1_bonus = ifelse(is.na(w1_bonus), 0, w1_bonus),
                             w2_bonus = match(substrRight(tr_resp_correct,1), letters), w2_bonus = ifelse(is.na(w2_bonus), 0, w2_bonus),
                             bonus = round((s1_bonus / pointsPerCent_s1 + s2_bonus / pointsPerCent_s2 + w1_bonus + w2_bonus + mem_bonus) / 100, 2))
write.table(df.demo %>% dplyr::select(WorkerID = subject, Bonus = bonus),
            paste0(path, 'Bonuses.csv'), row.names = FALSE, col.names = FALSE, sep = ",")

## save
save.image(paste0(path, 'analysis.rdata'))