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

path = 'months_int/pilot1/'
cs.flipped = T

numWords = 12;
numTrials = 128;
minNAs = 1;
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
  
  
  # which words were in cs
  cs = (df.s2.temp %>% filter(question == 'choice-set'))$choice
  if (length(cs) > 0) {
    cs = as.string.vector.noquotes(gsub('\"', '', cs))
    for (j in 1:length(cs)) {
      cs.split = strsplit(cs[j], ":")[[1]]
      word = cs.split[1]
      val = cs.split[2]
      word_rows = df.words$word == word & df.words$subject == subj
      
      # in later versions, "1" means no and "0" means yes
      val = ifelse(cs.flipped, ifelse(val == "1", "0", ifelse(val == "0", "1", val)), val)
      
      if (length(choice) > 0) {
        df.words$in.cs[word_rows] = ifelse(val == "1" | word == choice, T, ifelse(val == "0", F, NA))
      } else {
        df.words$in.cs[word_rows] = NA
      }
    }
  } else {
    df.words$in.cs[word_rows] = NA
  }
}

# s1
df.s1 = df.s1.raw %>% filter(subject %in% subjlist) %>%
  mutate(word_chosen = ifelse(choice, alt, word))

for (i in 1:nrow(df.s1)) {
  subj = df.s1$subject[i]
  
  v1 = df.s1$value[i]
  v2 = df.s1$value2[i]
  choice = df.s1$choice[i]
  
  df.s1$correct_choice[i] = ifelse(length(v2) > 0, ifelse(v1 == v2, 1, ifelse(v1 > v2, choice == 0, choice == 1)), -1)
  
  df.s1$rel_val[i] = ifelse(v1 == 8, v1 - v2, v2 - v1)
  df.s1$focal_word[i] = ifelse(v1 == 8, df.s1$word[i], df.s1$alt[i])
}

df.s1.subj = df.s1 %>% group_by(subject) %>%
  summarize(pctCorrect_choice = mean(correct_choice, na.rm = T), numTrials = n())

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
  
  df.s1.temp = df.s1 %>% filter(subject == subj)
  df.words$num_chosen[i] = sum(word == df.s1.temp$word_chosen)
  
  df.words$avg_rel[i] = ifelse(df.words$cond[i] > 0, mean(df.s1.temp$rel_val[df.s1.temp$focal_word == df.words$word[i]]), NA)
}

df.words$value.cond = factor(df.words$cond %in% c(1,2), c(F,T), c('Low', 'High'))
df.words$freq.cond = factor(df.words$cond %in% c(1,3), c(F,T), c('Rare', 'Often'))
df.words = df.words %>% mutate(chosen = ifelse(in.cs, 0, NA), chosen_noNA = 0)

## s2
for (i in 1:nrow(df.s2)) {
  subj.name = df.s2$subject[i]
  wordlist = (df.words %>% filter(subject == subj.name))$word
  
  cind = df.s2$choice_real_ind[i]
  creal = df.s2$choice_real[i]
  
  word_rows = subj.name == df.words$subject & creal == df.words$word
  
  df.words$chosen[word_rows] = 1
  df.words$chosen_noNA[word_rows] = 1
}

df.s2 = df.s2 %>% mutate(bonus_value = ifelse(is.na(choice_real_ind), 0, s2_value))

df.s2.filt = df.s2 %>% filter(subject %in% include_names & question_order == 0)
df.words.filt = df.words %>% filter(subject %in% include_names)
df.demo.filt = df.demo %>% filter(subject %in% include_names)

# check out data ----------------------------------------------------------

# check that words are properly balanced
df.words.filt %>% group_by(cond) %>% summarize(rel = mean(avg_rel), s1_value = mean(s1_value), num_chosen = mean(num_chosen),
                                               s2_value = mean(s2_value))
ggplot(df.words.filt, aes(x = s1_value, y = num_chosen)) + geom_point()

# plot just the 8 words
graph1 = df.words.filt %>% filter(cond > 0) %>%
  group_by(value.cond, freq.cond, subject) %>%
  summarize(in.cs = mean(in.cs)) %>%
  group_by(value.cond, freq.cond) %>%
  summarize(in.cs = mean(in.cs, na.rm = T), in.cs.se = sqrt(in.cs * (1-in.cs) / n()))
ggplot(graph1, aes(x = freq.cond, y = in.cs, group = value.cond, color = value.cond)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = in.cs - in.cs.se, ymax = in.cs+in.cs.se), width = .2)

# plot all words as a function of frequency
graph2 = df.words.filt %>% mutate(often = factor(num_chosen > 8, c(F,T), c('Rare', 'Often'))) %>% group_by(often, subject) %>%
  summarize(in.cs = mean(in.cs)) %>%
  group_by(often) %>%
  summarize(in.cs = mean(in.cs, na.rm = T), in.cs.se = sqrt(in.cs * (1-in.cs) / n()))
ggplot(graph2, aes(x = often, y = in.cs)) + geom_point(size = 5) +
  geom_errorbar(aes(ymin = in.cs - in.cs.se, ymax = in.cs+in.cs.se), width = .2) +
  xlab('Choice frequency') + ylab('Prob. in consideration set')

# plot all words as a function of both
graph3 = df.words.filt %>%
  mutate(s1value.fac = cut(s1_value, 2, c('Low', 'High')), chosen.fac = cut(num_chosen, 2, c('Rare', 'Often'))) %>%
  group_by(s1value.fac, chosen.fac, subject) %>%
  summarize(in.cs = mean(in.cs)) %>%
  group_by(s1value.fac, chosen.fac) %>%
  summarize(in.cs = mean(in.cs, na.rm = T), in.cs.se = sqrt(in.cs * (1-in.cs) / n()))
ggplot(graph3, aes(x = chosen.fac, y = in.cs, group = s1value.fac, color = s1value.fac)) +
  geom_point(size = 5) + geom_line() +
  geom_errorbar(aes(ymin = in.cs - in.cs.se, ymax = in.cs+in.cs.se), width = .2) +
  #geom_smooth(method='lm') +
  #xlab('Stage 1 value') + ylab('Prob. in\nconsideration set') +
  #scale_y_continuous(breaks = c(.3,.4), limits = c(.3,.41)) + 
  #scale_x_continuous(breaks = c(1,12)) +
  theme(axis.title = element_text(size = 24))



## stats!
# s1 -> cs
m.s1.cs = glmer(in.cs~s1_value*num_chosen+(1+s1_value*num_chosen||subject) + (1+s1_value*num_chosen||word_ind),
                data = df.words.filt %>% filter(cond == 0),
                family='binomial')
summary(m.s1.cs)

# bonuses, modeling -----------------------------------------------------------------

## bonuses
test = df.s2 %>% group_by(subject) %>% summarize(num0 = sum(question_order == 0), num1 = sum(question_order == 2))
test$subject[which(test$num0 != 1)]
df.s2.subj = df.s2 %>% filter(question_order == 0) %>%
  mutate(s2_bonus = ifelse(is.na(s2_value), 0, s2_value),
         mem_bonus = 0)
df.demo = df.demo %>% filter() %>% mutate(s2_bonus = I(df.s2.subj$s2_bonus), mem_bonus = I(df.s2.subj$mem_bonus),
                             bonus = round((s1_bonus / pointsPerCent_s1 + s2_bonus / pointsPerCent_s2  + mem_bonus) / 100, 2))
write.table(df.demo %>% dplyr::select(WorkerID = subject, Bonus = bonus),
            paste0(path, 'Bonuses.csv'), row.names = FALSE, col.names = FALSE, sep = ",")

## save
save.image(paste0(path, 'analysis.rdata'))
