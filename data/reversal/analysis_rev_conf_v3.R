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
require(boot)

theme_update(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(colour = "black"),
             axis.text=element_text(size=20, colour = "black"), axis.title=element_text(size=18, face = "bold"), axis.title.x = element_text(vjust = 0),
             legend.title = element_text(size = 24, face = "bold"), legend.text = element_text(size = 20), plot.title = element_text(size = 26, face = "bold", vjust = 1))


setwd("~/Me/Psychology/Projects/choicesets/git/data/reversal")

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

numWords = c(11,4);
numTrials = c(110,36);
minNAs = 1;
path = 'confounded/v3/real1/'
pointsPerCent_s1 = 5;
pointsPerCent_s2 = 1;
pointsPerWord = 3; # for memory condition
allBonus = 25;
numRealQuestions = 1;
type = 0;
maxRepeats = 2;
numQuestions = 2;

# Load data
df.demo.raw = read.csv(paste0(path, 'demo.csv'), stringsAsFactors = F) %>% arrange(subject) %>% mutate(total_time_real = total_time / 60000)
df.words.raw = read.csv(paste0(path, 'words.csv'), stringsAsFactors = F) %>% arrange(subject, word_ind)
df.s1.raw = read.csv(paste0(path, 's1.csv'), stringsAsFactors = F) %>% arrange(subject);
df.s2.raw = read.csv(paste0(path, 's2.csv'), stringsAsFactors = F) %>% arrange(subject, question_order)

subjlist = unique(df.s2.raw$subject)

df.demo = df.demo.raw %>% filter(subject %in% subjlist)

# words
df.words = df.words.raw %>% filter(subject %in% subjlist) %>%
  mutate(repeated = word_ind == lead(word_ind), repeated = ifelse(is.na(repeated), F, repeated)) %>% 
  filter(!repeated)

# s2
df.s2 = df.s2.raw %>% filter(subject %in% subjlist)
df.s2$choice = toupper(df.s2$choice)
df.s2$scratch = gsub("[.]", ",", toupper(as.character(df.s2$scratch)))
df.s2$all_values = as.character(df.s2$all_values)
df.s2$comp_check_pass = as.numeric(df.s2$comp_check_pass)
df.s2$cond = factor(df.s2$cond, c('small', 'large'), c('small', 'large'))

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
# df.s1 = df.s1.raw %>% filter(subject %in% subjlist) %>%
#   mutate(correct_word = ain(toupper(resp), word, maxDist = 2), correct_val = resp2 == value, word_chosen = ifelse(choice, alt, word),
#          correct_choice = choice == ifelse(value == alt, choice, ifelse(value > alt, 0, 1)))
# df.s1.subj = df.s1 %>% group_by(subject) %>%
#   summarize(pctCorrect_words = mean(correct_word, na.rm = T), pctCorrect_val = mean(correct_val, na.rm = T),
#             pctCorrect_choice = mean(correct_choice, na.rm = T), numTrials = n())

df.s1 = df.s1.raw %>% filter(subject %in% subjlist) %>%
  mutate(word_chosen = ifelse(choice, alt, word))
for (i in 1:nrow(df.s1)) {
  subj = df.s1$subject[i]
  
  v1 = df.words$s1_value[df.words$subject == subj & df.words$word == df.s1$word[i]]
  v2 = df.words$s1_value[df.words$subject == subj & df.words$word == df.s1$alt[i]]
  choice = df.s1$choice[i]
  
  df.s1$correct_choice[i] = ifelse(length(v2) > 0, ifelse(v1 == v2, 1, ifelse(v1 > v2, choice == 0, choice == 1)), -1)
}

df.s1.subj = df.s1 %>% group_by(subject) %>%
  summarize(pctCorrect_choice = mean(correct_choice, na.rm = T), numTrials = n())


# compute exclusion -------------------------------------------------------

df.s2.excl = df.s2 %>% filter(subject %in% subjlist) %>%
  group_by(subject) %>%
  summarize(comp_check_pass = mean(comp_check_pass[question_order == 0]),
            comp_check_pass2 = mean(comp_check_pass2[question_order == 0]),
            numNAs = sum(is.na(choice_real)),
            numRepeats = sum(choice_real == lag(choice_real), na.rm = T),
            numTrials = n(),
            cond = cond[1])

# recall

recalled = matrix(F, nrow = nrow(df.s2.excl), ncol = numWords)
recalled_ever = matrix(F, nrow = nrow(df.s2.excl), ncol = numWords)
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
  numWords_temp = ifelse(df.s2.temp.mem$cond == 'large', numWords[1], numWords[2])
  
  if (length(wordlist) == numWords_temp) {
    for (j in 1:numWords_temp) {
      which_word = amatch(wordlist[j], words_temp, maxDist = 2, nomatch = 0)
      recalled[i,j] = which_word > 0
      
      df.words$recall[df.words$subject == subj.name & df.words$word == wordlist[j]] = recalled[i,j]
      
      recalled_ever[i,j] = recalled[i,j] | any(na.omit(df.s2.temp$choice_real_ind) == j)
      df.words$recall.ever[df.words$subject == subj.name & df.words$word == wordlist[j]] = recalled_ever[i,j]
      
      df.words$order[df.words$subject == subj.name & df.words$word == wordlist[j]] = which_word
    }
  }
}

# exclusion

include_rows = NULL
include_names = NULL

for (subj in 1:length(subjlist)) {
  subj.name = subjlist[subj]
  df.s1.subj.temp = df.s1.subj %>% filter(subject == subj.name)
  df.s2.subj.temp = df.s2.excl %>% filter(subject == subj.name)
  df.demo.temp = df.demo %>% filter(subject == subj.name)
  recalled.subj = recalled[subj,]
  
  exclude = df.demo.temp$write_down == 'Yes' || df.s2.subj.temp$comp_check_pass < 1 ||
    df.s2.subj.temp$numNAs > minNAs || df.s2.subj.temp$numTrials != numQuestions ||
    df.s1.subj.temp$numTrials != ifelse(df.s2.subj.temp$cond == 'large', numTrials[1], numTrials[2]) ||
    df.s1.subj.temp$pctCorrect_choice < .7 || any(!recalled.subj[c(1,2,3)]) #|| !any(recalled.subj[5:12])
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
  df.words$high_s1value[i] = df.words$s1_value[i] > median(s1_valuelist)
  df.words$rank_s1value[i] = s1_valuelist_rank[df.words$word_ind[i] + 1]

  s2_valuelist = (df.words %>% filter(subject == subj))$s2_value
  s2_valuelist_rank = rank(s2_valuelist, ties.method = 'max')
  df.words$high_s2value[i] = df.words$s2_value[i] > median(s2_valuelist)
  df.words$rank_s2value[i] = s2_valuelist_rank[df.words$word_ind[i] + 1]
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
  df.s2$rank_s1value[i] = ifelse(is.na(cind), NA, df.words$rank_s1value[word_rows])
  
  df.s2$s2_value[i] = ifelse(is.na(cind), NA, df.words$s2_value[word_rows])
  df.s2$high_s2value[i] = ifelse(is.na(cind), NA, df.words$high_s2value[word_rows])
  df.s2$rank_s2value[i] = ifelse(is.na(cind), NA, df.words$rank_s2value[word_rows])
  
  s2_valuelist = (df.words %>% filter(subject == subj.name))$s2_value
  df.s2$median_value[i] = median(s2_valuelist)
}

df.s2 = df.s2 %>% mutate(s2_subj_ind = as.numeric(as.factor(subject)), # just for modeling
                         bonus_value = ifelse(is.na(choice_real_ind), 0, s2_value))

df.s2.filt = df.s2 %>% filter(subject %in% include_names & question_order == 0) %>%
  mutate(choice_type = factor(choice_real_ind, c(1,2,3,4:11), c('high_s1', 'low_s1', 'high_s2', rep('distractor', 8))))

df.words.filt = df.words %>% filter(subject %in% include_names)
df.demo.filt = df.demo %>% filter(subject %in% include_names)
recalled_ever.filt = recalled_ever[include_rows,]

# check out data ----------------------------------------------------------

# s2 rank value
ggpiestats(df.s2.filt, high_s2value)
gghistostats(df.s2.filt, rank_s2value, test.value = 6.5, centrality.para = 'mean', type = 'np')

# s1 high value
ggpiestats(data = df.s2.filt, main = high_s1value)
gghistostats(df.s2.filt, s1_value)
grouped_ggpiestats(cond, data = df.s2.filt, main = high_s1value)
wilcox.test(df.s2.filt$rank_s1value, mu = 7)

# it's (1,3,2,...) b/c I accidentally put low_s1 before mid_s1 in the javascript
df.graph = df.s2.filt %>%
  group_by(cond) %>% summarize(high_s1 = mean(choice_type == 'high_s1'), high_s1.se = sqrt(high_s1 * (1-high_s1) / n()),
                               #mid_s1 = mean(choice_type == 'mid_s1'), mid_s1.se = sqrt(mid_s1 * (1-mid_s1) / n()),
                               low_s1 = mean(choice_type == 'low_s1'), low_s1.se = sqrt(low_s1 * (1-low_s1) / n()),
                               high_s2 = mean(choice_type == 'high_s2'), high_s2.se = sqrt(high_s2 * (1-high_s2) / n()),
                               distractor = mean(choice_type == 'distractor'), distractor.se = sqrt(distractor * (1-distractor) / n()))
df.graph$distractor = df.graph$distractor / ifelse(df.graph$cond == 'large', 8, 1)

df.graph2 = data.frame(choice = NULL, type = NULL, cond = NULL)
df.graph2 = rbind(df.graph2, data.frame(choice = df.graph$high_s1[1], se = df.graph$high_s1.se[1], type = 'high_s1', cond = 'small'))
#df.graph2 = rbind(df.graph2, data.frame(choice = df.graph$mid_s1[1], se = df.graph$mid_s1.se[1], type = 'mid_s1', cond = 'small'))
df.graph2 = rbind(df.graph2, data.frame(choice = df.graph$low_s1[1], se = df.graph$low_s1.se[1], type = 'low_s1', cond = 'small'))
df.graph2 = rbind(df.graph2, data.frame(choice = df.graph$high_s2[1], se = df.graph$high_s2.se[1], type = 'high_s2', cond = 'small'))
df.graph2 = rbind(df.graph2, data.frame(choice = df.graph$distractor[1], se = df.graph$distractor.se[1], type = 'distractor', cond = 'small'))
df.graph2 = rbind(df.graph2, data.frame(choice = df.graph$high_s1[2], se = df.graph$high_s1.se[2], type = 'high_s1', cond = 'large'))
#df.graph2 = rbind(df.graph2, data.frame(choice = df.graph$mid_s1[2], se = df.graph$mid_s1.se[2], type = 'mid_s1', cond = 'large'))
df.graph2 = rbind(df.graph2, data.frame(choice = df.graph$low_s1[2], se = df.graph$low_s1.se[2], type = 'low_s1', cond = 'large'))
df.graph2 = rbind(df.graph2, data.frame(choice = df.graph$high_s2[2], se = df.graph$high_s2.se[2], type = 'high_s2', cond = 'large'))
df.graph2 = rbind(df.graph2, data.frame(choice = df.graph$distractor[2], se = df.graph$distractor.se[2], type = 'distractor', cond = 'large'))

ggplot(df.graph2 %>% mutate(type = factor(type,
                                          c('high_s1', 'low_s1', 'high_s2', 'distractor'),
                                          c('High S1', 'Low S1', 'High S2', 'Distractor'))),
       aes(x = type, y = choice, fill = cond, group = cond)) +
  geom_bar(stat = 'identity', position = position_dodge()) +
  geom_errorbar(aes(ymin = choice - se, ymax = choice + se), width = .2,
                position = position_dodge(.9)) +
  xlab('Option') +
  ylab('Prob. of choosing option') +
  guides(fill = guide_legend(title = "Condition"))

fn = function(df.in, ind) {
  df = df.in[ind,] %>%
    summarize(high_s1 = mean(choice_type == 'high_s1'), low_s1 = mean(choice_type == 'low_s1'),
              high_s2 = mean(choice_type == 'high_s2'), distractor = mean(choice_type == 'distractor')) %>%
    mutate(distractor = distractor / (8*(cond == 'large') + 1*(cond == 'small'))) %>%
    select(high_s1, low_s1, high_s2, distractor, cond)
  
  df[df == 0] = 1 / nrow(df.in)
  lod.high_s1 = log(df$high_s1[2] / (1 - df$high_s1[2])) - log(df$high_s1[1] / (1 - df$high_s1[1]))
  # lod.mid_s1 = log(df$mid_s1[2] / (1 - df$mid_s1[2])) - log(df$mid_s1[1] / (1 - df$mid_s1[1]))
  lod.low_s1 = log(df$low_s1[2] / (1 - df$low_s1[2])) - log(df$low_s1[1] / (1 - df$low_s1[1]))
  lod.high_s2 = log(df$high_s2[2] / (1 - df$high_s2[2])) - log(df$high_s2[1] / (1 - df$high_s2[1]))
  # lod.distractor = log(df$distractor[2] / (1 - df$distractor[2])) - log(df$distractor[1] / (1 - df$distractor[1]))
  
  #m_highS1 = glm(chose_highS1 ~ cond, data = df, family = 'binomial')
  #m_lowS1 = glm(chose_lowS1 ~ cond, data = df, family = 'binomial')
  
  stat = lod.high_s1 - lod.low_s1
  #stat = (df$high_s1[2] - df$high_s1[1]) - (df$low_s1[2] - df$low_s1[1])
  
  while (is.infinite(stat)) {
    stat = fn(df.in, sample(nrow(df.in), nrow(df.in), replace = T))
  }
  
  return(stat)
}

getCI = function(df, stat.fn) {
  bs = boot(df, stat.fn, 1000)
  bs.ci = boot.ci(bs)
  return(bs.ci$bca[c(4,5)])
}

getCI(df.s2.filt %>% group_by(cond), fn)

stat = vector(mode = 'integer', length = 10000)
for (i in 1:10000) {
  df.s2.filt.temp = df.s2.filt
  df.s2.filt.temp$cond = sample(df.s2.filt$cond)
  stat[i] = fn(df.s2.filt.temp %>% group_by(cond), 1:nrow(df.s2.filt.temp))
}

true.stat = fn(df.s2.filt %>% group_by(cond), 1:nrow(df.s2.filt))
quantile(stat, .95)
pctile = ecdf(stat)
pctile(true.stat)

# power analysis
n_power_bs = 100
power_ss = 200
sig = logical(n_power_bs)
cis = matrix(nrow = n_power_bs, ncol = 2)
pctiles = matrix(nrow = n_power_bs, ncol = 1)

getPerm = function(df, stat.fn) {
  stat = vector(mode = 'integer', length = 1000)
  for (i in 1:1000) {
    df.temp = df
    df.temp$cond = sample(df$cond)
    stat[i] = stat.fn(df.temp %>% group_by(cond), 1:nrow(df.temp))
  }
  
  true.stat = stat.fn(df %>% group_by(cond), 1:nrow(df))
  pctile = ecdf(stat)
  return(pctile(true.stat))
}

for (i in 1:n_power_bs) {
  ind = sample(1:nrow(df.s2.filt), power_ss, replace = T)
  pct = getPerm(df.s2.filt[ind,] %>% group_by(cond), fn)
  sig[i] = pct > .95
  pctiles[i,] = pct
}
mean(sig)

# single logistic regressions

df.analysis = df.s2.filt %>%
  mutate(chose_highS1 = choice_type == 'high_s1', chose_lowS1 = choice_type == 'low_s1', 
         chose_highS2 = choice_type == 'high_s2')
m_highS2 = glm(chose_highS2 ~ cond, data = df.analysis, family = 'binomial')
summary(m_highS2)
m_highS1 = glm(chose_highS1 ~ cond, data = df.analysis, family = 'binomial')
summary(m_highS1)
m_lowS1 = glm(chose_lowS1 ~ cond, data = df.analysis, family = 'binomial')
summary(m_lowS1)
m_lowS1_null = glm(chose_lowS1 ~ 1, data = df.analysis, family = 'binomial')
summary(m_lowS1)


# distractor-analysis -----------------------------------------------------

df.s2.filt.distr = df.s2 %>% filter(subject %in% include_names & question_order == 0) %>%
  mutate(choice_type = factor(choice_real_ind, c(1,2,3,4:7,8:11), c('high_s1', 'low_s1', 'high_s2', rep('distractor_lo', 4), rep('distractor_hi', 4))))

# it's (1,3,2,...) b/c I accidentally put low_s1 before mid_s1 in the javascript
df.graph = df.s2.filt.distr %>%
  group_by(cond) %>% summarize(high_s1 = mean(choice_type == 'high_s1'), high_s1.se = sqrt(high_s1 * (1-high_s1) / n()),
                               low_s1 = mean(choice_type == 'low_s1'), low_s1.se = sqrt(low_s1 * (1-low_s1) / n()),
                               high_s2 = mean(choice_type == 'high_s2'), high_s2.se = sqrt(high_s2 * (1-high_s2) / n()),
                               distractor_hi = mean(choice_type == 'distractor_hi'), distractor_hi.se = sqrt(distractor_hi * (1-distractor_hi) / n()),
                               distractor_lo = mean(choice_type == 'distractor_lo'), distractor_lo.se = sqrt(distractor_lo * (1-distractor_lo) / n()))
#df.graph$distractor = df.graph$distractor / ifelse(df.graph$cond == 'large', 4, 1)

df.graph2 = data.frame(choice = NULL, type = NULL, cond = NULL)
df.graph2 = rbind(df.graph2, data.frame(choice = df.graph$high_s1[1], se = df.graph$high_s1.se[1], type = 'high_s1', cond = 'small'))
#df.graph2 = rbind(df.graph2, data.frame(choice = df.graph$mid_s1[1], se = df.graph$mid_s1.se[1], type = 'mid_s1', cond = 'small'))
df.graph2 = rbind(df.graph2, data.frame(choice = df.graph$low_s1[1], se = df.graph$low_s1.se[1], type = 'low_s1', cond = 'small'))
df.graph2 = rbind(df.graph2, data.frame(choice = df.graph$high_s2[1], se = df.graph$high_s2.se[1], type = 'high_s2', cond = 'small'))
df.graph2 = rbind(df.graph2, data.frame(choice = df.graph$distractor_hi[1], se = df.graph$distractor_hi.se[1], type = 'distractor_hi', cond = 'small'))
df.graph2 = rbind(df.graph2, data.frame(choice = df.graph$distractor_lo[1], se = df.graph$distractor_lo.se[1], type = 'distractor_lo', cond = 'small'))
df.graph2 = rbind(df.graph2, data.frame(choice = df.graph$high_s1[2], se = df.graph$high_s1.se[2], type = 'high_s1', cond = 'large'))
#df.graph2 = rbind(df.graph2, data.frame(choice = df.graph$mid_s1[2], se = df.graph$mid_s1.se[2], type = 'mid_s1', cond = 'large'))
df.graph2 = rbind(df.graph2, data.frame(choice = df.graph$low_s1[2], se = df.graph$low_s1.se[2], type = 'low_s1', cond = 'large'))
df.graph2 = rbind(df.graph2, data.frame(choice = df.graph$high_s2[2], se = df.graph$high_s2.se[2], type = 'high_s2', cond = 'large'))
df.graph2 = rbind(df.graph2, data.frame(choice = df.graph$distractor_hi[2], se = df.graph$distractor_hi.se[2], type = 'distractor_hi', cond = 'large'))
df.graph2 = rbind(df.graph2, data.frame(choice = df.graph$distractor_lo[2], se = df.graph$distractor_lo.se[2], type = 'distractor_lo', cond = 'large'))

ggplot(df.graph2 %>% mutate(type = factor(type,
                                          c('high_s1', 'low_s1', 'high_s2', 'distractor_hi', 'distractor_lo'),
                                          c('High S1', 'Low S1', 'High S2', 'Distractor High', 'Distractor Low'))),
       aes(x = type, y = choice, fill = cond, group = cond)) +
  geom_bar(stat = 'identity', position = position_dodge()) +
  geom_errorbar(aes(ymin = choice - se, ymax = choice + se), width = .2,
                position = position_dodge(.9)) +
  xlab('Option') +
  ylab('Prob. of choosing option') +
  guides(fill = guide_legend(title = "Condition"))


# recall ------------------------------------------------------------------
df.recall = df.words.filt %>%
  mutate(word_ind_1ind = word_ind + 1, word_type = factor(word_ind_1ind, c(1,3,2,4,5:12), c('high_s1', 'mid_s1', 'low_s1', 'high_s2', rep('distractor', 8)))) %>%
  filter(cond == 'small') %>% group_by(word_type) %>%
  summarize(prob.recall = mean(recall.ever), prob.recall.se = sqrt(prob.recall * (1-prob.recall) / n()))
ggplot(df.recall, aes(x = word_type, y = prob.recall)) +
  geom_bar(stat = 'identity', position = position_dodge()) +
  geom_errorbar(aes(ymin = prob.recall - prob.recall.se, ymax = prob.recall + prob.recall.se), width = .2,
                position = position_dodge(.9)) +
  xlab('Option') +
  ylab('Prob. of recalling option')


df.recall = df.words.filt %>%
  mutate(word_ind_1ind = word_ind + 1, word_type = factor(word_ind_1ind, c(1,3,2,4,5:12), c('high_s1', 'mid_s1', 'low_s1', 'high_s2', rep('distractor', 8)))) %>%
  filter(cond == 'large') %>% group_by(word_type, word) %>%
  summarize(prob.recall = mean(recall.ever), prob.recall.se = sqrt(prob.recall * (1-prob.recall) / n()))
ggplot(df.recall, aes(x = word, y = prob.recall, fill = word_type, group = word_type)) +
  geom_bar(stat = 'identity', position = position_dodge()) +
  geom_errorbar(aes(ymin = prob.recall - prob.recall.se, ymax = prob.recall + prob.recall.se), width = .2,
                position = position_dodge(.9)) +
  xlab('Option') +
  ylab('Prob. of recalling option') +
  theme(axis.text.x = element_text(angle=90, hjust = 1, vjust = 0.5))

# chi-squared
df.graph.counts = df.s2.filt %>%
  mutate(choice_type = factor(choice_real_ind, c(1,2,3,4:11), c('high_s1', 'low_s1', 'high_s2', rep('distractor', 8)))) %>%
  group_by(cond) %>% summarize(high_s1 = sum(choice_type == 'high_s1'),
                               low_s1 = sum(choice_type == 'low_s1'),
                               high_s2 = sum(choice_type == 'high_s2'),
                               distractor = sum(choice_type == 'distractor'))

df.graph.counts.1 = df.graph.counts %>%
  mutate(other = high_s1 + low_s1 + distractor) %>% select(high_s1, low_s1) %>%
  data.matrix
chisq.test(df.graph.counts.1)

# logit -------------------------------------------------------------------


df.logit = data.frame(Subj = NULL, Trial = NULL, OptionID = NULL, Choice = NULL, MFval = NULL, MBval = NULL, nExposures = NULL, Recalled = NULL, Question = NULL)

for (subj in 1:nrow(df.demo.filt)) {
  subj.name = df.demo.filt$subject[subj]
  recalled.temp = recalled_ever.filt[subj, ]
  #num.recalled.temp = sum(recalled.temp)
  num.recalled.temp = 5
  
  df.words.temp = df.words.filt %>% filter(subject == subj.name)
  df.s2.temp = df.s2.filt %>% filter(subject == subj.name) %>% arrange(question_order)

  nAnswered = 1
  
  Subj.col = rep(subj, num.recalled.temp * nAnswered)
  Condition.col = rep(df.s2.temp$cond, num.recalled.temp * nAnswered)
  
  OptionID_real.col = rep(which(recalled.temp), nAnswered)
  OptionID.col = rep(1:num.recalled.temp, nAnswered)
  Trial.col = rep(1:nAnswered, each = num.recalled.temp)
  Question.col = rep(df.s2.temp$question_ind[!is.na(df.s2.temp$choice_real_ind)], each = num.recalled.temp)
  
  temp.choice = matrix(0, nrow = nAnswered, ncol = num.recalled.temp)
  ind = 1
  for (q in 1:numRealQuestions) {
    if (!is.na(df.s2.temp$choice_real_ind[q])) {
      #choice = logical(num.recalled.temp)
      #choice[which(df.s2.temp$choice_real_ind[q] == which(recalled.temp))] = TRUE
      choice = logical()
      temp.choice[ind,] = choice
      
      ind = ind + 1
    }
  }
    
  Choice.col = as.vector(t(temp.choice))
  
  df.logit = rbind(df.logit,
                   data.frame(Subj = Subj.col, Trial = Trial.col, OptionID = OptionID_real.col, Choice = Choice.col,
                              Condition = Condition.col))
}

df.logit = df.logit %>% mutate(Trial_unique = paste(Subj, Trial, sep="_"),
                               high_s1 = as.numeric(OptionID == 1),
                               mid_s1 = as.numeric(OptionID == 3),
                               low_s2 = as.numeric(OptionID == 2),
                               high_s2 = as.numeric(OptionID == 4),
                               distractor = as.numeric(OptionID > 4))
df.logit2 = mlogit.data(df.logit, choice = "Choice", shape = "long", id.var = "Subj", alt.var = "OptionID", chid.var = "Trial_unique")

m = mlogit(Choice ~ 1, data = df.logit2)
summary(m)

# bonuses, modeling -----------------------------------------------------------------

## bonuses
recalled_total = recalled
nrecall_bonus = rowSums(recalled_total)
df.s2.subj = df.s2 %>% filter(question_order == 0) %>%
  mutate(s2_bonus = ifelse(is.na(s2_value), 0, s2_value))
for (i in 1:nrow(df.s2.subj)) {
  nrecall_bonus = (df.words %>% filter(subject == df.s2.subj$subject[i]) %>% mutate(nrecall = sum(recall)))$nrecall[1]
  numWords.temp = ifelse(df.s2.subj$cond[i] == 'large', numWords[1], numWords[2])
  if (length(nrecall_bonus) > 0 && !is.na(nrecall_bonus)) {
    df.s2.subj$mem_bonus[i] = nrecall_bonus * pointsPerWord +
      allBonus * (nrecall_bonus == numWords.temp)
  } else {
    df.s2.subj$mem_bonus[i] = 0
  }
  
  s1_bonus = (df.demo %>% filter(subject == df.s2.subj$subject[i]))$s1_bonus
  if (length(s1_bonus) > 0 && !is.na(s1_bonus)) {
    df.s2.subj$s1_bonus[i] = s1_bonus
  } else {
    df.s2.subj$s1_bonus[i] = 0 
  }
}

df.s2.subj = df.s2.subj %>%
  mutate(bonus = round((s1_bonus / pointsPerCent_s1 + s2_bonus / pointsPerCent_s2  + mem_bonus) / 100, 2))
write.table(df.s2.subj %>% dplyr::select(WorkerID = subject, Bonus = bonus),
            paste0(path, 'Bonuses.csv'), row.names = FALSE, col.names = FALSE, sep = ",")

## save
save.image(paste0(path, 'analysis.rdata'))
