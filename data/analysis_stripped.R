# setup -------------------------------------------------------------------

require(dplyr)
require(ggplot2)
require(ggstatsplot)
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

se = function(x) {return(sd(x, na.rm = T) / sqrt(length(x)))}
dodge <- position_dodge(width=0.9)


# import data -------------------------------------------------------------

numWords = 12;
numTrials = 0;
numQuestions = 3;
minNAs = 1;
path = 'data/value/v3/real5/'
numRealQuestions = 1;
pointsPerCent = 1;
pointsPerWord = 3;
maxRepeats = 0;
allBonus = 25;

# Load data
df.demo = read.csv(paste0(path, 'demo.csv'), stringsAsFactors = F) %>% arrange(subject) %>% mutate(total_time_real = total_time / 60000)
df.words.raw = read.csv(paste0(path, 'words.csv'), stringsAsFactors = F) %>% arrange(subject, word_ind)
df.s2.raw = read.csv(paste0(path, 's2.csv'), stringsAsFactors = F) %>% arrange(subject, question_order)

subjlist = df.demo$subject

# exclusion ---------------------------------------------------------------
df.words = df.words.raw %>%
  mutate(doubled = ifelse(is.na(lead(word)), FALSE, word == lead(word) & subject == lead(subject))) %>%
  filter(doubled == FALSE & subject %in% subjlist)

df.s2 = df.s2.raw %>% filter(subject %in% subjlist)
df.s2$choice = toupper(df.s2$choice)
df.s2$scratch = gsub("[.]", ",", toupper(as.character(df.s2$scratch)))
df.s2$all_values = as.character(df.s2$all_values)

for (i in 1:nrow(df.s2)) {
  subj.name = df.s2$subject[i]
  wordlist = (df.words %>% filter(subject == subj.name))$word
  c = gsub("\n.*","",df.s2$choice[i])
  creal = wordlist[amatch(c, wordlist, maxDist = 2)]
  cind = getIndex(creal, wordlist)
  
  df.s2$choice_real[i] = creal
  df.s2$choice_real_ind[i] = cind
}

df.s2.excl = df.s2 %>% filter(subject %in% subjlist) %>%
  group_by(subject) %>%
  summarize(comp_check_pass = mean(comp_check_pass),
            numNAs = sum(is.na(choice_real)),
            numRepeats = sum(choice_real == lag(choice_real), na.rm = T),
            numTrials = n())

## Compute recalled
recalled = matrix(F, nrow = nrow(df.s2.excl), ncol = numWords)
recalled_ever = matrix(F, nrow = nrow(df.s2.excl), ncol = numWords)
recalled_val = matrix(F, nrow = nrow(df.s2.excl), ncol = numWords)
df.words$recall = NULL
df.words$recall.ever = NULL
df.words$order = NULL

for (i in 1:nrow(df.s2.excl)) {
  subj.name = df.s2.excl$subject[i]
  df.words.temp = df.words %>% filter(subject == subj.name)
  df.s2.temp = df.s2 %>% filter(subject == subj.name & question == 'Memory')
  
  words_temp = trimws(as.string.vector(df.s2.temp$choice))
  
  wordlist = df.words.temp$word
  
  if (length(wordlist) == numWords) {
    for (j in 1:numWords) {
      which_word = amatch(wordlist[j], words_temp, maxDist = 2, nomatch = 0)
      recalled[i,j] = which_word > 0
      
      df.words$recall[df.words$subject == subj.name & df.words$word == wordlist[j]] = recalled[i,j]
      
      recalled_ever[i,j] = recalled[i,j] | any(na.omit(df.s2.temp$choice_real_ind) == j)
      df.words$recall.ever[df.words$subject == subj.name & df.words$word == wordlist[j]] = recalled_ever[i,j]
      
      df.words$order[df.words$subject == subj.name & df.words$word == wordlist[j]] = which_word
    }
  }
}

# Exclude if any of these: cor in s1 < .75, comp_check_pass < .5, pctCorrect_words < .75, pctCorrect_pts < .75, numNAs > 3, numRepeats > 2, numRecalled < 5
include_rows = NULL
include_names = NULL

for (subj in 1:length(subjlist)) {
  subj.name = subjlist[subj]
  df.s2.subj.temp = df.s2.excl %>% filter(subject == subj.name)
  df.demo.temp = df.demo %>% filter(subject == subj.name)

  exclude = df.demo.temp$write_down == 'Yes' || df.s2.subj.temp$comp_check_pass < .5 || df.s2.subj.temp$numRepeats > maxRepeats ||
    df.s2.subj.temp$numNAs > minNAs || sum(recalled[subj,]) < 5 || df.s2.subj.temp$numTrials != numQuestions
  
  if (exclude) {
    include_rows[subj] = FALSE
  } else {
    include_rows[subj] = TRUE
    include_names = c(include_names, subj.name)
  }
}



# data manipulation -------------------------------------------------------

## words

for (i in 1:nrow(df.words)) {
  subj = df.words$subject[i]
  df.s2.temp = df.s2 %>% filter(subject == subj & question_order == 0)
  
  s1_valuelist = (df.words %>% filter(subject == subj))$s1_value
  s1_valuelist_rank = rank(s1_valuelist, ties.method = 'max')
  s1_valuelist_indiv = (df.words %>% filter(subject == subj & recall.ever == 1 & word_ind != (df.s2.temp$choice_real_ind - 1)))$s1_value
  df.words$high_s1value[i] = df.words$s1_value[i] > median(s1_valuelist)
  df.words$rank_s1value[i] = s1_valuelist_rank[df.words$word_ind[i] + 1]
  df.words$high_s1value_indiv[i] = df.words$s1_value[i] > median(s1_valuelist_indiv)

  s2_valuelist = (df.words %>% filter(subject == subj))$s2_value
  s2_valuelist_rank = rank(s2_valuelist, ties.method = 'max')
  s2_valuelist_indiv = (df.words %>% filter(subject == subj & recall.ever == 1 & word_ind != (df.s2.temp$choice_real_ind - 1)))$s2_value
  df.words$high_s2value[i] = df.words$s2_value[i] > median(s2_valuelist)
  df.words$rank_s2value[i] = s2_valuelist_rank[df.words$word_ind[i] + 1]
  df.words$high_s2value_indiv[i] = df.words$s2_value[i] > median(s2_valuelist_indiv)
}

df.words.filt = df.words %>% filter(subject %in% include_names)

## s2
for (i in 1:nrow(df.s2)) {
  subj.name = df.s2$subject[i]
  wordlist = (df.words %>% filter(subject == subj.name))$word
  s1_valuelist = (df.words %>% filter(subject == subj.name))$s1_value
  s1_valuelist_rank = rank(s1_valuelist, ties.method = 'max')
  s2_valuelist = (df.words %>% filter(subject == subj.name))$s2_value
  s2_valuelist_rank = rank(s2_valuelist, ties.method = 'max')
  
  cind = df.s2$choice_real_ind[i]
  creal = df.s2$choice_real[i]
  
  word_rows = subj.name == df.words$subject & creal == df.words.filt$word
  
  
  df.s2$s1_value[i] = ifelse(is.na(cind), NA, df.words$s1_value[word_rows])
  df.s2$high_s1value[i] = ifelse(is.na(cind), NA, df.words$high_s1value[word_rows])
  df.s2$high_s1value_indiv[i] = ifelse(is.na(cind), NA, df.words$high_s1value_indiv[word_rows])
  df.s2$rank_s1value[i] = ifelse(is.na(cind), NA, df.words$rank_s1value[word_rows])
  
  df.s2$s2_value[i] = ifelse(is.na(cind), NA, df.words$s2_value[word_rows])
  df.s2$high_s2value[i] = ifelse(is.na(cind), NA, df.words$high_s2value[word_rows])
  df.s2$high_s2value_indiv[i] = ifelse(is.na(cind), NA, df.words$high_s2value_indiv[word_rows])
  df.s2$rank_s2value[i] = ifelse(is.na(cind), NA, df.words$rank_s2value[word_rows])
  
  # df.s2$s2_value[i] = ifelse(is.na(cind) | df.s2$question_ind[i] == 0, NA, s2_val)
  # df.s2$high_s2value[i] = ifelse(is.na(cind) | df.s2$question_ind[i] == 0, NA, s2_val > median(all_vals))
  # df.s2$rank_s2value[i] = ifelse(is.na(cind) | df.s2$question_ind[i] == 0, NA, all_vals_rank[cind])
  
  df.s2$median_value[i] = ifelse(df.s2$question_ind[i] == 0, median(s1_valuelist), median(s2_valuelist))
}

df.s2 = df.s2 %>% mutate(s2_subj_ind = as.numeric(as.factor(subject)), # just for modeling
                         doubled = ifelse(is.na(choice_real_ind), NA, ifelse(is.na(lead(choice_real_ind)), F, choice_real_ind == lead(choice_real_ind)) |
                                            ifelse(is.na(lag(choice_real_ind)), F, choice_real_ind == lag(choice_real_ind))),
                         bonus_value = ifelse(is.na(choice_real_ind), 0, ifelse(doubled, 0, s2_value)),
                         same.early = early1 == early2)

df.s2.subj = df.s2 %>% group_by(subject) %>% summarize(s2_bonus = sum(bonus_value))

df.s2.q1 = df.s2 %>% filter(question_order == 1)
df.s2.filt = df.s2.q1 %>% filter(subject %in% include_names)

# check out data ----------------------------------------------------------

# s2 rank value
ggpiestats(df.s2.filt, high_s2value)
ggpiestats(df.s2.filt, high_s2value_indiv)
gghistostats(df.s2.filt, rank_s2value, test.value = 6, centrality.para = 'mean', type = 'np')

# s1 high value
grouped_ggpiestats(early1, data = df.s2.filt, main = high_s1value)
ggpiestats(data = df.s2.filt, main = high_s1value)
ggpiestats(df.s2.filt, high_s1value_indiv)
gghistostats(df.s2.filt, rank_s1value, test.value = 6, centrality.para = 'mean')

# logit test
df.logit = data.frame(Subj = NULL, Trial = NULL, OptionID = NULL, Choice = NULL, MFval = NULL, MBval = NULL, nExposures = NULL, Recalled = NULL, Question = NULL)

for (subj in 1:nrow(df.demo)) {
  subj.name = df.demo$subject[subj]
  recalled.temp = recalled_ever[subj, ]
  #recalled.temp = !logical(numWords)
  num.recalled.temp = sum(recalled.temp)
  
  df.words.temp = df.words.filt %>% filter(subject == subj.name)
  df.s2.temp = df.s2.filt %>% filter(subject == subj.name) %>% arrange(question_order)
  
  nAnswered = sum(!is.na(df.s2.temp$choice_real_ind))
  
  if (nAnswered > 0 & subj.name %in% include_names) {
    Subj.col = rep(subj, num.recalled.temp * nAnswered)
    Condition.col = rep(df.s2.temp$cond, num.recalled.temp * nAnswered)
    Same.col = rep(df.s2.temp$same.early, num.recalled.temp * nAnswered)
    
    MFval.col = rep(df.words.temp$s1_value[recalled.temp], nAnswered)
    MFhigh.col = rep(df.words.temp$high_s1value_indiv[recalled.temp] * 1, nAnswered)
    MFrank.col = rep(df.words.temp$rank_s1value[recalled.temp] * 1, nAnswered)
    MBval.col = rep(df.words.temp$s2_value[recalled.temp], nAnswered)
    MBhigh.col = rep(df.words.temp$high_s2value_indiv[recalled.temp] * 1, nAnswered)
    MBrank.col = rep(df.words.temp$rank_s2value[recalled.temp] * 1, nAnswered)
    nExposures.col = rep(df.words.temp$exposures[recalled.temp], nAnswered)
    Recalled.col = rep(df.words.temp$recall.ever[recalled.temp] * 1, nAnswered)
    numChosen.col = rep(df.words.temp$numChosen_high[recalled.temp], nAnswered)
    OptionID_real.col = rep(which(recalled.temp), nAnswered)
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
                                MFval = MFval.col, MBval = MBval.col, MFhigh = MFhigh.col, MBhigh = MBhigh.col, 
                                MFrank = MFrank.col, MBrank = MBrank.col, Condition = Condition.col, Same = Same.col))
                                #Recall = Recalled.col, Question = Question.col, OptionID_real = OptionID_real.col))
    
  }
}

#df.logit = df.logit %>% mutate(MFcent = MFhigh - mean(MFhigh), MBcent = MBhigh - mean(MBhigh), Int = MFcent * MBcent,
#                               Choice = as.logical(Choice), Trial_unique = paste(Subj, Trial, sep="_"))

df.logit2 = mlogit.data(df.logit, choice = "Choice", shape = "long", id.var = "Subj", alt.var = "OptionID", chid.var = "Trial_unique")

m = mlogit(Choice ~ MFhigh * MBhigh | -1, df.logit2)#, panel = T,
  #rpar = c(MFcent = "n", MBcent = "n", Int = "n"), correlation = F, halton = NA, R = 1000, tol = .001)

summary(m)

# interaction graph

df.sum = df.logit %>% mutate(MFhigh = factor(MFhigh), MBhigh = factor(MBhigh)) %>%
  group_by(MFhigh,MBhigh,Same,Subj) %>% summarize(Choice = any(Choice)) %>%
  group_by(MFhigh,MBhigh,Same) %>%
  summarize(Choice.mean = mean(Choice), Choice.se = sqrt(Choice.mean * (1 - Choice.mean) / n()))
ggplot(data = df.sum, aes(x = MBhigh, y = Choice.mean, group = MFhigh, colour = MFhigh)) +
  geom_point(aes(size = 2)) + geom_line() +
  geom_errorbar(aes(ymin=Choice.mean - Choice.se, ymax = Choice.mean + Choice.se), width = .2) +
  guides(size = FALSE) + facet_wrap(~ Same)

## recall
nrecall = rowSums(recalled[include_rows,])
mean(nrecall)

df.words.filt = df.words %>% filter(subject %in% include_names)

# plot split by value
df.words.byvalue = df.words.filt %>% group_by(high_value, subject) %>% summarize(recall = mean(recall, na.rm = T)) %>%
  group_by(high_value) %>% summarize(recall.mean = mean(recall, na.rm = T), recall.se = se(recall))
ggplot(df.words.byvalue, aes(x = high_value, y = recall.mean)) +
  geom_bar(stat = "identity", position = dodge) +
  geom_errorbar(aes(ymax = recall.mean + recall.se, ymin = recall.mean - recall.se), width = .5, position = dodge) +
  xlab('') + ylab('') + guides(fill = F)

# did value influence recall?
#m.recall = glmer(recall ~ high_value + (0 + high_value | subject) + (1 | subject) + (1 | word),
#                 data = df.words %>% filter(subject %in% include_names), family = binomial)
m.recall = glmer(recall ~ high_value + (1 | word),
                 data = df.words.filt, family = binomial)
summary(m.recall)

## order effects
histogram(~ order | value, df.words[df.words$subject %in% include_names & df.words$recall == T, ])
m.order = lmer(order ~ high_value + (1 | subject) + (1 | word),
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
df.test = df.s2 %>% group_by(subject) %>% summarize(anyGood = any(!is.na(choice_real_ind)))

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

df.modeling = df.s2 %>% filter(subject %in% include_names & !is.na(choice_real_ind)) %>%
  mutate(all_values_nocomma = gsub(",", " ", all_values)) %>% 
  dplyr::select(s2_subj_ind, choice_real_ind, all_values_nocomma)
write.table(df.modeling, paste0(path, 'choices.csv'), row.names = F, col.names = F, sep=",")

## bonuses
recalled_total = recalled
nrecall_bonus = rowSums(recalled_total)
df.s2.subj = df.s2.subj %>% mutate(mem_bonus = nrecall_bonus * pointsPerWord + allBonus * (nrecall_bonus == 12))
df.demo = df.demo %>% mutate(s2_bonus = I(df.s2.subj$s2_bonus), mem_bonus = I(df.s2.subj$mem_bonus),
                             bonus = round((s1_bonus + s2_bonus + mem_bonus) / (pointsPerCent * 100), 2))
write.table(df.demo %>% select(WorkerID = subject, Bonus = bonus),
            paste0(path, 'Bonuses.csv'), row.names = FALSE, col.names = FALSE, sep = ",")

## save
save.image(paste0(path, 'analysis.rdata'))

# jphil’s stuff -----------------------------------------------------------



## word order raster plot:
cor.test(df.words$order[df.words$recall==TRUE],df.words$value[df.words$recall==TRUE])
plot <- df.words %>%
        filter(recall) %>%
        mutate(high_val = factor(c("Low Past Value","High Past Value")[as.factor(high_value)])) %>%
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
  mutate(high_val = factor(c("Low Past Value","High Past Value")[as.factor(high_value)])) %>%
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

s1 = c(20, 9, 4, 13, 15, 2, 24, 12, 1, 18, 25, 19)
s2 = c(14, 16, 4, 9, 5, 12, 15, 20, 19, 21, 1, 11)

plot(s1,s2)
