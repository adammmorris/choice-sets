require(ggplot2)
require(lme4)
require(lmerTest)
require(mlogit)
require(stringdist)
require(dplyr)

theme_update(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(colour = "black"),
             axis.text=element_text(size=20, colour = "black"), axis.title=element_text(size=18, face = "bold"), axis.title.x = element_text(vjust = 0),
             legend.title = element_text(size = 24, face = "bold"), legend.text = element_text(size = 20), plot.title = element_text(size = 26, face = "bold", vjust = 1))

setwd("~/Me/Psychology/Projects/choicesets/with_sam")
setwd("C:/Users/Jphil/Dropbox/choiceSets/choice-sets/")

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
path = 'data/cs_wg_v3_poss/real1/'

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
df.cors = df.words %>% group_by(subject) %>% filter(value %in% c(0,10)) %>% summarize(cors = cor(numChosen, value))

# get pctCorrects
df.s1.subj = df.s1 %>% group_by(subject) %>% summarize(pctCorrect_words = mean(correct_word), pctCorrect_val = mean(correct_val, na.rm = T), numTrials = n())

# Mutate df.s2
df.s2.prac = df.s2 %>% filter(practice == 1)
df.s2 = df.s2 %>% filter(practice == 0)

df.s2$prompt = toupper(df.s2$prompt)

for (i in 1:nrow(df.s2)) {
  subj.name = df.s2$subject[i]
  wordlist = (df.words %>% filter(subject == subj.name))$word
  c = df.s2$prompt[i]
  creal = wordlist[amatch(c, wordlist, maxDist = 2)]
  cind = getIndex(creal, wordlist)
  word_rows = subj.name == df.words$subject & creal == df.words$word
  s1_val = ifelse(is.na(cind), NA, df.words$value[word_rows])
  
  df.s2$choice_real[i] = creal
  df.s2$choice_real_ind[i] = cind
  df.s2$s1_value[i] = s1_val
  df.s2$s1_exposures[i] = ifelse(is.na(cind) | s1_val == -1, NA, df.words$exposures[word_rows])
  df.s2$s1_chosen[i] = ifelse(is.na(cind) | s1_val == -1, NA, df.words$numChosen[word_rows])
}
df.s2$s1_value = factor(df.s2$s1_value, levels = c(-1, 5, 0, 10), labels = c('absent', 'grey', 'low', 'high'))

#df.s2 = df.s2 %>% mutate(s2_subj_ind = as.numeric(as.factor(subject)))

df.s2.prac.subj = df.s2.prac %>% group_by(subject) %>%
  summarize(correct = mean(correct))
df.s2.subj = df.s2 %>% group_by(subject) %>%
  summarize(pctNA = mean(choice == -1))

## Compute exclusion
# Exclude if any of these: cor in s1 < .75, pctCorrect_words < .75, pctCorrect_pts < .75, pctCorrect_prac < .75, pctNA > .2
include_rows = NULL
include_names = NULL

for (subj in 1:nrow(df.demo)) {
  subj.name = df.demo$subject[subj]
  df.s1.subj.temp = df.s1.subj %>% filter(subject == subj.name)
  df.s2.subj.temp = df.s2.subj %>% filter(subject == subj.name)
  df.cors.temp = df.cors %>% filter(subject == subj.name)
  
  if (df.s1.subj.temp$pctCorrect_words < .75 || df.cors.temp$cors < .75 || df.s1.subj.temp$numTrials != 120 || df.s2.subj.temp$pctNA > .2) {
    include_rows[subj] = FALSE
  } else {
    include_rows[subj] = TRUE
    include_names = c(include_names, subj.name)
  }
}

## Check out data
se = function(x) {return(sd(x) / sqrt(length(x)))}
dodge <- position_dodge(width=0.9)

df.poss = df.s2 %>% filter(choice != -1 & modality == 'possible' & subject %in% include_names) %>%
  mutate(choice = ifelse(choice == 1, 0, 1))

df.halfway = df.poss %>% group_by(subject, cond, s1_value) %>%
  summarize(choice = mean(choice))

df.collapsed = df.halfway %>% group_by(cond, s1_value) %>%
  summarize(choice.mean = mean(choice), choice.se = se(choice))

ggplot(df.collapsed, aes(x = s1_value, y = choice.mean, group = cond, fill = cond)) +
  geom_bar(stat = "identity", position = dodge) +
  geom_errorbar(aes(ymax = choice.mean + choice.se, ymin = choice.mean - choice.se), width = .5, position = dodge) +
  xlab('') + ylab('')

m.simple = lmer(choice ~ cond * s1_value + (1 | subject), data = df.halfway %>% filter(s1_value %in% c('low', 'high')))
summary(m.simple)

m = glmer(choice ~ cond * s1_value + (1 | subject) + (0 + s1_value | subject) + (1 + cond * s1_value | prompt),
          data = df.s2 %>% filter(choice != -1 & modality == 'possible' & s1_value %in% c('low', 'high') & subject %in% include_names),
          family = binomial)
summary(m)

# all modalities
df.poss = df.s2 %>% filter(choice != -1 & subject %in% include_names & s1_value %in% c('low', 'high')) %>%
  mutate(choice = ifelse(choice == 1, 0, 1), modal_type = as.factor(ifelse(modality %in% c('possible', 'could'), 'possibility', 'normative')))

df.halfway = df.poss %>% group_by(subject, cond, modality, s1_value) %>%
  summarize(choice = mean(choice))

df.collapsed = df.halfway %>% group_by(cond, modality, s1_value) %>%
  summarize(choice.mean = mean(choice), choice.se = se(choice))

ggplot(df.collapsed, aes(x = s1_value, y = choice.mean, group = cond, fill = cond)) +
  geom_bar(stat = "identity", position = dodge) +
  geom_errorbar(aes(ymax = choice.mean + choice.se, ymin = choice.mean - choice.se), width = .5, position = dodge) +
  xlab('') + ylab('') + guides(fill = F) +
  facet_wrap(~ modality)

# collapsed
df.halfway = df.poss %>% group_by(subject, cond, modal_type, s1_value) %>%
  mutate(choice = ifelse(modal_type == 'possibility', choice, ifelse(choice == 1, 0, 1)))
  summarize(choice = mean(choice))

m.simple = lmer(choice ~ cond * s1_value * modal_type + (1 | subject), data = df.halfway)
summary(m.simple)

## Get bonuses
df.demo = df.demo %>% mutate(bonus = round(s1_bonus / (pointsPerCent * 100), 2))
write.table(df.demo %>% select(WorkerID = subject, Bonus = bonus),
            paste0(path, 'Bonuses - cs_wg_v3_poss_real1.csv'), row.names = FALSE, col.names = FALSE, sep = ",")

save.image(paste0(path, 'analysis.rdata'))

### SIMULATIONS

df.sim = read.csv('simulations/results/wg_v2/mixture-mf-mb.csv')
m.sim = runLogit(df.sim)
summary(m.sim)


### Jphil graphs  
df.collapsed$cond <- factor(df.collapsed$cond)
df.collapsed$cond  <- factor(c("Reflection","Time Pressure")[df.collapsed$cond])

df.collapsed$s1_value <- factor(df.collapsed$s1_value)
df.collapsed$s1_value <- factor(c(c("Absent Words","Grey Words","Low-Value Words","High-Value Words"))[df.collapsed$s1_value])
df.collapsed$s1_value <- factor(df.collapsed$s1_value, levels=c("Absent Words","Grey Words","High-Value Words","Low-Value Words"))
df.collapsed$cond <- factor(df.collapsed$cond, levels=c("Time Pressure","Reflection"))

blackGreyPalette <- c("#2C3539", "#999999")
blackGreyPalette <- c("#999999","#2C3539")
jphilPalette <- c("darkorange3","lightblue","darkgreen","azure4")


ggplot(df.collapsed, aes(x = cond, y = 100-(choice.mean*100), fill = cond)) +
  geom_bar(stat = "identity", position = dodge) +
  geom_errorbar(aes(ymax = 100-(choice.mean*100) + choice.se*100, ymin = 100-(choice.mean*100) - choice.se*100), width = .5, position = dodge) +
  xlab('') + ylab('% of Words Judged to be Possible to Select') +
  facet_grid(~s1_value) +
  scale_fill_manual(values=blackGreyPalette) +
  theme_bw() +
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,legend.title=element_blank()
    ,legend.position = c(.125,.9)
    ,legend.text=element_text(size=rel(1.75))
    #,axis.title=element_blank()
    ,axis.text.y=element_text(size=rel(1.75))
    ,strip.text = element_text(size=rel(1.7))
    ,axis.text.x=element_blank()
    ,axis.ticks = element_blank()
  )
  
ggplot(df.collapsed, aes(x = s1_value, y = choice.mean, group = cond, fill = cond)) +
  geom_bar(stat = "identity", position = dodge) +
  geom_errorbar(aes(ymax = choice.mean + choice.se, ymin = choice.mean - choice.se), width = .5, position = dodge) +
  xlab('') + ylab('') + 
  facet_wrap(~modality) +
  scale_fill_manual(values=blackGreyPalette) +
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


