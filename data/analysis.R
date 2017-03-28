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

numWords = 20;
numQuestions = 9; # including memory
pointsPerCent = 5;
pointsPerWord = 10; # for memory condition
path = 'data/cs_wg_v2/real4/'

# Load data
df.demo = read.csv(paste0(path, 'demo.csv')) %>% arrange(subject)
df.s1 = read.csv(paste0(path, 's1.csv')) %>% arrange(subject, word_ind)
df.s2 = read.csv(paste0(path, 's2.csv')) %>% arrange(subject)

## Fix DFs

# Mutate df.demo
df.demo = df.demo %>% mutate(total_time_real = total_time / 60000)

# Get wordlist
df.words = df.s1 %>% top_n(numWords) %>% select(word, word_ind) %>% arrange(word_ind)
wordlist = as.character(df.words$word)
subjlist = unique(as.character(df.s2$subject))

# Mutate df.s1
df.s1 = df.s1 %>% mutate(abs_value = abs(value), pos = value > 0, numChosen = 0)

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

df.s2 = df.s2 %>% rowwise() %>%
  mutate(choice_real = wordlist[amatch(choice, wordlist, maxDist = 2)],
             choice_real_ind = getIndex(choice_real, wordlist),
             value = ifelse(is.na(choice_real_ind), 0, as.numeric.vector(all_values)[choice_real_ind]),
             s2_subj_ind = which(subject == subjlist)) %>% # don't use that ind for anything serious
  ungroup() %>% mutate(doubled = ifelse(is.na(choice_real_ind), NA, ifelse(is.na(lead(choice_real_ind)), F, choice_real_ind == lead(choice_real_ind)) |
                         ifelse(is.na(lag(choice_real_ind)), F, choice_real_ind == lag(choice_real_ind))),
                       bonus_value = ifelse(is.na(choice_real_ind), 0, ifelse(doubled, 0, value)))

df.s2$rank_value = NULL
df.s2$num_ties = NULL
for (subj in 1:nrow(df.s2)) {
  df.s2$rank_value[subj] = ifelse(is.na(df.s2$choice_real_ind[subj]), -1, 21 - which(df.s2$value[subj] == sort(unique(as.numeric.vector(df.s2$all_values[subj])), decreasing = T)))
  df.s2$num_ties[subj] = ifelse(is.na(df.s2$choice_real_ind[subj]), -1, sum(df.s2$value[subj] == as.numeric.vector(df.s2$all_values[subj])))
  df.s2$s1_value[subj] = ifelse(is.na(df.s2$choice_real_ind[subj]), NA, df.s1$value[as.character(df.s1$subject) == as.character(df.s2$subject[subj]) & as.character(df.s2$choice_real[subj]) == as.character(df.s1$word)])
  df.s2$s1_exposures[subj] = ifelse(is.na(df.s2$choice_real_ind[subj]), NA, df.s1$exposures[as.character(df.s1$subject) == as.character(df.s2$subject[subj]) & as.character(df.s2$choice_real[subj]) == as.character(df.s1$word)])
  df.s2$s1_chosen[subj] = ifelse(is.na(df.s2$choice_real_ind[subj]), NA, df.s1$numChosen[as.character(df.s1$subject) == as.character(df.s2$subject[subj]) & as.character(df.s2$choice_real[subj]) == as.character(df.s1$word)])
}

df.s2 = df.s2 %>% mutate(pos = s1_value > 0, abs_s1_value = abs(s1_value))

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
sep = rep(',', nrow(df.mem))
sep[3] = '[\n ]+'
sep[9] = '[\n ]+' # SET THIS BY HAND
sep[13] = " "
sep[40] = "\n"
sep[41] = "\n"
sep[59] = "\n"
sep[63] = "\n"
temp = NULL
pctCorrect_words = NULL
pctCorrect_pts = NULL
recalled = matrix(F, nrow = nrow(df.mem), ncol = numWords)
recalled_ever = matrix(F, nrow = nrow(df.mem), ncol = numWords)
df.s1$recall = NULL

for (i in 1:nrow(df.mem)) {
  subj.name = df.demo$subject[i]
  temp[i] = strsplit(df.mem$scratch[i], sep[i])
  
  df.s2.temp = df.s2 %>% filter(as.character(subj.name) == subject)
  
  for (j in 1:numWords) {
    recalled[i,j] = ain(wordlist[j], trimws(temp[i][[1]]), maxDist = 2)
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
      df.s2.subj$numRepeats[i] > 1) { #|| sum(recalled[i,]) < 5) {
    exclude[i] = T
    exclude_names = c(exclude_names, as.character(subj.name))
  } else {
    exclude[i] = F
    include_names = c(include_names, as.character(subj.name))
  }
}
nrecall = rowSums(recalled[!exclude,])
mean(nrecall)
nrecall_bonus = rowSums(recalled)

df.s2.subj = df.s2.subj %>% mutate(mem_bonus = nrecall_bonus[df.mem$subject == subject] * pointsPerWord)

# Test what affected recall
m.recall = glmer(recall ~ abs_value + (0 + abs_value | subject) + (1 | subject) + (1 | word), data = df.s1[!exclude, ], family = binomial)
summary(m.recall)

# Graph relationship to recall
df.s1.collapsed = df.s1 %>% group_by(exposures) %>% summarize(recall = mean(recall, na.rm = T))
plot(df.s1.collapsed$recall ~ df.s1.collapsed$exposures)

## Check out df.s2 stats
hist(df.s2[!(df.s2$subject %in% exclude_names), ]$s1_value, breaks = 15, main = "S1 values of words chosen in S2", xlab = "S1 value")
mean(df.s2[!(df.s2$subject %in% exclude_names), ]$s1_value, na.rm = T)

hist(df.s2[!(df.s2$subject %in% exclude_names), ]$s1_exposures, breaks = 15)
mean(df.s2[!(df.s2$subject %in% exclude_names), ]$s1_exposures, na.rm = T)

hist(df.s2[!(df.s2$subject %in% exclude_names), ]$abs_s1_value, breaks = 15)
mean(df.s2[!(df.s2$subject %in% exclude_names), ]$abs_s1_value, na.rm = T)

hist(df.s1[df.s1$subject %in% include_names, ]$numChosen, breaks = 15)
mean(df.s1[df.s1$subject %in% include_names, ]$numChosen, na.rm = T)
hist(df.s2[!(df.s2$subject %in% exclude_names), ]$s1_chosen, breaks = 15)
mean(df.s2[!(df.s2$subject %in% exclude_names), ]$s1_chosen, na.rm = T)

## Get bonuses
df.demo = df.demo %>% mutate(s2_bonus = I(df.s2.subj$s2_bonus), mem_bonus = I(df.s2.subj$mem_bonus),
                             bonus = round((s1_bonus + s2_bonus + mem_bonus) / (pointsPerCent * 100), 2))
write.table(df.demo %>% filter(id >= 150) %>% select(WorkerID = subject, Bonus = bonus),
          paste0(path, 'Bonuses - cs_wg_v2_real4.csv'), row.names = FALSE, col.names = FALSE, sep = ",")

## Prepare for mlogit
numRealQuestions = numQuestions - 1
df.logit = data.frame(Subj = NULL, Trial = NULL, OptionID = NULL, Choice = NULL, MFval = NULL, MBval = NULL, nExposures = NULL, Recalled = NULL)

for (subj in 1:nrow(df.demo)) {
  subj.name = df.demo$subject[subj]
  #recalled.temp = recalled_ever[subj, ]
  recalled.temp = !logical(numWords)
  num.recalled.temp = sum(recalled.temp)
  df.s1.temp = df.s1 %>% filter(as.character(subj.name) == subject) %>% arrange(word_ind)
  df.s1.temp = df.s1.temp[!duplicated(df.s1.temp$word), ]
  df.s2.temp = df.s2 %>% filter(as.character(subj.name) == subject) %>% arrange(question_order)
  nAnswered = sum(!is.na(df.s2.temp$choice_real_ind))

  if (nAnswered > 0 & (!subj.name %in% exclude_names)) {
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
        mbvals = as.numeric.vector(df.s2.temp$all_values[q])
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
  df = df %>% mutate(MFcent = (abs(MFval) == 5) - mean(abs(MFval) == 5), MBcent = MBval - mean(MBval), Int = MFcent * MBcent)
  df.m = my.mlogit.data(df, choice = "Choice", shape = "long", id.var = "Subj", alt.var = "OptionID", chid.var = "Trial_unique")
  
  m = mlogit(Choice ~ MFcent + MBcent + Int + Recall | -1, df.m, panel = T,
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
df.rank = df.s2 %>% filter(!(subject %in% exclude_names) & rank_value != -1) %>%
  mutate(five = abs(s1_value) == 5) %>%
  group_by(rank_value) %>%
  summarize(numChosen_S2 = n(), numOpps_S2 = mean(num_ties), prob_rank = numChosen_S2 / numOpps_S2 / 584)
df.rank$prob_rank[df.rank$five == FALSE] = df.rank$prob_rank[df.rank$five == FALSE] / 4

base = ggplot(data = df.rank, aes(x = rank_value, y = prob_rank, group = five, colour = five)) +
  geom_point()
base
base + geom_smooth(method = 'lm', formula = y ~ exp(1 * x))
base + geom_smooth(method = 'lm', formula = y ~ choose(x - 1, 10))
#base + stat_function(fun = function(x) {.0091 * exp(.1 * x) - .0266}) +
#  stat_function(fun = function(x) {7.22e-06 * choose(x - 1, 4) - 6.447e-04})

### SIMULATIONS

df.sim = read.csv('simulations/results/wg_v2/mixture-mf-mb.csv')
m.sim = runLogit(df.sim)
summary(m.sim)











## my mlogit.data function
# my.mlogit.data = function (data, choice, shape = c("wide", "long"), varying = NULL, 
#           sep = ".", alt.var = NULL, chid.var = NULL, alt.levels = NULL, 
#           id.var = NULL, opposite = NULL, drop.index = FALSE, ranked = FALSE, 
#           ...) 
# {
#   if (is.null(chid.var)) {
#     chid.name <- "chid"
#     chid.is.variable <- FALSE
#   } else {
#     chid.name <- chid.var
#     chid.is.variable <- ifelse(is.null(data[[chid.var]]), 
#                                FALSE, TRUE)
#   }
# 
#   choice.name <- choice
#   choice <- data[[choice]]
#   
#   alt.name <- alt.var
#   alt.is.variable <- TRUE
#   if (!is.factor(data[[alt.name]])) 
#     data[[alt.name]] <- factor(data[[alt.name]])
#   alt.levels <- levels(data[[alt.name]])
#   J <- length(alt.levels)
#   alt <- data[[alt.name]]
#     
#   n <- nrow(data)/J
#   if (!chid.is.variable) {
#     chid <- rep(1:n, each = J)
#   } else {
#     chid <- data[[chid.name]]
#   }
#   
#   if (!is.logical(data[[choice.name]])) {
#     if (is.factor(choice) && "yes" %in% levels(choice)) 
#       data[[choice.name]] <- data[[choice.name]] == 
#         "yes"
#     if (is.numeric(choice)) 
#       data[[choice.name]] <- data[[choice.name]] != 
#         0
#   }
#   
#   chid <- as.factor(chid)
#   alt <- as.factor(alt)
#   row.names(data) <- paste(chid, alt, sep = ".")
#   
#   chidpos <- which(names(data) == chid.name)
#   altpos <- which(names(data) == alt.name)
#   
#   if (!is.null(id.var)) {
#     idpos <- which(names(data) == id.var)
#     id.var <- as.factor(data[[id.var]])
#   }
#   
#   index <- data.frame(chid = chid, alt = alt)
#   
#   if (!is.null(id.var)) {
#     index <- cbind(index, id = id.var)
#   }
#   
#   rownames(index) <- rownames(data)
#   attr(data, "index") <- index
#   attr(data, "class") <- c("mlogit.data", "data.frame")
#   attr(data, "choice") <- choice.name
#   
#   data
# }