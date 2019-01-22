load('months_confounded/real1/analysis.rdata')

df1 = df.words.filt %>%
  mutate(s1value.fac = cut(s1_value, 2, c('Low', 'High')), chosen.fac = cut(num_chosen, 12, 1:12)) %>%
  group_by(s1value.fac, chosen.fac) %>%
  dplyr::select(s1value.fac, chosen.fac, in.cs)

load('months_confounded/real3/analysis.rdata')

df2 = df.words.filt %>%
  mutate(s1value.fac = cut(s1_value, 2, c('Low', 'High')), chosen.fac = cut(num_chosen, 12, 1:12)) %>%
  group_by(s1value.fac, chosen.fac) %>%
  dplyr::select(s1value.fac, chosen.fac, in.cs)

load('months_freq/v1/real1/analysis.rdata')

df3 = df.words.filt %>% filter(cond == 0) %>%
  mutate(s1value.fac = cut(s1_value, 2, c('Low', 'High')), chosen.fac = cut(num_chosen, 12, 1:12)) %>%
  group_by(s1value.fac, chosen.fac) %>%
  dplyr::select(s1value.fac, chosen.fac, in.cs)

df = rbind(df1,df2,df3)

df.graph = df %>% summarize(in.cs = mean(in.cs, na.rm = T), in.cs.se = sqrt(in.cs * (1-in.cs) / n()))

df.graph.trimmed = df.graph %>% filter(!(chosen.fac %in% c(1,2,3) & s1value.fac == 'High'), !(chosen.fac %in% c(10,11,12) & s1value.fac == 'Low'))

ggplot(df.graph.trimmed, aes(x = chosen.fac, y = in.cs, group = s1value.fac, color = s1value.fac)) +
  geom_point(size = 5) + geom_line() +
  geom_errorbar(aes(ymin = in.cs - in.cs.se, ymax = in.cs+in.cs.se), width = .2) +
  #geom_smooth(method='lm') +
  #xlab('Stage 1 value') + ylab('Prob. in\nconsideration set') +
  #scale_y_continuous(breaks = c(.3,.4), limits = c(.3,.41)) + 
  #scale_x_continuous(breaks = c(1,12)) +
  theme(axis.title = element_text(size = 24))

