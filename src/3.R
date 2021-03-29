setwd('src')

library(dplyr)

raw_moon = readLines('speech_moon.txt', encoding='UTF-8')
moon = raw_moon %>% as_tibble() %>% mutate(president='moon')

raw_park = readLines('speech_park.txt', encoding='UTF-8')
park = raw_park %>% as_tibble() %>% mutate(president='park')

bind_speeches = bind_rows(moon, park) %>% select(president, value)
head(bind_speeches)
tail(bind_speeches)

library(stringr)

speeches = bind_speeches %>% 
  mutate(value=str_replace_all(value, '[^가-힣]', ' '), 
          value=str_squish(value))

speeches


library(tidytext)
library(KoNLP)

speeches = speeches %>%
  unnest_tokens(input=value, output=word, token=extractNoun)
speeches

frequency = speeches %>%
  count(president, word) %>%
  filter(str_count(word) > 1)

head(frequency)

df = tibble(x=c(1:100))
df %>% slice_max(x, n=3)

df %>% arrange(desc(x)) %>% head(5)
df %>% top_n(x, n=5) %>% arrange(desc(x))

top10 = frequency %>%
  group_by(president) %>%
  slice_max(n, n=10)
top10

top10 %>% filter(president=='park') %>% print(n=Inf)

df = tibble(x=c('A', 'B', 'C', 'D'), y=c(4, 3, 2, 2))
df %>% slice_max(y, n=3)
df %>% slice_max(y, n=3, with_ties=F)

top10 = frequency %>% group_by(president) %>% slice_max(n, n=10, with_ties=F)
top10

library(ggplot2)
ggplot(top10, aes(x=reorder(word, n), y=n, fill=president)) +
  geom_col() + coord_flip() + facet_wrap(~ president)

ggplot(top10, aes(x=reorder(word, n), y=n, fill=president)) +
  geom_col() + coord_flip() + facet_wrap(~ president, scales='free_y')

top10 = frequency %>% filter(word != '국민') %>% 
  group_by(president) %>% slice_max(n, n=10, with_ties=F)
top10

ggplot(top10, aes(x=reorder(word, n), y=n, fill=president)) +
  geom_col() + coord_flip() + facet_wrap(~ president, scales='free_y')

ggplot(top10, aes(x=reorder_within(word, n, president), y=n, fill=president)) +
  geom_col() + coord_flip() + facet_wrap(~ president, scales='free_y')

ggplot(top10, aes(x=reorder_within(word, n, president), y=n, fill=president)) +
  geom_col() + coord_flip() + facet_wrap(~ president, scales='free_y') + 
  scale_x_reordered() + labs(x=NULL) + theme(text=element_text(family='nanumgothic'))

df_long = frequency %>% group_by(president) %>% slice_max(n, n=10) %>%
  filter(word %in% c('국민', '우리', '정치', '행복'))
df_long

library(tidyr)
df_wide = df_long %>% pivot_wider(names_from=president, values_from=n)
df_wide

df_wide = df_long %>% pivot_wider(names_from=president, values_from=n, values_fill=list(n=0))
df_wide

frequency_wide = frequency %>% pivot_wider(names_from=president, values_from=n, values_fill=list(n=0))
frequency_wide

frequency_wide = frequency_wide %>%
  mutate(ratio_moon=((moon + 1)/(sum(moon + 1))),
         ratio_park=((park + 1)/(sum(park + 1))))
frequency_wide

frequency_wide = frequency_wide %>%
  mutate(odds_ratio = ratio_moon / ratio_park)

frequency_wide %>% arrange(-odds_ratio)

frequency_wide %>% arrange(odds_ratio)

top10 = frequency_wide %>%
  filter(rank(odds_ratio) <= 10 | rank(-odds_ratio) <= 10)
top10

top10 %>% arrange(-odds_ratio) %>% print(n=Inf)

rank(-c(10, 20, 30))

df = tibble(x=c(2, 5, 10))
df %>% mutate(y = rank(x))
df %>% mutate(y = rank(-x))

top10 = top10 %>%
  mutate(president=ifelse(odds_ratio > 1, 'moon', 'park'),
         n=ifelse(odds_ratio > 1, moon, park))
top10

ggplot(top10, aes(x=reorder_within(word, n, president), y=n, fill=president)) +
  geom_col() + coord_flip() + facet_wrap(~ president, scales='free_y') +
  scale_x_reordered() + labs(x=NULL)

speeches_sentence = bind_speeches %>%
  as_tibble() %>%
  unnest_tokens(input=value, output=sentence, token='sentences')
head(speeches_sentence)
tail(speeches_sentence)

speeches_sentence %>%
  filter(president == 'moon' & str_detect(sentence, '복지국가'))

speeches_sentence %>%
  filter(president == 'park' & str_detect(sentence, '행복'))

frequency_wide %>%
  arrange(abs(1 - odds_ratio)) %>%
  head(10)

frequency_wide %>%
  filter(moon >= 5 & park >= 5) %>%
  arrange(abs(1 - odds_ratio)) %>%
  head(10)

frequencey_wide = frequency_wide %>%
  mutate(log_odds_ratio=log(odds_ratio))

frequencey_wide %>% arrange(-log_odds_ratio)
frequencey_wide %>% arrange(log_odds_ratio)

