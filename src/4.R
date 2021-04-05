setwd('src')

install.packages('textclean')

library(dplyr)
library(readr)
library(stringr)
library(tidytext)
library(textclean)
library(ggplot2)
library(tidyr)

dic = read_csv('knu_sentiment_lexicon.csv')

dic %>% filter(polarity == 2) %>% arrange(word)

dic %>% filter(polarity == -2) %>% arrange(word)

dic %>% filter(word %in% c('좋은', '나쁜'))

dic %>% filter(word %in% c('기쁜', '슬픈'))

dic %>% filter(!str_detect(word, '[가-힣]')) %>% arrange(word)

dic %>% mutate(sentiment=ifelse(polarity >= 1, 'pos', 
                          ifelse(polarity <= -1, 'neg', 'neu'))) %>% count(sentiment)

df = tibble(sentence=c('디자인 예쁘고 마감도 좋아서 만족스럽다.',
                       '디자인은 괜찮다, 그런데 마감이 나쁘고 가격도 비싸다.'))
df

df = df %>% unnest_tokens(input=sentence, output=word, token='words', drop=F)
df %>% print(n=Inf)

df = df %>% left_join(dic, by='word') %>%
  mutate(polarity=ifelse(is.na(polarity), 0, polarity))
df

score_df = df %>%
  group_by(sentence) %>%
  summarise(score = sum(polarity))
score_df

raw_news_comment = read_csv('news_comment_parasite.csv')

news_comment = raw_news_comment %>%
  mutate(id=row_number(), reply=str_squish(replace_html(reply)))

glimpse(news_comment)

word_comment = news_comment %>%
  unnest_tokens(input=reply, output=word, token='words', drop=F)

word_comment %>% select(word, reply)

word_comment = word_comment %>%
  left_join(dic, by='word') %>%
  mutate(polarity=ifelse(is.na(polarity), 0, polarity))

word_comment %>% select(word, polarity)

word_comment = word_comment %>% mutate(sentiment=ifelse(polarity==2, 'pos',
                                                ifelse(polarity==-2, 'neg', 'neu')))
word_comment

word_comment %>% count(sentiment)

top10_sentiment = word_comment %>%
  filter(sentiment != 'neu') %>%
  count(sentiment, word) %>%
  group_by(sentiment) %>%
  slice_max(n, n=10)

ggplot(top10_sentiment, aes(x=reorder(word, n), y=n, fill=sentiment)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label=n), hjust=-0.3) +
  facet_wrap(~ sentiment, scales='free') +
  scale_y_continuous(expand=expansion(mult=c(0.05, 0.15))) +
  labs(x=NULL)
  
score_comment = word_comment %>%
  group_by(id, reply) %>%
  summarise(score=sum(polarity)) %>%
  ungroup()

score_comment %>% select(score, reply)

score_comment %>% select(score, reply) %>% arrange(-score)

score_comment %>% select(score, reply) %>% arrange(score)

score_comment %>% count(score) %>% print(n=Inf)

score_comment = score_comment %>%
  mutate(sentiment=ifelse(score >= 1, 'pos',
                  ifelse(score <= -1, 'neg', 'neu')))

frequency_score = score_comment %>%
  count(sentiment) %>%
  mutate(ratio = n/sum(n) * 100)

frequency_score

ggplot(frequency_score, aes(x=sentiment, y=n, fill=sentiment)) +
  geom_col() +
  geom_text(aes(label=n), vjust=-0.3) +
  scale_x_discrete(limits=c('pos', 'neu', 'neg'))

score_comment %>% count(score) %>% print(n=Inf)

score_comment = score_comment %>%
  mutate(sentiment = ifelse(score >= 1, 'pos',ifelse(score <= -1, 'neg', 'neu')))

score_comment

frequency_score = score_comment %>%
  count(sentiment) %>%
  mutate(ratio = n/sum(n) * 100)

ggplot(frequency_score, aes(x=sentiment, y=n, fill=sentiment)) +
  geom_col() +
  geom_text(aes(label=n), vjust=-0.3) +
  scale_x_discrete(limits=c('pos', 'neu', 'neg'))

df = tibble(country=c('Korea', 'Korea', 'Japen', "Japen"),
            sex=c('M', 'F', 'M', 'F'),
            ratio=c(60, 40, 30, 70))
df

ggplot(df, aes(x=country, y=ratio, fill=sex)) + geom_col()

ggplot(df, aes(x=country, y=ratio, fill=sex)) +
  geom_col() +
  geom_text(aes(label=paste0(ratio, '%')),
            position=position_stack(vjust=0.5))

frequency_score$dummy = 0

ggplot(frequency_score, aes(x=dummy, y=ratio, fill=sentiment)) +
  geom_col() +
  geom_text(aes(label=paste0(round(ratio, 1), '%')),
            position=position_stack(vjust=0.5)) +
            theme(axis.title.x=element_blank(),
                  axis.text.x=element_blank(),
                  axis.ticks.x=element_blank())

comment = score_comment %>%
  unnest_tokens(input=reply, output=word, token='words', drop=F) %>%
  filter(str_detect(word, '[가-힣]') & str_count(word) >= 2)

frequency_word = comment %>% count(sentiment, word, sort=T)

frequency_word %>% filter(sentiment=='pos')

frequency_word %>% filter(sentiment == 'neg')

comment_wide = frequency_word %>%
  filter(sentiment != 'neu') %>%
  pivot_wider(names_from=sentiment, values_from=n, values_fill=list(n=0))

comment_wide

comment_wide = comment_wide %>%
  mutate(log_odds_ratio=log(((pos + 1) / (sum(pos + 1))) / 
                            ((neg + 1) / (sum(neg + 1)))))

comment_wide

top10 = comment_wide %>%
  group_by(sentiment=ifelse(log_odds_ratio > 0, 'pos', 'neg')) %>%
  slice_max(abs(log_odds_ratio), n=10, with_ties=F)
