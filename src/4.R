setwd('src')

install.packages('textclean')

library(dplyr)
library(readr)
library(stringr)
library(tidytext)
library(textclean)
library(ggplot2)

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
  
  