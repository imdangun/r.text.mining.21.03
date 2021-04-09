setwd('src')

install.packages('tm')
install.packages('topicmodels')
install.packages('scales')

library(readr)
library(dplyr)
library(stringr)
library(textclean)
library(tidytext)
library(KoNLP)
library(tm)
library(topicmodels)
library(scales)
library(ggplot2)

raw_news_comment = read_csv('news_comment_parasite.csv') %>%
  mutate(id=row_number())

news_comment = raw_news_comment %>%
  mutate(reply=str_replace_all(reply, '[^가-힣]', ' '),
         reply=str_squish(reply)) %>%
  distinct(reply, .keep_all=T) %>%
  filter(str_count(reply, boundary('word')) >= 3)

comment = news_comment %>%
  unnest_tokens(input=reply, output=word, token=extractNoun, drop=F) %>%
  filter(str_count(word) > 1) %>%
  group_by(id) %>%
  distinct(word, .keep_all=T) %>%
  ungroup() %>%
  select(id, word)

count_word = comment %>%
  add_count(word) %>%
  filter(n <= 200) %>%
  select(-n)

count_word %>%
  count(word, sort=T) %>%
  print(n=200)

stopword = c('들이', '하다', '하게', '하면', '해서', '이번', '하네',
             '해요', '이것', '니들', '하기', '하지', '한거', '해주',
             '그것', '어디', '여기', '까지', '이거', '하신', '만큼')

count_word = count_word %>%
  filter(!word %in% stopword) %>%
  mutate(word = recode(word, 
                       '자랑스럽습니' = '자랑',
                       '자랑스럽' = '자랑',
                       '자한' = '자유한국당',
                       '문재' = '문재인',
                       '한국의' = '한국',
                       '그네' = '박근혜',
                       '추카' = '축하',
                       '정경' = '정경심',
                       '방탄' = '방탄소년단'))

stopword = tibble(word=c('들이', '하다', '하게', '하면', '해서', '이번', '하네',
                         '해요', '이것', '니들', '하기', '하지', '한거', '해주',
                         '그것', '어디', '여기', '까지', '이거', '하신', '만큼'))

write_csv(stopword, 'stopword.csv')

count_word = count_word %>% filter(!word %in% stopword$word)

count_word = count_word %>% anti_join(stopword, by='word')
#

count_word_doc = count_word %>% count(id, word, sort=T)

dtm_comment = count_word_doc %>%
  cast_dtm(document=id, term=word, value=n)

dtm_comment

as.matrix(dtm_comment[1:8, 1:8])

lda_model = LDA(dtm_comment, k=8, method='Gibbs', control=list(seed=1234))
lda_model

glimpse(lda_model)

term_topic = tidy(lda_model, matrix='beta')
term_topic

term_topic %>% count(topic)

term_topic %>% filter(topic == 1) %>% summarise(sum_beta=sum(beta))

term_topic %>% filter(term == '작품상')

term_topic %>% filter(topic == 6) %>% arrange(-beta)

terms(lda_model, 20) %>% data.frame()

top_term_topic = term_topic %>%
  group_by(topic) %>%
  slice_max(beta, n=10)

ggplot(top_term_topic,
       aes(x=reorder_within(term, beta, topic),
           y=beta,
           fill=factor(topic))) +
  geom_col(show.legend=F) +
  facet_wrap(~ topic, scales='free', ncol=4) +
  coord_flip() +
  scale_x_reordered() +
  scale_y_continuous(n.breaks=4, labels=number_format(accuracy=.01)) +
  labs(x=NULL) +
  theme(text=element_text(family='nanumgothic'))

doc_topic = tidy(lda_model, matrix='gamma')
doc_topic

doc_topic %>% count(topic)

doc_topic %>% filter(document == 1) %>%
  summarise(sum_gamma = sum(gamma))

doc_class = doc_topic %>%
  group_by(document) %>%
  slice_max(gamma, n=1)

doc_class

doc_class$document = as.integer(doc_class$document)

news_comment_topic = raw_news_comment %>%
  left_join(doc_class, by=c('id'='document'))

news_comment_topic %>% select(id, topic)

news_comment_topic %>% count(topic)

news_comment_topic = news_comment_topic %>% na.omit()

doc_topic %>%
  group_by(document) %>%
  slice_max(gamma, n=1) %>%
  count(document) %>%
  filter(n >= 2)

set.seed(1234)
doc_class_unique = doc_topic %>%
  group_by(document) %>%
  slice_max(gamma, n=1) %>%
  slice_sample(n=1)

doc_class_unique %>%
  count(document, sort=T)

top_terms = term_topic %>%
  group_by(topic) %>%
  slice_max(beta, n=6, with_ties=F) %>%
  summarise(term=paste(term, collapse=', '))

