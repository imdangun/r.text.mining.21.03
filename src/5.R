package_version(R.version)

setwd('src')

install.packages('ggraph')

library(readr)
library(dplyr)
library(textclean)
library(tidytext)
library(KoNLP)
library(tidyr)
library(stringr)
library(widyr)
library(tidygraph)
library(ggraph)
library(showtext)
library(ggplot2)

raw_news_comment = read_csv('news_comment_parasite.csv')

news_comment = raw_news_comment %>%
  select(reply) %>%
  mutate(reply=str_replace_all(reply, '[^가-힝]', ' '),
         reply=str_squish(reply),
         id=row_number())

comment_pos = news_comment %>%
  unnest_tokens(input=reply, output=word, token=SimplePos22, drop=F)

comment_pos %>% select(word, reply)

comment_pos = comment_pos %>%
  separate_rows(word, sep='[+]')

comment_pos %>% select(word, reply)

noun = comment_pos %>%
  filter(str_detect(word, '/n')) %>%
  mutate(word=str_remove(word, '/.*$'))

noun %>% select(word, reply)

noun %>% count(word, sort=T)

pvpa = comment_pos %>% 
  filter(str_detect(word, '/pv|/pa')) %>% 
  mutate(word=str_replace(word, '/.*$', '다'))

pvpa %>% select(word, reply)

pvpa %>% count(word, sort=T)

comment = bind_rows(noun, pvpa) %>%
  filter(str_count(word) >= 2) %>%
  arrange(id)

comment %>% select(word, reply)

comment_new = comment_pos %>%
  separate_rows(word, sep='[+]') %>%
  filter(str_detect(word, '/n|/pv|/pa')) %>%
  mutate(word=ifelse(str_detect(word, '/pv|/pa'),
                     str_replace(word, '/.*$', '다'),
                     str_remove(word, '/.*$'))) %>%
  filter(str_count(word) >= 2) %>%
  arrange(id)

pair = comment %>%
  pairwise_count(item=word, feature=id, sort=T)

pair

pair %>% filter(item1 == '영화')

pair %>% filter(item1 == '봉준호')

graph_comment = pair %>%
  filter(n >= 25) %>%
  as_tbl_graph()

graph_comment

ggraph(graph_comment) +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label=name))

font_add_google(name='Nanum Gothic', family='nanumgothic')
showtext_auto()

set.seed(1234)

ggraph(graph_comment, layout='fr') +
  geom_edge_link(color='gray50', alpha=0.5) +
  geom_node_point(color='lightcoral', size=5) +
  geom_node_text(aes(label=name), repel=T, size=5, family='nanumgothic') +
  theme_graph()

word_network = function(x) {
  ggraph(x, layout='fr') +
    geom_edge_link(color='gray50', alpha=0.5) +
    geom_node_point(color='lightcoral', size=5) +
    geom_node_text(aes(label=name), repel=T, size=5, family='nanumgothic') +
    theme_graph()
}

set.seed(1234)
word_network(graph_comment)

comment = comment %>%
  mutate(word=ifelse(str_detect(word, '감독') & !str_detect(word, '감독상'), '봉준호', word),
         word=ifelse(word == '오르다', '올리다', word),
         word=ifelse(str_detect(word, '축하'), '축하', word))

pair = comment %>%
  pairwise_count(item=word, feature=id, sort=T)

graph_comment = pair %>%
  filter(n >= 25) %>%
  as_tbl_graph()

set.seed(1234)
word_network(graph_comment)

set.seed(1234)
graph_comment = pair %>%
  filter(n >= 25) %>%
  as_tbl_graph(directed=F) %>%
  mutate(centrality=centrality_degree(),
         group=as.factor(group_infomap()))

graph_comment

set.seed(1234)
ggraph(graph_comment, layout='fr') +
  geom_edge_link(color='gray50', alpha=0.5) +
  geom_node_point(aes(size=centrality, color=group), show.legend=F) +
  scale_size(range=c(5, 15)) +
  geom_node_text(aes(label=name), repel=T, size=5, family='nanumgothic') +
  theme_graph()

graph_comment %>% filter(name == '봉준호')

graph_comment %>%
  filter(group == 4) %>%
  arrange(-centrality) %>%
  data.frame()

graph_comment %>%
  arrange(-centrality)

graph_comment %>%
  filter(group == 2) %>%
  arrange(-centrality) %>%
  data.frame()

new_comment %>%
  filter(str_detect(reply, '봉준호') & str_detect(reply, '대박')) %>%
  select(reply)

news_comment %>%
  filter(str_detect(reply, '박근혜') & str_detect(reply, '블랙리스트')) %>%
  select(reply)

news_comment %>%
  filter(str_detect(reply, '기생충') & str_detect(reply, '조국')) %>%
  select(reply)

word_cors = comment %>%
  add_count(word) %>%
  filter(n >= 20) %>%
  pairwise_cor(item=word, feature=id, sort=T)

word_cors

word_cors %>% filter(item1 == '대한민국')

word_cors %>% filter(item1 == '역사')

target = c('대한민국', '역사', '수상소감', '조국', '박근혜', '블랙리스트')

top_cors = word_cors %>%
  filter(item1 %in% target) %>%
  group_by(item1) %>%
  slice_max(correlation, n=8)

top_cors$item1 = factor(top_cors$item1, levels=target)

ggplot(top_cors, aes(x=reorder_within(item2, correlation, item1),
                     y=correlation, fill=item1)) +
  geom_col(show.legend=F) +
  facet_wrap(~item1, scales='free') +
  coord_flip() +
  scale_x_reordered() +
  labs(x=NULL) +
  theme(text=element_text(family='nanumgothic'))

set.seed(1234)
graph_cors = word_cors %>%
  filter(correlation >= 0.15) %>%
  as_tbl_graph(directed=F) %>%
  mutate(centrality=centrality_degree(), group=as.factor(group_infomap()))

set.seed(1234)
ggraph(graph_cors, layout='fr') +
  geom_edge_link(color='gray50', 
                 aes(edge_alpha=correlation, 
                     edge_width=correlation),
                 show.legend=F) +
  scale_edge_width(range=c(1, 4)) +
  geom_node_point(aes(size=centrality, color=group), show_legend=F) +
  scale_size(range=c(5, 10)) +
  geom_node_text(aes(label=name), repel=T, size=5, family='nanumgothic') +
  theme_graph()

text = tibble(value='대한민국은 민주공화국이다. 대한민국의 주권은 국민에게 있고, 
              모든 권력은 국민으로부터 나온다.')

text %>% unnest_tokens(input=value, output=word, token='ngrams', n=2)

text %>% unnest_tokens(input=value, output=word, token='ngrams', n=3)

text %>% unnest_tokens(input=value, output=word, token='words')

text %>% unnest_tokens(input=value, output=word, token='ngrams', n=1)

comment_new = comment_pos %>%
  separate_rows(word, sep='[+]') %>%
  filter(str_detect(word, '/n|/pv|/pa')) %>%
  mutate(word=ifelse(str_detect(word, '/pv|/pa'),
                     str_replace(word, '/.*$', '다'),
                     str_remove(word, '/.*$'))) %>%
  filter(str_count(word) >= 2) %>%
  arrange(id)

comment_new = comment_new %>%
  mutate(word=ifelse(str_detect(word, '감독') & !str_detect(word, '감독상'), '봉준호', word),
         word=ifelse(word=='오르다', '올리다', word),
         word=ifelse(str_detect(word, '축하'), '축하', word))

comment %>% select(word)

line_comment = comment %>%
  group_by(id) %>%
  summarise(sentence=paste(word, collapse=' '))

line_comment

bigram_comment = line_comment %>%
  unnest_tokens(input=sentence, output=bigram, token='ngrams', n=2)

bigram_comment

bigram_seperated = bigram_comment %>%
  separate(bigram, c('word1', 'word2'), sep=' ')

