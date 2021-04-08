getwd()
setwd('src')

install.packages('multilinguer')
library(multilinguer)
install_jdk()

install.packages('stringr')
install.packages('hash')
install.packages('tau')
install.packages('Sejong')
install.packages('RSQLite')
install.packages('devtools')
install.packages('textclean')

install.packages('remotes')
remotes::install_github('haven-jeon/KoNLP',
                        upgrade='never',
                        INSTALL_opts=c('--no-multiarch'),
                        force=T)

useNIADic()

library(dplyr)
library(multilinguer)
library(KoNLP)
library(tidytext)
library(stringr)
library(textclean)
library(tibble)
library(ggplot2)
library(showtext)

text = tibble(value=c('대한민국은 민주공화국이다.',
                      '대한민국의 주권은 국민에게 있고, 모든 권력은 국민으로부터 나온다.'))
text

extractNoun(text$value)

text %>% unnest_tokens(input=value, output=word, token=extractNoun)

raw_moon = readLines('speech_moon.txt', encoding='UTF-8')

moon = raw_moon %>% str_replace_all('[^가-힣]', ' ') %>%
  str_squish() %>%
  as_tibble()

word_noun = moon %>%
  unnest_tokens(input=value, output=word, token=extractNoun)
word_noun

word_noun = word_noun %>%
  count(word, sort=T) %>%
  filter(str_count(word) > 1)

top20 = word_noun %>% head(20)

font_add_google(name='Nanum Gothic', family='nanumgothic')
showtext_auto()

ggplot(top20, aes(x=reorder(word, n), y=n)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label=n), hjust=-0.3) +
  labs(x=NULL) +
  theme(text=element_text(family='nanumgothic'))

font_add_google(name='Black Han Sans', family='blackhansans')
showtext_auto()

ggplot(word_noun, aes(labe=word, size=n, col=n)) +
  geom_text_wordcloud(seed=124, family='blackhansans') +
  scale_radius()