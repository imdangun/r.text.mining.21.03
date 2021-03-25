getwd()
setwd('src')

install.packages('tidytext')
install.packages('stringr')
install.packages('dplyr')
install.packages('ggplot2')
install.packages('ggwordcloud')
install.packages('showtext')

library(dplyr)
library(tidytext)
library(stringr)
library(ggplot2)
library(ggwordcloud)
library(showtext)

raw_moon = readLines('speech_moon.txt', encoding='UTF-8') # UTF-8 이 대문자이어야 한다.
head(raw_moon)

txt = '치킨은!! 맛있다. xyz 정말 맛있다!@#'
txt

str_replace_all(string=txt, pattern='[^가-힣]', replacement=' ')

moon = raw_moon %>% str_replace_all('[^가-힣]', ' ')
head(moon)

?str_replace_all

txt = '치킨은  맛있다  정말 맛있다 '
txt

str_squish(txt)

moon = moon %>% str_squish()
head(moon)

moon = as_tibble(moon)
moon

moon = raw_moon %>%
  str_replace_all('[^가-힣]', ' ') %>%
  str_squish() %>%
  as_tibble()

as_tibble(iris)

text = tibble(value='대한민국은 민주공화국이다. 대한민국의 주권은 국민에게 있고, 
              모든 권력은 국민으로부터 나온다.')
text


text %>% unnest_tokens(input=value, output=word, token='sentences')

text %>% unnest_tokens(input=value, output=word, token='words')

text %>% unnest_tokens(input=value, output=word, token='characters')

word_space = moon %>% unnest_tokens(input=value, output=word, token='words')
word_space

# word_space = word_space %>% count(word, sort=T)
# word_space
# 
# str_count('배')
# str_count('사과')
# 
# word_space = word_space %>% filter(str_count(word) > 1)
# word_space

word_space = word_space %>% count(word, sort=T) %>% 
  filter(str_count(word) > 1)

top20 = word_space %>% head(20)
top20

ggplot(top20, aes(x=reorder(word, n), y=n)) + geom_col() 
ggplot(top20, aes(y=reorder(word, n), x=n)) + geom_col()
ggplot(top20, aes(x=reorder(word, n), y=n)) + geom_col() + coord_flip()

ggplot(top20, aes(x=reorder(word, n), y=n)) + geom_col() + coord_flip() +
  geom_text(aes(label=n), hjust=-0.3) +
  labs(title='문재인 대통령 출마 연설문 단어 빈도', x=NULL, y=NULL) +
  theme(title=element_text(size=12))

ggplot(word_space, aes(label=word, size=n)) +
  geom_text_wordcloud(seed = 1234) +
  scale_radius(limits=c(3, NA), range=c(3,30)) +
  scale_color_gradient(low='#66aaf2', high='#004EA1') +
  theme_minimal()

font_add_google(name='Nanum Gothic', family='nanumgothic')
showtext_auto()

ggplot(word_space, aes(label=word, size=n)) +
  geom_text_wordcloud(seed = 1234, family='nanumgothic') +
  scale_radius(limits=c(3, NA), range=c(3,30)) +
  scale_color_gradient(low='#66aaf2', high='#004ea1') +
  theme_minimal()

font_add_google(name='Black Han Sans', family='blackhansans')
showtext_auto()

ggplot(word_space, aes(label=word, size=n)) +
  geom_text_wordcloud(seed = 1234, family='blackhansans') +
  scale_radius(limits=c(3, NA), range=c(3,30)) +
  scale_color_gradient(low='#66aaf2', high='#004ea1') +
  theme_minimal()

font_add_google(name='Gamja Flower', family='gamjaflower')
showtext_auto()

ggplot(top20, aes(x=reorder(word, n), y=n)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label=n), hjust=-0.3) +
  labs(title='문재인 대통령 출마 선언문 단어 빈도', x=NULL, y=NULL) +
  theme(title=element_text(size=12), text=element_text(family='gamjaflower'))
