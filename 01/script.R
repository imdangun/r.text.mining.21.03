getwd()
setwd('01')

Sys.setlocale('LC_CTYPE', 'ko_KR.UTF-8')
raw_moon = readLines('speech.txt', encoding='UTF-8') # UTF-8 이 대문자이어야 한다.
head(raw_moon)

txt = '치킨은!! 맛있다. xyz 정말 맛있다!@#'
txt

install.packages('stringr')
library(stringr)
str_replace_all(string=txt, pattern='[^가-힣]', replacement=' ')

moon = raw_moon %>% str_replace_all('[^가-힣]', ' ')
head(moon)
