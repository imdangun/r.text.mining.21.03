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

install.packages('remotes')
remotes::install_github('haven-jeon/KoNLP',
                        upgrade='never',
                        INSTALL_opts=c('--no-multiarch'),
                        force=T)
library(KoNLP)
useNIADic()

library(dplyr)

text = tibble(value=c('대한민국은 민주공화국이다.',
                      '대한민국의 주권은 국민에게 있고, 모든 권력은 국민으로부터 나온다.'))
text

extractNoun(text$value)

library(tidytext)
text %>% unnest_tokens(input=value, output=word, token=extractNoun)
