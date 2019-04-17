require(tidyverse)

setwd('~/Documents/data/gov.uk-international-announcements/')

load('d.Rdata')
d1 = d

# d %>% 
#   filter(str_detect(title, fixed('speech', ignore_case=T))) %>% 
#   filter(str_detect(title, fixed('foreign sec', ignore_case=T))) %>% 
#   head(1) %>% .[['text']] %>% cat

load('d-orig.Rdata')
d0 = d; rm(d)
d0 = d0 %>% mutate(published = as.Date(published))

table(d0$url %in% d1$url)
table(d0$title %in% d1$title)

d = rbind(mutate(d0, set = 'v1') %>% select(-type), mutate(d1, set = 'v2'))

d %>% ggplot(aes(published, fill = set)) +
  geom_histogram(position = 'dodge')

dv0 = d %>% arrange(desc(set), published) %>% filter(!duplicated(url)) %>% filter(set == 'v1') %>% 
  filter(published >= '2018-01-01') %>% filter(published < '2019-01-01')

View(dv0)

d %>% filter(title == 'DFID statement on the death of Becky Dykes') %>% .[['url']]


x = d %>% count(title, sort =T) %>% filter(n == 2)

f = d %>% filter(title %in% x$title) %>% mutate(url = str_remove(url, '-+[0-9]+$')) %>% 
  count(url) %>% filter(n == 1) %>% .[['url']]

fx = d %>% filter(url %in% f) %>% arrange(title) %>% View

dim(d)
d %>% filter(str_detect(url, '^/government/')) %>% nrow

