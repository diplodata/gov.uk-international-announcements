require(tidyverse)

setwd('~/Documents/data/gov.uk-international-announcements/')

load('dat.Rdata')
d = bind_rows(dat, .id = 'qry') %>% select(-query) %>% mutate(qry = str_remove(qry, ' [0-9]+$'))
length(unique(d$url))

du = d %>% filter(!duplicated(url))
du %>% count(qry, sort = T) %>% View

d %>% mutate(dup = duplicated(url)) %>% 
  group_by(qry) %>% summarise(true = sum(dup) / length(dup)) %>% arrange(-true)

du %>% count(org, sort = T)

du %>% ggplot(aes(published)) + geom_histogram()

write_csv(du, 'dat.csv')
