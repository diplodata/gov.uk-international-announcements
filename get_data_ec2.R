require(tidyverse)
require(httr)
require(rvest)
filter = dplyr::filter

setwd('/home/ubuntu/gov.uk-international-announcements')

# load('d.Rdata')

# all international announcements
# u = 'https://www.gov.uk/government/announcements?announcement_filter_option=all&departments=all&from_date=&keywords=&page=%s&people=all&subtaxons=all&taxons=37d0fa26-abed-4c74-8835-b3b51ae1c8b2&to_date=&world_locations=all'
u0 = 'https://www.gov.uk/search/news-and-communications?level_one_taxon=37d0fa26-abed-4c74-8835-b3b51ae1c8b2&order=updated-newest&page='
u1 = '&public_timestamp%5Bfrom%5D=&public_timestamp%5Bto%5D='

# defence announcements (checks show all are included in 'international' category)
# u = 'https://www.gov.uk/government/announcements?announcement_filter_option=all&departments=all&from_date=&keywords=&page=%s&people=all&subtaxons=all&taxons=e491505c-77ae-45b2-84be-8c94b94f6a2b&to_date=&world_locations=all'

# DExEU - a very small number not included in international
# u = 'https://www.gov.uk/government/announcements?announcement_filter_option=all&departments=department-for-exiting-the-european-union&from_date=&keywords=&page=%s&people=all&subtaxons=all&taxons=all&to_date=&world_locations=all'

parse_item = function(item){
  item_url = html_node(item, 'a') %>% html_attr('href')
  item_title = html_node(item, 'a') %>% html_text %>% str_trim
  item_ts = html_node(item, 'time') %>% html_attr('datetime') %>% substr(1,19) %>% as.Date()
  item_org = html_node(item, '.metadata-text-value') %>% html_text %>% str_trim
  tibble(published = item_ts, org = item_org, title = item_title, url = item_url)
}

# scrape the results pages

dat = list()

for(i in 1:547){ #547
  message(i)
  url = paste0(u0, i, u1)
  dom0 = GET(url)
  dom = read_html(dom0)
  items = html_nodes(dom, '.document')
  dat[[i]] = map_df(items, parse_item)
}

# get the html and parse text for each announcement

d = dat %>% bind_rows() %>% mutate(html = NA_character_, text = NA_character_)

for(i in 1:nrow(d)){
  message(i)
  url = paste0('https://www.gov.uk', d$url[i])
  dom0 = GET(url)
  dom = read_html(dom0)
  d$html[i] = html_node(dom, '#content') %>% as.character()
  d$text[i] = html_node(dom, '#content') %>% html_text %>% str_replace_all('\n[ \t]+', '\n') %>% 
    str_replace_all('\n[ \t]+\n', '\n\n') %>% 
    str_replace_all('\n\n+', '\n') %>% str_remove('Share this page(.*\n)+')
  if(is.na(d$org[i])) d$org[i] = html_node(dom, '.app-c-publisher-metadata__definition') %>% 
    html_text %>% str_trim
}

save(d, file = 'd.Rdata')
message('done')

# d %>% mutate(org = fct_lump(org, 9) %>% fct_infreq %>% fct_rev) %>% ggplot(aes(org)) + geom_bar() + coord_flip()
# d %>% mutate(type = fct_infreq(type) %>% fct_rev) %>% ggplot(aes(type)) + geom_bar() + coord_flip()
# d %>% mutate(week = lubridate::floor_date(published, 'week')) %>% ggplot(aes(week)) + geom_bar()
