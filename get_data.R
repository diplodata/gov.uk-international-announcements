require(tidyverse)
require(httr)
require(rvest)
filter = dplyr::filter

setwd('~/Documents/data/gov.uk-international-announcements/')

# all international announcements
u = 'https://www.gov.uk/government/announcements?announcement_filter_option=all&departments=all&from_date=&keywords=&page=%s&people=all&subtaxons=all&taxons=37d0fa26-abed-4c74-8835-b3b51ae1c8b2&to_date=&world_locations=all'

# defence announcements (checks show all are included in 'international' category)
# u = 'https://www.gov.uk/government/announcements?announcement_filter_option=all&departments=all&from_date=&keywords=&page=%s&people=all&subtaxons=all&taxons=e491505c-77ae-45b2-84be-8c94b94f6a2b&to_date=&world_locations=all'

# DExEU - a very small number not included in international
# u = 'https://www.gov.uk/government/announcements?announcement_filter_option=all&departments=department-for-exiting-the-european-union&from_date=&keywords=&page=%s&people=all&subtaxons=all&taxons=all&to_date=&world_locations=all'


parse_item = function(item){
  item_url = html_node(item, 'a') %>% html_attr('href')
  item_title = html_node(item, 'h3') %>% html_text %>% str_trim
  item_ts = html_node(item, 'time.public_timestamp') %>% html_attr('datetime') %>% 
    substr(1,19) %>% as.POSIXct(format='%Y-%m-%dT%H:%M:%S')
  item_org = html_node(item, '.organisations') %>% html_text %>% str_trim
  item_type = html_node(item, '.display-type') %>% html_text %>% str_trim
  data_frame(published = item_ts, org = item_org, type = item_type, title = item_title, url = item_url)
}

# scrape the results pages

dat = list()

for(i in 1:6){
  message(i)
  url = sprintf(u, as.character(i))
  dom0 = GET(url)
  dom = read_html(dom0)
  items = html_nodes(dom, '.document-row')
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
}

# save(d, file = 'd.Rdata')

d %>% mutate(org = fct_lump(org, 9) %>% fct_infreq %>% fct_rev) %>% ggplot(aes(org)) + geom_bar() + coord_flip()
d %>% mutate(type = fct_infreq(type) %>% fct_rev) %>% ggplot(aes(type)) + geom_bar() + coord_flip()
d %>% mutate(week = lubridate::floor_date(published, 'week')) %>% ggplot(aes(week)) + geom_bar()

