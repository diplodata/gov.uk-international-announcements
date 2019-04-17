require(tidyverse)
require(httr)
require(rvest)
filter = dplyr::filter

# setwd('~/Documents/data/gov.uk-international-announcements/')
setwd('/mnt/data/gov.uk-international-announcements/')

queries = read_csv('gov.uk-query.csv')

get_page_count = function(u){
  doc = xml2::read_html(u)
  html_node(doc, '.gem-c-pagination__link-label') %>% html_text() %>% 
    str_extract('[0-9]+$') %>% as.numeric()
}

parse_item = function(item){
  item_url = html_node(item, 'a') %>% html_attr('href')
  item_title = html_node(item, 'a') %>% html_text %>% str_trim
  item_ts = html_node(item, 'time') %>% html_attr('datetime') %>% substr(1,19) %>% as.Date()
  item_org = html_node(item, '.metadata-text-value') %>% html_text %>% str_trim
  tibble(published = item_ts, org = item_org, title = item_title, url = item_url)
}

dat = list()

for(k in 1:nrow(queries)){
  u = queries$url[k]
  message(queries$category[k])
  N = get_page_count(u)
  for(i in 1:N){
    url = paste0(u, '&page=', i)
    dom0 = GET(url)
    dom = read_html(dom0)
    items = html_nodes(dom, '.document')
    dat[[length(dat)+1]] = map_df(items, parse_item) %>% mutate(query = queries$category[i])
    if(length(dat) %% 100){
      message(i)
      save(dat, file = 'dat.Rdata')
    }
    Sys.sleep(.2)
  }
}

save(dat, file = 'dat.Rdata')
message('complete')
