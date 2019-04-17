require(tidyverse)
require(httr)
require(rvest)
filter = dplyr::filter

# setwd('~/Documents/data/gov.uk-international-announcements/')
setwd('/mnt/data/gov.uk-international-announcements/')

queries = read_csv('gov.uk-query.csv')

get_page_count = function(u){
  doc = xml2::read_html(u)
  if(length(html_node(doc, '.gem-c-pagination__link-label')) > 0){
    return(html_node(doc, '.gem-c-pagination__link-label') %>% html_text() %>% 
             str_extract('[0-9]+$') %>% as.numeric())
  }
  html_node(doc, '.page-numbers') %>% html_text() %>% 
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
  set = queries$category[k]
  message(set)
  N = get_page_count(u)
  message('N = ', N)
  if(is.na(N)) N = 1
  for(i in 1:N){
    url = ifelse(N > 1, paste0(u, '&page=', i), u)
    dom0 = GET(url)
    dom = read_html(dom0)
    items = html_nodes(dom, '.document')
    if(length(items) == 0) items = html_nodes(dom, '.document-row')
    if(length(items) > 0) dat[[paste(set,i)]] = map_df(items, parse_item) %>% mutate(query = queries$category[i])
    if(N > 200 & length(dat) %% 100 == 0){
      message(i)
      save(dat, file = 'dat.Rdata')
    }
    Sys.sleep(.2)
  }
  save(dat, file = 'dat.Rdata')
}

message('complete')
