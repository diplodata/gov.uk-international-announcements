require(tidyverse)
filter = dplyr::filter

# setwd('~/Documents/data/gov.uk-international-announcements/reports/')
setwd('/mnt/data/gov.uk-international-announcements/reports/')

d = read_csv('../dat.csv')

for(i in 4:nrow(d)){ #nrow(d)
  u = paste0('https://www.gov.uk/api/content', d$url[i])
  tryCatch({
    download.file(u, destfile = paste0(i, '.json'))
  }, error = function(e){ print(as.character(e)) })
  Sys.sleep(1.2)
}

message('done')
