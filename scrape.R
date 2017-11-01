require(tidyverse)
require(rvest)
require(stringr)

scrap_mzes_projs <- function(url) {
  read_html(url) ->proj_a
  proj_a %>% html_nodes("div.views-field-field-project-number") %>% html_text %>% str_trim -> proj_num
  proj_a %>% html_nodes("div.research-project-title") %>% html_text %>% str_trim -> proj_title
  proj_a %>% html_nodes("div.research-project-title") %>% html_nodes('a') %>% html_attr('href') -> proj_url
  return(data_frame(proj_num, proj_title, proj_url))
}

allprojs <- map_dfr(c("http://www.mzes.uni-mannheim.de/d7/en/research-areas/a-european-societies-and-their-integration", "http://www.mzes.uni-mannheim.de/d7/en/research-areas/b-european-political-systems-and-their-integration"), scrap_mzes_projs)

scrap_mzes_proj_members <- function(proj_url) {
    print(proj_url)
    proj_html <- read_html(paste0('http://www.mzes.uni-mannheim.de', proj_url))
    proj_html %>% html_nodes('a') %>% html_attr('href') %>% grep('/profiles/', x = ., value = TRUE) %>% str_extract("/[a-zA-Z\\-]+$") %>% unique -> member
    Sys.sleep(3) ## Don't ddos mzes web server
    return(data_frame(proj_url, member))
}

allmembers <- map_dfr(allprojs$proj_url, scrap_mzes_proj_members)

#allmembers %>% group_by(member) %>% tally %>% arrange(desc(n))

allmembers %>% left_join(allprojs, by="proj_url") %>% mutate(member = gsub("^/", "", member)) -> mzes_proj_members


saveRDS(mzes_proj_members, "mzes_proj_members.RDS")
write.csv(mzes_proj_members, "mzes_proj_members.csv")

