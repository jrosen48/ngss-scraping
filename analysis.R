library(tidyverse)
library(rvest)

base_url <- "https://www.nextgenscience.org"

pages_with_standards <- 1:34 %>% 
  str_c(base_url, "/search-standards?page=", .)

f <- function(url) {
  
  lego_movie <- read_html(url)
  
  rating <- lego_movie %>% 
    html_nodes("a") %>%
    html_attr("href")
  
  rating
}

l <- pages_with_standards %>% 
  map(f)

l <- unlist(l)

all_the_pes <- str_sub(l, start = 1, end = 4) %>% 
  str_detect("/pe/")

ls <- l[all_the_pes]

ls <- ls[!is.na(ls)]

ls <- str_c(base_url, ls)

get_the_dimensions <- function(url) {
  read_html(url) %>% 
    html_nodes("h3 a") %>% 
    html_text() %>% 
    str_trim() %>% 
    unique()
}

spec_dimensions <- ls %>% 
  map(get_the_dimensions)

t <- tibble(url = ls,
       dimensions = spec_dimensions)

write_rds(t, 'standards-data.rds')

t %>% 
  unnest(dimensions) %>% 
  select(dimensions) %>% 
  filter(dimensions != "" & !is.na(dimensions)) %>% 
  mutate(dimensions = str_replace_all(dimensions, "\n", ""),
         dimensions = str_replace_all(dimensions, "  ", " "),
         dimensions = tolower(dimensions)) %>% 
  distinct() %>% 
  arrange(dimensions) %>% 
  pull()
 
str_re
