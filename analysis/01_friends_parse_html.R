library(tidyverse)
library(tidytext)
library(rvest)
library(XML)
setwd("~/Documents/Projects/friends-master/season")

files <- list.files(path = ".")

files <- files[-c(96, 166)] 

## test

files[1:100]

# Season 1
files[5] %>%
  XML::htmlTreeParse( useInternal = TRUE) %>%
  xpathApply(., '//p', xmlValue) %>%
  unlist() %>%
  str_trim()
# Season 2
files[30] %>%
  XML::htmlTreeParse( useInternal = TRUE) %>%
  xpathApply(., '//p/text()[preceding-sibling::br and following-sibling::br]', xmlValue) %>%
  unlist() %>%
  str_trim()


cleanText <- function(x){
  # remove unicode characters
  x <- gsub("[^[:alpha:][:blank:][:punct:]0-9]", "", x)
  x <- gsub("Â|â", "", x)
  # remove new lines CHANGED TO SPACE BECAUSE OF EP 1 FORMATTING
  x <- gsub("\n", " ", x)
  # remove back slashes
  x <- gsub('\"', "", x, fixed = TRUE)
  x <- str_trim(x)
  x <- str_to_lower(x)
  return(x)
}

main <- c("chandler", "ross", "monica", "phoebe", "joey", "rachel")

scriptParser <- function(html){
  
  main <- c("chandler", "ross", "monica", "phoebe", "joey", "rachel")
  
  # read in raw html
  raw_html <- XML::htmlTreeParse(html, useInternal = TRUE)
  
  title <- unlist(xpathApply(raw_html, '//title', xmlValue))
  
  # most lines are given by <p></p> elements
 text <- raw_html %>%
  xpathApply(., '//p', xmlValue) %>%
  unlist() %>%
  str_trim()
  
  # accounts for formatting in seasons 2
  if(length(text) < 20 ){
    print(html)
    text <- raw_html %>%
      xpathApply(., '//p/text()[preceding-sibling::br and following-sibling::br]', xmlValue) %>%
      unlist() %>%
      str_trim()
  }
  

  id <-  (gsub(".*?([0-9]{4}).*", "\\1", html)) 
  # account for 2 part episodes
  if(grepl("-", html)) {
    id <-  (gsub(".*?([0-9]{4}\\-[0-9]{4}).*", "\\1", html))
  }
  
  # cleaning
  
  clean_text <- text %>% 
    map(~ cleanText(.x)) 
  
  dat <- tibble(clean_text, id, title) %>%
    unnest(clean_text) %>%
    filter(str_detect(clean_text, "")) %>%
    # determining whether a line is a scene, title, action or person speaking
    mutate(type =   ifelse(str_detect(clean_text, "^(\\[sc)|(\\(at)|(\\[at)"), "scene",      # scenes are on singular lines, enclosed in square brackets annd ending in round
                          ifelse(str_detect(clean_text, "^(\\()|(\\<)"), "action",              # actions are on singular lines, enclosed in round and angularbrackets
                                       ifelse(str_detect(clean_text, "^written"), "written",     # indicates who the episode was written by
                                              ifelse(str_detect(clean_text, "^[a-z](.*):"), "person",  # if doesn't match anything above, and has a semicolon, should be a person speaking
                                                     NA)))))
  # return(dat)

  
  # dataframe for use in renaming names in 207, 208, 210
  name_map <- data.frame(main) %>% t() %>% set_names("chan", "ross", "mnca", "phoe", "joey", "rach")
  
  # splitting lines into speaker where applicable
  clean_dat <- dat %>%
    separate(clean_text, into = c("person", "line"), sep = ":") %>%
    mutate(line = str_trim(line)) %>%
    mutate(person = ifelse(person %in% names(name_map), name_map[person], person)) %>%
    mutate(season = substr(id, 1, 2))
  
  return(clean_dat)
}

# using map to at a glance see any issues when reading in episodes, of course i could just use map_dfr
friends_script <- files %>% map(~ scriptParser(.x))

friends_script <- friends_script %>% bind_rows() #%>% 

# scene identifier is created outside the function for simpler debugging
friends_lines <- friends_script %>%
  # filter(type %in% c("person", "scene")) %>% 
  group_by(id) %>%
  # if there is a scene change.. 
  mutate(scene = ifelse(type == "scene", 1, NA)) %>%
  # https://stackoverflow.com/questions/23340150/replace-missing-values-na-with-most-recent-non-na-by-group
  # creates scene variable indicating scene number of giveen episode based on appearance of 'scene' in the type column 
  mutate(scene2 = cumsum(0 + !is.na(scene))) %>%
  ungroup() %>%
  select(-scene)

# saving the data
write_csv(friends_lines, "../data/friends_lines.csv")




# challenges... ---- 

# double eps0213,0616, 0924, 1018, missing eps -  0911, 0924 fixed by replacing tags with <p></p>
# season 2 weird naming for some characters
friends_lines %>% distinct(id) %>% count()

# finding the weirdly named characters in season 2


friends_lines %>% filter(str_detect(id, "^[02]{2}")) %>%
  # separate(clean_text, into = c("person", "line"), sep = ":") %>%
  # mutate(line = str_trim(line)) %>% 
  filter(type == "person", str_detect(person, "^chan$"))  %>%
  distinct(id)


friends_lines %>% separate(clean_text, into = c("person", "line"), sep = ":") %>%
  mutate(line = str_trim(line)) %>%
  # 
  mutate(person = ifelse(person %in% names(name_map), name_map[person], person)) 

scriptParser(files[200])
  html_nodes(xpath = '//p/text()[preceding-sibling::br and following-sibling::br]') %>%
  html_text(trim = TRUE) %>%
  # remove carriage returns, this only applies to 177..
  gsub("\r", "", .) %>% 
  # split on \n\n, using this as a delimiter
  str_split("\n\n") %>%
  # convert to tibble
  tibble(X1 = unlist(.)
         # title = .[[1]][1]
  ) %>% select(-.) %>% 
  # as_tibble() %>%
  # extract season and episode from file name
  mutate(id= (gsub(".*?([0-9]{4}).*", "\\1", html))) %>%
  set_names(c("X1", "id"))
