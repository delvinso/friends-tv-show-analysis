library(tidyverse)
library(RColorBrewer)
library(scales)

setwd("~/Documents/Projects/friends-master")
# read in data
friends_lines <- read_csv( "data/friends_lines.csv")

main <- c("chandler", "ross", "monica", "phoebe", "joey", "rachel")

# ----- Sanity checks ----

  # check if any scenes have less than 2 scenes. this would indicate regex not capturing scene type lines. can also manually verify
  friends_lines %>%
    group_by(id) %>%
    count(scene2) %>%
    summarize(max = max(scene2)) %>% 
    arrange(max)# %>% 
  # 2 episodes are iffy. 203 and 206 lack scenes. rest could be legit?

  # average number of scenes across all episodes
  friends_lines %>%
    group_by(id) %>%
    summarize(max = max(scene2)) %>% ungroup() %>%
    summarize(avg_scenes = mean(max))
  
  # line count by episode - seems fine 
  friends_lines %>% filter(type %in% c("person", "scene")) %>% count(id) %>% arrange(n)
  
  # are NA lines proper?
  friends_lines %>%
    select(-title) %>% 
    filter(is.na(type))
    # count(person)# %>% View()

  # total number of scenes 
  friends_lines %>%
    group_by(id) %>%
    count(scene2) %>% 
    summarize(max = max(scene2)) %>% 
    arrange(max) %>% ungroup() %>%
    summarize(num_scenes = sum(max))
  # slate reports 2941 - http://www.slate.com/articles/arts/culturebox/2014/05/friends_chandler_joey_ross_rachel_monica_phoebe_which_friends_were_closest.html#lf_comment=162692251
  

# EDA ----

# ------ Lines across seasons  ----
png(filename = "output/lines_across_season.png", width = 640, height = 480)
  friends_lines %>% 
    filter(person %in% main) %>% 
    count(person, sort = TRUE) %>%
      ggplot(aes(reorder(x = person, n), y = n)) +
      geom_col(aes(fill = person), colour = "black") + 
      geom_label(aes(label = scales::comma(n), y = n, fill = person), hjust = 0.5) +
      coord_flip() +
      scale_fill_brewer(palette = "Dark2", guide = FALSE) +
      theme_dark_ds(base_size = 16) +
      labs(x = "", y = "Number of Lines", caption = "delvinso.github.io") + 
      scale_y_continuous(limits = c(0, 10000), labels = comma) + 
      ggtitle("Friends - Line Distribution Across Seasons")
dev.off()

# ---- Lines over seasons by character ----
png(filename = "output/lines_by_season.png", width = 960, height = 720)

friends_lines %>% 
  filter(person %in% main) %>% 
  count(person, season, sort = TRUE) %>%
  group_by(person) %>%
  mutate(avg_lines = mean(n)) %>% 
  ggplot(aes(x = season, y = n)) +
    geom_col(aes(fill = person)) + 
    geom_hline(aes(yintercept = avg_lines, group = person), linetype = "dashed") + 
    geom_label(aes(label = scales::comma(n), y = n, fill = person), hjust = 0) + 
    scale_y_continuous(limits = c(0, 1500), labels = comma,
                       expand = expand_scale(mult = c(0, 0.25))
    ) + 
    coord_flip() +
    scale_fill_brewer(palette = "Dark2", guide = FALSE) + 
    theme_dark_ds(base_size = 16) +
    labs(x = "", y = "Number of Lines", caption = "delvinso.github.io") + 
    ggtitle("Friends - Line Distribution By Season") +
    facet_wrap(~ person)
dev.off()
# ----- Lines by episode over time -----
  friends_lines %>% 
    filter(person %in% main) %>%
    count(person, id, sort = TRUE) %>%
    mutate(new_id = as.factor(id)) %>%
      ggplot(aes(x = new_id, y = n)) +
      geom_point(aes(colour = person), size = 0.5, alpha = 0.8) +
      geom_smooth(stat = "smooth", se = FALSE, method = "auto", aes(group = person, colour = person), alpha = 0.8) + 
      scale_fill_brewer(palette = "Dark2") +
      theme_dark_ds(base_size = 16) + 
      ggtitle("Friends - # of Lines by Episode")

  # lines by episode by season
  friends_lines %>% 
    filter(type == "person") %>% 
    filter(person %in% main) %>%
    count(person, id, season, sort = TRUE) %>%
    mutate(new_id = as.factor(id)) %>%
      ggplot(aes(x = new_id, y = n)) +
      geom_point(aes(colour = person), size = 0.5, alpha = 0.8) +
      geom_smooth(stat = "smooth", se = FALSE, method = "auto", aes(group = person, colour = person), alpha = 0.8) + 
      scale_fill_brewer(palette = "Dark2") +
      facet_wrap(~season, scales = "free_x") + 
      # coord_flip() +
      theme(axis.text.x = element_text(size = 6, angle = 90, hjust = 1)) +
      theme_bw(base_size = 16)
  
# ----- 1. Sentiment by character through the series  ----

# Tokenizing lines into words ----
  
  friends_words <- friends_lines %>% 
    select(-title) %>% 
    filter(type == "person", person %in% main) %>%
    # total lines by person throughout series
    group_by(person) %>% mutate(total_line = row_number()) %>%
    # total lines by person by season
    group_by(season, person) %>% mutate(season_line = row_number()) %>%
    # total lines by person by episode 
    group_by(id, person) %>% mutate(episode_line = row_number()) %>% 
    # group_by(person, id, season, scene2, line_num = 1:n()) %>% 
    unnest_tokens(word, line, token = "words") %>% 
    ungroup() %>%
    # remove apostrophes in contractions. this is going to screw
    # me over later when removing stop words.
    mutate(word = gsub("'", "", word))
  
  friends_words
  
  # average lines per scene
  friends_lines %>% 
    filter(person %in% main) %>% 
    group_by(id, scene2) %>% summarize(n = n()) %>%
    group_by(id) %>% summarize(avg = mean(n()))  %>%
    ggplot(aes(x = id, y = avg)) + geom_point()
  
  # determine what the number of lines of the index should be- use the average lines per person per episode
  
  friends_lines %>% 
    filter(person %in% main) %>% 
    group_by(id, person) %>% 
    count() %>% 
    group_by(person) %>%
    summarize(avg = mean(n)) # roughly ~30 lines
  
  # finding the index where each season ends as a marker
  season_index_end <- friends_words %>% 
    anti_join(stop_words) %>%
    inner_join(get_sentiments("afinn")) %>% 
    # calculate index based on total lines
    group_by(person, index = total_line%/% 30) %>%
    distinct(season, person, id, total_line, index) %>%
    group_by(season, person) %>%
    distinct(season, person, index = max(index))  %>% ungroup()
  
  # AFINN sentiment across all lines 
  png(filename = "output/friends_afinn_over_episodes.png", width = 960, height = 580)
  friends_words %>% 
    anti_join(stop_words) %>%
    inner_join(get_sentiments("afinn")) %>% 
    group_by(person, index = total_line%/% 30) %>%
    # group_by(person, id) %>%
    summarize(sum_score = sum(score)) %>% 
      ggplot(aes(x = index, y = sum_score)) + 
      geom_point(aes(colour = person), size = 0.5) + 
      facet_grid(~ person, scale = "free_x") +
      scale_fill_brewer(palette = "Dark2", guide = FALSE) +
      theme_dark_ds(base_size = 16) +
      geom_line(stat = "smooth", method = "loess", aes(group = person, colour = person), size = 1, alpha = 0.9, se = FALSE) + 
    # geom_line(stat = "smooth", method = "loess", size = 2, alpha = 0.8, se = FALSE) + 
      # geom_smooth(method = "loess", aes(group = person, colour = person), size = 2, alpha = 0.4, se = FALSE) +
      # scale_y_continuous(breaks= seq(-50, 100, by = 5)) +
      geom_hline(aes(yintercept = 0), linetype = "dashed", alpha = 0.8, colour = "grey") +
      # stuff for labelling the index for seasons
      geom_vline(data = season_index_end, aes(xintercept = index, group = person), linetype = "dotted", colour = "grey") +
      geom_text(data = season_index_end, aes(label = paste("Season", season),  y = 40, group = person), colour = "grey", angle = 90, vjust = -0.5, size = 3) +
      labs(y = "AFINN Score", x = "Index (30 Lines)", caption = "delvinso.github.io") +
      guides(colour = FALSE) + 
      ggtitle("Sentiment Analysis of Friends", "Transcripts of All 236 Friends Episodes")
  dev.off()
  
  # BING sentiment across all lines
  friends_words %>% 
    anti_join(stop_words) %>%
    inner_join(get_sentiments("bing")) %>% 
    # inner_join(get_sentiments("nrc")) %>%
    group_by(person, id, index = total_line%/% 30) %>%
    count(person, id, index, sentiment) %>% 
    spread(sentiment, n) %>%
    mutate(ratio = positive / negative,
           sentiment = positive - negative) %>% 
    ggplot(aes(x = index, y = ratio)) + 
    # geom_bar(stat = "identity", aes(fill = person)) +
    geom_point(aes(colour = person), size = 0.3) + 
    facet_grid(~ person, scale = "free_x") +
    scale_fill_brewer(palette = "Dark2") + 
    guides(colour = FALSE) +
    theme_dark_ds(base_size = 16) +
    geom_smooth(method = "loess", aes(group = person, colour = person), alpha = 0.8, se = FALSE) +
    # scale_y_continuous(breaks= seq(-50, 100, by = 5)) +
    geom_hline(aes(yintercept = 0), linetype = "dashed")
  
  # NRC
  
  friends_words %>% 
    anti_join(stop_words) %>%
    inner_join(get_sentiments("nrc")) %>% 
    group_by(person, sentiment, id, index = total_line%/% 30) %>%
    count(person, sentiment) %>% 
    ggplot(aes(x = index, y = n)) + 
    geom_point(aes(colour = person), size = 0.3, alpha = 0.5) + 
    facet_grid(person ~ sentiment, scale = "free_y") +
    scale_fill_brewer(palette = "Dark2") + 
    guides(colour = FALSE) +
    theme_dark_ds(base_size = 16) +
    geom_smooth(method = "loess", aes(group = person, colour = person), alpha = 0.8) +
    scale_y_continuous(breaks= seq(-50, 100, by = 5)) +
    geom_hline(aes(yintercept = 0), linetype = "dashed", alpha = 0.8)


  # how many lines do chandlers tokenized words consist of ? 39 
  friends_words %>% ungroup() %>% filter(id == "0101", person == "chandler") %>% count(id, person, line_num) 
  # sanity check - number of chandler's lines in ep 1 season 1, 39 
  friends_lines %>% filter(type == "person", id == "0101", person == "chandler")

  # ----- 2. Which of the Friends were the best of friends? ----
  
  # prone to error as there were several episodes where scenes were not properly counted..
  co_occur_df <- friends_lines %>%
    select(-title) %>% 
    filter(person %in% main) %>%
    group_by(id) %>% 
    distinct(person, scene2) %>% 
    # https://stackoverflow.com/questions/46536183/generate-all-possible-pairs-and-count-frequency-in-r
    # create an identifier, ie whether a person was present in the scene
    mutate(n = 1) %>%
    # turns the cast into a column, whose value indicates whether they were present in the scene
    spread(person, n, fill=0) %>% 
    ungroup() %>% select(-c(id, scene2)) %>% 
    {crossprod(as.matrix(.))} %>%
    # `diag<-`(0)
    replace(upper.tri(., diag = T), NA) %>%
    reshape2::melt(na.rm = T)
  
  # heat map of co occurences 
  png(filename = "output/friends_cooccurences.png", width = 640, height = 600, bg = "#252525")
  co_occur_df %>% 
    ggplot(aes(x = Var1, y = Var2, fill = (value))) + 
    geom_tile(alpha = 0.8, colour = "black") +
    geom_text(aes(label = value, size = value), colour = "black", alpha = 0.8) +
    viridis::scale_fill_viridis(option = "viridis", direction = -1, begin = 0.2, end = 1, guide = FALSE) +
    # scale_fill_distiller("YlGnBlu", direction = -1) +
    scale_size_continuous(range = c(3, 8), guide = FALSE) +
    coord_equal() +
    theme_dark_ds(base_size = 14) + 
    labs(x = "", y = "") +
    ggtitle("Which of the two Friends were best Friends?", "Based on co-occurences in scenes") +
    labs(caption = "delvinso.github.io")
  dev.off()
  
  
# ---- 3. Distinguishing Words ----
# word most unique to a person as determined by (person says word)^2/anyone says word * anyone speaks/person speaks

# https://www.reddit.com/r/dataisbeautiful/comments/8a4gbr/the_office_characters_most_distinguishing_words_oc/
 
# line totals by person and overall totals
friends_line_totals <- friends_lines %>%
  select(-title) %>% 
  filter(person %in% main) %>% 
  # total lines
  mutate(line_total = n()) %>% 
  # running total of persons lines and total lines by person
  group_by(person) %>% mutate(line_person = row_number(),
                              line_total_person = n()) %>% ungroup() %>%
  distinct(person, line_total_person, line_total)

friends_line_totals

  # word totals by person and overall totals
friends_word_totals <- friends_words %>% 
  anti_join(stop_words) %>%
  # total words
  mutate(total_words = n()) %>%
  group_by(person) %>%
  mutate(person_words = n()) %>%
  distinct(person, word, total_words, person_words)

friends_word_totals
 
 
  unique_words <- friends_words %>% 
    # anti_join(stop_words) %>% 
    na.omit() %>% 
    # determine how many times each person said a specific word
    group_by(person, word) %>%
    summarize(person_word = n()) %>% 
    # determine how many times a word was said at all
    group_by(word) %>% 
    mutate(anyone_says_word = sum(person_word)) %>% ungroup() %>% 
    #join on word and line totals
    left_join(friends_word_totals) %>% left_join(friends_line_totals) %>%
    # calculating uniqueness of each word
    mutate(word2_freq = person_word^2/anyone_says_word, 
           word3_freq = person_word^3/anyone_says_word,
           line_ratio = line_total/line_total_person,
           word_ratio = total_words/person_words,
           unique = word2_freq * word_ratio,
           unique3 = word3_freq * word_ratio
           # unique_line_denom = word2_freq * line_ratio
           ) %>%
  # for each word, determine the person with the highest 'weight' as given by unique.
  # done because several people may have high 'weight' for a word. 
  group_by(word) %>%
  top_n(n = 1, wt = unique) %>%
  arrange(desc(unique)) %>%
  # now for each person, give their top 30 words. because we previously grouped by word and took only the person with the greatest weight, all words here will
  # be unique to a given person (ie. not even if 'uhm' scores high across everyone, it will only appear with the person who had the highest unique score for 'uhm').
  group_by(person) %>% 
  # remove additional stop words
  filter(!word %in% c("id", "uh", "ah", "wouldnt", 
                     "didnt", "ive", "wasnt", 
                     "2", "7", "4", "wont", "youre",  
                     "hes", "doesnt", "umm", 
                     "ill", "theyre", 
                     "dont", "ohh", "im", "gonna")) %>% 
  top_n(n = 30, wt = unique) %>% 
  arrange(person, person_word) 
  
  # ---- Word clouds ----
  library(wordcloud)
  library(RColorBrewer)
  palette <- "Dark2"
  pal <- rep(brewer.pal(8, palette), each = ceiling(30 / 9))[30:1]
  
  indiv_colors <- c("Purples", "Reds", "Greys", "Blues", "Greens", "Oranges") %>% 
    map(~ rep(brewer.pal(9, .), each = ceiling(30 / 9))[5:34])
  
  # ---- Individual word clouds ----
  for( i in 1:length(main)){
    
    current_cast <- main[i]
    pal <- indiv_colors[[i]]
    
    png(paste0("output/distinguishing_words/", current_cast, "_distinguishing_words.png"), 4.5 * 100, 4.5 * 100, res = 100)
    par(bg = "grey95")
    
    unique_words %>%
      filter(person == current_cast) %>% 
      with(., wordcloud(word, freq = unique, random.order = FALSE, rot.per = 0.1,
                     ordered.colors = TRUE, colors = pal))
    title(paste0(current_cast, "'s Most Distinguishing Words"))
    dev.off()
  }
  # ---- Ensemble word cloud (need to save manually though) ----
  par(mfrow = c(2, 3))
  # png("output/distinguishing_words/cast_most_distinguishing_words.png", width = 640, height = 720)
    # par(bg = "grey95")
    for( i in 1:length(main)){
      
      current_cast <- main[i]
      pal <- indiv_colors[[i]]
      
      unique_words %>% 
        filter(person == current_cast) %>% 
        with(., wordcloud(word, 
                          freq = unique, 
                          scale = c(6, 1.5),
                          random.order = FALSE, rot.per = 0.1,
                          ordered.colors = TRUE, colors = pal, fixed.asp = TRUE))
      
      title(paste0(current_cast, " - Most Distinguishing Words"))
    }
  # dev.off()
  # return to 1x1 grid
  par(mfrow = c(1, 1))
  
  # ---- Bar graph of uniqueness  ----
  
  # bar graphs for uniqueness 
  unq_words_for_plot <- unique_words %>%
    select(person, word, person_word) %>% ungroup() %>%
    mutate(order = row_number())
      
    png(filename = "output/distinguishing_words/friends_distinguish_words_using_words.png", width = 960, height = 580)
    unq_words_for_plot %>% 
        ggplot(aes(x = order, y = person_word)) + 
        geom_col(aes(fill = person), alpha = 0.8) + 
        # geom_label(aes(label = scales::comma(person_word), fill = person)) + 
        facet_wrap(~ person, scales = "free") + 
        scale_fill_brewer(palette = "Dark2", guide = FALSE) +
        scale_x_continuous(
          labels = unq_words_for_plot$word,
          breaks = unq_words_for_plot$order,
          expand = expand_scale(mult = c(0, 0))
        ) + 
        scale_y_continuous(expand = expand_scale(mult = c(0, 0.25))) + 
        theme_dark_ds(base_size = 16) + # ADD XY TO THEME) + 
        labs(x = "", y = "# of times spoken", caption = "delvinso.github.io") + 
        ggtitle("The One With The Most Distinguishing Words", "Transcripts of All 236 Friends Episodes") + 
        coord_flip() 
    dev.off()


# ---- Misc. word eda -----
    
    # ----- Words by season by character -----
    png(filename = "output/words_by_season.png", width = 960, height = 720)
    
    friends_words %>% 
      count(person, season) %>%
      ggplot(aes(x = season, y = n)) +
      geom_col(aes(fill = person)) + 
      geom_label(aes(label = scales::comma(n), y = n, fill = person), hjust = 0) + 
      facet_wrap(~ person)  +
      scale_fill_brewer(palette = "Dark2", guide = FALSE) +
      scale_y_continuous(label = scales::comma, limits = c(0, 15000)) + 
      theme_dark_ds() + 
      coord_flip() +
      labs(x = "", y = "Number of Words", caption = "delvinso.github.io") + 
      ggtitle("Friends - Word Distribution by Seasons") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold")) 
    dev.off()
    
    # ----- Words across seasons -----
    png(filename = "output/words_across_season.png", width = 640, height = 480)
    
    friends_words %>% 
      count(person) %>%
      ggplot(aes(x = person, y = n, fill = person)) +
      scale_fill_brewer(palette = "Dark2", guide = FALSE) +
      geom_col(alpha = 0.95) + 
      geom_label(aes(label = scales::comma(n), y = n, fill = person), hjust = 0.5) + 
      coord_flip() +
      theme_dark_ds() +
      labs(x = "", y = "Number of  Words", caption = "delvinso.github.io") + 
      scale_y_continuous(limits = c(0, 110000), labels = comma) + 
      ggtitle("Friends - Word Distribution Across Seasons")
    dev.off()

    # ----- Words by season -----
      friends_words %>% 
        count(season) %>%
        ggplot(aes(x = season, y = n, fill = season)) +
        geom_col(alpha = 0.95, show.legend = FALSE) + 
        geom_label(aes(label = scales::comma(n), y = n), hjust = 0.5) + 
        scale_y_continuous(labels = scales::comma) +
        coord_flip() +
        theme_dark_ds(base_size = 16)
  
    # ----- Word distribution by season by character ------
    friends_words %>% 
      count(person, season, sort = TRUE) %>%
      group_by(season) %>%  
      mutate(avg_words = mean(n)) %>%
      ggplot(aes(x = season, y = avg_words)) +
      geom_col(aes(fill = person)) + 
      # geom_hline(aes(yintercept = avg_words, group = person), linetype = "dashed") + 
      geom_label(aes(label = n, y = n, fill = person), hjust = 0) + 
      # scale_y_continuous(limits = c(0, 5000)) + 
      coord_flip() +
      scale_fill_brewer(palette = "Dark2") +
      guides(fill = FALSE) +
      theme_dark_ds(base_size = 16) +
      labs(x = "", y = "Number of Lines") + 
      ggtitle("Friends - Word Distribution Over Seasons") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
      facet_wrap(~ person)
    
# ---- 4. How much did the friends make per line? Per Word? ----
  
  # season 2: 30k is an average - from wikipedia
  
  # salary by season
  friends_lines %>% distinct(season, id) %>% filter(str_detect(id, "-"))
  
  salary_season <- tibble(season = c(paste0("0", 1:9), 10), 
                          salary_episode = c(22500, 30000, 75000, 85000, 100000, 125000, 750000, 750000, 1000000, 1000000),
                          salary_ep_adj = c(38325.76, 49692.72, 120668.74, 123690.44, 154870.55, 189405.01, 1099473.00, 1069656.78, 1403218.45, 1381951.09),
                          n = c(24, 24, 25, 24, 24, 25, 24, 24, 24, 18))
  
  salary_values <- salary_season %>% mutate(season_salary = salary_episode * n,
                           season_salary_adj = salary_ep_adj * n) %>%
    mutate(total_salary = sum(season_salary),
              total_salary_adj = sum(season_salary_adj))
  
  salary_values 

  # lines per friend by season 
  fd_lns_per_season <- friends_lines %>% 
    select(-title) %>% 
    filter(person %in% main) %>% 
    count(person, season) %>%
    mutate(total_lines = sum(n)) %>%
    rename("num_lines" = n) %>%
    group_by(person) %>% mutate(total_lines_person = sum(num_lines)) 
  # words per friend by season
  fd_wds_per_season <- friends_words %>%
    count(person, season) %>% 
    mutate(total_words = sum(n)) %>%  rename("num_words" = n) %>%
    group_by(person) %>% mutate(total_words_person = sum(num_words)) 
  
  # putting our data together
  word_lines_per <- full_join(fd_lns_per_season, fd_wds_per_season) %>%
    full_join(salary_values ) %>% ungroup()
  word_lines_per
  # calculating pay per word, line per season and overall pay per line, per word - ADJUSTED FOR 2018 INFLATION
  pay_word_line <- word_lines_per %>% 
    mutate(pay_per_line_per_season = season_salary_adj/num_lines,
           pay_per_word_per_season = season_salary_adj/num_words,
           overall_pay_per_line = total_salary_adj/total_lines_person,
           overall_pay_per_word = total_salary_adj/total_words_person)  %>%
    ungroup()
  
  pay_word_line 
  
  cast_names <- c("chandler" = "matthew perry (chandler)",
                  "ross" = "david schwimmer (ross)",
                  "monica" = "courteney cox (monica)",
                  "rachel" = "jennifer aniston (rachel)",
                  "phoebe" = "lisa kudrow (phoebe)",
                  "joey" = "matt leblanc (joey)")
  # pay per word per season
  png(filename = "output/pay_per_word_per_season.png", width = 960, height = 740)
  pay_word_line %>%
    distinct(person, season, pay_per_word_per_season) %>% 
    mutate(person = ifelse(person %in% names(cast_names), cast_names[person], person)) %>% 
    ggplot(aes(x = season, y = pay_per_word_per_season)) +
    geom_col(aes(fill = person), colour = "black", alpha = 0.975) + 
    geom_label(aes(label = scales::dollar(round(pay_per_word_per_season, 2)), fill = person), hjust = 0, nudge_y = 50) + 
    facet_wrap(~ person) + 
    scale_y_continuous(limits = c(0, 5000), label = scales::dollar) +
    scale_fill_brewer(palette = "Set2", guide = FALSE) + 
    labs(y = "Dollars (USD)", x = "Season", caption = "delvinso.github.io") + 
    coord_flip()  +
    theme_dark_ds(base_size = 16, grid = "X") +
    theme(axis.text.x = element_blank()) + 
    ggtitle("How much did the Friends make per word per season?", "Adjusted for inflation 2018")
  dev.off()
  
  # pay per line per season
  png(filename = "output/pay_per_line_per_season.png", width = 960, height = 740)
  pay_word_line %>% 
    distinct(person, season, pay_per_line_per_season) %>% 
    mutate(person = ifelse(person %in% names(cast_names), cast_names[person], person)) %>% 
    ggplot(aes(x = season, y = pay_per_line_per_season)) +
    geom_col(aes(fill = person), colour = "black", alpha = 0.975) + 
    geom_label(aes(label = scales::dollar(round(pay_per_line_per_season, 2)), fill = person), hjust = 0, nudge_y = 500) + 
    facet_wrap(~ person, scales = "free") + 
    scale_fill_brewer(palette = "Set2", guide = FALSE) + 
    coord_flip()  +
    theme_dark_ds(base_size = 16, grid = "X") +
    # hrbrthemes::theme_modern_rc(base_size = 16, grid = "X") + theme(strip.text = element_text(colour = "white")) + 
    # theme_ipsum_rc(grid = "x") + 
    theme(axis.text.x = element_blank()) + 
    labs(y = "Dollars (USD)", x = "Season", caption = "delvinso.github.io") + 
    scale_y_continuous(limits = c(0, 50000),  label = scales::dollar) +
    ggtitle("How much did the Friends make per line per season?", "Adjusted for inflation 2018")
  dev.off()
  
  # pay per word and line overall
  
  png(filename = "output/overall_pay_per_line_and_word_per_season.png", width = 640, height = 480)
  word_lines_per %>%
    distinct(person, total_lines_person, total_words_person, total_salary_adj) %>% 
    mutate(person = ifelse(person %in% names(cast_names), cast_names[person], person)) %>% 
    group_by(person) %>%
    summarize(overall_pay_per_line = total_salary_adj/total_lines_person,
                       overall_pay_per_word = total_salary_adj/total_words_person) %>%
    gather(key = stat, value = overall_pay, -one_of("person")) %>%
    mutate(stat = replace(stat, stat == "overall_pay_per_line", "USD Per Line"),
           stat = replace(stat, stat == "overall_pay_per_word", "USD Per Word")) %>% 
    ggplot(aes(x = reorder(person, overall_pay), y = overall_pay, fill = person)) +
    geom_col(colour = "black", alpha = 0.975) + 
    facet_wrap(~ stat, scales = "free_x", nrow = 2) + 
    scale_y_continuous(label =  scales::dollar, limits = ) + 
    geom_label(aes(label = scales::dollar(round(overall_pay, 0)), fill = person), hjust = 0.5) + 
    scale_fill_brewer(palette = "Set2", guide = FALSE) + 
    theme_dark_ds(base_size = 16, grid = "X") + 
    theme(axis.text.x = element_blank()) + 
    ggtitle("How much did the Friends make per line and word across the series?", "Adjusted for inflation 2018") +
    coord_flip() +
    labs(x = "", y = "Dollars (USD)", caption = "delvinso.github.io")
  dev.off()

# ---- Recurring character stuff ----
  
  # proportion of lines by main cast, and by recurring characters. 
  all_lines <- friends_lines %>%
    select(-title) %>% 
    # only characters
    filter(type == "person") %>%
    mutate(season = as.integer(season)) %>% 
    # totals by person
    group_by(person) %>%
    summarize(total_lines_person = n()) %>%
    ungroup() %>% mutate(total_lines = sum(total_lines_person)) %>% 
    arrange(desc(total_lines_person)) #%>%
  
  all_lines %>% 
    top_n(30) %>%
    print(n = 30)
  
  all_lines %>%
    filter(total_lines_person > quantile(total_lines_person, 0.95)) %>% 
    mutate(is_main = ifelse(person %in% main, "main", "recurr")) %>%
    group_by(is_main) %>%
    summarize(line_total_group = sum(total_lines_person))
  
  # by season?

  # ---- word length ----
  
  
  # who has the most unique word length count in their vocabulary? 
  friends_words %>%
    distinct(person, word, season) %>% 
    mutate(nchar = nchar(word)) %>% 
    group_by(person, season) %>% 
    # left_join(salary_season %>% select(season, n) %>% rename("num_eps" = "n")) 
    summarize(n = n(), avg_char = mean(nchar), 
              stdev_char = sqrt(var(nchar) )) %>% 
    ggplot(aes(x = season, y = avg_char)) + 
    geom_point(aes(colour = person)) + 
    # geom_line(aes(group = person, colour = person), alpha = 0.8) + 
    geom_smooth(aes(group = person, colour = person), se = FALSE, alpha = 0.8) +
    # geom_errorbar(aes(ymin = avg_char - stdev_char, ymax = avg_char + stdev_char, colour = person, width = 0.2), alpha = 0.5) + 
    scale_colour_brewer(palette = "Dark2")
  
  # ---- non-unique vocab by episode - relative frequency ----
  friends_words %>% ungroup() %>% 
    mutate(nchar_word = nchar(word)) %>% 
    group_by(person, id) %>% 
    summarize(avg_char = mean(nchar_word),
              n = n(),
              stdev_char = sqrt(var(nchar_word))) %>% 
    ggplot(aes(x = id, y = avg_char)) + 
    geom_point(aes(colour = person)) + 
    # geom_line(aes(group = person, colour = person), alpha = 0.8) + 
    geom_smooth(aes(group = person, colour = person), se = FALSE, alpha = 0.8) + 
    # geom_errorbar(aes(ymin = avg_char - stdev_char, ymax = avg_char + stdev_char, colour = person, width = 0.2), alpha = 0.5) + 
    scale_colour_brewer(palette = "Dark2")
  
  # ---- non-unique vocab by season - relative frequency ----
  # normalize this by season length?
  friends_words %>% ungroup() %>% 
    mutate(nchar_word = nchar(word)) %>% 
    group_by(person, season) %>% 
    summarize(n = n(),
              total_nchar = sum(nchar_word)) %>%
    left_join(salary_season %>% select(season, n) %>% rename("num_eps" = "n")) %>% 
    mutate(totaln_char_norm = total_nchar/ num_eps) %>% 
    # summarize(avg_char = mean(nchar_word),
    #           n = n(),
    #           stdev_char = sqrt(var(nchar_word))) %>% 
    ggplot(aes(x = season, y = totaln_char_norm)) + 
    geom_point(aes(colour = person)) + 
    # geom_line(aes(group = person, colour = person), alpha = 0.8) + 
    geom_smooth(aes(group = person, colour = person), se = FALSE, alpha = 0.8) + 
    # geom_errorbar(aes(ymin = avg_char - stdev_char, ymax = avg_char + stdev_char, colour = person, width = 0.2), alpha = 0.5) + 
    scale_colour_brewer(palette = "Dark2")
  
  
# helpers ----

library(hrbrthemes)
import_roboto_condensed()

theme_ds <- function(base_size = 16){
  theme_bw(base_size) + 
    theme(axis.text = element_text(face = "bold"),
          plot.title = element_text(face = "bold", hjust = 0.5),
          plot.subtitle = element_text( hjust = 0.5),
          legend.direction = "horizontal",
          legend.position = "bottom",
          strip.background = element_rect(fill = "white")) 
}



theme_dark_ds <- function(base_size = 12, grid = "x"){
  
  theme <- theme_bw(base_size) + 
    theme(  text = element_text(face = "bold", family = "Roboto Condensed", colour = "grey"),
            plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5, face = "bold"),
            #axis.text.x = element_text(angle = 45, hjust = 1),  
            axis.text = element_text(colour = "grey", face = "bold"),
            axis.ticks = element_line(colour = "grey"),
            panel.border = element_rect(colour = "white"),
            plot.background = element_rect(fill = "#252525", colour = "#252525"),
            panel.background = element_rect(fill = "#252525", colour = "#252525"),
            # panel.grid.major = element_line(colour = "grey", size = 1),
            # panel.grid.major = element_blank(),
            # panel.grid.minor = element_blank(),
            legend.background = element_rect(fill = "#252525", size = 6),
            legend.key = element_rect(colour = "grey", fill = "#252525"),
            strip.background = element_rect(fill = "#252525", colour = "white"),
            strip.text = element_text(colour = "grey", face = "bold")
    ) 
  
  # if(grid == "Y"){
  #   theme + theme(panel.grid.major.y = element_blank())
  # }
  # if(grid == "X"){
  #   theme + theme(panel.grid.major.x = element_blank())
  # }
  # if(grid == "y"){
  #   theme + theme(panel.grid.minor.y = element_blank())
  # }
  # if(grid == "x"){
  #   theme + theme(panel.grid.minor.x = element_blank())
  # }
  # from hrbr themes
  if (inherits(grid, "character")) {
    if (regexpr("X", grid)[1] < 0) 
      theme <- theme + theme(panel.grid.major.x = element_blank())
    if (regexpr("Y", grid)[1] < 0) 
      theme <- theme + theme(panel.grid.major.y = element_blank())
    if (regexpr("x", grid)[1] < 0) 
      theme <- theme + theme(panel.grid.minor.x = element_blank())
    if (regexpr("y", grid)[1] < 0) 
      theme <- theme + theme(panel.grid.minor.y = element_blank())
  }
}
