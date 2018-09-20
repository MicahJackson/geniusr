## Exploring Eminem sentiment by album


## attach libraries and load source files for functions ####
library(dplyr)
library(purrr)
library(stringr)
library(tidytext)
library(reshape2)
library(tidyr)
library(ggplot2)
library(here)

## need to update your file path or use 'here' package
file.sources <- list.files(here("Documents", "textAnalysis", "geniusr", "R"), full.names = TRUE)
sapply(file.sources,source,.GlobalEnv)

## insert token to begin
genius_token()


## begin analysis ####

## create custom stops to take out of songs
rap_stops <- tibble(word = c("i'm", "i'ma", "ya", "ain't", "i'mma", "em", "yo", "hol", "ha", "ah", "uh"), lexicon = 'rap')

## get list of all albums, cut down to official discography
em_album_list <- get_artist_albums(45) %>%
    filter(album_id %in% c(2867, 2871, 2873, 2876, 2878, 2904, 11166, 43437, 376740, 457633))

## grab lyrics and remove background words
em_disco_lyrics <- scrape_lyrics_discography(em_album_list$album_id) %>%
    filter( !str_detect(line, pattern = "\\*."))


## create data frame with info of each word that is not in stop words list. Give order to albums.
em_disco_tokens <- em_disco_lyrics %>%
    unnest_tokens(word, line) %>%
    anti_join(bind_rows(stop_words, rap_stops)) %>%
    transform(album_name = factor(album_name, levels=c('Infinite', 'The Slim Shady LP',
        'The Marshall Mathers LP', 'The Eminem Show', 'Encore', 'Relapse',
        'Recovery', 'The Marshall Mathers LP2', 'Revival', 'Kamikaze'), ordered=TRUE)) %>%
    arrange(album_name) %>%
    as_tibble()

## calculate songs and words counts
em_songs_per_album <- em_disco_tokens %>% select(album_name, song_title) %>% unique() %>% count(album_name)
em_total_words_album <- em_disco_tokens %>% count(album_name)
em_total_uniques_album <- em_disco_tokens %>% select(album_name, word) %>% unique() %>% count(album_name)
em_words_songs_album <- em_songs_per_album %>% inner_join(em_total_words_album, by='album_name') %>%
    inner_join(em_total_uniques_album, by='album_name') %>% rename(n_songs = n.x, n_total = n.y, n_uniques = n)
em_avg_word_songs <- em_words_songs_album %>% transmute(album_name = album_name, avg_total_per_song = n_total/n_songs, avg_uniques_per_song = n_uniques/n_songs)


## nrc sentiment (joy, anger, fear, anticipation, etc.)
em_sentiment <- em_disco_tokens %>%
    inner_join(get_sentiments("nrc")) %>%
    filter(!sentiment %in% c('positive', 'negative')) %>%
    count(album_name, sentiment) %>%
    spread(sentiment, n, fill=0)

## nrc normalized sentiment by album. Look at avg sentiment
## per word so an album with more words won't have higher sentiment score
em_norm_sentiment_total <- em_disco_tokens %>%
    inner_join(get_sentiments("nrc")) %>%
    count(album_name, sentiment) %>%
    inner_join(em_total_words_album, by='album_name') %>%
    mutate(avg_sent_per_word = n.x/n.y) %>%
    select(-n.x, -n.y) %>%
    filter(!sentiment %in% c('negative', 'positive')) %>%
    spread(sentiment, avg_sent_per_word, fill = 0)



## tidy up. reshape from wide to long format.
em_sent_total_melt <- reshape2::melt(em_norm_sentiment_total,
    variable.name = "emotion",
    value.names = "value",
    id.vars = c("album_name")) %>%
    rename(rating = value) %>%
    mutate(rating = 100*rating) %>%
    transform(album_name = factor(album_name, levels=c('Infinite', 'The Slim Shady LP',
        'The Marshall Mathers LP', 'The Eminem Show', 'Encore', 'Relapse',
        'Recovery', 'The Marshall Mathers LP2', 'Revival', 'Kamikaze'), ordered=TRUE)) %>%
    transform(emotion = factor(emotion, levels=c('anger', 'anticipation', 'disgust', 'fear',
        'sadness', 'joy', 'surprise', 'trust'), ordered=TRUE)) %>%
    arrange(emotion, album_name)


## line plots of emotions by album. MMLP2 has noticeably more words
em_sent_melt %>%
    ggplot(aes(x=album_name, y=rating, group=emotion, color=emotion)) +
    geom_line() +
    geom_point()

em_sent_total_melt %>%
    ggplot(aes(x=album_name, y=rating, group=emotion, color=emotion)) +
    geom_line() +
    geom_point()

## with facet wrap. Makes it less cluttered.
em_sent_total_melt %>%
    ggplot(aes(x=album_name, y=rating, group=1, color=emotion)) +
    geom_line() +
    geom_point() +
    facet_wrap(~emotion) +
    scale_x_discrete(labels = c('Inf', 'SSLP', 'MMLP', 'EmSh',
        'Enc', 'Rlps', 'Rcvr', 'MMLP2', 'Rev', 'Kaze')) +
    xlab('Album') +
    guides(color=FALSE)


## look at emotions that are easier to understand
em_sent_total_melt %>%
    filter(emotion %in% c('anger', 'fear', 'sadness', 'joy')) %>%
    ggplot(aes(x=album_name, y=rating, group=emotion, color=emotion)) +
    geom_line() +
    geom_point()

## facet wrap easy emotions
em_sent_total_melt %>%
    filter(emotion %in% c('anger', 'fear', 'sadness', 'joy')) %>%
    ggplot(aes(x=album_name, y=rating, group=1, color=emotion)) +
    geom_line() +
    geom_point() +
    facet_wrap(~emotion) +
    scale_x_discrete(labels = c('Inf', 'SSLP', 'MMLP', 'EmSh',
        'Enc', 'Rlps', 'Rcvr', 'MMLP2', 'Rev', 'Kaze')) +
    xlab('Album') +
    guides(color=FALSE)

## Not Afraid is on Recovery. Yet, high fear value. Could this song be driving it up?
## (Hint: YES). FUTURE: Should look at sentiment scores by song.


## histogram of emotions by album.
## don't try this with 10 albums and 8 emotions.
## Too much clutter.
em_sent_total_melt %>%
    filter(emotion %in% c('anger', 'fear', 'sadness', 'joy')) %>%
    ggplot(aes(x=emotion, y=rating, fill=emotion)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~album_name, scales = "free_y") +
    labs(y = "Eminem Discography: Album Emotion Scores",
        x = NULL) +
    coord_flip() +
    theme_minimal()




## tf_idf sentiment ####
## Normalized sentiment not as informative as I hoped. Issues with word counts.
## tf-idf will give us most distinguishing words then we can look at sentiments of those.
## FUTURE: Look at different tf-idf weighting schemes


## albums with word counts
disco_words <- em_disco_tokens %>%
    count(album_name, word, sort = TRUE) %>%
    ungroup()

## calculate tf-idf scores
disco_tf_idf <- disco_words %>%
    bind_tf_idf(word, album_name, n) %>%
    arrange(desc(tf_idf))

## find top tf-idf values
top_tf_idfs <- disco_tf_idf %>%
    group_by(album_name) %>%
    top_n(200, tf_idf) %>%
    filter(n>2) %>%
    arrange(album_name, desc(tf_idf))

View(top_tf_idfs)

top_tf_idfs %>%
    select(-n) %>%
    count(album_name)

## most "important words" are names
disco_tf_idf %>%
    arrange(desc(tf_idf)) %>%
    mutate(word = factor(word, levels = rev(unique(word)))) %>%
    group_by(album_name) %>%
    top_n(5) %>%
    ungroup() %>%
    ggplot(aes(word, tf_idf, fill = album_name)) +
    geom_col(show.legend = FALSE) +
    labs(x = NULL, y = "tf-idf") +
    facet_wrap(~album_name, ncol = 2, scales = "free_y") +
    coord_flip()


disco_tf_idf %>%
    arrange(desc(tf_idf)) %>%
    mutate(word = factor(word, levels = rev(unique(word)))) %>%
    group_by(album_name) %>%
    top_n(10) %>%
    ungroup %>%
    ggplot(aes(word, tf_idf, fill = album_name)) +
    geom_col(show.legend = FALSE) +
    labs(x = NULL, y = "tf-idf") +
    facet_wrap(~album_name, ncol = 2, scales = "free_y") +
    coord_flip()

# can think of this plot as "most-distinguishing" words for each album
# if it shows up a lot in this album relative to the rest of the albums


## look at sentiment of top tf-idf words.
## select words
em_tf_sentiment <- top_tf_idfs %>%
    select(-n) %>%
    inner_join(get_sentiments("nrc")) %>%
    count(album_name, sentiment) %>%
    inner_join(top_tf_idfs %>% select(-n) %>% count(album_name), by='album_name') %>%
    mutate(norm_sentiment = n.x/n.y) %>%
    select(-n.x, -n.y) %>%
    filter(!sentiment %in% c('negative', 'positive')) %>%
    spread(sentiment, norm_sentiment, fill=0)

## melt to long format
em_tf_melt <- reshape2::melt(em_tf_sentiment,
    variable.name = "emotion",
    value.names = "value",
    id.vars = c("album_name")) %>%
    rename(rating = value) %>%
    transform(album_name = factor(album_name, levels=c('Infinite', 'The Slim Shady LP',
        'The Marshall Mathers LP', 'The Eminem Show', 'Encore', 'Relapse',
        'Recovery', 'The Marshall Mathers LP2', 'Revival', 'Kamikaze'), ordered=TRUE)) %>%
    transform(emotion = factor(emotion, levels=c('anger', 'anticipation', 'disgust', 'fear',
        'sadness', 'joy', 'surprise', 'trust'), ordered=TRUE)) %>%
    arrange(emotion, album_name)

## plot emotions across albums
em_tf_melt %>%
    ggplot(aes(x=album_name, y=rating, group=1, color = emotion)) +
    geom_line() +
    geom_point() +
    facet_wrap(~ emotion) +
    scale_x_discrete(labels = c('Inf', 'SSLP', 'MMLP', 'EmSh',
        'Enc', 'Rlps', 'Rcvr', 'MMLP2', 'Rev', 'Kaze')) +
    xlab('Album') +
    guides(color=FALSE)
