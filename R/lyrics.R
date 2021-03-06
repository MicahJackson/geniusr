#' Retrieve lyrics associated with a Genius lyrics page URL
#'
#' Scrape lyrics from a Genius' lyric page using it's associated URL. Best used with \code{\link{scrape_tracklist}}, when song IDs aren't returned - otherwise, \code{\link{scrape_lyrics_id}} is recommended.
#' @param song_lyrics_url song lyrics url (like in \code{song_lyrics_url} returned by \code{\link{get_song_meta}})
#' @param access_token Genius' client access token, defaults to \code{genius_token}
#' @importFrom purrr "%>%"
#' @examples
#' \dontrun{
#' scrape_lyrics_url(song_lyrics_url = "https://genius.com/Kendrick-lamar-dna-lyrics")
#' }
#' @export
scrape_lyrics_url <- function(song_lyrics_url) {

    # start session
    # if page is not found, outputs error, but continues running
    tryCatch(session <- suppressWarnings(xml2::read_html(song_lyrics_url)),
        error = function(cond){
            print(paste(song_lyrics_url, 'is not available.'))
            closeAllConnections()
        })

    # if no fatal errors, continue here
    if(exists('session')){

        # get meta data
        song <- rvest::html_nodes(session, ".header_with_cover_art-primary_info-title") %>%
            rvest::html_text()

        artist <- rvest::html_nodes(session, ".header_with_cover_art-primary_info-primary_artist") %>%
            rvest::html_text()

        # read lyrics
        lyrics <- rvest::html_nodes(session, ".lyrics p")

        # ensure line breaks are preserved correctly
        xml2::xml_find_all(lyrics, ".//br") %>% xml2::xml_add_sibling("p", "\n")
        xml2::xml_find_all(lyrics, ".//br") %>% xml2::xml_remove()

        # get plain text lyrics
        lyrics <- rvest::html_text(lyrics)

        # split on line break
        lyrics <- unlist(stringr::str_split(lyrics, pattern = "\n"))

        # remove empty strings
        lyrics <- lyrics[lyrics != ""]

        # convert curly apostrophes to straight
        lyrics <- stringr::str_replace(lyrics, "’", "'")

        # remove lines with square brackets
        # like [Intro], [Verse n], [Outro]
        lyrics <- lyrics[!stringr::str_detect(lyrics, pattern = "\\[|\\]")]
        if(length(lyrics)==0){lyrics <- NA}

        # Convert to tibble
        lyrics <- tibble::tibble(line = lyrics)

        # add song metadata
        lyrics$song_lyrics_url <- song_lyrics_url
        lyrics$song_title <- song
        lyrics$artist_name <- artist

        # Remove lines with things such as [Intro: person & so and so]
        return(tibble::as_tibble(lyrics))
    } else{
        return(tibble::tibble(line = NA, song_lyrics_url = song_lyrics_url,
            song_title = NA, artist_name = NA))
    }

}



#' Retrieve lyrics associated with a Genius song ID
#'
#' Scrape lyrics from Genius' lyric pages using an associated song ID.
#' @param song_id song ID (like in \code{song_id} returned by \code{\link{search_song}})
#' @param access_token Genius' client access token, defaults to \code{genius_token}
#' @importFrom purrr "%>%"
#' @examples
#' \dontrun{
#' scrape_lyrics_id(song_id = 3214267)
#' }
#' @export
scrape_lyrics_id <- function(song_id, access_token=genius_token()) {

    # get song meta data
    tryCatch( meta_url <- get_song_meta(song_id, access_token) %>% select(song_lyrics_url) %>% unlist(),
        error = function(cond){
            print('url not available')
            closeAllConnections
        })

    if(exists('meta_url')){ scrape_lyrics_url(meta_url) }


}


## Additional lyric functionality ####

#' Retrieve lyrics from a tibble containing the song_number, _title, _lyrics_url. Best used with \code{\link{scrape_tracklist}} and \code{select}.
#' This function's primary use is inside of scrape_lyrics_tracklist.
#'
#' Scrape lyrics for a Genius song using associated tracklist info.
#' @param song_number song number returned from \code{scrape_tracklist}
#' @param song_title song title returned from \code{scrape_tracklist} or \code{get_song_meta}
#' @param song_lyrics_url song url returned from \code{scrape_tracklist} or \code{get_song_meta}
#' @param access_token Genius' client access token, defaults to \code{genius_token}
#' @importFrom purrr "%>%"
#' @examples
#' \dontrun{
#' grab_url_lyrics(song_number = 1, song_title = 'BLOOD.', song_lyrics_url = 'https://genius.com/Kendrick-lamar-blood-lyrics')
#' }
#' @export
grab_url_lyrics <- function(song_number, song_title, song_lyrics_url){

    ## song_info is a 1-row tibble
    ## song_number, song_title, song_lyrics_url

    ## output is tibble with lyrics, song_lyrics_url,
    ## song_title, and track_number

    number <- song_number
    title <- song_title
    song_lyrics_url <- song_lyrics_url

    tryCatch(session <- suppressWarnings(xml2::read_html(song_lyrics_url)),
        error = function(cond){
            print(paste(song_lyrics_url, 'is not available.'))
            closeAllConnections()
        }
    )

    if(exists('session')){

        # read lyrics
        lyrics <- rvest::html_nodes(session, ".lyrics p")

        # ensure line breaks are preserved correctly
        xml2::xml_find_all(lyrics, ".//br") %>% xml2::xml_add_sibling("p", "\n")
        xml2::xml_find_all(lyrics, ".//br") %>% xml2::xml_remove()

        # get plain text lyrics
        lyrics <- rvest::html_text(lyrics)

        # split on line break
        lyrics <- unlist(stringr::str_split(lyrics, pattern = "\n"))

        # remove empty strings
        lyrics <- lyrics[lyrics != ""]

        # convert curly apostrophes to straight
        lyrics <- stringr::str_replace(lyrics, "’", "'")

        # remove lines with square brackets
        # like [Intro], [Verse n], [Outro]
        lyrics <- lyrics[!stringr::str_detect(lyrics, pattern = "\\[|\\]")]
        if(length(lyrics)==0){lyrics <- NA}

        # Convert to tibble
        lyrics <- tibble::tibble(line = lyrics)

        # add song metadata
        lyrics$song_lyrics_url <- song_lyrics_url
        lyrics$song_title <- title
        lyrics$track_number <- number

        return(tibble::as_tibble(lyrics))

    } else{
        return(tibble::tibble(line = NA, song_lyrics_url = song_lyrics_url,
            song_title = title, track_number = number))
    }

}

#' Retrieve lyrics a tibble of tracklist info
#'
#' Scrape lyrics from a Genius' album using its associated tracklist.
#' @param tracklist A tibble containing columns: song_number, song_title, song_lyrics_url. Pulled from \code{scrape_tracklist}.
#' @importFrom purrr "%>%" pmap_dfr
#' @examples
#' \dontrun{
#' scrape_lyrics_tracklist(tracklist = scrape_tracklist(337082) %>% select(song_number, song_title, song_lyrics_url))
#' }
#' @export
scrape_lyrics_tracklist <- function(tracklist){

    ## tracklist is the first 3 columns of output from
    ## scrape_tracklist function: song_number, _title, _lyrics_url.

    ## primary use is inside of scrape_lyrics_album, but can
    ## be used as stand-alone function.

    return(purrr::pmap_dfr(tracklist, grab_url_lyrics))

}


#' Retrieve lyrics associated with a Genius Album ID
#'
#' Scrape lyrics from a Genius' album using its associated ID.
#' @param album_id An album ID (\code{album_id} returned in \code{\link{get_song_meta}})
#' @param access_token Genius' client access token, defaults to \code{genius_token}
#' @importFrom purrr "%>%"
#' @examples
#' \dontrun{
#' scrape_lyrics_album(album_id = 337082)
#' }
#' @export
scrape_lyrics_album <- function(album_id, access_token = genius_token()){

    ## tracklist is from `scrape_tracklist` function.
    ## tibble with the following features (in order):
    ## song_number, song_title, song_lyrics_url,
    ## album_name, album_id, artist_id, artist_name, artist_url

    ## get full tracklist info
    tracks <- scrape_tracklist(album_id, access_token)

    ## pull off album and artist info
    album_plus_artist_info <- tracks %>%
        select(album_name, album_id, artist_id, artist_name, artist_url) %>%
        unique()

    # uniqueness check
    if(nrow(album_plus_artist_info) > 1){stop('Non-unique album and artist info')}

    # pull first songs' info
    tracklist <- tracks %>% select(song_number, song_title, song_lyrics_url)

    # get lyrics for the tracklist of the album
    lyrics <- scrape_lyrics_tracklist(tracklist)

    # bind album and artist info to tibble
    full <- lyrics %>% mutate(album_name = album_plus_artist_info$album_name,
        album_id = album_plus_artist_info$album_id,
        artist_id = album_plus_artist_info$artist_id,
        artist_name = album_plus_artist_info$artist_name,
        artist_url = album_plus_artist_info$artist_url)

    return(full)

}


#' Retrieve lyrics associated with a tibble Genius Album IDs
#'
#' Scrape lyrics from Genius' albums using their associated IDs.
#' @param album_ids A tibble or vector of album IDs (\code{album_id} returned in \code{\link{get_song_meta}})
#' @param access_token Genius' client access token, defaults to \code{genius_token}
#' @importFrom purrr "%>%" map_df
#' @examples
#' \dontrun{
#' scrape_lyrics_discography(album_ids = tibble(album_337082)
#' }
#' @export
scrape_lyrics_discography <- function(album_ids, access_token = genius_token()){

    ## album ids should be tibble or vector of album_ids

    # unlist in case of tibble/list
    ids <- unlist(album_ids)

    # walk along ids and scrape lyrics from each id
    purrr::map_df(ids, scrape_lyrics_album, access_token)

}

