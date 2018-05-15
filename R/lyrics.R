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
        lyrics$song_name <- song
        lyrics$artist_name <- artist

        # Remove lines with things such as [Intro: person & so and so]
        return(tibble::as_tibble(lyrics))
    } else{
        return(tibble::tibble(line = NA, song_lyrics_url = song_lyrics_url,
            song_name = NA, artist_name = NA))
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
