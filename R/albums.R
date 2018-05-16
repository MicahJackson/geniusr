#' Return album of a song by song_id
#'
#' Scrape lyrics from Genius' albums using their associated IDs.
#' @param song_id A song_id returned in \code{\link{get_song_meta}})
#' @importFrom purrr "%>%"
#' @examples
#' \dontrun{
#' album_of_song(song_id = 3214267)
#' }
#' @export
album_of_song <- function(song_id){

    ## grab the album of a song from meta data
    ## primary use inside of get_artist_albums

    ## output tibble with album_name and _id
    album <- get_song_meta(song_id) %>% select(album_name, album_id)
    return(album)
}



#' Retrieve tibble of albums and their respective IDs for a given artist_id.
#'
#' Scrape lyrics from Genius' albums using their associated IDs.
#' @param artist_id An artist_id returned from \code{search_artist}
#' @param include_features Whether to return results where artist isn't the primary artist (logical, defaults to FALSE)
#' @param access_token Genius' client access token, defaults to \code{genius_token}
#' @importFrom purrr "%>%" map_dfr
#' @examples
#' \dontrun{
#' get_artist_albums(artist_id = 1421)
#' }
#' @export
get_artist_albums <- function(artist_id, include_features = FALSE, access_token = genius_token()){

    ## output a tibble with names of all albums put out by artist

    # retrieve all songs by artist
    songs <- get_artist_songs(artist_id, include_features = include_features, access_token)

    # pull unique albums
    album_list <- songs %>% dplyr::bind_cols(purrr::map_dfr(songs$song_id, album_of_song)) %>%
        select(album_name, album_id) %>% unique() %>% na.omit() %>% arrange(album_id)

    return(album_list)

}
