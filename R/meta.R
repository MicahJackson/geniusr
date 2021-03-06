#' Retrieve meta data for a song
#'
#' The Genius API lets you search for meta data for a song, given a song ID.
#' @param song_id A song ID (\code{song_id} returned in \code{\link{search_song}})
#' @param access_token Genius' client access token, defaults to \code{genius_token}
#' @examples
#' \dontrun{
#' get_song_meta(song_id = 3039923)
#' }
#' @export
get_song_meta <- function(song_id, access_token=genius_token()) {

  # base URL
  base_url <- "api.genius.com/songs/"

  # search for track
  req <- httr::GET(url = paste0(base_url, song_id),
                   httr::add_headers(Authorization=paste0("Bearer ", access_token)))

  # stop if unexpected request status returned
  httr::stop_for_status(req)

  # extract request content
  res <- httr::content(req)

  # pull song meta without request meta
  song_meta <- res$response$song

  # grab album, artist, stat data
  alb <- song_meta$album
  art <- song_meta$primary_artist
  stat <- song_meta$stats

  # make list for song_info
  song_info <- tibble::lst(song_meta$id,
        song_meta$title_with_featured,
        song_meta$url,
        song_meta$song_art_image_url,
        song_meta$release_date,
        stat$pageviews,
        song_meta$annotation_count,
        art$id,
        art$name,
        art$url,
        alb$id,
        alb$name,
        alb$url)

  # find list indices of NULL values, change to NA
  ndxNULL <- which(unlist(lapply(song_info, is.null)))
  song_info[names(ndxNULL)] <- NA

  # name song_info list
  names(song_info) <- c('song_id', 'song_name', 'song_lyrics_url', 'song_art_image_url', 'release_date',
        'pageviews','annotation_count','artist_id', 'artist_name','artist_url','album_id','album_name','album_url')

  return(tibble::as_tibble(song_info))
}


#' Retrieve meta data for an artist
#'
#' The Genius API lets you search for meta data for an artist, given an artist ID.
#' @param artist_id An artist ID (\code{artist_id} returned in \code{\link{search_artist}})
#' @param access_token Genius' client access token, defaults to \code{genius_token}
#' @examples
#' \dontrun{
#' get_artist_meta(artist_id = 16751)
#' }
#' @export
get_artist_meta <- function(artist_id, access_token=genius_token()) {

  # base URL
  base_url <- "api.genius.com/artists/"

  # search for artist
  req <- httr::GET(url = paste0(base_url, artist_id),
                   httr::add_headers(Authorization=paste0("Bearer ", access_token)))

  # stop if unexpected request status returned
  httr::stop_for_status(req)

  # extract request content
  res <- httr::content(req)

  # drill down
  res <- res$response$artist

  # extract artist info from returned results
  artist_info <- tibble::lst(
      res$id,
      res$name,
      res$url,
      res$image_url,
      res$followers_count
  )

  ## find list indices of NULL values, change to NA
  ndxNULL <- which(unlist(lapply(artist_info, is.null)))
  artist_info[names(ndxNULL)] <- NA

  ## name artist_info list
  names(artist_info) <- c('artist_id', 'artist_name', 'artist_url', 'artist_image_url',
      'followers_count')

  # isolate unique pairs
  return(tibble::as_tibble(artist_info))

}


#' Retrieve meta data for an album
#'
#' The Genius API lets you search for an album's meta data, given an album ID.
#' @param album_id An album ID (\code{album_id} returned in \code{\link{get_song_meta}})
#' @param access_token Genius' client access token, defaults to \code{genius_token}
#' @examples
#' \dontrun{
#' get_album_meta(album_id = 337082)
#' }
#' @export
get_album_meta <- function(album_id, access_token=genius_token()) {

  # API ----------------------

  # base URL
  base_url <- "api.genius.com/albums/"

  # search for album
  req <- httr::GET(url = paste0(base_url, album_id),
                   httr::add_headers(Authorization=paste0("Bearer ", access_token)))

  # stop if unexpected request status returned
  httr::stop_for_status(req)

  # extract request content
  res <- httr::content(req)

  # drill down
  res <- res$response$album
  art <- res$artist

  # extract album info from returned results
  album_info <- tibble::lst(
      res$id,
      res$name,
      res$url,
      res$cover_art_url,
      res$release_date,
      res$song_pageviews,
      art$id,
      art$name,
      art$url
  )

  ## find list indices of NULL values, change to NA
  ndxNULL <- which(unlist(lapply(album_info, is.null)))
  album_info[names(ndxNULL)] <- NA

  ## name album_info list
  names(album_info) <- c('album_id', 'album_name', 'album_url', 'album_cover_art_url',
      'album_release_date', 'pageviews', 'artist_id', 'artist_name', 'artist_url')

  # isolate unique pairs
  return(tibble::as_tibble(album_info))

}
