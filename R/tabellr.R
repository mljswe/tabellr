



#' prepare json-post
#'
#' @description reads scb_body.txt (json-body) and scb_url.txt (url). designed to work with api at scb.se
#' @return list: [1] = url [2] = body in json
#' @param path_to_file file path to folder with scb_url.txt and scb_body.txt
#' @details response format in body from scb.se is defined as 'px', but scb_sup changes it to json.
#'
scb_sup <- function(path_to_files = p1) {
  # read url
  scb_url <-
    readr::read_file(file.path(path_to_files, "scb_url.txt"))

  #read body
  json_fraga2 <-
    readr::read_file(file.path(path_to_files, "scb_body.txt"))

  # format body
  bodytxt <- jsonlite::fromJSON(json_fraga2,
                                simplifyVector = FALSE,
                                simplifyDataFrame = FALSE)

  # change response format
  bodytxt$response$format <- "json"

  #put in list and out
  json_out <- list(scb_url, bodytxt)
  return(json_out)
}


#' parse json respone from scb.se#'
#'
#' @param ldatajson json response (content) from scb.se api
#' @references copied in its entiery from https://dittochdata.netlify.app/2018/06/10/ladda-hem-data-fr%C3%A5n-scb-del-2-anv%C3%A4nda-scb-s-api/
#' @author Christian Lindell
#' @return table with correctly formated columns
#'
fparse_jsontab_scb <- function(ldatajson) {
  dftab <-
    dplyr::bind_cols(
      do.call(rbind.data.frame, ldatajson$data$key),
      do.call(rbind.data.frame, ldatajson$data$values)
    )
  names(dftab) <- ldatajson$columns$text
  print(dftab)
  dftab <- dplyr::mutate_if(dftab, is.factor, as.character)
  for (i in 1:ncol(dftab)) {
    typ <- ldatajson$columns[[i, "type"]]
    if (typ == "c") {
      dftab[i] <- as.numeric(dftab[[i]])
    }
    else if (typ == "t" &
             !is.na(as.numeric(dftab[[1, i]])) &
             !(ldatajson$columns$text[i] %in% c("\u00e5r", "year"))) {
      dftab[i] <- as.numeric(dftab[[i]])
    }
    else if (typ == "t" &
             !is.na(as.numeric(dftab[[1, i]])) &
             ldatajson$columns$text[i] %in% c("\u00e5r", "year")) {
      dftab[i] <- as.integer(dftab[[i]])
    }
  }
  return(dftab)
}




#' post, extract content and parse
#' @description makes the post, extracts payload and parses with fparse_jsontab_scb
#' @return correctly formated table
#'
scb_results <- function() {
  req <- httr::POST(scb_sup()[[1]],
                    body = scb_sup()[[2]],
                    encode = "json",
                    httr::verbose())

  lanswer <- httr::content(req, simplifyDataFrame = TRUE)

  resultat <- fparse_jsontab_scb(lanswer)
  return(resultat)
}


#' unpack meta-data from scb.se
#'
#' @param n number of fields to unpack
#' @details "tabellinnehall" and "vartannat ar" excluded - no meta needed for these
#' @return list with lookups for table dimensions
scb_meta_unpack <- function(n) {
  var <- scb_meta_results$variables[[n]]$text

  if (var %in% c("tabellinneh\u00e5ll", "vartannat \u00e5r")) {
    return(NULL)

  } else {
    code <- scb_meta_results$variables[[n]]$values
    text <- scb_meta_results$variables[[n]]$valueTexts

    out <- as.data.frame(cbind(unlist(code), unlist(text)))

    colnames(out) <- c(paste0(var), paste0(var, "_text"))

    return(out)
  }
}



#' clean and store in list
#'
#' @return lookup-table with meta data for a single dimesion
#'
scb_meta_func <- function() {
  scb_meta_results <<- httr::content(httr::GET(scb_sup()[[1]]))
  out <-
    list(lapply(1:length(scb_meta_results$variables), scb_meta_unpack))
  out <- lapply(out, function(x)
    x[lengths(x) > 0])
  rm(scb_meta_results, envir = .GlobalEnv)
  return(out)
}




#' create table with data from scb.se
#'
#' @return df with data from selected indicator on scb.se
#' @description inner function loops over all lookups in scb_metadata.
#' @export
#'
scb_tabell <- function(path_to = p1) {
  scb_resultat <- scb_results()
  scb_metadata <- scb_meta_func()

  func <- function(n) {
    merge_frame <- as.data.frame(scb_metadata[[1]][n])
    dplyr::left_join(scb_resultat, merge_frame, by = colnames(merge_frame[1]))
  }

  for (i in 1:length(scb_metadata[[1]])) {
    scb_resultat <- func(i)
  }
  return(scb_resultat)
}
