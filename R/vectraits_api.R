# Sean Sorek 9/29/2023
# Based on code written by ???

get_web_data <- function(data_URL) {
  if (!exists("webDataLibrariesOpen")) {
    webDataLibrariesOpen <- TRUE
  }
  web_data <- httr::GET(url = data_URL)
  sc <- httr::status_code(web_data)
  if (sc >= 300 || sc < 200) stop("Data fetch failed. HTTP Code: ", sc)

  out <- jsonlite::fromJSON(
    httr::content(web_data, "text", encoding = "UTF-8"),
    flatten = TRUE
  )
  return(out)
}


ask_for_ID <- function(td) {
  # need to check if session is interactive before prompting input
  ID <- readline(prompt = "Enter a dataset ID: ")
  if (!is.integer(ID) || ID < 1 || ID > td) {
    stop(
      "The dataset ID ", ID,
      " is invalid or is out of range. Please choose a number between 1 and ", td, "."
    )
  }
  ID
}


#' Retreive a Dataset from VecTraits
#'
#' @param ID integer, the dataset to retrieve
#'
#' @return A dataframe containing the dataset in Vectraits corresponding to ID
#' @export
#'
get_dataset <- function(ID = -1) {
  td <- as.integer(
    get_web_data("https://vectorbyte-qa.crc.nd.edu/portal/api/vectraits-explorer/?format=json")$data$count
  )
  if (!is.numeric(ID) || ID < 1 || ID > td) {
    if (interactive()) {
      dataset_ID <- ask_for_ID(td)
    } else {
      stop(
        "The dataset ID ", dataset_ID,
        " is invalid or is out of range. Please choose a number between 1 and ", td, "."
      )
    }
  } else {
    dataset_ID <- ID
  }

  dataset <- get_web_data(paste0(
    "https://vectorbyte-qa.crc.nd.edu/portal/api/vectraits-dataset/",
    trunc(dataset_ID), #dont wanna throw an error for non-integer values
    "/?format=json"
  ))
  return(dataset)
}
