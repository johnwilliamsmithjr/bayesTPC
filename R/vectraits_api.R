# Sean Sorek 11/7/2023
# Based on code written by Wesley McGinn

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


ask_for_ID <- function(max_id) {
  # need to check if session is interactive before prompting input
  c <- getOption("ask_dataset_ID.con", stdin()) #for testing. or really weird use cases ig
  cat("Enter a dataset ID: ")
  ID <- readLines(con = c, n = 1)
  ID <- as.integer(ID)

  if (is.na(ID) || ID < 1 || ID > max_id) {
    stop(
      "The dataset ID ", ID,
      " is invalid or is out of range. Please choose a number between 1 and ", max_id, "."
    )
  }
  ID
}


#' Retreive a Dataset from VecTraits
#'
#' @param ID integer, the dataset to retrieve.
#' @param check_interactive logical, should the user be prompted to input an ID if one isn't provided? Default is TRUE
#'
#' @return A dataframe containing the dataset in Vectraits corresponding to ID
#' @export
#'
get_VB_dataset <- function(ID = -1, check_interactive = TRUE) {
  if (length(ID) != 1) stop("Invalid length for input 'ID'. If pulling multiple datasets, please use get_datasets() instead.")
  td <- as.integer(
    get_web_data("https://vectorbyte.crc.nd.edu/portal/api/vectraits-explorer/?format=json")$data$count
  )

  if (!is.numeric(ID) || is.na(ID) || ID < 1 || ID > td) {
    if (interactive() && check_interactive) {
      dataset_ID <- ask_for_ID(td)
    } else {
      stop(
        "The dataset ID ", ID,
        " is invalid or is out of range. Please choose a number between 1 and ", td, "."
      )
    }
  } else {
    dataset_ID <- ID
  }

  dataset <- get_web_data(paste0(
    "https://vectorbyte.crc.nd.edu/portal/api/vectraits-dataset/",
    trunc(dataset_ID), #dont wanna throw an error for non-integer values
    "/?format=json"
  ))
  class(dataset[[1]]) <- c("data.frame","Vectraits_dataset")
  return(dataset[[1]])
}

#' Retrieve multiple datasets from VecTraits
#'
#' @param IDS integer, the datasets to retrieve.
#' @param safety logical, should an error be thrown if too many datasets are retreived?. Default is TRUE.
#' @return A list of the datasets requested.
#' @export
get_VB_datasets <- function(IDS, safety = TRUE) {
  if (length(IDS) < 1) stop("Input 'IDS' must have at least length 1.")
  if(length(IDS) > 10) {
    if (safety) {
      stop("Attempt to retreive too many datasets. Either set safety = FALSE or retreive fewer datasets.")
    } else {
      warning("Pulling a large number of datasets may take a while.\n", immediate. = TRUE)
    }

  }
  out <- list()
  for (i in 1:length(IDS)) {
    cat("Retrieving dataset:",IDS[i],"\n")
    out[[i]] <- get_VB_dataset(IDS[i], check_interactive = F)
  }
  out
}


#' Searches and retrieves Vectraits datasets
#'
#' @param keywords character, one or more search terms.
#' @param safety logical, should an error be thrown if too many datasets are retreived?. Default is TRUE.
#'
#' @return A list of all datasets matching the search terms provided.
#' @export
find_VB_datasets <- function(keywords, safety = TRUE) {
  stopifnot("Invalid keyword" = is.character(keywords))
  stopifnot("Keyword must be provided" = length(keywords) > 0)

  set_search <- get_web_data(
    paste(
      c(
        "https://vectorbyte.crc.nd.edu/portal/api/vectraits-explorer/?format=json&keywords=",
        gsub(" ", "%20", paste(keywords, collapse = "%20"))
      ),
      collapse = ""
    )
  )


  cat(paste0(length(set_search$ids)," datasets found. Attempting to retreive.\n\n"))
  return(get_VB_datasets(set_search$ids, safety))

}
