is_VecTraits_alive <- function() {
  VecTraits <- tryCatch(
    httr::GET(url = "https://vectorbyte-qa.crc.nd.edu/portal/api/vectraits-explorer/?format=json")
    , error = function(e) 404
  )

  return(httr::status_code(VecTraits) == 200)
}
