is_VecTraits_alive <- function() {
  VecTraits <- httr::GET(url = "https://vectorbyte-qa.crc.nd.edu/portal/api/vectraits-explorer/?format=json")
  return(httr::status_code(VecTraits) == 200)
}
