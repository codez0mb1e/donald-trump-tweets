

#'
#' Microsoft Cognitive Services Text Analytics APIs
#'



cognitive_services <- list(
  host = "westus2.api.cognitive.microsoft.com",
  version = "2.1"
)

get_api_url <- function(service_name) {
  stopifnot(is.character(service_name))
  
  return(sprintf("https://%s/text/analytics/v%s/%s", 
                 cognitive_services$host, 
                 cognitive_services$version, 
                 service_name))
}



#' Get result from Sentiment Analysis API
#'
#' @param dt Dataframe that contains id_str and text fields
#' @param api_key Cognitive Services API key
#' @param language Langauage (support only English)
#' @param verbose 
#'
#' @return Dataframe with sentment scores
#' 
get_sentiment <- function(dt, api_key, language = "en", verbose = F) {
  require(dplyr)
  require(purrr)
  
  request_api(dt, get_api_url("sentiment"), api_key, language, verbose) %>% 
    flatten_df()
}



#' Get result from Key Phrase Extraction API 
#'
#' @param dt Dataframe that contains id_str and text fields
#' @param api_key Cognitive Services API key
#' @param language Langauage (support only English)
#' @param verbose 
#'
#' @return Dataframe with sentment scores
#' 
get_key_phrases <- function(dt, api_key, language = "en", verbose = F) {
  request_api(dt, get_api_url("keyPhrases"), api_key, language, verbose)
}



#' Request to Text Analytics API (internal method)
#'
#' @param dt 
#' @param api_url 
#' @param api_key 
#' @param language 
#' @param verbose 
#'
request_api <- function(dt, api_url, api_key, language, verbose) {
  require(dplyr)
  require(httr)
  require(jsonlite)
  
  stopifnot(
    is.data.frame(dt),
    is.character(api_url),
    is.character(api_key),
    is.character(language) && language == "en",
    is.logical(verbose)
  )
  
  
  request_body <- tibble(
    id = dt$id,
    language = rep(language, nrow(dt)),
    text = dt$text,
  )
  
  if (verbose) print(sprintf("Processing tweets number: %s, size: %s", nrow(request_body), format(object.size(request_body), units = "Kb")))
  
  
  request_body_json <- toJSON(list(documents = request_body), auto_unbox = T)
  
  
  responses <- tryCatch(
    {
      result <- POST(api_url,
                     body = request_body_json,
                     add_headers(.headers = c("Content-Type" = "application/json", "Ocp-Apim-Subscription-Key" = api_key)))
    },
    warning=function(cond) {
      warning("Warn")
    },
    error=function(cond) {
      warning("Error")
    },
    finally={
      if (verbose) message("Completed.")
    }
  )    
  
  
  content(responses)
}

