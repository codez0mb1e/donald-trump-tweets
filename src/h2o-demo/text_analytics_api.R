
#'
#' Text Analytics APIs functions
#'


#' Get sentiment from Microsoft Cognitive Services Text Analytics API
#'
#' @param dt Dataframe that contains id_str and text fields
#' @param api_key Cognitive Services API key
#'
#' @return Dataframe with sentment scores
#' 
get_sentiment <- function(dt, api_key, verbose = F) {
  require(dplyr)
  require(purrr)
  require(httr)
  require(jsonlite)
  require(purrr)
  
  stopifnot(
    is.data.frame(dt),
    is.character(api_key)
  )
  
  
  request_body <- tibble(
    id = dt$id_str,
    language = rep("en", nrow(dt)),
    text = dt$text,
  )
  
  if (verbose) print(sprintf("Processing tweets number: %s, size: %s", nrow(request_body), format(object.size(request_body), units = "Kb")))
  
  request_body_json <- toJSON(list(documents = request_body), auto_unbox = T)
  
  
  out <- tryCatch(
    {
      result <- POST("https://westus2.api.cognitive.microsoft.com/text/analytics/v2.0/sentiment",
                     body = request_body_json,
                     add_headers(.headers = c("Content-Type" = "application/json", "Ocp-Apim-Subscription-Key" = api_key)))
    },
    error=function(cond) {
      message("Error")
    },
    warning=function(cond) {
      warning("Warn")
    },
    finally={}
  )    
  
  
  content(result) %>% 
    flatten_df()
}



