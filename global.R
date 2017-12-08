library(shiny)
library(elastic)
library(elasticdsl)
library(data.table)

elastic::connect(es_port = 9200)

size <- 10000

# remove unnecessary columns from elastic query result
clean_data_frame <- function(queryResult) {
  
  queryResult$`_index` <- NULL
  queryResult$`_type` <- NULL
  queryResult$`_id` <- NULL
  queryResult$`_score` <- NULL
  queryResult$sort <- NULL
  
  for (i in 1:length(queryResult)) {
    names(queryResult)[i] <- unlist(strsplit(names(queryResult)[i], "[.]"))[[2]]
  }
  
  return(queryResult[ , order(names(queryResult))])
}

meta_df <- clean_data_frame(Search(index = "meta", size = 10000, asdf = TRUE)$hits$hits)
types_df <- clean_data_frame(Search(index = "types", body = '{"sort" : { 
                       "Id" : {
                                    "order" : "asc"
                                    }
                                    }}
                                    ', size = size, asdf = TRUE)$hits$hits)

mappingForMeasurements <- meta_df$FeatureId
mappingForDates <- meta_df$DateColumn
