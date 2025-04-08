get_cdc_data = function(endpoint){
  dataframe <- request(endpoint) |> 
    req_url_query("$limit" = 10000000) |>
    req_perform() |> 
    resp_body_json(simplifyVector = TRUE)
  return(dataframe)
}