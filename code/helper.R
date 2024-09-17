# Nvidia chat ####
chat_nvidia <- function(user_message,
                        history = NULL,
                        api_key, 
                        model_llm, 
                        temp = 0.2, 
                        topp = 0.7, 
                        max_token = 1024) {
  user_prompt <-  list(
    list(
      role = "system",
      content = "As an influencer, you are giving talk in Tedx.You can talk about various topics - 
      Option A: Nature or environment or climate, 
      Option B: Science or Technology, 
      Option C: Education or knowledge, 
      Option D: Social Media, 
      Option E: Economy or Finance, 
      Option F: Travel or new places or new experiences, 
      Option G: Others"
    ),
    list(role = "user", content = user_message)
  )
  prompt   <- c(history, user_prompt) |> purrr::compact()
  
  base_url <- "https://integrate.api.nvidia.com/v1"
  body     <- list(model = model_llm, messages = prompt, temperature = temp, top_p = topp,
                   max_tokens = max_token)
  req <-
    resp <-
    request(base_url) |>
    req_url_path_append("chat/completions") |>
    req_auth_bearer_token(token = api_key) |>
    req_headers("Content-Type" = "application/json") |>
    req_user_agent("Soumyadipta Das") |>
    req_body_json(body) |>
    req_retry(max_tries = 4) |>
    req_throttle(rate = 15) |>
    req_perform()
  
  openai_chat_response <-
    resp |> resp_body_json(simplifyVector = TRUE)
  
  return(fromJSON(openai_chat_response$choices$message$content))
}
