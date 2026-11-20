# Identity comes only from the fixed gateway, never message/query fields.
pa_platform_identity <- function(req) {
  if (!requireNamespace('curl', quietly = TRUE)) stop('identity_unavailable', call. = FALSE)
  headers <- list()
  for (key in c('cookie', 'authorization')) {
    value <- req$headers[[key]]
    if (is.character(value) && length(value) == 1L && !is.na(value) && nzchar(value)) headers[[key]] <- value
  }
  if (!length(headers)) stop('identity_unavailable', call. = FALSE)
  h <- curl::new_handle(timeout = 6, connecttimeout = 3, followlocation = FALSE)
  curl::handle_setheaders(h, .list = headers)
  # 身份名来自本机配置，不写死：写死会把身份问到另一台应用上。
  me_url <- if (exists('pa_gateway_me_url', mode = 'function')) pa_gateway_me_url() else NA_character_
  if (is.na(me_url) || !nzchar(me_url)) stop('identity_unavailable', call. = FALSE)
  r <- curl::curl_fetch_memory(me_url, handle = h)
  if (r$status_code != 200L) stop('identity_unavailable', call. = FALSE)
  identity <- jsonlite::fromJSON(rawToChar(r$content), simplifyVector = FALSE)
  if (!is.list(identity) || !isTRUE(identity$authenticated)) stop('identity_unavailable', call. = FALSE)
  identity
}

pa_handoff_config <- function() {
  origin <- Sys.getenv('PA_PLATFORM_ORIGIN', unset = '')
  agent <- Sys.getenv('PA_PLATFORM_AGENT_UID', unset = '')
  app_origin <- Sys.getenv('PA_APP_ORIGIN', unset = '')
  valid_origin <- function(x) grepl('^https://[A-Za-z0-9][A-Za-z0-9.-]*(:[0-9]{1,5})?$', x)
  if (!valid_origin(origin) || !valid_origin(app_origin) ||
      !grepl('^[1-9][0-9]{0,18}$', agent)) return(NULL)
  list(platform_origin = origin, app_origin = app_origin, agent_uid = agent)
}
