library(yaml)

.dstack_env <- new.env()
.dstack_env$dstack_dir <- ".dstack"
.dstack_env$in_place_config_data <- list()

#' YAML-based Configuration
#'
#' It tries to find YAML file in working directory looking for \code{.dstack/config.yaml} by default.
#' If it's failed it tries to use global setting in home directory in the same relative path.
#'
#' @return A function that returns a list that contains user, token and server for specified profile.
yaml_config <- function () {
  conf <- .read_yaml_config(.dstack_env$dstack_dir, global = NULL, error_if_does_not_exist = TRUE)
  return(function (profile) {
    profile <- conf$yaml$profiles[[profile]]
    server <- if (is.null(profile$server)) "https://api.dstack.ai" else profile$server
    return(list(token=profile$token, user=profile$user, server=server))
  })
}

in_place_config <- function () {
  return(function(profile) {
    return(.dstack_env$in_place_config_data$profiles[[profile]])
  })
}

.dstack_env$config <- yaml_config

get_config <- function () {
  return(.dstack_env$config())
}

.home <- function () {
  home <- path.expand('~')
  return(if (Sys.info()["sysname"] == "Windows") paste0(home, "/..") else home)
}

.read_yaml_config <- function (dstack_dir, global, error_if_does_not_exist) {
  path <- paste0(dstack_dir, "/", "config.yaml")
  if ((is.null(global) && !file.exists(path)) || (!is.null(global) && global)) path <- paste0(.home(), "/", path)
  yaml <- if (!file.exists(path)) {
    if (error_if_does_not_exist) stop(paste0("can't load config file ", path))
    list()
  } else read_yaml(path, fileEncoding = "UTF-8")
  return(list(yaml = yaml, file = path))
}

configure <- function (profile, user, token, persist = c("global", "local", "in_place"),
                       server = NULL, dstack_dir = .dstack_env$dstack_dir) {
  persist <- match.arg(persist)
  if (persist == "in_place") {
    server <- if (is.null(server)) "https://api.dstack.ai" else server
    .dstack_env$in_place_config_data$profiles[[profile]] <- list(user = user, token = token, server = server)
    .dstack_env$config <- in_place_config
  } else {
    config <- .read_yaml_config(dstack_dir, global = (persist == "global"), error_if_does_not_exist = FALSE)
    config$yaml$profiles[[profile]] <- if (is.null(server)) list(user = user, token = token)
                              else list(user = user, token = token, server = server)
    dstack_path <- dirname(config$file)
    if (!file.exists(dstack_path)) dir.create(dstack_path)
    write_yaml(config$yaml, config$file, fileEncoding = "UTF-8")
    .dstack_env$config <-  yaml_config
    .dstack_env$dstack_dir <- dstack_dir
  }
}

list_profiles <- function () {
  conf <- get_config()
  for (p in conf) {
    print(p)
  }
}