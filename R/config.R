library(yaml)
library(rlang)

.dstack_env <- new.env()
.dstack_env$dstack_dir <- ".dstack"
.dstack_env$in_place_config_data <- list()
.dstack_env$protocol_factory <- NULL

Profile <- Class("Profile", list(
  name = NULL,
  user = NULL,
  token = NULL,
  server = NULL,
  verify = NULL,

  initialize = function(name, user, token, server, verify = TRUE) {
    self$name <- name
    self$user <- user
    self$token <- token
    self$server <- server
    self$verify <- verify
  })
)

Config <- Interface("Config", list(
  get_profile = function(profile) { },
  list_profiles = function() { }
))

YamlConfig <-
  Class("YamlConfig",
        inherit = Config,
        private = list(
          yaml = NULL,
          file = NULL
        ),
        public = list(
          get_profile = function(profile) {
            profile <- private$yaml$profiles[[profile]]
            server <- profile$server %||% "https://api.dstack.ai"
            return(Profile$new(name = profile, token = profile$token, user = profile$user, server = server))
          },
          list_profiles = function() {
            return(names(private$yaml$profiles))
          },
          initialize = function(yaml, file) {
            private$yaml <- yaml
            private$file <- file
          }
        )
  )

InPlaceConfig <-
  Class("InPlaceConfig",
        inherit = Config,
        public = list(
          get_profile = function(profile) {
            return(.dstack_env$in_place_config_data$profiles[[profile]])
          },
          list_profiles = function() {
            return(names(.dstack_env$in_place_config_data$profiles))
          }
        ))

#' YAML-based Configuration
#'
#' It tries to find YAML file in working directory looking for \code{.dstack/config.yaml} by default.
#' If it's failed it tries to use global setting in home directory in the same relative path.
#'
#' @return A function that returns a list that contains user, token and server for specified profile.
yaml_config <- function() {
  conf <- .read_yaml_config(.dstack_env$dstack_dir, global = NULL, error_if_does_not_exist = TRUE)
  return(YamlConfig$new(conf$yaml, conf$file))
}

#' In-Place Configuration
#'
#' It is used to configure dstack in R session without creating any configuration file on disk.
in_place_config <- function() {
  return(InPlaceConfig$new())
}

.dstack_env$config <- yaml_config

get_config <- function() {
  return(.dstack_env$config())
}

.home <- function() {
  home <- path.expand('~')
  return(if (Sys.info()["sysname"] == "Windows") paste0(home, "/..") else home)
}

.read_yaml_config <- function(dstack_dir, global, error_if_does_not_exist) {
  path <- paste0(dstack_dir, "/", "config.yaml")
  if ((is.null(global) && !file.exists(path)) || (!is.null(global) && global)) path <- paste0(.home(), "/", path)
  yaml <- if (!file.exists(path)) {
    if (error_if_does_not_exist) stop(paste0("can't load config file ", path))
    list()
  } else read_yaml(path, fileEncoding = "UTF-8")
  return(list(yaml = yaml, file = path))
}

#' Configure 'dstack'
#'
#' Function allows to add or replace profile.
#' @param profile A name of profile. It will be "default" if not specified.
#' @param user Username in 'dstack'.
#' @param token A token. It can be obtained from \url{https://dstack.ai} web site.
#' @param persist Persistence level. It can be 'local' - this means that config will be stored in working directory,
#' 'global' - config will be stored in user's home,cor 'in-place' - in this case config will be store in the memory
#' and exists only while R session exists.
#' @param server Server to connect. By default it's \code{NULL}, so default API endpoint will be used.
#' @param dstack_dir Directory where dstack files are stored. By default it is '.dstack', so in the case of global persistence
#' path to config file will be \code{~/.dstack/config.yaml}.
configure <- function(profile = "default", user, token, persist = c("global", "local", "in_place"),
                      server = NULL, dstack_dir = .dstack_env$dstack_dir) {
  persist <- match.arg(persist)
  if (persist == "in_place") {
    server <- server %||% "https://api.dstack.ai"
    .dstack_env$in_place_config_data$profiles[[profile]] <- Profile$new(name = profile, user = user, token = token, server = server)
  } else {
    config <- .read_yaml_config(dstack_dir, global = (persist == "global"), error_if_does_not_exist = FALSE)
    config$yaml$profiles[[profile]] <-
      if (is.null(server)) list(user = user, token = token)
      else list(user = user, token = token, server = server)
    dstack_path <- dirname(config$file)
    if (!file.exists(dstack_path)) dir.create(dstack_path)
    write_yaml(config$yaml, config$file, fileEncoding = "UTF-8")
    .dstack_env$dstack_dir <- dstack_dir
  }
}

#' Use Specified Configuration
#'
#' @param config_func A function to be used for configuration. It can be \code{yaml_config} or \code{in_place_config}.
#' @examples
#' use_config(yaml_config) # to use standard YAML configuration file
#' use_config(in_place_config) # to use "in-place" configuration which is stored in memory
use_config <- function(config_func) {
  .dstack_env$config <- config_func
}

#' List Profiles
list_profiles <- function() {

  .show_token <- function(token) {
    l <- nchar(token)
    stars <- do.call(paste0, as.list(rep("*", l - 4)))
    return(paste0(stars, substring(token, l - 3, l)))
  }

  conf <- get_config()
  for (name in conf$list_profiles()) {
    p <- conf$get_profile(name)
    cat(paste(name,
              paste0("\tUser: ", p$user),
              paste0("\tToken: ", .show_token(p$token)),
              paste0("\tServer:", p$server),
              sep = "\n"
    ))
    cat("\n")
  }
}