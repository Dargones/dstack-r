library(yaml)

#' YAML-based Configuration
#'
#' It tries to find YAML file in working directory looking for \code{.dstack/config.yaml} by default.
#' If it's failed it tries to use global setting in home directory in the same relative path.
#'
#' @param dstack_dir Directory which contains config.yaml, locally or globally, by default it is \code{.dstack}.
#' @return A function that returns a list that contains user, token and server for specified profile.
yaml_config <- function (dstack_dir = ".dstack") {
  path <- paste0(dstack_dir, "/", "config.yaml")
  if (!file.exists(path)) path <- paste0(.home(), "/", path)
  if (!file.exists(path)) stop(paste0("can't load config file ", path))
  yaml <- read_yaml(path, fileEncoding = "UTF-8")
  return(function (profile) {
    profile <- yaml$profiles[[profile]]
    server <- if (is.null(profile$server)) "https://api.dstack.ai" else profile$server
    return(list(token=profile$token, user=profile$user, server=server))
  })
}

.home <- function () {
  home <- path.expand('~')
  return(if (Sys.info()["sysname"] == "Windows") paste0(home, "/..") else home)
}
