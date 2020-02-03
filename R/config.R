library(yaml)

.yaml_config <- function () {
  path <- ".dstack/config.yaml"
  if (!file.exists(path)) path <- paste0(path.expand('~'), "/", path)
  if (!file.exists(path)) stop("can't load config file")
  yaml <- read_yaml(path, fileEncoding = "UTF-8")
  return(function (profile) { return(yaml$profiles[[profile]]$token) })
}