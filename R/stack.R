library(uuid)
library(nanotime)
library(rjson)
library(httr)
library(rlist)

.error <- function (message) {
  stop(message)
}

.check <- function (res, error) {
  if (http_error(res)) error(http_status(res)$message)
}

version <- "0.1.0"

create_frame <- function (stack, handler,
                          profile    = "default",
                          auto_push  = FALSE,
                          protocol   = NULL,
                          config     = yaml_config(),
                          encryption = no_encryption) {
  conf <- config(profile)
  protocol <- if (is.null(protocol)) json_protocol(conf$server) else protocol
  frame <- list(stack      = stack,
                user       = conf$user,
                token      = conf$token,
                handler    = handler,
                auto_push  = auto_push,
                protocol   = protocol,
                encryption = encryption,
                data       = list(),
                index      = 0)
  frame$id <- UUIDgenerate()
  frame$timestamp <- as.character(as.integer64(as.numeric(Sys.time()) * 1000)) # milliseconds
  send_access(frame)
  return(frame)
}

commit <- function (frame, obj, description=NULL, params=NULL) {
  data <- frame$handler$as_frame(obj, description, params)
  encrypted_data <- frame$encryption(data)
  frame$data <- append(frame$data, list(list.clean(encrypted_data)))
  if (frame$auto_push == TRUE) {
    frame <- push_data(encrypted_data)
  }
  return(frame)
}

push_data <- function (frame, data) {
  f <- new_frame(frame)
  f$index <- frame$index
  f$attachments[[1]] <- data
  frame$index <- frame$index + 1
  send_push(f)
  return(frame)
}

push <- function (frame) {
  f <- new_frame(frame)
  if (frame$auto_push == FALSE) {
    f$attachments <- frame$data
  } else {
    f$total <- frame$index
  }
  return(send_push(frame, f))
}

push_frame <- function (stack, obj, descripton = NULL, params = NULL,
                        handler    = ggplot_handler(),
                        protocol   = NULL,
                        profile    = "default",
                        config     = yaml_config(),
                        encryption = no_encryption) {
  frame <- create_frame(stack      = stack,
                        handler    = handler,
                        protocol   = protocol,
                        profile    = profile,
                        config     = config,
                        encryption = encryption)
  frame <- commit(frame, obj, descripton, params)
  return(push(frame))
}

stack_path <- function(frame) {
  return(paste(frame$user, frame$stack, sep="/"))
}

new_frame <- function (frame) {
  return(list(stack     = stack_path(frame),
              token     = frame$token,
              id        = frame$id,
              timestamp = frame$timestamp,
              type      = frame$handler$type,
              client    = "dstack-r",
              version   = version,
              os        = get_os_info()))
}

send_access <- function (frame) {
  req <- list(stack = stack_path(frame), token = frame$token)
  res <- frame$protocol("/stacks/access", req)
  return(res)
}

send_push <- function (frame, f) {
  res <- frame$protocol("/stacks/push", f)
  return(res)
}

json_protocol <- function (server, error = .error) {
  return(function (endpoint, data) {
    auth <- paste0("Bearer ", data$token)
    body <- list.remove(data, "token")
    # r <- with_config(verbose(), POST(paste0(server, endpoint),
    r <- POST(paste0(server, endpoint),
              body = body, encode = "json",
              add_headers(.headers = c("Authorization"=auth)))
    .check(r, error)
    return(content(r, "parsed"))
  })
}

no_encryption <- function (data) {
  return(data)
}

get_os_info <- function() {
  info <- Sys.info()
  return(list(sysname=info["sysname"], release=info["release"], version=info["version"], machine=info["machine"]))
}
