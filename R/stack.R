library(uuid)
library(nanotime)
library(rjson)
library(httr)
library(rlist)

.error <- function (status, message) {
  stop(message)
}

.check <- function (res, error) {
  if (res$status != 0) error(res$status, res$message)
}

create_frame <- function (stack, token, handler,
                          auto_push  = FALSE,
                          protocol   = json_protocol("https://api.dstack.ai"),
                          encryption = no_encryption,
                          error      = .error) {
  frame <- list(stack      = stack,
                token      = token,
                handler    = handler,
                auto_push  = auto_push,
                protocol   = protocol,
                encryption = encryption,
                data       = list(),
                index      = 0,
                error      = error)
  frame$id <- UUIDgenerate()
  frame$timestamp <- as.character(as.integer64(nanotime(Sys.time())))
  .check(send_access(frame), error)
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
  .check(send_push(f), frame$error)
  return(frame)
}

push <- function (frame) {
  f <- new_frame(frame)
  if (frame$auto_push == FALSE) {
    f$attachments <- frame$data
    .check(send_push(frame, f), frame$error)
  } else {
    f$total <- frame$index
    .check(send_push(frame, f), frame$error)
  }
}

new_frame <- function (frame) {
  return(list(stack     = frame$stack,
              token     = frame$token,
              id        = frame$id,
              timestamp = frame$timestamp,
              type      = frame$handler$type))
}


send_access <- function (frame) {
  req <- list(stack = frame$stack, token = frame$token)
  res <- frame$protocol("/stacks/access", req)
  return(res)
}

send_push <- function (frame, f) {
  res <- frame$protocol("/stacks/push", f)
  return(res)
}

json_protocol <- function (server) {
  return(function (endpoint, data) {
    r <- POST(paste0("https://api.dstack.ai", endpoint), body = data, encode = "json")
    return(content(r, "parsed"))
  })
}

no_encryption <- function (data) {
  return(data)
}

