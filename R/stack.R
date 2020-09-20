library(uuid)
library(bit64)
library(rjson)
library(httr)
library(rlist)
library(rlang)

.error <- function(message) {
  stop(message)
}

.check <- function(res, error) {
  if (http_error(res)) error(http_status(res)$message)
}

version <- "0.3.0"

PushResult <- Class("PushResult", list(
  id = NULL,
  url = NULL,

  initialize = function(frame_id, url) {
    stopifnot(is.character(frame_id), length(frame_id) == 1)
    stopifnot(is.character(url), length(url) == 1)

    self$id <- frame_id
    self$url <- url
  },

  print = function(...) {
    cat(self$url, "\n", sep = "")
    invisible(self)
  }
))

StackFrame <-
  Class("StackFrame",
        public = list(

          access = NULL,
          auto_push = FALSE,
          context = NULL,
          data = NULL,
          index = 0,
          id = NULL,
          timestamp = NULL,

          initialize = function(context, access,
                                auto_push = FALSE) {
            stopifnot(is.logical(auto_push), length(auto_push) == 1)

            self$context <- context
            self$access <- access
            self$auto_push <- auto_push
            self$id <- UUIDgenerate()
            self$timestamp <- as.character(as.integer64(as.numeric(Sys.time()) * 1000))
          },

          add = function(obj, description = NULL, params = NULL, handler = NULL, ...) {
            stopifnot(!is.null(obj))
            stopifnot(!is.null(description) && is.character(description), length(description) == 1)

            handler <- handler %||% auto_handler()

            params <- .list_merge(params, list(...))
            data <- handler(obj, description, params)
            self$data <- append(self$data, list(list.clean(data)))

            if (self$auto_push == TRUE) {
              private$push_data(data)
            }
          },

          push = function(message = NULL) {
            f <- private$new_frame()
            if (!is.null(message)) {
              f$message <- message
            }
            if (self$auto_push == FALSE) {
              f$attachments <- self$data
            } else {
              f$size <- private$index
            }
            return(private$send_push(f))
          },

          send_access = function() {
            context <- self$context
            res <- context$protocol$access(context$stack_path(), context$profile$token)
            return(res)
          }
        ),

        private = list(

          push_data = function(data) {
            f <- private$new_frame()
            f$index <- self$index
            f$attachments <- list(data)
            self$index <- self$index + 1
            private$send_push(f)
          },

          send_push = function(f) {
            context <- self$context
            res <- context$protocol$push(context$stack_path(), context$profile$token, f)
            return(res)
          },

          new_frame = function() {
            profile <- self$context$profile
            return(list(stack = .stack_path(profile$user, self$context$stack),
                        token = profile$token,
                        id = self$id,
                        timestamp = self$timestamp,
                        client = "dstack-r",
                        version = version,
                        os = .get_os_info()))
          }
        ))

#' Create a New Frame in Stack
#'
#' Frame is kind of revision of data user is going to publish. It consists of one or more views. Views are usually plots
#' with some parameters to distinguish one plot from another. This function creates a frame and by default it checks
#' permission to publish to this stack.
#'
#' @param stack A name of stack to use.
#' @param profile A profile refers to credentials, i.e. username and token. Default profile is named 'default'.
#' The system is looking for specified profile as follows:
#' it looks into working directory to find a configuration file (local configuration),
#' if the file doesn't exist it looks into user directory to find it (global configuration).
#' The best way to manage profiles is to have dstack CLI tools installed. These tools are written in Python 3,
#' so you have to install dstack support. In the case of PyPI you should type
#'
#' \code{$ pip install dstack}
#'
#'or
#'
#' \code{$ conda install -c dstack.ai dstack}
#'
#' We recommend to use local (virtual) environment to install the package.
#' You can use this command in console:
#'
#' \code{$ dstack config list}
#'
#' to list existing profiles or add or replace token by following command
#'
#' \code{$ dstack config add <PROFILE>}
#'
#' or simply
#'
#' \code{$ dstack config add}
#'
#' if profile is not specified 'default' profile will be created. The system asks you about token
#' from command line, make sure that you have already obtained token from the site.
#' @param auto_push Tells the system to push frame just after commit.
#' It may be useful if you want to see result immediately. Default is \code{FALSE}.
#' @param protocol Protocol to use, usually it is \code{NULL} it means that \code{json_protocol} will be used.
#' @param check_access Check access to specified stack, default is \code{TRUE}.
#' @return New frame.
#' @examples
#' \donttest{
#' library(ggplot2)
#' library(dstack)
#' image <- qplot(clarity, data = diamonds, fill = cut, geom = "bar")
#' frame <- create_frame(stack = "diamonds")
#' frame <- commit(frame, image, "Diamonds bar chart")
#' print(push(frame)) # print actual stack URL
#' }
create_frame <- function(stack,
                         profile = "default",
                         access = c(NULL, "public", "private"),
                         auto_push = FALSE,
                         check_access = TRUE) {
  access <- match.arg(access)
  context <- .create_context(stack, profile)
  frame <- StackFrame$new(context, access, auto_push)
  if (check_access) frame$send_access()
  return(frame)
}

.create_context <- function(stack, profile) {
  config <- get_config()
  profile <- config$get_profile(profile)
  protocol <- .create_protocol(profile)
  context <- Context$new(stack, profile, protocol)
}

.create_protocol <- function(profile) {
  if (is.null(.dstack_env$protocol_factory)) {
    .dstack_env$protocol_factory <- JsonProtocolFactory$new()
  }

  return(.dstack_env$protocol_factory$create(profile))
}

#' Commit Data to Stack Frame
#'
#' Function adds a new view to the stack frame.
#' Multiple views can be added to one frame, but in this case every plot must be supplied with certain parameters
#' to distiguish one view from another. In the case of single plot parameters are not necessary.
#' For multiple views parameters will be automaticaly converted to UI controls like sliders and drop down lists.
#'
#' @param frame A frame you want to commit.
#' @param obj A data to commit. Data will be preprocessed by the handler but dependently on auto_push
#' mode will be sent to server or not. If auto_push is False then the data won't be sent.
#' Explicit push call need anyway to process committed data. auto_push is useful only in the
#' case of multiple data objects in the stack frame, e.g. set of plots with settings.
#' @param description Description of the data.
#' @param params Parameters associated with this data, e.g. plot settings.
#' @param handler A handler which can be specified in the case of custom content,
#' but by default it is \code{auto_handler}.
#' @param ... Optional parameters is an alternative to \code{params}. If both are present this one will be merged into params.
#' @return Changed frame.
#' @examples
#' \donttest{
#' library(ggplot2)
#' library(dstack)
#' image <- qplot(clarity, data = diamonds, fill = cut, geom = "bar")
#' frame <- create_frame(stack = "diamonds")
#' frame <- commit(frame, image, "Diamonds bar chart")
#' print(push(frame)) # print actual stack URL
#' }
commit <- function(frame, obj, description = NULL, params = NULL, handler = NULL, ...) {
  frame$add(obj, description, params, handler, ...)
  return(frame)
}

#' Push All Commits to Server
#'
#' Tis function is used to send a banch of commited plots to server or say server that operation with this frame is done.
#' In the case of 'auto_push' mode it sends only a total number
#' of views in the frame. So call this method is obligatory to close the frame anyway.
#'
#' @param frame A frame to push.
#' @param message Push message. \code{NULL} by default.
#' @return Stack URL.
#' @examples
#' \donttest{
#' library(ggplot2)
#' library(dstack)
#' image <- qplot(clarity, data = diamonds, fill = cut, geom = "bar")
#' frame <- create_frame(stack = "diamonds")
#' frame <- commit(frame, image, "Diamonds bar chart")
#' print(push(frame)) # print actual stack URL
#' }
push <- function(frame, message = NULL) {
  return(frame$push(message))
}

#' Creates a Frame, Commits and Pushes Data in a Single Operation
#'
#' In the case of one plot per push you can use do all operations in one call.
#' This function creates a frame, commits view and pushes all data to server.
#' The main difference in behaviour in this case is the function creates frame
#' without permission check, so be sure that you have certain permission to push in the stack.
#'
#' @param stack A name of stack to use.
#' @param obj Object to commit and push, e.g. plot.
#' @param description Optional description of the object.
#' @param params Optional parameters.
#' @param message Push message. \code{NULL} by default.
#' @param profile Profile you want to use, i.e. username and token. Default profile is 'default'.
#' @param handler Specify handler to handle the object, if it's None then \code{auto_handler} will be used.
#' @param protocol Protocol to use, usually it is \code{NULL} it means that \code{json_protocol} will be used.
#' @param ... Optional parameters is an alternative to \code{params}. If both are present this one will be merged into params.
#' @return Stack URL.
#' @examples
#' \donttest{
#' library(ggplot2)
#' library(dstack)
#' image <- qplot(clarity, data = diamonds, fill = cut, geom = "bar")
#' push_frame("diamonds", image, "Diamonds bar chart")
#' }
push_frame <- function(stack, obj, description = NULL, params = NULL,
                       message = NULL,
                       profile = "default",
                       handler = auto_handler(), ...) {
  params <- .list_merge(params, list(...))
  frame <- create_frame(stack = stack,
                        profile = profile,
                        check_access = FALSE)
  frame <- commit(frame, obj, description, params, handler)
  return(push(frame, message))
}

.stack_path <- function(user, stack) {
  return(if (startsWith(stack, "/")) substring(stack, 2) else paste(user, stack, sep = "/"))
}

#' JSON Protocol Implementation to Connect API Server
#'
#' Protocol is an abstraction which allows to send data to server.
#' This function implements JSON-based protocol. It provides token in
#' 'Authorization' header.
#' @param server A server to connect.
#' @param error An error handling function.
#' @return A function that implements JSON protocol.
json_protocol <- function(server, error = .error) {
  return(JsonProtocol$new(server = server, error = error))
}

.get_os_info <- function() {
  info <- Sys.info()
  return(list(sysname = info["sysname"], release = info["release"], version = info["version"], machine = info["machine"]))
}

.list_eq <- function(x, y) {
  if (length(y) == 0) {
    return(length(x) == 0)
  } else {
    x <- x[order(names(x))]
    y <- y[order(names(y))]
    return(identical(x, y))
  }
}

.list_merge <- function(x, y) {
  if (is.null(x)) {
    x <- list()
  }
  if (is.null(y)) {
    y <- list()
  }
  for (n in names(y)) {
    x[n] <- y[n]
  }
  if (length(x) == 0) {
    return(NULL)
  } else {
    return(x)
  }
}

#' Pull data object from stack frame (head) which matches specified parameters.
#'
#' @param stack Stack you want to pull from.
#' @param profile Profile to use. 'default' will be used if profile is not specified.
#' @param filename Filename if you want to save downloaded file to disk.
#' Lifespan of URL is limited by minutes, so filename is highly recommended for large files (> 5Mb),
#' especially in the case of interactive computations. Specify the parameter in the case of non-text data.
#' @param error HTTP error handling function.
#' @param params Optional parameters to match.
#' @param ... Parameters to match. Can be used as alternative to \code{params}. In the case of both are present this one will be merged into params.
#' @return If filename is not NULL then it will be filename itself, otherwise it can be URL in the case of large files.
#' @examples
#' \donttest{
#' df <- read.csv(pull("/public_datasets/fusionbase/covid19-germany", "Bundesland name"="All"))
#' summary(df)
#' }
pull <- function(stack, profile = "default", filename = NULL, error = .error, params = NULL, ...) {
  config <- get_config()
  profile <- config$get_profile(profile)
  if (is.null(profile)) stop(paste0("Can not find profile '", profile, "'"))
  params <- .list_merge(params, list(...))
  auth <- paste0("Bearer ", profile$token)
  stack_path <- .stack_path(profile$user, stack)
  url <- paste(profile$server, "stacks", stack_path, sep = "/")
  r <- GET(url = url, encode = "json", add_headers(.headers = c("Authorization" = auth)))
  .check(r, error)
  res <- content(r, "parsed")
  for (index in seq_along(res$stack$head$attachments)) {
    attach <- res$stack$head$attachments[[index]]
    if (.list_eq(attach$params, params)) {
      frame <- res$stack$head$id
      attach_url <- paste(profile$server, "attachs", stack_path, frame, index - 1, sep = "/")
      attach_url <- paste0(attach_url, "?download=true")
      r <- GET(url = attach_url, add_headers(.headers = c("Authorization" = auth)))
      .check(r, error)
      res <- content(r, "parsed")
      if (is.null(res$attachment$data)) {
        if (!is.null(filename)) {
          download.file(url = res$attachment$download_url, filename, quiet = TRUE)
          return(filename)
        } else {
          return(res$attachment$download_url)
        }
      } else {
        text <- rawToChar(base64enc::base64decode(res$attachment$data))
        if (is.null(filename)) filename <- tempfile()
        file <- file(filename)
        writeLines(text, file)
        close(file)
        return(filename)
      }
    }
  }
  stop(paste0("Can't match parameters ", paste(names(params), params, sep = "=", collapse = ";")))
}
