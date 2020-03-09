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

#' Create a New Frame in Stack
#'
#' Frame is kind of revision of data user is going to publish. It consists of one or more views. Views are usually plots
#' with some parameters to distinguish one plot from another. This function creates a frame and by default it checks
#' permission to publish to this stack.
#'
#' @param stack A name of stack to use.
#' @param profile A profile refers to credentials, i.e. username and token. Default profile is named 'default'.
#' @param handler A handler which can be specified in the case of custom content,
#' but by default it is \code{json_handler}.
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
#' \code{$ dstack config --list}
#'
#' to list existing profiles or add or replace token by following command
#'
#' \code{$ dstack config --profile <PROFILE>}
#'
#' or simply
#'
#' \code{$ dstack config}
#'
#' if profile is not specified 'default' profile will be created. The system asks you about token
#' from command line, make sure that you have already obtained token from the site.
#' @param auto_push Tells the system to push frame just after commit.
#' It may be useful if you want to see result immediately. Default is \code{FALSE}.
#' @param protocol Protocol to use, usually it is \code{NULL} it means that \code{json_protocol} will be used.
#' @param config A configuration, by default it it will be obtained from YAML configuration files, so \code{yaml_config} will be used.
#' @param encryption This is a ecryption method. By default is \code{NULL} and no encryption will be used.
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
create_frame <- function (stack,
                          profile      = "default",
                          handler      = ggplot_handler(),
                          auto_push    = FALSE,
                          protocol     = NULL,
                          config       = yaml_config(),
                          encryption   = NULL,
                          check_access = TRUE) {
  if (is.null(encryption)) encryption <- .no_encryption
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
  if (check_access) .send_access(frame)
  return(frame)
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
commit <- function (frame, obj, description=NULL, params=NULL) {
  data <- frame$handler$to_frame_data(obj, description, params)
  encrypted_data <- frame$encryption(data)
  frame$data <- append(frame$data, list(list.clean(encrypted_data)))
  if (frame$auto_push == TRUE) {
    frame <- push_data(frame, encrypted_data)
  }
  return(frame)
}

push_data <- function (frame, data) {
  f <- .new_frame(frame)
  f$index <- frame$index
  f$attachments <- list(data)
  frame$index <- frame$index + 1
  .send_push(frame, f)
  return(frame)
}

#' Push All Commits to Server
#'
#' Tis function is used to send a banch of commited plots to server or say server that operation with this frame is done.
#' In the case of 'auto_push' mode it sends only a total number
#' of views in the frame. So call this method is obligatory to close the frame anyway.
#'
#' @param frame A frame to push.
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
push <- function (frame) {
  f <- .new_frame(frame)
  if (frame$auto_push == FALSE) {
    f$attachments <- frame$data
  } else {
    f$size <- frame$index
  }
  return(.send_push(frame, f))
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
#' @param profile Profile you want to use, i.e. username and token. Default profile is 'default'.
#' @param handler Specify handler to handle the object, if it's None then \code{ggplot_handler} will be used.
#' @param protocol Protocol to use, usually it is \code{NULL} it means that \code{json_protocol} will be used.
#' @param config Configuration to manage profiles. If it is unspecified \code{yaml_config} will be used.
#' @param encryption Encryption method by default \code{no_encryption} will be used.
#' @return Stack URL.
#' @examples
#' \donttest{
#' library(ggplot2)
#' library(dstack)
#' image <- qplot(clarity, data = diamonds, fill = cut, geom = "bar")
#' push_frame("diamonds", image, "Diamonds bar chart")
#' }
push_frame <- function (stack, obj, description = NULL, params = NULL,
                        profile    = "default",
                        handler    = ggplot_handler(),
                        protocol   = NULL,
                        config     = yaml_config(),
                        encryption = .no_encryption) {
  frame <- create_frame(stack        = stack,
                        handler      = handler,
                        protocol     = protocol,
                        profile      = profile,
                        config       = config,
                        encryption   = encryption,
                        check_access = FALSE)
  frame <- commit(frame, obj, description, params)
  return(push(frame))
}

.stack_path <- function(frame) {
  return(if (startsWith(frame$stack, "/")) substring(frame$stack, 2) else paste(frame$user, frame$stack, sep="/"))
}

.new_frame <- function (frame) {
  return(list(stack     = .stack_path(frame),
              token     = frame$token,
              id        = frame$id,
              timestamp = frame$timestamp,
              type      = frame$handler$type,
              client    = "dstack-r",
              version   = version,
              os        = .get_os_info()))
}

.send_access <- function (frame) {
  req <- list(stack = .stack_path(frame), token = frame$token)
  res <- frame$protocol("/stacks/access", req)
  return(res)
}

.send_push <- function (frame, f) {
  res <- frame$protocol("/stacks/push", f)
  return(res)
}

#' JSON Protocol Implementation to Connect API Server
#'
#' Protocol is an abstraction which allows to send data to server.
#' This function implements JSON-based protocol. It provides token in
#' 'Authorization' header.
#' @param server A server to connect.
#' @param error An error handling function.
#' @return A function that implements JSON protocol.
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

.no_encryption <- function (data) {
  return(data)
}

.get_os_info <- function() {
  info <- Sys.info()
  return(list(sysname=info["sysname"], release=info["release"], version=info["version"], machine=info["machine"]))
}
