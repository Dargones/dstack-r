library(rlist)
library(R6)

.classname <- function(object) {
  return(class(object)[[1]])
}

.abstract_methods <- function(inherit, implement, abstract = list()) {
  inherit <- inherit %||% list(abstract = list())
  abstract <- append(abstract, inherit$abstract)

  implement <- list.flatten(list.map(implement, .$abstract))

  return(.merge_abstract_methods(abstract, implement))
}

.merge_abstract_methods <- function(abstract, implement) {
  methods <- append(abstract, implement)
  reduced <- list()
  ns <- names(methods)

  for(i in seq_along(methods)) {
    k <- ns[i]
    reduced[[k]] <- append(reduced[[k]], methods[[i]])
  }

  for (name in names(reduced)) {
    ms <- reduced[[name]]
    if (!.are_signatures_compatible(ms)) {
      stop(paste("Found methods with different signatures", ms))
    }
    reduced[[name]] <- reduced[[name]][[1]]
  }

  return(reduced)
}

.are_signatures_compatible <- function(ms) {
  return(if (length(ms) > 1) {
    length(unique(list.map(ms, args(.)))) == 1
  } else {
    TRUE
  })
}

.check_implemented_methods <- function(abstract, public) {
  intersection <- intersect(names(abstract), names(public))
  result <- list()

  for(m in intersection) {
    if (!.are_signatures_compatible(append(abstract[[m]], public[[m]]))) {
      stop(paste("Trying to implement the method but with different signature", ms))
    }
  }

  for(a in names(abstract)) {
    if (all(intersection != a)) {
      result[[a]] <- abstract[[a]]
    }
  }

  return(result)
}

Class <- function(classname = NULL, public = list(), private = NULL, active = NULL, implement = list(), parent_env = parent.frame(), ...) {
  #stopifnot(all(implement$is_interface))

  if (is.null(public$initialize)) {
    public$initialize <- function() { }
  }

  elipsis <- duplicate(list(...), shallow = TRUE)

  inherit <- elipsis$inherit

  if (!is.list(implement)) implement <- list(implement)

  abstract <- .abstract_methods(inherit, implement)

  missing_methods <- .check_implemented_methods(abstract, public)

      if (length(missing_methods) > 0) {
      stop(paste(classname, "must implement all abstract methods of parent class or be declarated as abstract:", names(missing_methods)))
  }

  generator <- R6Class(classname = classname, public = public, private = private, active = active, parent_env = parent_env, ...)

  generator$is_abstract <- FALSE

  generator$is_interface <- FALSE

  return(generator)
}

AbstractClass <- function(classname = NULL, public = list(), private = NULL, active = NULL,
                          abstract = list(), implement = NULL, parent_env = parent.frame(), ...) {

  if (!is.list(implement)) implement <- list(implement)

  is_interface <- length(public) == 0 && length(abstract) > 0

  public$initialize <- function() {
    stop(paste(.classname(self), "is an abstract class that can't be initialized."))
  }

  generator <- R6Class(classname = classname, public = public, private = private, active = active, parent_env = parent_env, ...)

  generator$is_abstract <- TRUE

  generator$is_interface <- is_interface

  abstract <- .abstract_methods(generator$inherit, implement, abstract)

  generator$abstract <- .check_implemented_methods(abstract, public)

  return(generator)
}

Interface <- function(classname = NULL, abstract = list(), parent_env = parent.frame(), inherit = NULL) {
  if (!is.list(inherit)) inherit <- list(inherit)
  abstract <- .abstract_methods(NULL, inherit, abstract)
  return(AbstractClass(classname = classname, private = NULL, active = NULL, abstract = abstract, parent_env = parent_env))
}