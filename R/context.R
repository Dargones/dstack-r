library(R6)

Context <-
  Class("Context", list(
    stack = NULL,
    profile = NULL,
    protocol = NULL,

    initialize = function(stack, profile, protocol) {
      stopifnot(is.character(stack), length(stack) == 1)
      # TODO: check profile and protocol type
      self$stack <- stack
      self$profile <- profile
      self$protocol <- protocol
    },

    stack_path = function() {
      return(.stack_path(self$profile$user, self$stack))
    }
  ))
