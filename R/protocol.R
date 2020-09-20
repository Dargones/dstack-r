

Protocol <-
  Interface("Protocol", list(
    push = function(stack, token, data) { },
    access = function(stack, token) { },
    pull = function(stack, token, params) { },
    download = function(url) { }
  ))

TestProtocol <-
  Class("TestProtocol",
        implement = Protocol,
        public = list(
          data = list(),
          token = NULL,

          push = function(stack, token, data) {
            self$data$stack <- stack
            self$handle(data, token)
          },

          access = function(stack, token) {
            self$handle(list(stack=stack), token)
          },

          pull = function(stack, token, params) { },
          download = function(url) { },

          handle = function(data, token) {
            stack <- data$stack
            self$data[[stack]] <- data
            self$token <- token
            return(list(url=paste0("https://api.dstack.ai/",stack)))
          },

          get_data = function(stack) {
            return(self$data[[paste0("user/", stack)]])
          }
        ))

JsonProtocol <-
  Class("JsonProtocol",
        implement = Protocol,
        public = list(
          server = NULL,
          verify = NULL,
          error = NULL,

          initialize = function(server, verify = TRUE, error = .error) {
            self$server <- server
            self$verify <- verify
            self$error <- error
          },

          push = function(stack, token, data) {
            body <- data
            body$stack <- stack

            if (object.size(body) < private$max_size) {
              res <- private$do_request(self$server, "/stacks/push", body, token, self$error)
            } else {
              content <- list()
              for (index in seq_along(body$attachments)) {
                data <- body$attachments[[index]]$data
                body$attachments[[index]]$data <- NULL
                content <- list.append(content, base64enc::base64decode(data))
                body$attachments[[index]]$length <- length(content[[index]])
              }
              res <- private$do_request(self$server, "/stacks/push", body, token, self$error)
              for (attach in res$attachments) {
                private$do_upload(attach$upload_url, content[[attach$index + 1]])
              }
            }
            return(res)
          },

          access = function(stack, token) {
            res <- private$do_request("/stacks/access", list(stack = stack), token)
            return(res)
          },

          pull = function(stack, token, params) { },

          download = function(url) { }
        ),
        private = list(
          max_size = 5000000,

          do_request = function(server, endpoint, body, token, error) {
            auth <- if (!is.null(token)) c("Authorization" = paste0("Bearer ", token)) else c()

            # r <- with_config(verbose(), POST(paste0(server, endpoint),
            r <- POST(paste0(server, endpoint),
                      body = body, encode = "json",
                      add_headers(.headers = auth))
            .check(r, error)
            return(content(r, "parsed"))
          },

          do_upload = function(upload_url, data) {
            r <- PUT(url = upload_url, body = data)
            return(r)
          }
        ))

ProtocolFactory <-
  Interface(classname = "ProtocolFactory", list(
    create = function(profile) { }
  ))

JsonProtocolFactory <-
  Class(classname = "JsonProtocolFactory",
        implement = ProtocolFactory,

        public = list(
          create = function (profile) {
            return(JsonProtocol$new(profile$server, profile$verify))
          }
        ))

TestProtocolFactory <-
  Class(classname = "TestProtocolFactory",
        implement = ProtocolFactory,
        public = list(
          protocol = NULL,

          initialize = function(protocol) {
            self$protocol <- protocol
          },

          create = function(profile) {
            return(self$protocol)
          }
        ))