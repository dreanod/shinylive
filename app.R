library(shiny)
library(yaml)
library(purrr)
library(here)

dataset <- list()
dataset$distributions <- yaml::yaml.load_file("distributions.yml")

# new_sev_distr <- function(distr, params) {
#   structure(
#     list(
#       name = distr,
#       parameters = params
#     ),
#     class = "sev_distr"
#   )
# }
#
# #' @tests
# #'   print(sev_distr("pareto1"))
# #'
# #' @export
# print.sev_distr <- function(x, ...) {
#   str(x)
# }
#
# #' @import actuar
# sev_distr <- function(distr) {
#   if (! distr %in% list_sev_distr())
#     stop(paste("unknown severity distribution", distr))
#
#   params <- dataset$distributions |> purrr::detect(function(x) {
#     x$name == distr
#   })
#   new_sev_distr(distr, params$parameters)
# }
#
# get_distr <- function(distr) {
#   UseMethod("get_distr")
# }
#
# #' @export
# get_distr <- function(distr) {
#   distr[["name"]]
# }
#
# get_params <- function(distr) {
#   UseMethod("get_params")
# }
#
# #' @export
# get_params.sev_distr <- function(distr) {
#   nms <- distr[["parameters"]] |> purrr::map_chr("name")
#   val <- distr[["parameters"]] |> purrr::map_dbl("value")
#   setNames(val, nms)
# }
#
# set_params <- function(distr, params) {
#   UseMethod("set_params")
# }
#
# #' @tests
# #' mydistr <- sev_distr("pareto1")
# #' mydistr <- set_params(mydistr, list(min = 9999))
# #' str(mydistr)
# #'
# #' mydistr <- sev_distr("pareto1")
# #' mydistr <- set_params(mydistr, list(shape = 9999))
# #' str(mydistr)
# #'
# #' mydistr <- sev_distr("pareto1")
# #' mydistr <- set_params(mydistr, list(foo = 9999))
# #' str(mydistr)
# #'
# #' @export
# set_params.sev_distr <- function(distr, params) {
#   purrr::iwalk(params, function(val, nm) {
#     ind <- purrr::detect_index(distr$parameters, \(x) x$name == nm)
#     if (ind > 0)
#       distr$parameters[[ind]][["value"]] <<- val
#     else
#       stop(paste("unknown parameter", nm, "for distribution", distr$name))
#   })
#   distr
# }
#
# #' @export
# get_params.character <- function(distr) {
#   dataset$distributions |>
#     purrr::detect(function(x) x$name == distr) |>
#     purrr::pluck(2) |>
#     purrr::map_chr("name")
# }
#
# get_param_data <- function(distr) {
#   UseMethod("get_param_data")
# }
#
# #' @export
# get_param_data.sev_distr <- function(distr) {
#   distr[["parameters"]] |> purrr::map_dfr(function(x) {
#     tibble::as_tibble(x)
#   })
# }
#
# #' @export
# get_param_data.character <- function(distr) {
#   dataset$distributions |>
#     purrr::detect(function(x) x$name == distr) |>
#     purrr::pluck(2) |>
#     purrr::map_dfr(function(x) tibble::as_tibble(x))
# }
# # get_param_data("pareto1")
#
# proba_function <- function(distr, x) {
#   UseMethod("proba_function")
# }
#
# #' @export
# proba_function.sev_distr <- function(distr, x) {
#   fun <- paste0("p", get_distr(distr))
#   do.call(fun, c(list(q = x), get_params(distr)))
# }
#
# density_function <- function(distr, x) {
#   UseMethod("density_function")
# }
#
# #' @export
# density_function <- function(distr, x) {
#   fun <- paste0("d", get_distr(distr))
#   do.call(fun, c(list(x = x), get_params(distr)))
# }
#
# raw_moment <- function(distr, order) {
#   UseMethod("raw_moment")
# }
#
# #' @export
# raw_moment.sev_distr <- function(distr, order) {
#   fun <- paste0("m", get_distr(distr))
#   do.call(fun, c(list(order = order), get_params(distr)))
# }
#
# limited_moment <- function(distr, limit, order) {
#   UseMethod("limited_moment")
# }
#
# #' @export
# limited_moment.sev_distr <- function(distr, limit, order) {
#   fun <- paste0("lev", get_distr(distr))
#   do.call(fun, c(list(limit = limit, order = order), get_params(distr)))
# }
#
# ilf <- function(distr, limit, limit_ref) {
#   limited_moment(distr, limit, order = 1) /
#     limited_moment(distr, limit_ref, order = 1)
# }

#-----------------

list_sev_distr <- function() {
  purrr::map_chr(dataset$distributions, 1)
}

#-----------------

#' @import shiny

# Define UI for app that draws a histogram ----
ui <- fluidPage(

  # App title ----
  titlePanel("Severity Distributions"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # # Input: Slider for the number of bins ----
      # sliderInput(inputId = "bins",
      #             label = "Number of bins:",
      #             min = 1,
      #             max = 50,
      #             value = 30)

      shiny::selectInput(
        "sev_distr",
        "Select Severity Distribution",
        choices = list_sev_distr()
        # choices = c("pareto1", "pareto2", "pareto3", "pareto")
      ),

      shiny::wellPanel(
        shiny::h5("Parameters:"),
        shiny::uiOutput("params"),
        shiny::verbatimTextOutput("stats")
      ),
    ),

    # Main panel for displaying outputs ----
    mainPanel(

      shiny::verbatimTextOutput("out")

    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {

  # initial_distr <- sev_distr("pareto1")
  # initial_distr <- set_params(initial_distr, list(shape = 2, min = 999999))
  # distr <- mod_severity_distr_server(
  #   "app", initial_distr = shiny::reactiveVal(initial_distr)
  # )
  # output$out <- shiny::renderPrint({
  #   str(distr())
  # })
  #
  # shiny::observeEvent(initial_distr(), {
  #   shiny::req(class(initial_distr()) == "sev_distr")
  #   shiny::updateSelectInput(
  #     session, "sev_distr", selected = initial_distr()$name
  #   )
  # })
  #
  # output$params <- shiny::renderUI({
  #   shiny::req(input$sev_distr)
  #   distr <- if (input$sev_distr == initial_distr()$name) initial_distr() else input$sev_distr
  #   create_parameter_input(distr, ns)
  # })
  #
  # parameters <- shiny::reactive({
  #   shiny::req(input$sev_distr)
  #   params <- get_params(input$sev_distr)
  #   purrr::map(purrr::set_names(params), \(x) input[[x]])
  # })
  #
  # shiny::reactive({
  #   distr <- sev_distr(input$sev_distr)
  #   set_params(distr, parameters())
  # })
  # # Histogram of the Old Faithful Geyser Data ----
  # # with requested number of bins
  # # This expression that generates a histogram is wrapped in a call
  # # to renderPlot to indicate that:
  # #
  # # 1. It is "reactive" and therefore should be automatically
  # #    re-executed when inputs (input$bins) change
  # # 2. Its output type is a plot
  # # output$distPlot <- renderPlot({
  #
    # x    <- faithful$waiting
    # bins <- seq(min(x), max(x), length.out = input$bins + 1)
    #
    # hist(x, breaks = bins, col = "#75AADB", border = "white",
    #      xlab = "Waiting time to next eruption (in mins)",
    #      main = "Histogram of waiting times")
    #
    # })

}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
