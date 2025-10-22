## ----setup, include=FALSE-----------------------------------------------------
library(teal.modules.general)

## ----decorate_ElementaryTable, message=FALSE----------------------------------
library(teal.modules.general)

data <- teal_data(join_keys = default_cdisc_join_keys[c("ADSL", "ADRS")])
data <- within(data, {
  require(nestcolor)
  ADSL <- rADSL
})

insert_rrow_decorator <- function(default_caption = "I am a good new row") {
  teal_transform_module(
    label = "New row",
    ui = function(id) {
      shiny::textInput(shiny::NS(id, "new_row"), "New row", value = default_caption)
    },
    server = function(id, data) {
      moduleServer(id, function(input, output, session) {
        reactive({
          data() |>
            within(
              {
                table <- rtables::insert_rrow(table, rtables::rrow(new_row))
              },
              new_row = input$new_row
            )
        })
      })
    }
  )
}

app <- init(
  data = data,
  modules = modules(
    tm_t_crosstable(
      label = "Cross Table",
      x = data_extract_spec(
        dataname = "ADSL",
        select = select_spec(
          choices = variable_choices(data[["ADSL"]], subset = function(data) {
            idx <- !vapply(data, inherits, logical(1), c("Date", "POSIXct", "POSIXlt"))
            names(data)[idx]
          }),
          selected = "COUNTRY",
          multiple = TRUE,
          ordered = TRUE
        )
      ),
      y = data_extract_spec(
        dataname = "ADSL",
        select = select_spec(
          choices = variable_choices(data[["ADSL"]], subset = function(data) {
            idx <- vapply(data, is.factor, logical(1))
            names(data)[idx]
          }),
          selected = "SEX"
        )
      ),
      decorators = list(
        table = insert_rrow_decorator()
      )
    )
  )
)

if (interactive()) {
  shinyApp(app$ui, app$server)
}

## ----shinylive_iframe_1, echo = FALSE, out.width = '150%', out.extra = 'style = "position: relative; z-index:1"', eval = requireNamespace("roxy.shinylive", quietly = TRUE) && knitr::is_html_output() && identical(Sys.getenv("IN_PKGDOWN"), "true")----
# code <- paste0(c(
#   "interactive <- function() TRUE",
#   knitr::knit_code$get("setup"),
#   knitr::knit_code$get("decorate_ElementaryTable")
# ), collapse = "\n")
# 
# url <- roxy.shinylive::create_shinylive_url(code)
# knitr::include_url(url, height = "800px")

## ----decorate_ggplot, message=FALSE-------------------------------------------
library(teal.modules.general)

data <- teal_data(join_keys = default_cdisc_join_keys[c("ADSL", "ADRS")])
data <- within(data, {
  require(nestcolor)
  ADSL <- rADSL
})

ggplot_caption_decorator <- function(default_caption = "I am a good decorator") {
  teal_transform_module(
    label = "Caption",
    ui = function(id) {
      shiny::textInput(shiny::NS(id, "footnote"), "Footnote", value = default_caption)
    },
    server = function(id, data) {
      moduleServer(id, function(input, output, session) {
        reactive({
          data() |>
            within(
              {
                plot <- plot + ggplot2::labs(caption = footnote)
              },
              footnote = input$footnote
            )
        })
      })
    }
  )
}

app <- init(
  data = data,
  modules = modules(
    tm_a_regression(
      label = "Regression",
      response = data_extract_spec(
        dataname = "ADSL",
        select = select_spec(
          label = "Select variable:",
          choices = "BMRKR1",
          selected = "BMRKR1",
          multiple = FALSE,
          fixed = TRUE
        )
      ),
      regressor = data_extract_spec(
        dataname = "ADSL",
        select = select_spec(
          label = "Select variables:",
          choices = variable_choices(data[["ADSL"]], c("AGE", "SEX", "RACE")),
          selected = "AGE",
          multiple = TRUE,
          fixed = FALSE
        )
      ),
      decorators = list(
        plot = ggplot_caption_decorator("I am a Regression")
      )
    )
  )
)

if (interactive()) {
  shinyApp(app$ui, app$server)
}

## ----shinylive_iframe_2, echo = FALSE, out.width = '150%', out.extra = 'style = "position: relative; z-index:1"', eval = requireNamespace("roxy.shinylive", quietly = TRUE) && knitr::is_html_output() && identical(Sys.getenv("IN_PKGDOWN"), "true")----
# code <- paste0(c(
#   "interactive <- function() TRUE",
#   knitr::knit_code$get("setup"),
#   knitr::knit_code$get("decorate_ggplot")
# ), collapse = "\n")
# 
# url <- roxy.shinylive::create_shinylive_url(code)
# knitr::include_url(url, height = "800px")

## ----decorate_grob, message=FALSE---------------------------------------------
library(teal.modules.general)

data <- teal_data(join_keys = default_cdisc_join_keys[c("ADSL", "ADRS")])
data <- within(data, {
  ADSL <- rADSL
})

grob_caption_decorator <- function(default_caption = "I am a good decorator") {
  teal_transform_module(
    label = "Caption",
    ui = function(id) {
      shiny::textInput(shiny::NS(id, "footnote"), "Footnote", value = default_caption)
    },
    server = function(id, data) {
      moduleServer(id, function(input, output, session) {
        reactive({
          data() |>
            within(
              {
                footnote_grob <- grid::textGrob(
                  footnote,
                  x = 0, hjust = 0,
                  gp = grid::gpar(fontsize = 10, fontface = "italic", col = "gray50")
                )
                plot <- gridExtra::arrangeGrob(
                  plot,
                  footnote_grob,
                  ncol = 1,
                  heights = grid::unit.c(
                    grid::unit(1, "npc") - grid::unit(1, "lines"), grid::unit(1, "lines")
                  )
                )
              },
              footnote = input$footnote
            )
        })
      })
    }
  )
}

app <- init(
  data = data,
  modules = modules(
    tm_g_association(
      ref = data_extract_spec(
        dataname = "ADSL",
        select = select_spec(
          choices = variable_choices(
            data[["ADSL"]],
            c("SEX", "RACE", "COUNTRY", "ARM", "STRATA1", "STRATA2", "ITTFL", "BMRKR2")
          ),
          selected = "RACE"
        )
      ),
      vars = data_extract_spec(
        dataname = "ADSL",
        select = select_spec(
          choices = variable_choices(
            data[["ADSL"]],
            c("SEX", "RACE", "COUNTRY", "ARM", "STRATA1", "STRATA2", "ITTFL", "BMRKR2")
          ),
          selected = "BMRKR2",
          multiple = TRUE
        )
      ),
      decorators = list(
        plot = grob_caption_decorator("I am a Association")
      )
    )
  )
)

if (interactive()) {
  shinyApp(app$ui, app$server)
}

## ----shinylive_iframe_3, echo = FALSE, out.width = '150%', out.extra = 'style = "position: relative; z-index:1"', eval = requireNamespace("roxy.shinylive", quietly = TRUE) && knitr::is_html_output() && identical(Sys.getenv("IN_PKGDOWN"), "true")----
# code <- paste0(c(
#   "interactive <- function() TRUE",
#   knitr::knit_code$get("setup"),
#   knitr::knit_code$get("decorate_grob")
# ), collapse = "\n")
# 
# url <- roxy.shinylive::create_shinylive_url(code)
# knitr::include_url(url, height = "800px")

## ----decorate_datatables------------------------------------------------------
library(teal.modules.general)

data <- teal_data(join_keys = default_cdisc_join_keys[c("ADSL", "ADRS")])
data <- within(data, {
  require(nestcolor)
  ADSL <- rADSL
})
fact_vars_adsl <- names(Filter(isTRUE, sapply(data[["ADSL"]], is.factor)))
vars <- choices_selected(variable_choices(data[["ADSL"]], fact_vars_adsl))

dt_table_decorator <- function(color1 = "pink", color2 = "lightblue") {
  teal_transform_module(
    label = "Table color",
    ui = function(id) {
      selectInput(
        NS(id, "color"),
        "Table Color",
        choices = c("white", color1, color2),
        selected = "Default"
      )
    },
    server = function(id, data) {
      moduleServer(id, function(input, output, session) {
        reactive({
          data() |> within(
            {
              summary_table <- DT::formatStyle(
                summary_table,
                columns = attr(summary_table$x, "colnames")[-1],
                target = "row",
                backgroundColor = color
              )
            },
            color = input$color
          )
        })
      })
    }
  )
}

app <- init(
  data = data,
  modules = modules(
    tm_g_distribution(
      dist_var = data_extract_spec(
        dataname = "ADSL",
        select = select_spec(
          choices = variable_choices(data[["ADSL"]], c("AGE", "BMRKR1")),
          selected = "BMRKR1",
          multiple = FALSE,
          fixed = FALSE
        )
      ),
      strata_var = data_extract_spec(
        dataname = "ADSL",
        filter = filter_spec(
          vars = vars,
          multiple = TRUE
        )
      ),
      group_var = data_extract_spec(
        dataname = "ADSL",
        filter = filter_spec(
          vars = vars,
          multiple = TRUE
        )
      ),
      decorators = list(
        summary_table = dt_table_decorator()
      )
    )
  )
)

if (interactive()) {
  shinyApp(app$ui, app$server)
}

## ----shinylive_iframe_4, echo = FALSE, out.width = '150%', out.extra = 'style = "position: relative; z-index:1"', eval = requireNamespace("roxy.shinylive", quietly = TRUE) && knitr::is_html_output() && identical(Sys.getenv("IN_PKGDOWN"), "true")----
# code <- paste0(c(
#   "interactive <- function() TRUE",
#   knitr::knit_code$get("setup"),
#   knitr::knit_code$get("decorate_datatables")
# ), collapse = "\n")
# 
# url <- roxy.shinylive::create_shinylive_url(code)
# knitr::include_url(url, height = "800px")

## ----decorate_trellis, message=FALSE------------------------------------------
library(teal.modules.general)

data <- teal_data(join_keys = default_cdisc_join_keys[c("ADSL", "ADRS")])
data <- within(data, {
  require(nestcolor)
  ADSL <- rADSL
  ADRS <- rADRS
})

trellis_subtitle_decorator <- function(default_caption = "I am a good decorator") {
  teal_transform_module(
    label = "Caption",
    ui = function(id) shiny::textInput(shiny::NS(id, "footnote"), "Footnote", value = default_caption),
    server = function(id, data) {
      moduleServer(id, function(input, output, session) {
        reactive({
          data() |>
            within(
              {
                plot <- update(plot, sub = footnote)
              },
              footnote = input$footnote
            )
        })
      })
    }
  )
}

app <- init(
  data = data,
  modules = modules(
    tm_g_scatterplotmatrix(
      label = "Scatterplot matrix",
      variables = list(
        data_extract_spec(
          dataname = "ADSL",
          select = select_spec(
            choices = variable_choices(data[["ADSL"]]),
            selected = c("AGE", "RACE", "SEX"),
            multiple = TRUE,
            ordered = TRUE
          )
        ),
        data_extract_spec(
          dataname = "ADRS",
          filter = filter_spec(
            label = "Select endpoints:",
            vars = c("PARAMCD", "AVISIT"),
            choices = value_choices(data[["ADRS"]], c("PARAMCD", "AVISIT"), c("PARAM", "AVISIT")),
            selected = "INVET - END OF INDUCTION",
            multiple = TRUE
          ),
          select = select_spec(
            choices = variable_choices(data[["ADRS"]]),
            selected = c("AGE", "AVAL", "ADY"),
            multiple = TRUE,
            ordered = TRUE
          )
        )
      ),
      decorators = list(
        plot = trellis_subtitle_decorator("I am a Scatterplot matrix")
      )
    )
  )
)

if (interactive()) {
  shinyApp(app$ui, app$server)
}

## ----shinylive_iframe_5, echo = FALSE, out.width = '150%', out.extra = 'style = "position: relative; z-index:1"', eval = requireNamespace("roxy.shinylive", quietly = TRUE) && knitr::is_html_output() && identical(Sys.getenv("IN_PKGDOWN"), "true")----
# code <- paste0(c(
#   "interactive <- function() TRUE",
#   knitr::knit_code$get("setup"),
#   knitr::knit_code$get("decorate_trellis")
# ), collapse = "\n")
# 
# url <- roxy.shinylive::create_shinylive_url(code)
# knitr::include_url(url, height = "800px")

