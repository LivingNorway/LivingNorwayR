# Launch Shiny
#' This function launches a shiny app for meta-data entry
#' @keywords internal
#' @export

launchShiny <- function() {
  library(rhandsontable)
  library(shiny)
  app <- list(ui = fluidPage(
                  titlePanel("Edit Data File"),
                  helpText("Changes to the table will be automatically saved to the source file."),
                  # uncomment line below to use action button to commit changes
                  actionButton("saveBtn", "Save"),
                  rhandsontable::rHandsontableOutput("hot")
                ),
              server = function(input,output){
                fname = tempfile(fileext = ".csv")

                observe({
                  # remove button and isolate to update file automatically
                  # after each table change
                  input$saveBtn
                  hot = isolate(input$hot)
                  if (!is.null(hot)) {
                    write.csv(hot_to_r(input$hot), fname)
                    print(fname)
                  }
                })

                output$hot = rhandsontable::renderRHandsontable({
                  if (!is.null(input$hot)) {
                    DF = hot_to_r(input$hot)
                  } else {
                    DF = read_delim("data-raw/Metadata_terms.csv",
                                         ";", escape_double = FALSE, trim_ws = TRUE)
                  }

                  rhandsontable::rhandsontable(DF) %>%
                    rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE)
                })
              })

  runApp(app)
}

#launchShiny()

## NOT IN USE
