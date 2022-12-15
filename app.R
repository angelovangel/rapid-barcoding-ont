library(rhandsontable)
library(shiny)
library(shinydashboard)
library(plater)
library(tibble)
library(stringr)
library(reactable)
library(dplyr)

wells_colwise <- lapply(1:12, function(x) {str_c(LETTERS[1:8], x)}) %>% unlist()
barcodes <- str_c('barcode', formatC(1:96, width = 2, flag = '0'))

# creates base empty dataframe to view and fill later
make_dest <- function() {
    dest <- tibble(well = wells_colwise, 
                   sample_name = NA, 
                   barcode = NA, dna_size = NA, conc = NA)
    dest
}

example_table <- make_dest()
#example_table$source_well = factor(x = c('A1', 'B1', 'C1', rep(NA, 93)), levels = wells_colwise),
example_table$sample_name = c('sample1', 'sample2', 'sample3', rep(NA, 93))
example_table$barcode = factor(c('barcode01', 'barcode02', 'barcode03', rep(NA, 93)), levels = barcodes)
example_table$dna_size = c(10000, 20000, 20000, rep(NA, 93))
example_table$conc = c(12, 10, 3.5, rep(NA, 93))


ui <- dashboardPage(
  header = dashboardHeader(title = 'Generate ONT rapid barcoding Opentrons protocol', titleWidth = 800),
  sidebar = dashboardSidebar(disable = T),
  body = dashboardBody(
    fluidRow(
      box(width = 12, height = 2800, status = "info", solidHeader = FALSE, title = "Enter sample information and assign barcodes", collapsible = F,
          fluidRow(
            column(2, numericInput('ng', 'ng per reaction', value = 100, min = 10, max = 1000, step = 10)),
            column(2, actionButton('protocol', 'Show protocol', width = '100%', style = 'margin-top:25px')),
            column(2, actionButton('deck', 'Show deck layout', width = '100%', style = 'margin-top:25px'))
          ),
          
          column(4, rHandsontableOutput('hot')),
          column(8, reactableOutput('plate'))
          )
      )
  )
)
  
  

# server #
server = function(input, output, session) {
  
    output$hot <- renderRHandsontable({
      rhandsontable(example_table,
                    stretchH  = 'all',
                    height = 2800,
                    rowHeaders = NULL) %>%
        hot_col('well', readOnly = T) %>%
        hot_col('dna_size', format = '0')
    })
    
    plate <- reactive({
      if(!is.null(input$hot)) {
        df <- as.tibble(hot_to_r(input$hot)) %>% mutate(sample_name = str_c(sample_name, "<br>", barcode))
        plater::view_plate(
          #hot_to_r(input$hot) , 
          df,
          well_ids_column = 'well', columns_to_display = c('sample_name')
        )
      } else {
        plater::view_plate(example_table, well_ids_column = 'well', columns_to_display = c('sample_name', 'barcode'))
      }
    })
    
    output$plate <- renderReactable({
      reactable(plate()$sample_name, highlight = T, wrap = F, 
                bordered = T, compact = T, fullWidth = T, sortable = F, 
                defaultColDef = colDef(minWidth = 50,
                  html = TRUE, 
                  headerStyle = list(background = "#f7f7f8", fontSize = '80%'), style = list(fontSize = '80%')
                  )
                )
    })
}
  
  
shinyApp(ui, server)