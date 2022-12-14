library(rhandsontable)
library(shiny)
library(shinydashboard)
library(plater)
library(tibble)
library(stringr)

wells_colwise <- lapply(1:12, function(x) {str_c(LETTERS[1:8], x)}) %>% unlist()
barcodes <- str_c('barcode', formatC(1:96, width = 2, flag = '0'))

ui <- dashboardPage(
  header = dashboardHeader(title = 'Generate ONT rapid barcoding Opentrons protocol', titleWidth = 800),
  sidebar = dashboardSidebar(disable = T),
  body = dashboardBody(
    fluidRow(
      box(width = 12, height = 400, status = "info", solidHeader = FALSE, title = "Enter sample information and assign barcodes", collapsible = F,
          rHandsontableOutput('hot')
          )
      )
  )
)
  
  

# server #
server = function(input, output, session) {
  
  
  example_table <- tibble::tibble(
    source_well = factor(x = c('A1', 'B1', 'C1'), levels = wells_colwise),
    sample_name = c('sample1', 'sample2', 'sample3'),
    barcode = factor(c('barcode01', 'barcode02', 'barcode03'), levels = barcodes),
    dna_size = c(10000, 20000, 20000),
    conc = c(12, 10, 3.5)
  )
    output$hot <- renderRHandsontable({
      rhandsontable(example_table, height = 400) %>%
        hot_col('source_well', type = 'dropdown', strict = T) %>%
        hot_col('dna_size', format = '0')
    })
}
  
  
shinyApp(ui, server)