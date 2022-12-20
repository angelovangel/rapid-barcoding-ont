library(rhandsontable)
library(shiny)
library(shinydashboard)
#library(shinyalert)
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
                   sample = NA, barcode = NA, 
                   dna_size = NA, conc = NA, fmoles = NA, 
                   ul = NA, bc_count = NA, mycolor = NA)
    dest
}

example_table <- make_dest()
#example_table$source_well = factor(x = c('A1', 'B1', 'C1', rep(NA, 93)), levels = wells_colwise),
example_table$sample = c('sample1', 'sample2', 'sample3', rep(NA, 93))
example_table$barcode = factor(c('barcode01', 'barcode02', 'barcode03', rep('', 93)), levels = barcodes)
example_table$dna_size = c(10000, 20000, 20000, rep(NA, 93))
example_table$conc = c(12, 10, 3.5, rep(NA, 93))

tab1 <-  fluidRow(
  box(width = 12, height = 2800, status = "info", solidHeader = FALSE, 
      title = "Enter sample information and assign barcodes", collapsible = F,
      fluidRow(
        column(12, tags$p('This protocol will normalise templates, add rapid barcodes, and pool samples. Volumes out of range and duplicate barcodes will be marked in red')),
        column(3, selectizeInput('protocol_type', 'Select protocol', choices = c('plasmid', 'gDNA'), selected = 'plasmid')),
        column(3, numericInput('ng', 'ng per reaction (50-100 ng)', value = 100, min = 10, max = 500, step = 10)),
        #column(2, actionButton('protocol', 'Show protocol', width = '100%', style = 'margin-top:25px')),
        column(2, actionButton('deck', 'Show deck layout', width = '100%', style = 'margin-top:25px')),
        column(2, downloadButton('download', 'Download Opentrons protocol', width = '100%', style = 'margin-top:25px'))
      ),
      textOutput('protocol_instructions'),
      tags$hr(),
      column(4, rHandsontableOutput('hot')),
      column(8, reactableOutput('plate'), 
             tags$hr(),
             tags$p("ONT rapid barcode plate"), 
             reactableOutput('barcode_plate'))
  )
)

tab2 <- fluidRow(
  box(width = 12, status = "info", solidHeader = FALSE, title = "Opentrons protocol preview", collapsible = F,
      verbatimTextOutput('protocol_preview')
      )
)

ui <- dashboardPage(
  #useShinyalert(),
  
  header = dashboardHeader(title = 'Generate ONT rapid barcoding Opentrons protocol', titleWidth = 800),
  sidebar = dashboardSidebar(disable = T),
  body = dashboardBody(
   tabsetPanel(
     tabPanel(title = "Enter samples", icon = icon("vials"),
       tab1
     ),
     tabPanel(title = "Protocol preview",
       tab2
     )
   )
  )
)
  
  

# server #
server = function(input, output, session) {
  
  ### read template
  protocol_template <- readLines('opentrons-template.py', warn = F)
  
  ### REACTIVES
    protocol <- reactiveValues(bc_vol = 0, rxn_vol = 0, sample_vol = 0)
    
    hot <- reactive({
      if(!is.null(input$hot)) {
          as_tibble(hot_to_r(input$hot)) %>%
          mutate(fmoles = input$ng/((dna_size*617.96) + 36.04) * 1000000) %>%
          mutate(ul = input$ng/conc) %>%
          mutate(ul = if_else(ul > protocol$sample_vol, protocol$sample_vol, ul)) %>%
          add_count(barcode, name = 'bc_count') %>% # used to track if barcodes are unique 
          mutate(mycolor = if_else(bc_count > 1, 'orange', 'black'))
      } else {
          example_table
        }
    })
    
    plate <- reactive({
      if(!is.null(input$hot)) {
        df <- hot() %>% 
          mutate(sample = str_c(sample, "<br>", barcode))
        
        plater::view_plate(
          #hot_to_r(input$hot) , 
          df,
          well_ids_column = 'well', columns_to_display = c('sample')
        )
      } else {
        plater::view_plate(example_table, well_ids_column = 'well', columns_to_display = c('sample'))
      }
    })
    
  myvalues <- reactive({
    sample_wells <- wells_colwise[hot()$sample != ''] %>% str_replace_na(replacement = ' ')
    volume1 <- str_replace_na(hot()$ul, '0') # replace NA with 0, gDNA
    # water wells is always A1
    volume2 <- str_replace_na(protocol$sample_vol - hot()$ul, '0') # water
    barcode_wells <- wells_colwise[match(hot()$barcode, barcodes)] %>% str_replace_na(replacement = ' ')
    volume3 <- rep(protocol$bc_vol, 96)[match(hot()$barcode, barcodes)] %>% str_replace_na(replacement = '0') 
    # see this how it works
    # rep(1, 96)[match(c('barcode03', '', '', 'barcode01'), barcodes)]
      
      c(
        str_flatten(sample_wells, collapse = "','"),  
        str_flatten(volume1, collapse = ", "),
        str_flatten(volume2, collapse = ", "),
        str_flatten(barcode_wells, collapse = "','"),
        str_flatten(volume3, collapse = ", ")
      ) 
  })
      
  myprotocol <- reactive({
    str_replace(protocol_template, 'sourcewells1=.*', paste0("sourcewells1=['", myvalues()[1], "']")) %>%
      str_replace('volume1=.*', paste0('volume1=[', myvalues()[2], ']')) %>%
      str_replace('volume2=.*', paste0('volume2=[', myvalues()[3], ']')) %>%
      str_replace('sourcewells3=.*', paste0("sourcewells3=['", myvalues()[4], "']")) %>%
      str_replace('volume3=.*', paste0('volume3=[', myvalues()[5], ']'))
    
    })
      
      
  ### OBSERVERS
    observeEvent(input$deck, {
      showModal(
        modalDialog(title = 'Opentrons deck preview',
                    HTML('<img src="deck.png">'),
                    size = 'l', easyClose = T, 
        )
        )
    })
    
    
    observe({
      if(input$protocol_type == 'plasmid') {
        protocol$bc_vol <- 0.5
        protocol$rxn_vol <- 5
        protocol$sample_vol <- 4.5
      } else {
        protocol$bc_vol <- 1
        protocol$rxn_vol <- 10
        protocol$sample_vol <- 9
      }
    })
    
  ### OUTPUTS
    
    output$protocol_instructions <- renderText({
      paste0('Reaction volume is ', protocol$rxn_vol, ' ul (', 
             protocol$sample_vol, ' ul sample + ', protocol$bc_vol, 
             ' ul barcode). Use 50 - 100 ng for gDNA and approx 20 fmoles for plasmid.')
    })
    
    output$hot <- renderRHandsontable({
      rhandsontable(hot() %>% select(-c('bc_count', 'mycolor')),
                    stretchH  = 'all', 
                    height = 2800,
                    rowHeaders = NULL) %>%
        hot_col('well', readOnly = T) %>%
        hot_col('fmoles', readOnly = T) %>%
        hot_col('ul', readOnly = T) %>%
        hot_col('dna_size', format = '0') %>%
        hot_cell(1, 3, 'test')
    })
    
    
    output$plate <- renderReactable({
      reactable(plate()$sample, 
                highlight = T, wrap = F, 
                bordered = T, compact = T, fullWidth = T, sortable = F,
                defaultColDef = colDef(minWidth = 50,
                  html = TRUE, 
                  headerStyle = list(background = "#f7f7f8", fontSize = '80%'),
                  # color barcodes if duplicate
                  style = function(value) {
                    myvalue <- str_extract(value, 'barcode.*')
                    mydf <- hot()
                    if(!is.na(myvalue)) {
                      textcolor <-mydf$mycolor[match(myvalue, mydf$barcode)]
                    } else {
                      textcolor <- 'black'
                    }
                    #print(myvalue)
                    list(color = textcolor, fontSize = '80%')
                  }
                  )
                )
    })
    
    output$barcode_plate <- renderReactable({
      bc_plate <- make_dest()
      bc_plate$barcode <- barcodes
      bc_plate_view <- view_plate(bc_plate, well_ids_column = 'well', columns_to_display = 'barcode')
      reactable(bc_plate_view$barcode, wrap = F, bordered = T,
                compact = T, fullWidth = T, sortable = F, 
                defaultColDef = colDef(minWidth = 50,
                                       html = TRUE, 
                                       headerStyle = list(background = "#f7f7f8", fontSize = '80%'), 
                                       style = list(fontSize = '80%')
                                       )
                )
    })
    
    output$protocol_preview <- renderPrint({
     write(myprotocol(), file = "")
    })
    
    output$download <- downloadHandler(
      
      filename = paste0(format(Sys.time(), "%Y%m%d-%H%M%S"), '-ont-protocol.py'),
      content = function(con) {
        write(myprotocol(), con)
      }
    )
}
  
  
shinyApp(ui, server)