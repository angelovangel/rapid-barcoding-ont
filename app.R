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
        column(12, tags$p('This protocol will normalise templates, add rapid barcodes, and pool samples. Use 50 - 100 ng for gDNA and approx 20 fmoles for plasmid. Volumes out of range and duplicate barcodes will be marked in red')),
        column(2, selectizeInput('protocol_type', 'Select protocol', choices = c('plasmid', 'gDNA'), selected = 'plasmid')),
        #column(3, uiOutput('sample_amount')),
        column(2, numericInput('ng', 'ng per reaction (50-100 ng)', value = 100, min = 10, max = 500, step = 10)),
        #column(2, actionButton('protocol', 'Show protocol', width = '100%', style = 'margin-top:25px')),
        column(2, actionButton('deck', 'Show deck layout', width = '100%', style = 'margin-top:25px')),
        column(2, downloadButton('download_samples', 'Download sample sheet', width = '100%', style = 'margin-top:25px')),
        column(2, downloadButton('download', 'Download Opentrons protocol', width = '100%', style = 'margin-top:25px'))
      ),
      uiOutput('protocol_instructions'),
      tags$hr(),
      column(4, 
             tags$p("Enter sample information here"),
             rHandsontableOutput('hot')),
      column(8, 
             tags$p("Sample - barcode plate preview"),
             reactableOutput('plate'), 
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
    protocol <- reactiveValues(bc_vol = 0, rxn_vol = 0, sample_vol = 0, total_fmoles = 0)
    
    hot <- reactive({
      if(!is.null(input$hot)) {
          as_tibble(hot_to_r(input$hot)) %>%
          #mutate(fmoles = input$ng/((dna_size*617.96) + 36.04) * 1000000) %>%
          mutate(fmoles = (ul * conc)/((dna_size*617.96) + 36.04) * 1000000) %>%
          mutate(ul = input$ng/conc) %>%
          mutate(
            ul = case_when(
              ul > protocol$sample_vol ~ protocol$sample_vol,
              ul < 0.5 ~ 0.5,
              TRUE ~ ul
            )
            #ul = if_else(ul > protocol$sample_vol, protocol$sample_vol, ul)
            ) %>%
          add_count(barcode, name = 'bc_count') %>% # used to track if barcodes are unique 
          mutate(mycolor = if_else(bc_count > 1, 'red', 'black'))
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
      str_replace('volume3=.*', paste0('volume3=[', myvalues()[5], ']')) %>%
      
      str_replace('barcode_vol = .*', paste0('barcode_vol = ', protocol$bc_vol)) %>%
      str_replace('total_rxn_vol = .*', paste0('total_rxn_vol = ', protocol$rxn_vol))
    
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
        protocol$total_fmoles <- sum(hot()$fmoles, na.rm = T)
      } else {
        protocol$bc_vol <- 1
        protocol$rxn_vol <- 10
        protocol$sample_vol <- 9
        protocol$total_fmoles <- sum(hot()$fmoles, na.rm = T)
      }
    })
    
  ### OUTPUTS
    # change to ng or fmol depending on protocol selected
    # output$sample_amount <- renderUI({
    #   if(input$protocol_type == 'plasmid') {
    #     numericInput('fmol', ' fmol per reaction (~ 20)', value = 20, min = 1, max = 200, step = 1)
    #   } else {
    #     numericInput('ng', 'ng per reaction (50-100 ng)', value = 100, min = 10, max = 500, step = 10)
    #   } 
    # })
    
    output$protocol_instructions <- renderText({
      HTML(
      paste0('Reaction volume is <b>', protocol$rxn_vol, '</b> ul (', 
             protocol$sample_vol, ' ul sample + ', protocol$bc_vol, 
             ' ul barcode). Minimal pipetting volume is 0.5 ul, maximum sample volume is ',
             protocol$sample_vol, ' ul. The pool will have a total of <b>', round(protocol$total_fmoles, 2), ' fmoles.</b>')
      )
    })
    
    
    renderer <- function() {
      if (input$protocol_type == 'plasmid') {
      "function(instance, td, row, col, prop, value, cellProperties) {
      Handsontable.renderers.NumericRenderer.apply(this, arguments);
      
      if (value >= 4.5 || value <= 0.5) {
      td.style.color = 'red'
      }
    }
    "
      } else {
      "function(instance, td, row, col, prop, value, cellProperties) {
      Handsontable.renderers.NumericRenderer.apply(this, arguments);
      
      if (value >= 9 || value <= 0.5) {
      td.style.color = 'red'
      }
    }
    "
      }
    }
    output$hot <- renderRHandsontable({
      
      # borders <- function(row_from, row_to) {
      #   list(
      #     range = list(from = list(row = row_from, col = 0), to = list(row = row_to, col = 6)),
      #     top = list(width = 0, color = 'darkblue'),
      #     bottom = list(width = 1, color = 'darkblue')
      #   )
      # }
      
      rhandsontable(hot() %>% select(-c('bc_count', 'mycolor')),
                    stretchH  = 'all',  
                    #svol = 9,
                    height = 2800,
                    rowHeaders = NULL) %>%
        hot_col('well', readOnly = T) %>%
        hot_col('fmoles', readOnly = T) %>%
        hot_col('ul', readOnly = T, renderer = renderer() ) %>% # highlight volumes > max
        hot_col('dna_size', format = '0') %>%
        #hot_cell(1, 3, 'test') %>%
        hot_validate_numeric('conc', min = 1, max = 5000, allowInvalid = T)
        # hot_table(customBorders = mapply(borders, 
        #                                  row_from = c(0, 7, 15, 23, 31, 39), 
        #                                  row_to = c(7, 15, 23, 31, 39, 47), 
        #                                  SIMPLIFY = F)
        #           )
        # too slow
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
    
    ### Downloads
    output$download <- downloadHandler(
      filename = function() {
        paste0(format(Sys.time(), "%Y%m%d-%H%M%S"), '-ont-protocol.py')
        },
      content = function(con) {
        write(myprotocol(), con)
      }
    )
    
    output$download_samples <- downloadHandler(
      filename = function() {
        paste0(format(Sys.time(), "%Y%m%d-%H%M%S"), '-samplesheet.csv')
      },
      content = function(con) {
        write.csv(hot() %>% select(-c('bc_count', 'mycolor')), con)
      }
      )
}
  
  
shinyApp(ui, server)