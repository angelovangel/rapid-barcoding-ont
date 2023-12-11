library(rhandsontable)
library(shiny)
library(shinydashboard)
#library(shinyalert)
library(plater)
library(tibble)
library(stringr)
library(reactable)
library(dplyr)
library(rmarkdown)
library(curl)
library(waiter)
library(scales)

wells_colwise <- lapply(1:12, function(x) {str_c(LETTERS[1:8], x)}) %>% unlist()
barcodes <- str_c('barcode', formatC(1:96, width = 2, flag = '0'))

# use prefixes in large numbers, label_number returns a function
silabel <- scales::label_number(scale_cut = scales::cut_short_scale(), accuracy = 1, suffix = ' bases')

# creates base empty dataframe to view and fill later
make_dest <- function() {
    dest <- tibble(well = wells_colwise, 
                   user = NA, sample = NA, dna_size = NA, 
                   conc = NA, barcode = NA, fmolperul = NA,
                   ul = NA, ng = NA, fmoles = NA, bc_count = NA, mycolor = NA)
    dest
}

example_table <- make_dest()
#example_table$source_well = factor(x = c('A1', 'B1', 'C1', rep(NA, 93)), levels = wells_colwise),
example_table$user = c(rep('angeloas', 2), 'putraa', rep(NA, 93))
example_table$sample = c('sample1', 'sample2', 'sample3', rep(NA, 93))
example_table$barcode = factor(c('barcode01', 'barcode02', 'barcode03', rep('', 93)), levels = barcodes)
#example_table$barcode = str_c('barcode', formatC(1:96, width = 2, flag = '0'))
example_table$dna_size = c(10000, 20000, 20000, rep(NA, 93))
example_table$conc = c(120, 80, 35, rep(NA, 93))

tab1 <-  fluidRow(
  box(width = 12, height = 2800, status = "info", solidHeader = FALSE, 
      title = "Enter sample information and assign barcodes", collapsible = F,
      fluidRow(
        column(12, tags$p('This protocol will normalise templates, add rapid barcodes, incubate, and pool samples. Use 50 ng for gDNA (> 4 samples) and approx 20 fmol for plasmid. Volumes out of range and duplicate barcodes will be marked in red')),
        column(2, selectizeInput('protocol_type', 'Select protocol', choices = c('plasmid', 'gDNA'), selected = 'gDNA')),
        column(2, uiOutput('sample_amount')),
        column(2, selectizeInput('sample_volume_factor', 
                              label = 'Sample vol factor', 
                              choices = list('1x' = 1, '1.5x' = 1.5, '2x' = 2), 
                              selected = '1x', multiple = F)
               ),
        column(2, selectizeInput('consolidate_volume_factor', 
                                 label = 'Consolidate vol factor', 
                                 choices = list('0.9x' = 0.9, '0.75x' = 0.75, '0.5x' = 0.5, '0.25x' = 0.25), 
                                 selected = '1x', multiple = F))
        ),
      fluidRow(
        column(12, uiOutput('protocol_instructions')),
        ),
      fluidRow(
        column(2, actionButton('protocol', 'Show complete protocol', 
                               width = '100%', 
                               style = 'margin-top:20px', 
                               onclick = "window.open('protocol.html', '_blank')"
                               )
               ),
        column(2, actionButton('deck', 'Show deck layout', width = '100%', style = 'margin-top:20px')),
        column(3, downloadButton('download_script', 'Opentrons script', width = '100%', style = 'margin-top:20px'),
                  downloadButton('download_samples', 'Sample sheet', width = '100%', style = 'margin-top:20px'),
                  checkboxInput('pause_before_inc', 'Pause before incubation (cover plate)', value = T)
               ),
        
        #column(2, downloadButton('download_script', 'Opentrons script', width = '100%', style = 'margin-top:15px')),
        column(4,
               uiOutput('user_select'),
               downloadButton('download_nxf_sample', 'Nextflow sample sheet', width = '100%', style = 'margin-top:0px'),
               #downloadButton('download_nxf_size', 'Nextflow size sheet', width = '100%', style = 'margin-top:0px')
               )
        #column(2, downloadButton('download_nxf_size', 'Nextflow size sheet', width = '100%', style = 'margin-top:15px'))
      ),
      tags$hr(),
      column(5, 
             tags$p("Source plate - enter sample information here"),
             rHandsontableOutput('hot')),
      column(7, 
             tags$p("Reaction plate preview"),
             reactableOutput('plate'), 
             tags$hr(),
             tags$p("ONT rapid barcode plate"), 
             rHandsontableOutput('barcode_plate')
             )
  )
)

tab2 <- fluidRow(
  box(width = 12, status = "info", solidHeader = FALSE, title = "", collapsible = F,
      uiOutput('wetlab')
      )
)
tab3 <- fluidRow(
  box(width = 12, status = "info", solidHeader = FALSE, title = "Opentrons protocol preview", collapsible = F,
      verbatimTextOutput('protocol_preview')
      )
)

ui <- dashboardPage(
  #useShinyalert(),
  
  header = dashboardHeader(title = 'Generate ONT rapid barcoding (SQK-RBK114) Opentrons protocol', titleWidth = 800),
  sidebar = dashboardSidebar(disable = T),
  body = dashboardBody(
   tabsetPanel(
     tabPanel(title = "Enter samples", icon = icon("list"), tab1),
     #tabPanel(title = "Wet lab protocol", icon = icon('vials'), tab2),
     tabPanel(title = "Opentrons script preview", icon = icon('code'), tab3)
   )
  )
)
  
  

# server #
server = function(input, output, session) {
  
  ### read template
  protocol_url <- "https://raw.githubusercontent.com/angelovangel/opentrons/main/protocols/02-ont-rapid-pcr.py"
  
  if (curl::has_internet()) {
    waiter_show(html = spin_wave())
    con <- url(protocol_url)
    protocol_template <- readLines(con, warn = F)
    close(con)
    waiter_hide()
  } else {
    protocol_template <- readLines('02-ont-rapid-pcr.py', warn = F)
  }
  
  
  ### REACTIVES
    protocol <- reactiveValues(bc_vol = 0, rxn_vol = 0, sample_vol = 0, total_fmoles = 0, total_ng = 0, total_bases = 0)
    
    hot <- reactive({
      if(!is.null(input$hot)) {
          as_tibble(hot_to_r(input$hot)) %>%
          mutate(
            fmolperul = conc/((dna_size*617.96) + 36.04) * 1000000,
            fmoles = ul * fmolperul) %>%
          # the ul needed is calculated as input$ng_or_fmoles/conc for gDNA ans input$ng_or_fmoles/fmoles for plasmid
          mutate(
            ul = case_when(
              input$protocol_type == 'plasmid' ~ input$ng_or_fmoles/fmolperul, 
              input$protocol_type == 'gDNA' ~ input$ng_or_fmoles/conc,
              TRUE ~ 0)
            ) %>%
          mutate(
            ul = case_when(
              ul > protocol$sample_vol ~ protocol$sample_vol,
              (ul > 0 & ul < 0.5) ~ 0.5,
              TRUE ~ ul
            )) %>%
          mutate(ng = ul * conc) %>%
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
    #sample_wells <- wells_colwise[hot()$sample != ''] %>% str_replace_na(replacement = ' ')
    
    # fix bug where deleting a well did not leave an empty entry in the vector
    sample_wells <- rep(' ', 96)
    i <- which(hot()$sample != '')
    sample_wells[i] <- hot()$well[i]
    
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
    
    # pause before bc addition
    if(input$pause_before_inc) {
      protocol_template <- str_replace(protocol_template, '# optional pause #', '')
    }
    str_replace(protocol_template, 'sourcewells1=.*', paste0("sourcewells1=['", myvalues()[1], "']")) %>%
      str_replace('volume1=.*', paste0('volume1=[', myvalues()[2], ']')) %>%
      str_replace('volume2=.*', paste0('volume2=[', myvalues()[3], ']')) %>%
      str_replace('sourcewells3=.*', paste0("sourcewells3=['", myvalues()[4], "']")) %>%
      str_replace('volume3=.*', paste0('volume3=[', myvalues()[5], ']')) %>%
      
      str_replace('barcode_vol = .*', paste0('barcode_vol = ', protocol$bc_vol)) %>%
      str_replace('total_rxn_vol = .*', paste0('total_rxn_vol = ', protocol$rxn_vol)) %>%
      str_replace('consolidate_vol_fraction = .*', paste0('consolidate_vol_fraction = ', input$consolidate_volume_factor))
    
    })
  
      
  ### OBSERVERS
  
  #protocol_md <- rmarkdown::render('www/protocol.Rmd')
  
    observeEvent(input$deck, {
      showModal(
        modalDialog(title = 'Opentrons deck preview',
                    HTML('<img src="deck-pcr.png">'),
                    size = 'l', easyClose = T, 
        )
      )
    })
    
    # observeEvent(input$protocol, {
    #   tags$a()
    #   #utils::browseURL('www/protocol.html')
    # })
    
    
    observe({
      if(input$protocol_type == 'plasmid') {
        protocol$bc_vol <- 0.5
        protocol$sample_vol <- 5 * as.numeric(input$sample_volume_factor)
        protocol$rxn_vol <- protocol$bc_vol + protocol$sample_vol
        protocol$total_fmoles <- sum(hot()$fmoles, na.rm = T)
        protocol$total_ng <- sum(hot()$ng, na.rm = T)
        protocol$total_bases <- sum(hot()$dna_size, na.rm = T)
      } else {
        protocol$bc_vol <- 1
        protocol$sample_vol <- 10 * as.numeric(input$sample_volume_factor)
        protocol$rxn_vol <- protocol$bc_vol + protocol$sample_vol
        protocol$total_fmoles <- sum(hot()$fmoles, na.rm = T)
        protocol$total_ng <- sum(hot()$ng, na.rm = T)
        protocol$total_bases <- sum(hot()$dna_size, na.rm = T)
      }
    })
    
  ## OUTPUTS
  # change to ng or fmol depending on protocol selected
  output$sample_amount <- renderUI({
    if(input$protocol_type == 'plasmid') {
      numericInput('ng_or_fmoles', 'fmol per reaction (~ 20)', value = 20, min = 1, max = 200, step = 1)
    } else {
      numericInput('ng_or_fmoles', 'ng per reaction (50-100 ng)', value = 50, min = 5, max = 500, step = 10)
    }
  })
  
  output$user_select <- renderUI({
    selectizeInput('user_selected', '', 
                   choices = unique(na.omit(hot()$user)), 
                   multiple =F, 
                   width = '100%')
  })
    
    output$protocol_instructions <- renderText({
      HTML(
      paste0('Reaction volume: <b>', protocol$rxn_vol, ' ul </b> (', 
             protocol$sample_vol, ' ul sample + ', protocol$bc_vol, 
             ' ul barcode). Min volume: 0.5 ul, max volume: ',
             protocol$sample_vol, ' ul. Pool: ', 
             round(protocol$total_fmoles, 0), 
             ' fmol and ', round(protocol$total_ng, 0),' ng, or ',
             round(protocol$total_fmoles/protocol$total_ng, 3),
             ' fmol/ng. Total bases in pool: ', silabel(protocol$total_bases)
             )
      )
    })
    
    
    renderer <- function() {
      if (input$protocol_type == 'plasmid') {
      # no other clever way of passing arguments to the JS function, so using paste0
      maxvolume <- 5 * as.numeric(input$sample_volume_factor) 
      paste0(
      "function(instance, td, row, col, prop, value, cellProperties) {
      Handsontable.renderers.NumericRenderer.apply(this, arguments);
      
      if (value >=", maxvolume," || value <= 0.5) {
      td.style.color = 'red'
      }
    }
    "
      )
      } else {
      maxvolume <- 10 * as.numeric(input$sample_volume_factor) 
      paste0(
      "function(instance, td, row, col, prop, value, cellProperties) {
      Handsontable.renderers.NumericRenderer.apply(this, arguments);
      
      if (value >=", maxvolume, " || value <= 0.5) {
      td.style.color = 'red'
      }
    }
    "
      )
      }
    }
    # renders first column well in grey for better plate overview
    rendergrey <- function() {
      "
    function(instance, td, row, col, prop, value, cellProperties) {
      Handsontable.renderers.TextRenderer.apply(this, arguments);
      
      tbl = this.HTMLWidgets.widgets[0]
      //hrows = tbl.params.greylines
      hrows = [0 ,8, 16, 24, 32, 40, 48, 56, 64, 72, 80, 88]
      hrows = hrows instanceof Array ? hrows : [hrows] 

      
      if (hrows.includes(row)) {
        td.style.background = '#D6EAF8';
      }
      
      return td;
  }"
    }
    
    output$hot <- renderRHandsontable({
      
      # borders <- function(row_from, row_to) {
      #   list(
      #     range = list(from = list(row = row_from, col = 0), to = list(row = row_to, col = 6)),
      #     top = list(width = 0, color = 'darkblue'),
      #     bottom = list(width = 1, color = 'darkblue')
      #   )
      # }
      
      rhandsontable(hot() %>% select(-c('bc_count', 'mycolor', 'fmolperul')),
                    stretchH  = 'all',  
                    #svol = 9,
                    height = 2800,
                    rowHeaders = NULL) %>%
        #hot_cols(renderer = rendergrey() ) %>%
        hot_col('well', readOnly = T, renderer = rendergrey() ) %>%
        hot_col('barcode', type = 'dropdown') %>%
        hot_col('ng', readOnly = T, type = 'numeric', format = '0.0') %>%
        hot_col('fmoles', readOnly = T, type= 'numeric', format = '0.0') %>%
        hot_col('ul', readOnly = T, renderer = renderer() ) %>% # highlight volumes > max
        hot_col('dna_size', format = '0') %>%
        #hot_cell(1, 3, 'test') %>%
        hot_validate_numeric('conc', min = 1, max = 5000, allowInvalid = T)
        #hot_cols(renderer = rendergrey() )
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
    
    output$barcode_plate <- renderRHandsontable({
      
      # this is a good snippet!
      printbc <- function(x, y) {
        str_c('barcode', formatC(x:y, width = 2, flag = '0'))
      }
      bcplate <- data.frame(mapply(printbc, seq(1,96, by = 8), seq(8, 96, by = 8)))
      rownames(bcplate) <- LETTERS[1:8]
      colnames(bcplate) <- 1:12
      
      rhandsontable(bcplate, readOnly = T)
    })
    
    
    output$protocol_preview <- renderPrint({
     write(myprotocol(), file = "")
    })
    
    
    ### Downloads
    output$download_script <- downloadHandler(
      filename = function() {
        paste0(format(Sys.time(), "%Y%m%d-%H%M%S"), '-ont-protocol.py')
        },
      content = function(con) {
        # at download time, replace name so that it appears on the Opentrons app
        replacement <- paste0(format(Sys.time(), "%Y%m%d-%H%M%S"), '-ont-protocol.py')
        write(myprotocol() %>%
                str_replace(pattern = "02-ont-rapid-pcr.py", 
                            replacement = replacement), 
              con)
      }
    )
    
    output$download_samples <- downloadHandler(
      filename = function() {
        paste0(format(Sys.time(), "%Y%m%d-%H%M%S"), '-samplesheet.csv')
      },
      content = function(con) {
        write.csv(hot() %>% select(-c('bc_count', 'mycolor')), con, row.names = F, quote = F)
      }
      )
    
    # Nextflow wf-clone validation sheets
    output$download_nxf_sample <- downloadHandler(
      filename = function() {
        paste0(format(Sys.time(), "%Y%m%d-%H%M%S"), '-',input$user_selected,'-nxfsample.csv')
      }, 
      content = function(con) {
        myfile <- hot() %>% 
          filter(user == input$user_selected) %>%
          mutate(alias = str_c(user, "_", sample), approx_size = dna_size) %>% 
          select(barcode, alias, approx_size)
        write.csv(myfile[complete.cases(myfile), ], con, row.names = F, quote = F)
      }
    )
    
    # output$download_nxf_size <- downloadHandler(
    #   filename = function() {
    #     paste0(format(Sys.time(), "%Y%m%d-%H%M%S"), '-',input$user_selected, '-nxfsize.csv')
    #   }, 
    #   content = function(con) {
    #     myfile <- hot() %>% 
    #       filter(user == input$user_selected) %>%
    #       mutate(alias = str_c(user, "_", sample), approx_size = dna_size) %>% 
    #       select(alias, approx_size)
    #     write.csv(myfile[complete.cases(myfile), ], con, row.names = F, quote = F)
    #   }
    # )
}
  
  
shinyApp(ui, server)