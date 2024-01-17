# bslib version
library(curl)
library(rhandsontable)
library(shiny)
library(shinyWidgets)
library(bslib)
library(bsicons)
library(shinyjs)
library(tibble)
library(stringr)
library(reactable)
library(dplyr)
library(rmarkdown)
library(plater)
library(shinyjs)
library(processx)
library(htmltools)
library(scales)

wells_colwise <- lapply(1:12, function(x) {str_c(LETTERS[1:8], x)}) %>% unlist()
barcodes <- str_c('barcode', formatC(1:96, width = 2, flag = '0'))

# use prefixes in large numbers, label_number returns a function
silabel <- scales::label_number(scale_cut = scales::cut_short_scale(), accuracy = 1, suffix = '')

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

# GUI

accordion1 <- list(
  selectizeInput('protocol_type', 'Protocol', choices = c('plasmid', 'gDNA'), selected = 'gDNA'),
  uiOutput('sample_amount'),
  downloadButton('download_script', 'Opentrons script', width = '100%', style = 'margin-top:20px'),
  downloadButton('download_samples', 'Sample sheet', width = '100%', style = 'margin-top:20px')
)
accordion2 <- list(
  checkboxInput('pause_before_inc', 'Pause before incubation (cover plate)', value = T),
  checkboxInput('reuse_tip', 'Reuse tip in pooling', value = F),
  selectizeInput(
    'sample_labware', 'Samples labware', 
    choices = c('Stacked strips/plate' = 'stack_plate_biorad96well', 'Biorad plate' = 'biorad_96_wellplate_200ul_pcr'), 
    multiple = F
    ),
  selectizeInput('sample_volume_factor', 
                 label = 'Sample vol factor', 
                 choices = list('1x' = 1, '1.5x' = 1.5, '2x' = 2), 
                 selected = '1x', multiple = F),
  selectizeInput('consolidate_volume_factor', 
                 label = 'Consolidate vol factor', 
                 choices = list('0.9x' = 0.9, '0.75x' = 0.75, '0.5x' = 0.5, '0.25x' = 0.25), 
                 selected = '1x', multiple = F),
  numericInputIcon('aspirate_speed', 'Aspirate speed', 
                   min = 5, max = 100, value = 100, step = 5, 
                   icon = list(NULL, icon("percent"))),
  numericInputIcon('dispense_speed', 'Dispense speed', 
                   min = 5, max = 100, value = 100, step = 5, 
                   icon = list(NULL, icon("percent")))
)

sidebar <- sidebar(
  accordion(
    open = T,
    accordion_panel(title = 'Select protocol', icon = bsicons::bs_icon('code-square'), accordion1),
    accordion_panel(title = 'Adjust protocol', icon = bsicons::bs_icon('sliders2'), accordion2)
  )  
)

# value boxes with useful information
# titles <- c('Rxn volume' = 'vbs1', 'Total fmoles' = 'vbs2', 'Total ng' = 'vbs3', 'Total bases' = 'vbs4')
# vbs <- list(
#   lapply(
#     titles,
#     function(x) {
#       value_box(title = names(x), value = textOutput(x), theme = 'secondary')
#     }
#   )
# )
vbs <- list(
  value_box(
    title = textOutput('vbs00'),
    value = textOutput('vbs0'),
    theme = 'secondary'
  ),
  value_box(
    title = 'Rxn volume',
    value = textOutput('vbs1'),
    #showcase = bs_icon('dot'),
    theme = 'secondary'
    #a(textOutput('vbs1b'))
  ),
  value_box(
    title = "Total fmoles",
    value = textOutput('vbs2'),
    #showcase = bs_icon("three-dots"),
    theme = "secondary"
  ),
  value_box(
    title = "Total ng",
    value = textOutput('vbs3'),
    #showcase = bs_icon("chevron-bar-down"),
    theme = "secondary"
  ),
  value_box(
    title = "Total bases",
    value = textOutput('vbs4'),
    #showcase = bs_icon("chevron-bar-down"),
    theme = "secondary"
  )
)
panel1 <- list(
  div(
    tags$a('Source plate', tooltip(bs_icon("info-circle"), 'Enter sample information here')),
    rHandsontableOutput('hot')
  ),
  div(
    tags$a('Reaction plate preview'),
    reactableOutput('plate'),
    tags$hr(),
    tags$a('ONT rapid barcoding plate'),
    rHandsontableOutput('barcode_plate')
  )
)

ui <- page_navbar(
  useShinyjs(),
  fillable = T,
  title = 'ONT rapid barcoding on Opentrons',
  theme = bs_theme(font_scale = 0.9, bootswatch = 'yeti', primary = '#229954'),
  sidebar = sidebar,
  nav_panel(
    'Samples and barcodes',
    div(
      layout_column_wrap(
        height = '83px',
        width = '200px',
        !!!vbs
      )
    ),
    div(
      layout_column_wrap(
        width = NULL, fill = F, 
        style = htmltools::css(grid_template_columns = 'auto 2fr'),
        #width = 1/2,
        panel1[[1]], panel1[[2]]
      )
    )
  ),
  nav_panel(
    'Opentrons protocol', verbatimTextOutput('protocol_preview')
  ),
  nav_panel(
    'Simulate run',
    actionButton('simulate', 'Run simulation', width = '25%'),
    verbatimTextOutput('stdout')
  ),
  nav_panel(
    'Deck layout', htmlOutput('deck')
  ),
  nav_panel(
    'Wetlab protocol', htmlOutput('wetlab')
    # actionButton(
    #   'protocol',
    #   'Show complete protocol', width = '25%', style = 'margin-top:20px',
    #   onclick = "window.open('protocol.html', '_blank')"
    # )
  )
)

server <- function(input, output, session) {
  
  # add opentrons_simulate path
  old_path <- Sys.getenv("PATH")
  Sys.setenv(PATH = paste(old_path, Sys.getenv('OPENTRONS_PATH'), sep = ":"))
  
  ### Protocol
  protocol_url <- "https://raw.githubusercontent.com/angelovangel/opentrons/main/protocols/02-ont-rapid-pcr.py"
  
  if (curl::has_internet()) {
    con <- url(protocol_url)
    protocol_template <- readLines(con, warn = F)
    close(con)
  } else {
    protocol_template <- readLines('02-ont-rapid-pcr.py', warn = F)
  }
  
  ### Reactives
  protocol <- reactiveValues(bc_vol = 0, rxn_vol = 0, sample_vol = 0, total_fmoles = 0, total_ng = 0, total_bases = 0, samples = 0, users = 0)
  
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
      str_replace('consolidate_vol_fraction = .*', paste0('consolidate_vol_fraction = ', input$consolidate_volume_factor)) %>%
      str_replace('source_labware = .*', paste0("source_labware = ", "'", input$sample_labware, "'")) %>%
      str_replace('aspirate_factor = .*', paste0('aspirate_factor = ', round(100/input$aspirate_speed, 2))) %>%
      str_replace('dispense_factor = .*', paste0('dispense_factor = ', round(100/input$dispense_speed, 2))) %>%
      str_replace('pool_reuse_tip = .*', paste0('pool_reuse_tip = ', if_else(input$reuse_tip, 'True', 'False')))
  })
  
  ### Observers
  observe({
    if(input$protocol_type == 'plasmid') {
      protocol$bc_vol <- 0.5
      protocol$sample_vol <- 5 * as.numeric(input$sample_volume_factor)
      protocol$rxn_vol <- protocol$bc_vol + protocol$sample_vol
      protocol$total_fmoles <- sum(hot()$fmoles, na.rm = T)
      protocol$total_ng <- sum(hot()$ng, na.rm = T)
      protocol$total_bases <- sum(hot()$dna_size, na.rm = T)
      protocol$samples <- 96 - str_count( myvalues()[1], ' ' )
      protocol$users <- length(unique(hot()$user)) - 1
      
    } else {
      protocol$bc_vol <- 1
      protocol$sample_vol <- 10 * as.numeric(input$sample_volume_factor)
      protocol$rxn_vol <- protocol$bc_vol + protocol$sample_vol
      protocol$total_fmoles <- sum(hot()$fmoles, na.rm = T)
      protocol$total_ng <- sum(hot()$ng, na.rm = T)
      protocol$total_bases <- sum(hot()$dna_size, na.rm = T)
      protocol$samples <- length(hot()$sample)
      protocol$samples <- 96 - str_count( myvalues()[1], ' ' )
      protocol$users <- length(unique(hot()$user)) - 1
    }
  })
  
  # 
  observeEvent(input$simulate, {
    # clear stdout
    shinyjs::html(id = "stdout", "")
    
    # check if opentrons_simulate is in path
    if (system2('which', args = 'opentrons_simulate') == 1) {
      shinyjs::html(id = 'stdout', "opentrons_simulate executable not found. Set the OPENTRONS_PATH variable to the opentrons_simulate path.")
      return()
    }
    
    # change button
    shinyjs::disable(id = 'simulate')
    shinyjs::html(id = 'simulate', "Working...")
    tmp <- tempfile('protocol', fileext = '.py')
    write(myprotocol(), file = tmp)
    #ht <- as_tibble(hot_to_r(input$hot))
    
    withCallingHandlers({
      if (FALSE) {
        processx::run(
          'echo', args = ("All volumes are 0, cannot simulate this!"),
          stderr_to_stdout = TRUE, 
          error_on_status = FALSE,
          stdout_line_callback = function(line, proc) {message(line)}
        )
      } else {
        processx::run(
          'opentrons_simulate', 
          args = c('-e', '-L', Sys.getenv('LABWARE_PATH'), tmp),
          stderr_to_stdout = TRUE, 
          error_on_status = FALSE,
          stdout_line_callback = function(line, proc) {message(line)}, 
        )
      }
      shinyjs::enable(id = 'simulate')
      shinyjs::html(id = 'simulate', "Run simulation")
    },
    message = function(m) {
      shinyjs::html(id = "stdout", html = m$message, add = TRUE); 
      shinyjs::runjs("document.getElementById('stdout').scrollTo(0,1e9);") 
      # scroll the page to bottom with each message, 1e9 is just a big number
    }
    )
  })
  
  ### Outputs
  # change to ng or fmol depending on protocol selected
  output$sample_amount <- renderUI({
    if(input$protocol_type == 'plasmid') {
      numericInput('ng_or_fmoles', 'fmol per reaction (~ 20)', value = 20, min = 1, max = 200, step = 1)
    } else {
      numericInput('ng_or_fmoles', 'ng per reaction (50-100 ng)', value = 50, min = 5, max = 500, step = 10)
    }
  })
  
  output$vbs00 <- renderText({
    paste0('Reactions (', protocol$users, ' users)')
  })
  output$vbs0 <- renderText({
    paste0(protocol$samples)
  })
  output$vbs1 <- renderText({
    paste0(protocol$rxn_vol, ' ul')
  })
  output$vbs2 <- renderText({
    paste0(round(protocol$total_fmoles, 0), ' fmol')
  })
  output$vbs3 <- renderText({
    paste0(round(protocol$total_ng, 0), ' ng')
  })
  output$vbs4 <- renderText({
    paste0(silabel(protocol$total_bases))
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
    rhandsontable(
      hot() %>% select(-c('bc_count', 'mycolor', 'fmolperul')),
      #svol = 9,
      rowHeaders = NULL) %>%
    hot_col('well', readOnly = T, renderer = rendergrey() ) %>%
    hot_col('barcode', type = 'dropdown') %>%
    hot_col('ng', readOnly = T, type = 'numeric', format = '0.0') %>%
    hot_col('fmoles', readOnly = T, type= 'numeric', format = '0.0') %>%
    hot_col('ul', readOnly = T, renderer = renderer() ) %>% # highlight volumes > max
    hot_col('dna_size', format = '0') %>%
    hot_col(c(2, 3), colWidths = 80) %>%
    hot_col(c(4, 5), colWidths = 50) %>%
    hot_validate_numeric('conc', min = 1, max = 5000, allowInvalid = T)
  })
  
  output$plate <- renderReactable({
    reactable(
      plate()$sample, 
      highlight = T, wrap = F, bordered = T, compact = T, fullWidth = T, sortable = F,
      defaultColDef = colDef(
        minWidth = 70, html = TRUE,
        headerStyle = list(background = "#f7f7f8", fontSize = '80%'),
        # color barcodes if duplicate
        style = function(value) {
          myvalue <- str_extract(value, 'barcode.*')
          mydf <- hot()
          if (!is.na(myvalue)) {
            textcolor <- mydf$mycolor[match(myvalue, mydf$barcode)]
          } else {
            textcolor <- 'black'
          }
          #print(myvalue)
          list(color = textcolor, fontSize = '70%')
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
    
    rhandsontable(bcplate, readOnly = T, stretchH = "all") #%>% 
      #hot_cols(colWidths = 60)
  })
  
  output$deck <- renderUI({
    HTML('<img src="deck-pcr.png" height="600">')
  })
  
  output$wetlab <- renderUI({
    tags$iframe(src= "protocol.html", width = 800, height = 800)
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
  
}

shinyApp(ui, server)

