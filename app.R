library(rhandsontable)
library(shiny)
library(shinyjs)
library(plyr)
library(dplyr)
library(data.table)
library(ggplot2)
library(reshape2)
library(plotrix)
library(stats)


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++    Helper function to render and save plots     +++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# UI side
plotDownloadUI <- function(id, height = 600, width = 800) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      selectizeInput(
        "selectvars",
        "Select row factors:",
        choices = "",
        selected = "",
        multiple = TRUE
      ),
      textInput("xlabel", "X-axis label", "Row Factors"),
      textInput("ylabel", "Y-axis label", "Values"),
      textInput("ptitle", "Plot title", "Values vs. Row Factors"),
      actionButton("replot", "(Re)plot", icon = icon("undo")),
      br(),
      br(),
      numericInput(
        ns("pwidth"),
        "Img Width",
        value = 5,
        min = 5,
        max = 20
      ),
      numericInput(
        ns("pheight"),
        "Img Height",
        value = 5,
        min = 5,
        max = 20
      ),
      downloadButton(ns("download_plot"), "Download figure")
    ),
    mainPanel(plotOutput(
      ns('plot'), height = height, width = width
    ))
  )
}

# Server side
plotDownload <- function(input, output, session, plotFun) {
  output$plot <- renderPlot({
    plotFun()
  })
  
  output$download_plot <- downloadHandler(
    filename = function() {
      "plot.png"
    },
    content = function(file) {
      ggsave(file,
             plotFun(),
             width = input$pwidth,
             height = input$pwidth)
    }
  )
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++    Server     +++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

server <- function(input, output, session) {
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  #+++++++++++++++++    Initialize TAB     +++++++++++++++++++
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  init_table <- function(input) {
    col_names <-
      input$CF %>% strsplit(., ",") %>%
      lapply(., trimws) %>%
      unlist() %>%
      rep(., each = input$rep)
    
    row_names <-
      input$RF %>%
      strsplit(., ",") %>%
      lapply(., trimws) %>%
      unlist()
    
    data <-
      matrix(
        data = 0,
        nrow = length(row_names),
        ncol = length(col_names),
        dimnames = list(row_names, col_names)
      )
  }
  
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  #+++++++++++++++    Capture curr TAB     +++++++++++++++++++
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  capture_curr_df <- function(input) {
    live_table <- isolate(input$exceltable)
    row_names <-
      input$RF %>% strsplit(., ",") %>% lapply(., trimws) %>% unlist()
    if (!is.null(live_table)) {
      dfin <- hot_to_r(input$exceltable) %>%
        as.data.frame(., row.names = row_names) %>%
        t(.) %>%
        melt(data = .)
      names(dfin) <- c("CF", "RF", "value")
      return(dfin)
    }
    
  }
  
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  #++++++++++++++++   Freeze/Unfreeze     ++++++++++++++++++++
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  observeEvent(input$freeBtn, {
    disable("CF")
    disable("RF")
    disable("rep")
  })
  
  observeEvent(input$unfBtn, {
    enable("CF")
    enable("RF")
    enable("rep")
  })
  
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  #+++++++++++++++    Compute modules     ++++++++++++++++++++
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  getaovmodel <- function(dfin, input) {
    row_size <-
      input$RF %>% strsplit(., ",") %>% lapply(., trimws) %>% unlist() %>% length()
    if (row_size < 2) {
      # Assuming one-way ANOVA
      return (aov(value ~ CF, data = dfin))
    }
    return(aov(value ~ CF * RF, data = dfin))
  }
  
  get_pairwise_t_test_values <- function(dfin, input) {
    fdata <- dfin
    row_size <-
      input$RF %>% strsplit(., ",") %>% lapply(., trimws) %>% unlist() %>% length()
    if (row_size < 2) {
      # Assuming pairwise t-test among CF
      return (pairwise.t.test(fdata$value, fdata$CF, p.adjust.method = "none"))
    }
    fdata$CFRF <- with(dfin, interaction(RF,  CF, sep = ':'))
    return(pairwise.t.test(fdata$value, fdata$CFRF, p.adjust.method = "none"))
  }
  
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  #+++++++++++++++    Reactive ANOVA     +++++++++++++++++++++
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  text_aov_reactive <- eventReactive(input$goBtn, {
    dfin <- capture_curr_df(input)
    aov.model <- getaovmodel(dfin, input)
    print(aov.model)
    br()
    br()
    cat("++++++++++++++++++++++++++++++++++++++++++++++++++++\n")
    print(summary(aov.model))
    br()
    br()
    cat("++++++++++++++++++++++++++++++++++++++++++++++++++++\n")
    cat("\n")
    cat("Coefficients")
    cat("\n")
    print(aov.model$coefficients)
  })
  
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  #+++++++++++++++    Reactive Fisher     ++++++++++++++++++++
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  text_fisher_reactive <- eventReactive(input$goBtn, {
    dfin <- capture_curr_df(input)
    get_fisher <- get_pairwise_t_test_values(dfin, input)
    print(get_fisher)
  })
  
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  #+++++++++++++++    Reactive barPlot     +++++++++++++++++++
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  data_summary <- function(data, varname, groupnames) {
    summary_func <- function(x, col) {
      xcol <- x[[col]]
      c(mean = mean(xcol, na.rm = TRUE),
        sd = std.error(xcol, na.rm = T)) #sd(xcol, na.rm = TRUE))
    }
    data_sum <- ddply(data, groupnames, .fun = summary_func,
                      varname)
    data_sum <- plyr::rename(data_sum, c("mean" = varname))
    return(data_sum)
  }
  
  bar_plot_reactive <- eventReactive(input$replot, {
    cols <- input$selectvars %>% sort()
    dfin <- capture_curr_df(input) %>% filter(., RF %in% cols)
    df3 <- data_summary(dfin,
                        varname = "value",
                        groupnames = c("CF", "RF"))
    
    # Standard deviation of the mean as error bar
    p <- ggplot(df3, aes(x = RF, y = value, fill = CF)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      # geom_text(aes(label=round(value,1)), vjust=1.6, color="white",
      #           position = position_dodge(0.9), size=3.5) +
      geom_errorbar(aes(ymin = value - sd, ymax = value + sd),
                    width = .2,
                    position = position_dodge(.9))
    
    p + scale_fill_grey(start = .3, end = .9) + theme_bw() + labs(x = input$xlabel,
                                                                       y = input$ylabel,
                                                                       title = input$ptitle)
  })
  
  observe({
    row_names <-
      input$RF %>% strsplit(., ",") %>% lapply(., trimws) %>% unlist()
    updateSelectizeInput(session,
                         "selectvars",
                         choices = row_names,
                         selected = row_names[1])
  })
  
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  #+++++++++++++++    Outputs to UI     ++++++++++++++++++++++
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # Output table
  output$exceltable <- renderRHandsontable({
    rhandsontable(
      reactive(init_table(input))(),
      useTypes = T,
      stretchH = "all",
      rowHeaderWidth = 200
    )
  })
  
  # ANOVA table
  output$anovatable <- renderPrint({
    text_aov_reactive()
  })
  
  # Bar plot
  callModule(plotDownload, "barPlot", bar_plot_reactive)
  
  # Fisher table
  output$fischertable <- renderPrint({
    text_fisher_reactive()
  })
  
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#++++++++++++   Graphical User Interface     +++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

ui <- navbarPage(
  "WebNova",
  tabPanel("Input Data",
           sidebarLayout(
             sidebarPanel(
               textInput("CF", "Column factors separated by comma", "Control, Group A"),
               numericInput(
                 "rep",
                 "# observations per column factor",
                 value = 2,
                 min = 2,
                 max = 1000
               ),
               textInput("RF", "Row factors separated by comma", "Analysis 1, Analysis 2"),
               actionButton("freeBtn", "Freeze"),
               actionButton("unfBtn", "Unfreeze"),
               actionButton("goBtn", "Calculate")
             ),
             mainPanel(rHandsontableOutput("exceltable"))
           )),
  tabPanel("ANOVA Table", verbatimTextOutput("anovatable")),
  tabPanel("Fisher Table", verbatimTextOutput("fischertable")),
  tabPanel("Bar charts", plotDownloadUI("barPlot"))
)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++    Entrypoint     +++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

shinyApp(ui = ui, server = server)
