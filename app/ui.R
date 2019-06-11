source("loadPkgs.R")

header <- dashboardHeader(
              title = "ShinyOmics",
              tags$li(class = "dropdown",
                      tags$a(href="#", style = "font-size: 20px;", "Multi-Omics Integrated Analysis Platform")
                    )
          )

sidebar <- dashboardSidebar( 
                sidebarMenu(
                    menuItem("Home", href = NULL, tabName = "home", icon = icon("home"), selected = T),
                    menuItem("Get data", icon = icon("folder-open"), tabName = "getdata"),
                    menuItem("Tools", tabName = "tools", icon = icon("cubes"), 
                        menuSubItem("ShinyOmics", tabName = "ShinyOmics", icon = icon("dot-circle-o"))#,
                        #menuSubItem("O2PLS", tabName = "O2PLS", icon = icon("dot-circle-o"))
                    )
                 ),
                 div(class="hide_when_sidebar_collapsed",
                    helpText("Developed by ", a("Yanhai Gong", href = "mailto:gongyh@qibebt.ac.cn"), 
                      br(), a("Single Cell Center @ Qibebt, CAS", 
                              href = "http://singlecellcenter.org/en/index.aspx", target='_blank'),
                      style = "padding-left:1em; padding-right:1em;position:absolute; bottom:1em; ")
                 )
)

body <- dashboardBody(
  shinyjs::useShinyjs(),
  tags$head(includeCSS("style.css")),
  tags$script(HTML("$('body').addClass('sidebar-mini');")),
  
  tabItems(
    tabItem(tabName = "home",
            fluidRow(
              box(
                title = h2("ShinyOmics - Multi-omics integrated analysis pipeline"), solidHeader = FALSE,
                collapsible = FALSE, width = 12, status="info", includeMarkdown("about.md")
              )
            )
    ),
    tabItem(tabName = "getdata",
            fluidRow(
              box( 
                useShinyjs(),
                tags$style(appCSS),
                title = "Choose file to upload",
                collapsed = TRUE, collapsible = FALSE, width=4,
                selectInput('sep', 'Separator', c(Comma=',',Semicolon=';',Tab='\t'), '\t'),
                selectInput('quote', 'Quote', c(None='', 'Double Quote'='"', 'Single Quote'="'"), '"'),
                checkboxInput(inputId = "header", label = "Header",value=TRUE),
                fileInput('file1', 'Transcriptome Matrix', accept = c('text/csv', 'text/comma-separated-values',
                            'text/tab-separated-values', 'text/plain', '.csv',
                            '.tsv', '.matrix', multiple = FALSE)),
                fileInput('file2', 'Proteome Matrix', accept = c('text/csv', 'text/comma-separated-values',
                            'text/tab-separated-values', 'text/plain', '.csv',
                            '.tsv', '.matrix', multiple = FALSE)),
                fileInput('file3', 'Gene Annotation', accept = c('text/csv', 'text/comma-separated-values',
                                                                 'text/tab-separated-values', 'text/plain', '.csv',
                                                                 '.tsv', '.matrix', multiple = FALSE))
              ),
              box(
                title = "Summary",
                collapsed = FALSE, collapsible = TRUE, width=8,
                #tags$head(tags$style(type = "text/css", "#DataSummary th {display:none;}")),
                formattableOutput("DataSummary"),
                tags$hr(),
                useShinyjs(),  # Set up shinyjs
                bsAlert("alert")
              )
            ),
            fluidRow(
              tabBox(title = "Preview", id = "previewDT", width = 12,
                tabPanel("Transcriptome", div(DT::dataTableOutput('dt1') %>% withSpinner(type = 8, color="#bf00ff", size = 1))),
                tabPanel("Proteome", div(DT::dataTableOutput('dt2') %>% withSpinner(type = 8, color="#bf00ff", size = 1)))
              )
            )
    ),
    tabItem(tabName = "ShinyOmics",
            fluidRow(tags$head(tags$style(".checkbox-inline {margin: 0 !important;} div.checkbox {margin-top: 10px;}")),
                     box(title = "ShinyOmics", status = "primary", solidHeader = TRUE,
                         collapsed = FALSE, collapsible = TRUE, width=12,
                         useShinyjs(),  # Include shinyjs
                         box(id = "checkList", title="analysis steps", status = "warning", collapsible = FALSE, solidHeader = TRUE, width=6, 
                                          checkboxGroupInput('kshapeSteps', "Select steps to perform:", selectSteps),
                                          actionButton("selectall", "Select All", class = "taskDFbutton")),
                                      
                         box(title="Running selected steps", status = "success", collapsible = FALSE, solidHeader = TRUE, width=6, 
                             "Selected steps:", br(),
                             textOutput("chsteps"), tags$hr(),
                             actionButton("RUN", "Run", width = "165px", icon("play-circle"), class = "runbutton", disabled = TRUE),
                             #withBusyIndicatorUI(downloadButton("report", "Generate report", class = "actbutton", disabled = TRUE)),
                             tags$style(type='text/css', "#RUN { margin-bottom:10px;margin-right: -7px;}"),
                             tags$style(type='text/css', "#report { margin-top:0px;margin-right: -7px;}"), br(),
                             conditionalPanel(condition="$('div#loadmessage').hasClass('shiny-busy')",
                                              tags$div("Running selected steps ...",id="loadmessage"), img(src="ajax-loader-bar.gif"))
                         )
                     )
            ),
            fluidRow(
              uiOutput("QC"),
              uiOutput("kshape"))
      )
  )
)

ui <- dashboardPage(skin="purple", header, sidebar, body)
