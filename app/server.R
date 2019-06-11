source("loadPkgs.R")

options(shiny.maxRequestSize = 6000*1024^2)

server <- function(input, output, session) {
  
  # read in the data
  observeEvent(input$file1, {
    req(input$file1)
    inFile <- input$file1
    
    createAlert(session, "alert", "message1", title = "Loading transcriptome matrix",
                content = "Loading ..., please wait.", style="info", dismiss = FALSE, append = FALSE)
    
    countMatrix <- read.delim(inFile$datapath, header = input$header,
                                sep = input$sep, quote = input$quote, stringsAsFactors=FALSE, row.names = 1)
    transMatrix$val <- countMatrix
        
    if (!is.null(countMatrix)){ 
        createAlert(session, "alert", "success1", title = "File Loading Complete",
                    content = "Please further load proteome matrix file.", style="success", dismiss = FALSE, append = FALSE)    
    }
    
  })
  
  observeEvent(input$file2, {
    req(input$file2)
    inFile <- input$file2
    
    createAlert(session, "alert", "message2", title = "Loading proteome matrix",
                content = "Loading ..., please wait.", style="info", dismiss = FALSE, append = FALSE)
    
    countMatrix <- read.delim(inFile$datapath, header = input$header,
                                sep = input$sep, quote = input$quote, stringsAsFactors=FALSE, row.names = 1)
    protMatrix$val <- countMatrix
      
    if (!is.null(countMatrix)){ 
        createAlert(session, "alert", "success2", title = "File Loading Complete",
                    content = "You can now proceed...", style="success", dismiss = FALSE, append = FALSE)
    }
    
  })
  
  observeEvent(input$file3, {
    req(input$file3)
    inFile <- input$file3
    
    createAlert(session, "alert", "message3", title = "Loading gene annotation file",
                content = "Loading ..., please wait.", style="info", dismiss = FALSE, append = FALSE)
    
    annoTable <- read.delim(inFile$datapath, header = input$header,
                              sep = input$sep, quote = input$quote, stringsAsFactors=FALSE, row.names = NULL)
    annotationTb$val <- annoTable
    #cat(file=stderr(), str(annoTable))
    colnames(annotationTb$val) <- c("id","annotation")
    
    if (!is.null(annoTable)){ 
      createAlert(session, "alert", "success3", title = "Annotation Loading Complete",
                  content = "You can now proceed...", style="success", dismiss = FALSE, append = FALSE)
    }
    
  })
  
  observeEvent(c(transMatrix, protMatrix),{
    output$DataSummary <- renderFormattable({
      if(! is.null(transMatrix$val) ) { 
        countFile1 <- transMatrix$val
        summaryCells1 <- ncol(countFile1)
        summaryGenes1 <- nrow(countFile1)
      } else {
        summaryCells1 <- 0
        summaryGenes1 <- 0
      }
      
      if(! is.null(protMatrix$val) ) { 
        countFile2 <- protMatrix$val
        summaryCells2 <- ncol(countFile2)
        summaryGenes2 <- nrow(countFile2)
      } else {
        summaryCells2 <- 0
        summaryGenes2 <- 0
      }
        
      summaryTab1 <- cbind(summaryCells1, summaryGenes1)
      summaryTab2 <- cbind(summaryCells2, summaryGenes2)
      summary1 <- rbind(summaryTab1, summaryTab2)
      colnames(summary1) <- c("Time_points", "Total_genes")
      rownames(summary1) <- c("Transcriptome", "Proteome")
      summarydt <- as.data.frame(summary1)

      formattable(summarydt, 
                list(Time_points = formatter(.tag = "span", style = function(x) style(display = "bar", direction = "rtl", font.size = "15px", `border-radius` = "5px", `padding-left` = "4px", `padding-right` = "4px", width = paste(proportion(x),"px",sep=""), `background-color` = csscolor("#ffa31a"))),       
                    Total_genes = formatter("span", style = function(x) style(display = "bar", direction = "rtl", font.size = "15px", `border-radius` = "5px", `padding-left` = "4px", `padding-right` = "4px", width = paste(proportion(x),"px",sep=""), `background-color` = csscolor("#ffa31a")))
                ))
    })
  })
  
  output$dt1 <- DT::renderDataTable({
    req(transMatrix$val)
    DT::datatable(transMatrix$val, escape=FALSE, selection='single', options = list(searchHighlight = TRUE, scrollX = TRUE)) %>%
      formatRound(colnames(transMatrix$val), 3)
  })
  
  output$dt2 <- DT::renderDataTable({
    req(protMatrix$val)
    DT::datatable(protMatrix$val, escape=FALSE, selection='single', options = list(searchHighlight = TRUE, scrollX = TRUE)) %>%
      formatRound(colnames(protMatrix$val), 3)
  })
  
  #####################################################################################
  
  isRequiredSTEP <- reactive({ # reactive part = thos code is repeated when user input changes
    validate( # define error messages if user doesn't choose anything
      need(input$kshapeSteps != "",
           "Please select at least one step to run the pipeline"),
      need(!is.null(transMatrix$val) && !is.null(protMatrix$val), "Please upload data matrix using 'Get Data'")
    )
  })
  
  # show list of chosen running steps
  observeEvent(input$kshapeSteps,{
    output$chsteps <- renderText({
      isRequiredSTEP()
      runsteps <- paste(input$kshapeSteps, collapse = ", ")
      runsteps
    })
  })
  
  # select all
  observe({
    if(input$selectall == 0) return(NULL)
    else if(input$selectall %% 2 == 0) 
    {
      updateCheckboxGroupInput(session,"kshapeSteps","Select steps to perform:", choices=selectSteps)
    } 
    else
    {
      updateCheckboxGroupInput(session,"kshapeSteps","Select steps to perform:", choices=selectSteps, selected=unlist(selectSteps))
    }
  })
  
  # enable run button
  observe({
    if(!is.null(input$kshapeSteps) && !is.null(transMatrix$val) && !is.null(protMatrix$val)){
      shinyjs::enable("RUN")
    }
    else{
      shinyjs::disable("RUN")
    }
  })
  
  
  hideBoxes <- reactive({
    perm.vector <- as.vector(input$kshapeSteps)
    perm.vector
  }) 
  
  # RUN qc
  observeEvent(input$RUN,{
    listSteps <- hideBoxes()
    if(input$RUN>0 && ("QC" %in% isolate(listSteps))) {
      shinyjs::addClass("div#loadmessage","shiny-busy")
      source("scripts/step-qc.R", local=TRUE)
    }
  })
  
  # render ui
  
  observe({
    if(input$RUN == 0) return() 
    isolate({
      listSteps <- hideBoxes()
      if("QC" %in% isolate(listSteps)){
        output[["QC"]] <- renderUI({
          box(id = "QCbox", title = "ShinyOmics results: quality control", status = "success", solidHeader = FALSE,
              collapsed = FALSE, collapsible = TRUE, width=12, 
              paste0(QCSummaryInfo$timepoints," timepoints, ",QCSummaryInfo$sameItems," corresponding isoforms.")
          )
        })
      }
      
      if("kshape" %in% isolate(listSteps)){
        output[["kshape"]] <- renderUI({
          box(
              fluidRow(
                shinyjs::useShinyjs(),
                column(2, numericInput("num_clusters","Number of clusters", min = 2, max = 10000, step = 1, value = 64)),
                column(2, numericInput("num_workers","Number of threads", min = 1, max = 1000, step = 1, value = 1)),
                column(2, numericInput("num_iter","Number of iterations", min = 1, max = 10000, step = 1, value = 1)),
                column(2, withBusyIndicatorUI(actionButton("calcKshape", "kshape Cluster",class="btn-primary"))),
                column(3, br(),strong("Tips: change threads to allow parallel computing, change iterations to get confidence estimation."),offset = 0.1)
              ),
              hr(),
              fluidRow(
                shinyjs::useShinyjs(),
                column(style="border-right: 1px lightgray solid;", 12,
                  fluidRow(
                    column(4, textInput("show_clusters","Clusters to display",value = "1:64")),
                    column(4, selectInput("show_type","Display type",
                            choices = c("Series"="series", "Centroids"="centroids", "Both"="sc"), 
                            selected = "sc", multiple = FALSE)),
                    column(4, actionButton("drawKshape", "Draw Clusters",class="btn-primary",disabled="disabled"))
                  ),
                  fluidRow(
                    column(12, plotOutput("clusters", height = "1024px") %>% withSpinner(type = 5, color="#bf00ff", size = 1))
                  )
                )
              ),
              hr(),
              fluidRow(
                shinyjs::useShinyjs(),
                column(style="border-right: 1px lightgray solid;", 12,
                       fluidRow(column(3, actionButton("stats", "See Details",class="btn-primary",disabled="disabled"), offset=1),
                                column(3, downloadButton("dl", "Export to CSV",class="btn-primary",disabled="disabled"), offset=1)),
                       fluidRow(column(12,br(),DT::dataTableOutput("stats_table") %>% withSpinner(type = 1, color="#bf00ff", size = 1))),
                       fluidRow(column(12,br(),plotOutput("selected_clusters", height = "800px") %>% withSpinner(type = 7, color="#bf00ff", size = 1)))
                )
              ),
              id = "kshapebox", title = "ShinyOmics results: kshape", status = "success", solidHeader = FALSE,
              collapsed = FALSE, collapsible = TRUE, width=12)
        })
      }
      
    })
  })
  
  # RUN kshape
  observeEvent(input$calcKshape,{
    listSteps <- hideBoxes()
    if(input$calcKshape>0 && ("kshape" %in% isolate(listSteps))) {
      shinyjs::disable("drawKshape")
      shinyjs::disable("stats")
      shinyjs::disable("dl")
      withBusyIndicatorServer("calcKshape", {
        source("scripts/step-kshape.R", local=TRUE)
      })
      shinyjs::enable("drawKshape")
      shinyjs::enable("stats")
      shinyjs::enable("dl")
    }
  })
  
  observeEvent(input$drawKshape, {
    if(input$drawKshape == 0) return() 
    req(kshapeResult$val)
    output$clusters <- renderPlot({
        plot(kshapeResult$val[[1]], type=isolate({input$show_type}), clus=str_eval(paste0("c(",isolate({input$show_clusters}),")")) )
      }, res=100)
    })
  
  # RUN stats
  observeEvent(input$stats,{
    req(kshapeResult$val)
    if(input$stats>0) {
      shinyjs::disable("stats")
      source("scripts/step-stats.R", local=TRUE)
      shinyjs::enable("stats")
    }
  })
  
  
  # Download results to csv file
  output$dl <- downloadHandler(
    filename = function() {
      paste(format(Sys.time(), "%Y%m%d%H%M%S"), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(statsResult$val, file, row.names = FALSE)
    }
  )
  
  #show stats table
  output$stats_table <- DT::renderDataTable({
    req(statsResult$val)
    DT::datatable(statsResult$val, escape=FALSE, selection = 'single', options = list(searchHighlight = TRUE, scrollX = TRUE)) %>%
      formatRound(columns=c('dist'), digits=4)
  })
  
  # show selected clusters and gene
  observeEvent(c(input$stats_table_rows_selected, dispResult$val), {
    req(input$stats_table_rows_selected)
    s <- statsResult$val[input$stats_table_rows_selected,] # id, same, tc, pc
    #cat(file=stderr(), str(s))
    sid <- s$id
    cls <- unique(c(s$trans_cluster,s$prot_cluster))
    req(dispResult$val)
    df_selected <- dispResult$val[dispResult$val$cluster %in% cls,]
    output$selected_clusters <- renderPlot({
      sids <- c(paste0("trans-",sid), paste0("prot-",sid))
      dist <- SBD(as.numeric(ctransMatrix$val[sids[1],]),as.numeric(cprotMatrix$val[sids[2],]),znorm=T)$dist
      
      #cat(file=stderr(), str(sids))
      #cat(file=stderr(), str(df_selected))
      d1 <- df_selected %>% mutate(highlight_flag = if_else(id %in% sids, T, F)) 
      d2 <- d1 %>% filter(highlight_flag==T) 
      ggplot(data=d1,aes(x=variable,y=value,group=id)) + geom_line(aes(colour=highlight_flag)) + 
        geom_line(data=d2, color="red",show.legend=F) + facet_grid(cols=vars(cluster)) + 
        scale_color_manual(values=c('#D3D3D3', 'red'), guide=FALSE) + theme_bw() +
        ggtitle(paste0("shape-based distance = ",dist),"Distances lie between 0 and 2, with 0 indicating perfect similarity.")
    })
  })
  
}
