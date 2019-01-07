
if(Sys.getenv('SHINY_PORT') == "") options(shiny.maxRequestSize=10000*1024^2)


values <- reactiveValues(val = NULL)

transMatrix <- reactiveValues(val=NULL)
protMatrix <- reactiveValues(val=NULL)
SummaryInfo <- reactiveValues(val=NULL)

options(shiny.maxRequestSize = 6000*1024^2)

selectSteps <- list("QC", "kshape")

ctransMatrix <- reactiveValues(val=NULL)
cprotMatrix <- reactiveValues(val=NULL)
annotationTb <- reactiveValues(val=NULL)
QCSummaryInfo <- reactiveValues(timepoints=NULL,sameItems=NULL)

kshapeResult <- reactiveValues(val=NULL)
statsResult <- reactiveValues(val=NULL)
dispResult <- reactiveValues(val=NULL)