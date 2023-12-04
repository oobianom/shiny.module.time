quickcode::clean(clearPkgs = TRUE) #clean environment, and remove previously loaded packages to avoid contamination
quickcode::libraryAll(shiny) #load key libraries and print a message with loaded libraries and versions


#----------SCALABLE TIME MODULE FUNCTIONS--------------------------------

initalizeTime <- function(v,t) {
  if (!inherits(env1[[t]], "numeric")) {
    env1[[t]] <- as.numeric(v)*60 + 1
    
  }else{
    env1[[t]] <- env1[[t]] - 1
    if(env1[[t]] < 0) env1[[t]] <- 0
  }
}

formattime <- function(t){
  if(is.numeric(t))sprintf("%s:%02d", floor(t / 60), t %% 60)
}

timerServer <- function(input, output, session,t1 = "time1",add = "add1",start = "startt",inTime = "unittime",outTime = "currentTime", pause ="pause"){
  env1[[t1]] <- "0"
  goldenT <- reactiveValues()
  goldenT[[pause]] <- TRUE #whether to start on load
  observeEvent(input[[add]], {
    p <- (as.numeric(env1[[t1]])/60) + 1
    env1[[t1]] <- "0"
    initalizeTime(v = p, t = t1)
  })
  observeEvent(input[[start]], {
    env1[[t1]] <- "0"
    initalizeTime(v = input[[inTime]], t = t1)
    goldenT[[pause]] <- FALSE
  })
  observeEvent(input[[pause]], {
    goldenT[[pause]] <- !goldenT[[pause]]
  })
  output[[outTime]] <- renderText({
    invalidateLater(1000, session)
    if ((env1[[t1]] > 0 | env1[[t1]] =="0") & (!goldenT[[pause]])) {
      initalizeTime(v = input[[inTime]], t = t1)
    }
    if(env1[[t1]] !="0")paste(formattime(env1[[t1]])) else "0:00"
  })
}



#----------CREATE AN APP TO SHOW FUNCTIONALITY--------------------------------
env1 <- new.env() #create environment for the app

# Define UI for displaying current time ----
ui <- fluidPage(

  # timer 1 UI
  numericInput("unittime","Enter time (minutes)",1),
  actionButton("startt", "START/RESTART"),
  actionButton("add1", "add 1min"),
  actionButton("pause", "PAUSE/CONTINUE"),
  h2(textOutput("currentTime")),
  hr(),hr(),

  # timer 2 UI
  numericInput("unittime2","Enter time (minutes)",2),
  actionButton("startt2", "START/RESTART"),
  actionButton("add12", "add 1min"),
  actionButton("pause2", "PAUSE/CONTINUE"),
  h2(textOutput("currentTime2"))
)

# Define server logic to show current time, update every second ----
server <- function(input, output, session) {
  # trigger time 1
  timerServer(input, output, session,
              t1 = "time1",
              add = "add1",
              start = "startt",
              inTime = "unittime",
              outTime = "currentTime", 
              pause ="pause")

  # trigger time 2
  timerServer(input, output, session,
              t1 = "time2",
              add = "add12",
              start = "startt2",
              inTime = "unittime2",
              outTime = "currentTime2", 
              pause ="pause2")
}

# Create Shiny app ----
shinyApp(ui, server)
