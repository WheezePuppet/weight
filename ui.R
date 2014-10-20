require(shiny)
require(lubridate)

source("db.R")

conn <- get.connection()
latestEntry <- dbGetQuery(conn,
    "select date,time from weight order by date desc limit 1")
latestDate <- ymd(latestEntry[[1]])
latestTime <- latestEntry[[2]]

if (latestTime == "am") {
    defaultDate <- latestDate
    defaultTime <- "pm"
} else {
    defaultDate <- latestDate + days(1)
    defaultTime <- "am"
}

shinyUI(fluidPage(
  
    ###  Application title
    headerPanel(paste("Stephen's Weight Loss")),

    ### Sidebar with text box for input search term
    tabsetPanel(id="data.source",selected="Weight",
        tabPanel("Data Entry",
            dateInput("date",label="",value=defaultDate),
            radioButtons("ampm",label="",choices=c("am","pm"),
                selected=defaultTime),
            numericInput("weight",label="Weight (lbs)",value="0"),
            numericInput("bodyfat",label="Body Fat %",value="0"),
            numericInput("water",label="Water %",value="0"),
            numericInput("muscle",label="Muscle (lbs)",value="0"),
            numericInput("bmr",label="BMR (kcals)",value="0"),
            numericInput("bone",label="Bone",value="0"),
            #tags$input(id="password",type="password",label="Data Entry password",value=""),
            textInput("password",label="Data Entry password",value=""),
            actionButton("enter",label="Enter data"),
            textOutput("msg")
        ),
        #tabPanel("Weight", 
        #    sidebarLayout(
        #        sidebarPanel(width=2),
        #        mainPanel(
        #            plotOutput("weight"))
        #    )
        #),
        tabPanel("Weight", plotOutput("weight")),
        tabPanel("Body Fat %", plotOutput("bodyfat")),
        tabPanel("Water %", plotOutput("water")),
        tabPanel("Muscle", plotOutput("muscle")),
        tabPanel("BMR", plotOutput("bmr")),
        tabPanel("Bone", plotOutput("bone"))
    )
))

