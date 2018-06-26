
require(shiny)
require(data.table)
require(dplyr)
require(lubridate)

source("db.R")

conn <- get.connection()

shinyServer(function(input,output,session) {

    output$msg <- renderText({
        if (input$enter == 0) {
            "" 
        } else {
            isolate({
                if (input$password == "iloverae") {
                    query(paste0("insert into weight values (",
                        "'",input$date,"',",
                        "'",input$ampm,"',",
                        "'",input$weightInput,"',",
                        "'",input$bodyfat,"',",
                        "'",input$water,"',",
                        "'",input$muscle,"',",
                        "'",input$bmr,"',",
                        "'",input$bone,"')"
                    ))
                "done."
                } else {
                    "Wrong password."
                }
            })
        }
    })

    data.set <- reactive({
        input$enter
        collect(tbl(
            src_mysql("stephen",user="stephen",password="davies4ever"),
                "weight"))  %>%
            mutate(date=ymd_hms(paste(date,
                ifelse(time == "am","09:00:00","21:00:00"))))
    })


    output$weight <- renderPlot({
        plot.metric(select(data.set(),1:3))
    })

    output$bodyfat <- renderPlot({
        plot.metric(select(data.set(),c(1:2,4)))
    })

    output$water <- renderPlot({
        plot.metric(select(data.set(),c(1:2,5)))
    })

    output$muscle <- renderPlot({
        plot.metric(select(data.set(),c(1:2,6)))
    })

    output$bmr <- renderPlot({
        plot.metric(select(data.set(),c(1:2,7)))
    })

    output$bone <- renderPlot({
        plot.metric(select(data.set(),c(1:2,8)))
    })

    plot.metric <- function(data.set) {
        metric.name <- names(data.set)[[3]]
        names(data.set)[[3]] <- "metric"
        p <- ggplot(data.set, aes(date,metric,fill=time))
        if(metric.name=="weight") {
            p <- p + ylim(180,220)
        }

        p <- p + geom_point(size=3,shape=21,color="black") + 
            stat_smooth(method="loess") +
            ggtitle(metric.name) +
            ylab(ifelse(metric.name=="Weight","Weight (lbs)","")) + 
            xlab("") +
            scale_fill_manual(values=c("yellow","black"))
            
        print(p)
    }

    output$dateTimeControls <- renderUI({
        data.set
        latestEntry <- query(
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

        list(
            dateInput("date",label="",value=defaultDate),
            radioButtons("ampm",label="",choices=c("am","pm"),
                selected=defaultTime)
        )
    })

})

query <- function(query.string) {
    tryCatch({
        return(dbGetQuery(conn,query.string))
        },
        catch= function(e) { cat("Error: ",e,"\n") }
    )
}
