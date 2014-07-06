
require(shiny)
require(data.table)

source("db.R")

shinyServer(function(input,output,session) {

    output$msg <- renderText({
        if (input$enter == 0) {
            "" 
        } else {
            isolate({
                if (input$password == "iloverae") {
                    dbGetQuery(get.connection(),
                        paste0("insert into weight values (",
                        "'",input$date,"',",
                        "'",input$ampm,"',",
                        "'",input$weight,"',",
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
        basic.data <- 
            data.table(dbGetQuery(get.connection(),
                "select * from weight order by date, time"))
        basic.data[,date:=ymd_hms(paste(basic.data$date,
            ifelse(basic.data$time == "am","09:00:00","21:00:00")))]
        basic.data
    })


    output$weight <- renderPlot({
        plot.metric(list(data=data.set()[[3]],
            name=names(data.set())[[3]]),
            data.set()$date)
    })

    output$bodyfat <- renderPlot({
        plot.metric(list(data=data.set()[[4]],
            name=names(data.set())[[4]]),
            data.set()$date)
    })

    output$water <- renderPlot({
        plot.metric(list(data=data.set()[[5]],
            name=names(data.set())[[5]]),
            data.set()$date)
    })

    output$muscle <- renderPlot({
        plot.metric(list(data=data.set()[[6]],
            name=names(data.set())[[6]]),
            data.set()$date)
    })

    output$bmr <- renderPlot({
        plot.metric(list(data=data.set()[[7]],
            name=names(data.set())[[7]]),
            data.set()$date)
    })

    output$bone <- renderPlot({
        plot.metric(list(data=data.set()[[8]],
            name=names(data.set())[[8]]),
            data.set()$date)
    })

    plot.metric <- function(metric,dates) {
        name <- metric$name
        metric <- metric$data
        m <- lm(metric~dates)
        if(name=="weight") {
            ylim <- c(180,220)
            h.lines <- seq(ylim[1],ylim[2],5)
        } else {
            min.m <- min(metric)
            max.m <- max(metric)
            ylim <- c(min.m - (max.m-min.m)/8,
                      max.m + (max.m-min.m)/8)
            h.lines <- NULL
        }

        plot(dates[grep("09:00",dates)],metric[grep("09:00",dates)],
            pch=21,bg="yellow",
            ylim=ylim,main=name,
            ylab=ifelse(name=="Weight","Weight (lbs)",""),xlab="")
        points(dates[!grepl("09:00",dates)],metric[!grepl("09:00",dates)],
            pch=19)
        abline(m,col="green")
        legend("topright",pch=c(21,19),legend=c("morning","evening"),
            pt.bg="yellow")

        lapply(h.lines, function(h.line) abline(h=h.line,col="grey",lty=2))

        change.per.week <-
            (m$fitted.values[length(metric)] - m$fitted.values[1]) /
                (length(metric)-1) * 7
        text(1399780242,184.4146,
            paste("losing",abs(round(change.per.week,2)),"per week"),
            cex=.8)
    }
})
