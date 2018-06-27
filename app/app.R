#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(RSQLite)
library(shiny)
library(shinyjs)
library(ggplot2)

sqlitePath <- "/db/diesel.db"
table <- "tbl1"

# Define the fields we want to save from the form
fields <- c("Datum", "Liter", "Preis")

# Shiny app with 3 fields that the user can submit data for
shinyApp(
  ui = fluidPage(
    
    titlePanel("d18m"),
    
    # without Shinyjs reset() does not work...
    useShinyjs(),
    
    #textInput("Datum", "Datum", ""),
    dateInput("Datum","Datum",Sys.Date(),"2000-01-01",Sys.Date(),format = "dd.mm.yyyy",NULL, weekstart = 0, language = "de", width= NULL ),
    textInput("Liter","Liter",""),
    #checkboxInput("lit", "I've built a Shiny app in R before", FALSE),
    textInput("Preis", "Gesamtpreis", ""),
    actionButton("submit", "Speichern"),
    
    tabsetPanel(
  #plot, do not know what plotclick means
      
     
      
        tabPanel("Preis pro Liter",
          plotOutput("dbplot" , 
                     click = "plot_click",
                     dblclick = "plot_dbclick",
                     hover = "plot_hover",
                     brush = "plot_brush"
                     ),
          verbatimTextOutput("info")
        )
        
     
      
    ),
  tabsetPanel(
    tabPanel("Datenbanken",
             DT::dataTableOutput("responses", width = 500), tags$hr()
    )
  )
  
  ),
  server = function(input, output, session) {
    
    output$dbplot <- renderPlot({

      # Connect to the database
      db <- dbConnect(SQLite(), sqlitePath)
      # Construct the fetching query
      query <- paste0("SELECT * FROM tbl1")
      # Submit the fetch query and disconnect
      data <- dbGetQuery(db, query)
      dbDisconnect(db)
      #pop <- data
      #names(pop) <- data
     # plot(as.matrix(data$Liter))
      data2 <- data[,1]
      #plot(as.Date(as.numeric(data[,1]),"1970-01-01"),data$Preis / data$Liter)
      d <- data.frame(Datum =as.Date(data$Datum, origin = "1970-01-01"), Preis = (data$Preis / data$Liter))
      ggplot(d,aes(x=Datum, y= Preis )) + geom_line() + geom_point() 
      
    })
    
    output$info <- renderText({
      xy_str <- function(e){
        if(is.null(e)) return ("NULL\n")
        paste0 ("x=", round(e$x,1), "y=", round(e$y,1),"\n")
      }
      xy_range_str <- function(e){
        if (is.null(e)) return ("NULL\n")
        paste0("xmin=", round(e$xmin,1), "xmax=", round(e$xmax,1),
               "ymin=", round(e$ymin,1), " ymax=", round(e$ymax,1))
      }
      
      paste0(
        "click: ", xy_str(input$plot_click),
        "dblclick: ", xy_str(input$plot_dblclick),
        "hover: ", xy_str(input$plot_hover),
        "brush: ", xy_str(input$plot_brush)
      )
    })
  
 

    # Whenever a field is filled, aggregate all form data
    formData <- reactive({
      #data <- loadData()
      data <- sapply(fields, function(x) input[[x]])
      data
    })
    
    # When the Submit button is clicked, save the form data
    observeEvent(input$submit, {
     saveData(formData())
      
      output$dbplot <- renderPlot({
        
        # Connect to the database
        db <- dbConnect(SQLite(), sqlitePath)
        # Construct the fetching query
        query <- paste0("SELECT * FROM tbl1")
        # Submit the fetch query and disconnect
        data <- dbGetQuery(db, query)
        dbDisconnect(db)
        #pop <- data
        #names(pop) <- data
        # plot(as.matrix(data$Liter))
        data2 <- data[,1]
        #plot(as.Date(as.numeric(data[,1]),"1970-01-01"),data$Preis / data$Liter)
        d <- data.frame(Datum = data$Datum, Preis = (data$Preis / data$Liter))
        ggplot(d,aes(x=as.Date(data$Datum, origin = "1970-01-01"), y= Preis)) + geom_line() + geom_point()
        
      })
    
      #set fields to defaul values
     reset("Datum")
     reset("Liter")
     reset("Preis")
     
    })
    
    # Show the previous responses
    # (update with current response when Submit is clicked)
    output$responses <- DT::renderDataTable({
      input$submit
      loadData()
    }) 
    


    
    saveData <- function(data) {
      # Connect to the database
      db <- dbConnect(SQLite(), sqlitePath)
      # Construct the update query by looping over the data fields
      query <- sprintf(
        "INSERT INTO %s (%s) VALUES ('%s')",
        table, 
        paste(names(data), collapse = ", "),
        paste(data, collapse = "', '")
      )
      # Submit the update query and disconnect
      dbGetQuery(db, query)
      dbDisconnect(db)
    }
    
    loadData <- function() {
      # Connect to the database
      db <- dbConnect(SQLite(), sqlitePath)
      # Construct the fetching query
      query <- sprintf("SELECT * FROM %s", table)
      # Submit the fetch query and disconnect
      data <- dbGetQuery(db, query)
      dbDisconnect(db)
      data
    }
    

    
  }
)


# Run the application 
#shinyApp(ui = ui, server = server)

