library(shiny)
library(shinydashboard)
library(htmltools)
library(RSQLite)
library(rmarkdown)
library(knitr)

# Define UI
ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", href = "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"),
    tags$link(rel = "stylesheet", href = "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap-theme.min.css"),
    tags$script(src = "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js")),
  # Die Einbindung des externen Stylesheets funktioniert leider nicht. 
  # tags$script(rel = "stylesheet", type = "text/css", href = here::here("www/style.css")),

  tags$style(
    "body {font-family: Arial;
      }",
    
    ".text-input {
                display: flex;
                justify-content: left;
                align-items: left;
                height: 400px;
                flex-direction: column;
                font-weight: 700;
                font-size: clamp(1.25rem, 3vw, 2rem);
                margin: 50px;
                    }",
    
    ".head {
                display: flex;
                justify-content: center;
                align-items: center;
                height: 300px;
                border: 3px solid darkblue;
                background-color: darkblue;
                flex-direction: column;
                }",
    ".div_logo {
              margin-left: 20px;
    }",
    ".logo {
              width: 15%;
              height: auto;
    }",
    "h1   {   color: white;
                font-weight: 1000;
                  }",
    "h2   {   color: white;
                  }",
    "p    {   font-size: clamp(1.25rem, 3vw, 2rem);
                  }",
    ".einleitung-text {
              margin: 50px;
    }",
    ".submit {
              margin-left: 150px;
              margin-bottom: 200px;
              margin-top: 100px;
              padding: 25px 45px;
              color: #801B23;
    }",
    ".download {
              margin-left: 250px;
              margin-bottom: 200px;
              margin-top: 100px;
              padding: 25px 35px;
              color: #801B23;
    }",
    ".danke {
              text-align: center;
              color: black;
    }"
  ),

  
  
  div(class="div_logo",
      img(class="logo", src="logo_ksa.png", alt="Das Logo des Kantons Zürichs")
      ),
  div(class="head",
      h1("Frageformular 2024"),
      h2("Kantonales Sozialamt - Sozialversicherungen")
  ),
  div(class = "einleitung-text",
      p("Pro Durchführungsstelle ist ein Fragebogen bis zum 28. Februar 2024 auszufüllen. Bei IKZL Gemeinden und SVA-Gemeinden sind alle Angaben durch die IKZL-Durchführungsstellen
         beziehungsweise durch die SVA gesamthaft zu erfassen (inklusive angeschlossene Gemeinden")),
  # Text input
  div(class = "text-input",
      textInput("dstelle", "Durchführungsstelle"),
      textInput("stellenleit", "Stellenleinter/in"),
      textInput("mail", "E-Mail Adresse"),
      textInput("telefon", "Telefonnummer")
      ),
  
  # Text area
  textAreaInput("comments", "Any additional comments?"),
  
  # Radio buttons
  radioButtons("color", "What's your favorite color?", choices = c("Red", "Blue", "Green")),
  
  # Checkbox group
  checkboxGroupInput("hobbies", "What are your hobbies?", choices = c("Reading", "Sports", "Music")),
  
  # Select input with multiple selection
  selectInput("fruits", "Which fruits do you like?", choices = c("Apple", "Banana", "Orange", "Mango"), 
              multiple = TRUE),
  
  # Slider input
  sliderInput("age", "How old are you?", min = 18, max = 100, value = 25),
  
  # Date input
  dateInput("dob", "Wann haben Sie den Fragebogen ausgefüllt?"),
  
   # Dankesseite
  conditionalPanel(
    condition = "input.submit > 0",
    tags$h1(class="danke", "Danke für das Ausfüllen des Fragebogens!")),
  
  # Action button
  actionButton(class="submit", "submit", "Submit"),
  
  downloadButton(class="download",outputId = "report", label = "Download PDF")

  
  
)


# Define server
server <- function(input, output, session) {
  # Create database connection
  con <- dbConnect(RSQLite::SQLite(), "test_fb.db")
  
  # Create table
  dbExecute(con, "CREATE TABLE IF NOT EXISTS testtabelle (dstelle TEXT, stellenleit TEXT, mail TEXT, telefon TEXT, comments TEXT, color TEXT, 
              hobbies TEXT, fruits TEXT, age INTEGER, dob DATE)")
  
  # Insert data into table
  insertData <- function(data) {
    query <- "INSERT INTO testtabelle (dstelle, stellenleit, mail, telefon, comments, color, hobbies, fruits, age, dob) 
            VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
    params <- list(data$dstelle, data$stellenleit, data$mail, data$telefon, data$comments, data$color, paste(data$hobbies, collapse = ","),
                   paste(data$fruits, collapse = ","), data$age, data$dob)
    result <- tryCatch(
      {
        dbExecute(con, query, params)
      },
      error = function(e) {
        print(paste0("Error: ", e$message))
      }
    )
    return(result)
  }
  
  
  observeEvent(input$submit, {
    # Submit the data to the database
    data <- list(dstelle = input$dstelle, stellenleit = input$stellenleit, mail = input$mail, telefon = input$telefon, comments = input$comments, color = input$color,
                 hobbies = input$hobbies, fruits = input$fruits, age = input$age, dob = input$dob)
    insertData(data)
  })
  
  # Disconnect from database when app closes
  onSessionEnded(function() {
    dbDisconnect(con)
  })

  
  # Create a reactive dataframe to store the form answers
#  form_answers <- reactive({
#    data.frame(dstelle = input$dstelle, stellenleit = input$stellenleit, mail = input$mail, telefon = input$telefon, comments = input$comments, color = input$color,
#               hobbies = input$hobbies, fruits = input$fruits, age = input$age, dob = input$dob)
#  })
  
  # Render the form answers as a table
#  output$form_table <- renderTable({
#    form_answers()
#  })
  
  # Download the PDF
  
  output$report = downloadHandler(
    filename = 'myreport.pdf',
    
    content = function(file) {
      out = knit2pdf('input.Rnw', clean = TRUE)
      file.rename(out, file) # move pdf to file for downloading
    },
    
    contentType = 'application/pdf'
  )
  
#  output$form_answers = downloadHandler(
#    filename = "fragebogenformular24.pdf",
#    
#    filename <- function() {
#      paste0(input$name, "_responses.pdf")
#    },
#      content = function(file) {
#      pdf_file <- file.path(tempdir(), "form_answers.Rmd")
#      file.copy("form_answers.Rmd", pdf_file, overwrite = TRUE)
#      # Set up paramters for .Rmd
#      params = list(data = form_answers)
#      
#      rmarkdown::render(pdf_file, 
#                        output_file = file,
#                        params = params,
#                        envir = new.env(parent = globalenv())
        
#      )
   
#    }
#  )
  
}


# Run app
shinyApp(ui, server)
