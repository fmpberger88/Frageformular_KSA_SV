library(shiny)
library(shinydashboard)
library(htmltools)
library(RSQLite)
library(rmarkdown)

# Define UI
ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", href = "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"),
    tags$link(rel = "stylesheet", href = "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap-theme.min.css"),
    tags$script(src = "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js")),
  
  
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
      "h1   {   color: white;
                font-weight: 1000;
                  }",
      "h2   {   color: white;
                  }",
      "p    {   font-size: 20px;
                  }"
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
  dateInput("dob", "What's your date of birth?"),
  
  # Action button
  actionButton("submit", "Submit"),

  # Display the thank you message
    conditionalPanel(condition = "input.submit < 0",
                     h2("Thank you for your submission!")),
    downloadButton(outputId = "downloadPDF", label = "Download PDF")
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
  
  # Define a reactive variable to track form submission
  form_submitted <- reactiveVal(FALSE)
  
  # Create an observer to update form_submitted when the form is submitted
  observeEvent(input$submit, {
    form_submitted(TRUE)
  })
  
  # Create a reactive dataframe to store the form answers
  form_answers <- reactive({
    data.frame(dstelle = input$dstelle, stellenleit = input$stellenleit, mail = input$mail, telefon = input$telefon, comments = input$comments, color = input$color,
               hobbies = input$hobbies, fruits = input$fruits, age = input$age, dob = input$dob)
  })
  
  # Render the form answers as a table
  output$form_table <- renderTable({
    form_answers()
  })
  
  # Download the PDF
  output$downloadPDF <- downloadHandler(
    filename = function() {
      paste0(input$name, "_responses.pdf")
    },
    content = function(file) {
      pdf_file <- file.path(tempdir())
      rmarkdown::render(
        input = "form_answers.Rmd",
        output_format = "pdf_document",
        output_file = pdf_file,
        params = list(data = formData())
      )
      file.copy(pdf_file, file)
    }
  )
  
}


# Run app
shinyApp(ui, server)
