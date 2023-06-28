library(shiny)

# Define UI
ui <- fluidPage(
  titlePanel("Invoice Generator"),
  sidebarLayout(
    sidebarPanel(
      textInput("work_item", "Work Item"),
      numericInput("quantity", "Quantity", value = 0),
      numericInput("individual_price", "Individual Price", value = 0),
      actionButton("add_item", "Add Item")
    ),
    mainPanel(
      tableOutput("invoice_table"),
      h4("Total Invoice Amount:"),
      verbatimTextOutput("total_invoice")
    )
  )
)

# Define server
server <- function(input, output, session) {  # Add 'session' argument
  # Create reactive values
  values <- reactiveValues(
    items = data.frame(
      Work_Item = character(),
      Quantity = double(),
      Individual_Price = double(),
      Total_Price = double()
    )
  )
  
  # Add item to the invoice
  observeEvent(input$add_item, {
    if (input$work_item != "" && input$quantity > 0 && input$individual_price > 0) {
      values$items <- rbind(values$items, data.frame(
        Work_Item = input$work_item,
        Quantity = input$quantity,
        Individual_Price = input$individual_price,
        Total_Price = input$quantity * input$individual_price
      ))
      
      # Reset input fields
      updateTextInput(session, "work_item", value = "")
      updateNumericInput(session, "quantity", value = 0)
      updateNumericInput(session, "individual_price", value = 0)
    }
  })
  
  # Render the invoice table
  output$invoice_table <- renderTable({
    printed_df = values$items
    colnames(printed_df) <- c('Work Item','Quantity','Individual Price','Total')
    printed_df
  })
  
  # Calculate and render the total invoice amount
  output$total_invoice <- renderPrint({
    total <- sum(values$items$Total_Price)
    currency <- "$"  # Change currency symbol if necessary
    paste0(currency, total)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
