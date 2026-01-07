# Load required packages
library(DBI)
library(RSQLite)
library(shiny)
library(shinyWidgets)
library(DT)

# Create / connect to SQLite database
con <- dbConnect(SQLite(), "pantry.db")

# Create tables
dbExecute(con, "
  CREATE TABLE IF NOT EXISTS pantry (
    ingredient TEXT PRIMARY KEY,
    quantity REAL,
    unit TEXT
  )
")

dbExecute(con, "
  CREATE TABLE IF NOT EXISTS recipes (
    name TEXT PRIMARY KEY,
    ingredients TEXT,
    servings INTEGER
  )
")

# Colors (warm brown tones)
colors <- list(
  header = "#8B5E3C",
  sidebar = "#A9746E",
  panel = "#F5E1D4",
  button = "#D2691E",
  accent = "#C19A6B",
  text = "#4B2E2B"
)

# Ingredient master list with units
ingredient_units <- data.frame(
  Ingredient = c("Chicken", "Soy Sauce", "Vinegar", "Garlic", "Bay Leaf",
                 "Peppercorns", "Pork", "Tamarind Paste", "Tomato", "Radish",
                 "Water Spinach", "Onion", "Canton Noodles", "Shrimp",
                 "Carrot", "Cabbage", "Pork Belly", "Salt", "Pepper", "Oil",
                 "Peanut Butter", "Banana Blossom", "Eggplant", "String Beans", 
                 "Annatto", "Oxtail"),
  Unit = c("grams", "ml", "ml", "cloves", "pieces", "tsp", "grams", "grams", 
           "grams", "grams", "grams", "pieces", "grams", "grams", "grams", 
           "grams", "grams", "tsp", "tsp", "ml", "grams", "grams", "grams", 
           "grams", "tsp", "grams"),
  stringsAsFactors = FALSE
)

# Filipino recipes (updated format)
recipes <- list(
  
  "Adobo" = list(
    ingredients = list(
      list(name="Chicken", qty=1000, unit="grams"),
      list(name="Soy Sauce", qty=120, unit="ml"),
      list(name="Vinegar", qty=100, unit="ml"),
      list(name="Garlic", qty=5, unit="cloves"),
      list(name="Bay Leaf", qty=2, unit="pieces"),
      list(name="Peppercorns", qty=1, unit="tsp")
    ),
    servings = 4
  ),
  
  "Sinigang na Baboy" = list(
    ingredients = list(
      list(name="Pork", qty=800, unit="grams"),
      list(name="Tamarind Paste", qty=50, unit="grams"),
      list(name="Tomato", qty=150, unit="grams"),
      list(name="Radish", qty=100, unit="grams"),
      list(name="Water Spinach", qty=50, unit="grams"),
      list(name="Onion", qty=1, unit="pieces")
    ),
    servings = 4
  ),
  
  "Pancit Canton" = list(
    ingredients = list(
      list(name="Canton Noodles", qty=200, unit="grams"),
      list(name="Shrimp", qty=150, unit="grams"),
      list(name="Chicken", qty=150, unit="grams"),
      list(name="Carrot", qty=50, unit="grams"),
      list(name="Cabbage", qty=50, unit="grams"),
      list(name="Soy Sauce", qty=30, unit="ml")
    ),
    servings = 3
  ),
  
  "Lechon Kawali" = list(
    ingredients = list(
      list(name="Pork Belly", qty=1000, unit="grams"),
      list(name="Salt", qty=1.5, unit="tsp"),
      list(name="Pepper", qty=1, unit="tsp"),
      list(name="Oil", qty=250, unit="ml")
    ),
    servings = 4
  ),
  
  "Kare-Kare" = list(
    ingredients = list(
      list(name="Oxtail", qty=800, unit="grams"),
      list(name="Peanut Butter", qty=100, unit="grams"),
      list(name="Banana Blossom", qty=150, unit="grams"),
      list(name="Eggplant", qty=100, unit="grams"),
      list(name="String Beans", qty=100, unit="grams"),
      list(name="Annatto", qty=1, unit="tsp")
    ),
    servings = 4
  ),
  
  "Tinolang Manok" = list(
    ingredients = list(
      list(name="Chicken", qty=1000, unit="grams"),
      list(name="Garlic", qty=4, unit="cloves"),
      list(name="Onion", qty=1, unit="pieces"),
      list(name="Oil", qty=15, unit="ml"),
      list(name="Salt", qty=1, unit="tsp"),
      list(name="Pepper", qty=0.5, unit="tsp")
    ),
    servings = 4
  ),
  
  "Chicken Afritada" = list(
    ingredients = list(
      list(name="Chicken", qty=800, unit="grams"),
      list(name="Tomato", qty=200, unit="grams"),
      list(name="Garlic", qty=4, unit="cloves"),
      list(name="Onion", qty=1, unit="pieces"),
      list(name="Carrot", qty=100, unit="grams"),
      list(name="Oil", qty=20, unit="ml"),
      list(name="Salt", qty=1, unit="tsp")
    ),
    servings = 4
  ),
  
  "Menudo" = list(
    ingredients = list(
      list(name="Pork", qty=700, unit="grams"),
      list(name="Tomato", qty=200, unit="grams"),
      list(name="Garlic", qty=4, unit="cloves"),
      list(name="Onion", qty=1, unit="pieces"),
      list(name="Carrot", qty=100, unit="grams"),
      list(name="Oil", qty=20, unit="ml"),
      list(name="Salt", qty=1, unit="tsp")
    ),
    servings = 4
  ),
  
  "Bicol Express" = list(
    ingredients = list(
      list(name="Pork Belly", qty=600, unit="grams"),
      list(name="Garlic", qty=4, unit="cloves"),
      list(name="Onion", qty=1, unit="pieces"),
      list(name="Oil", qty=15, unit="ml"),
      list(name="Salt", qty=1, unit="tsp"),
      list(name="Pepper", qty=0.5, unit="tsp")
    ),
    servings = 4
  ),
  
  "Laing" = list(
    ingredients = list(
      list(name="Taro Leaves", qty=50, unit="grams"),
      list(name="Garlic", qty=3, unit="cloves"),
      list(name="Onion", qty=1, unit="pieces"),
      list(name="Oil", qty=15, unit="ml"),
      list(name="Salt", qty=1, unit="tsp")
    ),
    servings = 3
  ),
  
  "Ginisang Monggo" = list(
    ingredients = list(
      list(name="Mung Beans", qty=200, unit="grams"),
      list(name="Garlic", qty=3, unit="cloves"),
      list(name="Onion", qty=1, unit="pieces"),
      list(name="Oil", qty=15, unit="ml"),
      list(name="Salt", qty=1, unit="tsp")
    ),
    servings = 4
  ),
  
  "Chicken Curry (Filipino Style)" = list(
    ingredients = list(
      list(name="Chicken", qty=900, unit="grams"),
      list(name="Garlic", qty=4, unit="cloves"),
      list(name="Onion", qty=1, unit="pieces"),
      list(name="Oil", qty=20, unit="ml"),
      list(name="Salt", qty=1, unit="tsp"),
      list(name="Pepper", qty=0.5, unit="tsp")
    ),
    servings = 4
  )
  
)



# Initialize pantry with default stock if empty
check_pantry <- dbGetQuery(con, "SELECT COUNT(*) as count FROM pantry")
if(check_pantry$count == 0) {
  initial_stock <- data.frame(
    ingredient = c("Chicken", "Soy Sauce", "Vinegar", "Garlic", "Bay Leaf",
                   "Peppercorns", "Pork", "Tamarind Paste", "Tomato", "Radish",
                   "Water Spinach", "Onion", "Canton Noodles", "Shrimp",
                   "Carrot", "Cabbage", "Pork Belly", "Salt", "Pepper", "Oil",
                   "Peanut Butter", "Banana Blossom", "Eggplant", "String Beans", 
                   "Annatto", "Oxtail"),
    quantity = c(1000, 200, 150, 10, 5, 5, 500, 50, 100, 80, 50, 2, 200, 100, 
                 80, 80, 500, 5, 5, 200, 0, 0, 0, 0, 0, 0),
    unit = c("grams", "ml", "ml", "cloves", "pieces", "tsp", "grams", "grams", 
             "grams", "grams", "grams", "pieces", "grams", "grams", "grams", 
             "grams", "grams", "tsp", "tsp", "ml", "grams", "grams", "grams", 
             "grams", "tsp", "grams"),
    stringsAsFactors = FALSE
  )
  dbWriteTable(con, "pantry", initial_stock, append = TRUE)
}

# UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML(sprintf("
      body {background-color: %s; color: %s;}
      .shiny-input-container {background-color: %s; padding: 10px; border-radius: 5px;}
      .panel-title, h4 {color: %s; font-weight: bold;}
      .btn {background-color: %s; color: white; border: none;}
      .btn:hover {background-color: %s;}
      .well {background-color: %s;}
    ", colors$panel, colors$text, colors$panel, colors$header, colors$button, 
                            colors$accent, colors$panel)))
  ),
  
  titlePanel(tags$span(style=sprintf("color:%s;", colors$header), 
                       "Smart Recipe Checker")),
  
  fluidRow(
    column(width = 4,
           wellPanel(
             h4("Check Recipe"),
             pickerInput("recipe_input", "Select a Recipe:",
                         choices = names(recipes),
                         options = list(`live-search`=TRUE)),
             actionButton("check_btn", "Can I Cook This?", icon = icon("search")),
             hr(),
             h4("Shopping List"),
             p("Missing ingredients for selected recipe:"),
             uiOutput("shopping_list_ui"),
             hr(),
             h4("Add to Stock"),
             radioButtons("ingredient_mode", NULL, 
                          choices = c("Select from list" = "select", 
                                      "Type my own" = "custom"),
                          selected = "select", inline = TRUE),
             conditionalPanel(
               condition = "input.ingredient_mode == 'select'",
               selectInput("add_ingredient", "Ingredient:", 
                           choices = ingredient_units$Ingredient)
             ),
             conditionalPanel(
               condition = "input.ingredient_mode == 'custom'",
               textInput("custom_ingredient", "Ingredient Name:", 
                         placeholder = "e.g., Fish Sauce"),
               selectInput("custom_unit", "Unit:", 
                           choices = c("grams", "ml", "pieces", "tsp", "tbsp", "cups", "kg", "liters"))
             ),
             fluidRow(
               column(6, numericInput("add_quantity", "Amount:", value = 100, min = 0)),
               column(6, uiOutput("unit_display"))
             ),
             actionButton("add_to_stock_btn", "Add to My Pantry", icon = icon("plus"), 
                          style = "width: 100%;")
           )
    ),
    column(width = 8,
           wellPanel(
             h4("Recipe Details"),
             verbatimTextOutput("recipe_details"),
             br(),
             uiOutput("cook_button_ui"),
             hr(),
             h4("Alternative Recipes You Can Cook"),
             verbatimTextOutput("alt_recipes"),
             hr(),
             h4("My Pantry Stock"),
             DT::dataTableOutput("stock_table"),
             br(),
             h4("Empty Items"),
             p(style="color: #666;", "Items with 0 quantity"),
             DT::dataTableOutput("empty_stock_table")
           )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Stock data as reactiveVal instead of reactive
  stock_data <- reactiveVal(NULL)
  
  reload_trigger <- reactiveVal(0)
  
  # Load stock from database
  load_stock <- function(){
    data <- dbGetQuery(con, "SELECT ingredient as Ingredient, quantity as Quantity, unit as Unit FROM pantry")
    stock_data(data)
    return(data)
  }
  
  # Initialize stock on startup
  observe({
    load_stock()
  })
  
  selected_recipe <- reactiveVal(NULL)
  missing_ingredients <- reactiveVal(NULL)
  
  # Render cook button only when can cook
  output$cook_button_ui <- renderUI({
    req(selected_recipe())
    missing <- missing_ingredients()
    
    if(is.null(missing) || length(missing) == 0) {
      actionButton("cook_btn", "Cook This Recipe!", icon = icon("fire"), 
                   style = sprintf("background-color: %s; color: white; font-size: 16px; padding: 10px 20px; width: 100%%;", colors$accent))
    } else {
      NULL
    }
  })
  
  # Display unit for selected ingredient
  output$unit_display <- renderUI({
    if(input$ingredient_mode == "select") {
      req(input$add_ingredient)
      unit <- ingredient_units$Unit[ingredient_units$Ingredient == input$add_ingredient]
      tags$p(style="margin-top: 25px; font-weight: bold;", unit)
    } else {
      req(input$custom_unit)
      tags$p(style="margin-top: 25px; font-weight: bold;", input$custom_unit)
    }
  })
  
  # Render stock table - only items with quantity > 0
  output$stock_table <- DT::renderDataTable({
    req(stock_data())
    current_stock <- stock_data()
    active_stock <- current_stock[current_stock$Quantity > 0, ]
    
    cat("Rendering stock table. Active items:", nrow(active_stock), "\n")
    
    DT::datatable(
      active_stock,
      editable = list(target = 'cell', disable = list(columns = c(0, 2))),
      rownames = FALSE,
      options = list(
        dom = 't',
        scrollX = TRUE,
        scrollY = "300px",
        paging = FALSE,
        ordering = TRUE
      )
    )
  })
  
  # Render empty stock table - items with quantity = 0
  output$empty_stock_table <- DT::renderDataTable({
    req(stock_data())
    current_stock <- stock_data()
    empty_stock <- current_stock[current_stock$Quantity == 0, ]
    
    cat("Rendering empty table. Empty items:", nrow(empty_stock), "\n")
    
    DT::datatable(
      empty_stock,
      rownames = FALSE,
      options = list(
        dom = 't',
        scrollX = TRUE,
        scrollY = "150px",
        paging = FALSE,
        ordering = TRUE
      )
    )
  })
  
  # Allow editing quantity only
  observeEvent(input$stock_table_cell_edit, {
    info <- input$stock_table_cell_edit
    if(info$col == 1) {
      current_stock <- stock_data()
      active_stock <- current_stock[current_stock$Quantity > 0, ]
      actual_ingredient <- active_stock[info$row + 1, "Ingredient"]
      new_quantity <- as.numeric(info$value)
      
      dbExecute(con, "UPDATE pantry SET quantity = ? WHERE ingredient = ?", 
                params = list(new_quantity, actual_ingredient))
      load_stock()  # Reload
    }
  })
  
  # Add to stock button
  observeEvent(input$add_to_stock_btn, {
    req(input$add_quantity)
    
    # Get ingredient name and unit based on mode
    if(input$ingredient_mode == "select") {
      req(input$add_ingredient)
      ingredient <- input$add_ingredient
      unit <- ingredient_units$Unit[ingredient_units$Ingredient == ingredient]
    } else {
      req(input$custom_ingredient, input$custom_unit)
      ingredient <- input$custom_ingredient
      unit <- input$custom_unit
      
      if(trimws(ingredient) == "") {
        showNotification("Please enter an ingredient name", type = "error")
        return()
      }
    }
    
    quantity <- input$add_quantity
    
    # Check if ingredient exists
    existing <- dbGetQuery(con, "SELECT quantity FROM pantry WHERE ingredient = ?", 
                           params = list(ingredient))
    
    if(nrow(existing) > 0) {
      # Update existing
      new_qty <- existing$quantity + quantity
      dbExecute(con, "UPDATE pantry SET quantity = ? WHERE ingredient = ?", 
                params = list(new_qty, ingredient))
    } else {
      # Insert new
      dbExecute(con, "INSERT INTO pantry (ingredient, quantity, unit) VALUES (?, ?, ?)", 
                params = list(ingredient, quantity, unit))
    }
    
    showNotification(paste("Added", quantity, unit, "of", ingredient), type = "message")
    reload_trigger(reload_trigger() + 1)  # Trigger reload
    
    if(input$ingredient_mode == "custom") {
      updateTextInput(session, "custom_ingredient", value = "")
    }
  })
  
  # Check recipe
  observeEvent(input$check_btn, {
    req(input$recipe_input)
    recipe <- recipes[[input$recipe_input]]
    selected_recipe(recipe)
    
    current_stock <- stock_data()
    missing <- lapply(recipe$ingredients, function(ing) {
      qty_in_stock <- current_stock$Quantity[current_stock$Ingredient == ing$name]
      if(length(qty_in_stock) == 0 || qty_in_stock < ing$qty) {
        return(ing)
      }
      NULL
    })
    missing <- Filter(Negate(is.null), missing)
    missing_ingredients(missing)
  })
  
  # Recipe details
  output$recipe_details <- renderText({
    req(selected_recipe())
    recipe <- selected_recipe()
    ingredients_text <- sapply(recipe$ingredients, function(ing) {
      paste0(ing$name, ": ", ing$qty, " ", ing$unit)
    })
    paste0("Servings: ", recipe$servings, "\n\nIngredients Needed:\n",
           paste(ingredients_text, collapse="\n"))
  })
  
  # Cook button - deducts ingredients from stock
  observeEvent(input$cook_btn, {
    req(selected_recipe())
    recipe <- selected_recipe()
    
    for(ing in recipe$ingredients) {
      current_qty <- dbGetQuery(con, "SELECT quantity FROM pantry WHERE ingredient = ?", 
                                params = list(ing$name))
      
      if(nrow(current_qty) > 0) {
        new_qty <- max(0, current_qty$quantity - ing$qty)
        dbExecute(con, "UPDATE pantry SET quantity = ? WHERE ingredient = ?", 
                  params = list(new_qty, ing$name))
      }
    }
    
    showNotification(paste("Cooked", input$recipe_input, "successfully! Ingredients deducted from pantry."), 
                     type = "message", duration = 5)
    
    load_stock()  # Reload
    
    current_stock <- stock_data()
    missing <- lapply(recipe$ingredients, function(ing) {
      qty_in_stock <- current_stock$Quantity[current_stock$Ingredient == ing$name]
      if(length(qty_in_stock) == 0 || qty_in_stock < ing$qty) {
        return(ing)
      }
      NULL
    })
    missing <- Filter(Negate(is.null), missing)
    missing_ingredients(missing)
  })
  
  # Alternative recipes
  output$alt_recipes <- renderText({
    req(stock_data())
    current_stock <- stock_data()
    alternatives <- names(recipes)[sapply(names(recipes), function(r) {
      all(sapply(recipes[[r]]$ingredients, function(ing) {
        qty_in_stock <- current_stock$Quantity[current_stock$Ingredient == ing$name]
        if(length(qty_in_stock) == 0) return(FALSE)
        qty_in_stock >= ing$qty
      }))
    })]
    alternatives <- setdiff(alternatives, input$recipe_input)
    if(length(alternatives) == 0) return("None available with current stock")
    paste(alternatives, collapse=", ")
  })
  
  # Shopping list with checkboxes and custom quantity inputs
  output$shopping_list_ui <- renderUI({
    missing <- missing_ingredients()
    
    if (is.null(missing) || length(missing) == 0) {
      return(tags$p(
        style = "color: green; font-weight: bold;",
        "✓ You have all ingredients!"
      ))
    }
    
    shopping_items <- lapply(seq_along(missing), function(i) {
      ing <- missing[[i]]
      
      fluidRow(
        style = "margin-bottom: 5px;",
        
        column(
          width = 6,
          checkboxInput(
            inputId = paste0("shop_", ing$name),
            label = paste0(ing$name, " (", ing$qty, " ", ing$unit, ")"),
            value = FALSE
          )
        ),
        
        column(
          width = 6,
          numericInput(
            inputId = paste0("shop_qty_", ing$name),
            label = NULL,
            value = ing$qty,
            min = 0,
            step = 10
          )
        )
      )
    })
    
    tagList(
      shopping_items,
      actionButton(
        "add_checked_btn",
        "Add Selected to Pantry",
        icon = icon("shopping-cart"),
        style = "width: 100%; margin-top: 10px;"
      )
    )
  })
  
  # Add checked shopping items to stock with custom quantities
  observeEvent(input$add_checked_btn, {
    missing <- missing_ingredients()
    if(is.null(missing) || length(missing) == 0) {
      showNotification("No missing ingredients", type = "warning")
      return()
    }
    
    added <- 0
    added_items <- c()
    
    for(ing in missing) {
      # Check if checkbox is checked
      checkbox_id <- paste0("shop_", ing$name)
      checkbox_value <- input[[checkbox_id]]
      
      cat("Checking:", ing$name, "Checkbox value:", checkbox_value, "\n")  # Debug
      
      if(!is.null(checkbox_value) && isTRUE(checkbox_value)) {
        # Get the custom quantity entered by user
        qty_id <- paste0("shop_qty_", ing$name)
        custom_qty <- input[[qty_id]]
        
        cat("Custom qty for", ing$name, ":", custom_qty, "\n")  # Debug
        
        if(is.null(custom_qty) || custom_qty <= 0) {
          showNotification(paste("Invalid quantity for", ing$name), type = "warning")
          next
        }
        
        # Check if exists in database
        existing <- dbGetQuery(con, "SELECT quantity FROM pantry WHERE ingredient = ?", 
                               params = list(ing$name))
        
        if(nrow(existing) > 0) {
          # Update existing
          new_qty <- existing$quantity + custom_qty
          dbExecute(con, "UPDATE pantry SET quantity = ? WHERE ingredient = ?", 
                    params = list(new_qty, ing$name))
          cat("Updated", ing$name, "to", new_qty, "\n")  # Debug
        } else {
          # Insert new
          dbExecute(con, "INSERT INTO pantry (ingredient, quantity, unit) VALUES (?, ?, ?)", 
                    params = list(ing$name, custom_qty, ing$unit))
          cat("Inserted", ing$name, "with", custom_qty, "\n")  # Debug
        }
        added <- added + 1
        added_items <- c(added_items, ing$name)
      }
    }
    
    cat("Total added:", added, "\n")  # Debug
    
    if(added > 0) {
      showNotification(paste("Added", added, "items:", paste(added_items, collapse=", ")), 
                       type = "message", duration = 5)
      
      # Force reload by incrementing trigger
      new_val <- reload_trigger() + 1
      cat("Setting reload_trigger to:", new_val, "\n")  # Debug
      reload_trigger(new_val)
      
      # Small delay to ensure database is updated
      Sys.sleep(0.1)
      
      # Recheck recipe to update shopping list and cook button
      recipe <- selected_recipe()
      if(!is.null(recipe)) {
        current_stock <- load_stock()
        missing_new <- lapply(recipe$ingredients, function(ing) {
          qty_in_stock <- current_stock$Quantity[current_stock$Ingredient == ing$name]
          if(length(qty_in_stock) == 0 || qty_in_stock < ing$qty) {
            return(ing)
          }
          NULL
        })
        missing_new <- Filter(Negate(is.null), missing_new)
        missing_ingredients(missing_new)
        cat("Updated missing ingredients. New count:", length(missing_new), "\n")  # Debug
      }
    } else {
      showNotification("No items selected. Please check the boxes for items you want to add.", 
                       type = "warning")
    }
  })
  
  # Disconnect database on session end
  session$onSessionEnded(function() {
    dbDisconnect(con)
  })
}

# Run app
shinyApp(ui, server)