# Load required packages
library(DBI)
library(RSQLite)
library(shiny)
library(shinyWidgets)
library(DT)
library(httr)
library(jsonlite)

# Colors (warm brown tones)
colors <- list(
  header = "#8B5E3C",
  header2 = "#FFFFFF",
  sidebar = "#A9746E",
  panel = "#F5E1D4",
  button = "#D2691E",
  accent = "#C19A6B",
  text = "#4B2E2B"
)

# Hardcoded credentials
VALID_USERNAME <- "demo"
VALID_PASSWORD <- "demo123"

# Ingredient master list with units
ingredient_units <- data.frame(
  Ingredient = c("Chicken", "Soy Sauce", "Vinegar", "Garlic", "Bay Leaf",
                 "Peppercorns", "Pork", "Tamarind Paste", "Tomato", "Radish",
                 "Water Spinach", "Onion", "Noodles", "Shrimp",
                 "Carrot", "Cabbage", "Pork Belly", "Salt", "Pepper", "Oil",
                 "Peanut Butter", "Banana Blossom", "Eggplant", "String Beans", 
                 "Annatto", "Oxtail", "Taro Leaves", "Mung Beans"),
  Unit = c("grams", "ml", "ml", "cloves", "pieces", "tsp", "grams", "grams", 
           "grams", "grams", "grams", "pieces", "grams", "grams", "grams", 
           "grams", "grams", "tsp", "tsp", "ml", "grams", "grams", "grams", 
           "grams", "tsp", "grams", "grams", "grams"),
  stringsAsFactors = FALSE
)

# Function to get all recipes from TheMealDB
get_all_recipes <- function() {
  tryCatch({
    all_recipes <- character(0)
    letters_to_check <- c("a", "b", "c", "d", "e", "f", "g", "h", "l", "m", "p", "r", "s", "t")
    
    for (letter in letters_to_check) {
      url <- paste0("https://www.themealdb.com/api/json/v1/1/search.php?f=", letter)
      res <- GET(url, timeout(10))
      
      if (status_code(res) == 200) {
        data <- fromJSON(content(res, "text", encoding = "UTF-8"))
        if (!is.null(data$meals)) {
          all_recipes <- c(all_recipes, data$meals$strMeal)
        }
      }
      Sys.sleep(0.1)
    }
    
    all_recipes <- unique(all_recipes)
    all_recipes <- sort(all_recipes)
    return(all_recipes)
  }, error = function(e) {
    return(c("Chicken Adobo", "Beef Curry", "Pad Thai", "Spaghetti Bolognese"))
  })
}

# TheMealDB API functions
search_recipe <- function(query) {
  tryCatch({
    url <- paste0("https://www.themealdb.com/api/json/v1/1/search.php?s=", URLencode(query))
    res <- GET(url, timeout(10))
    
    if (status_code(res) != 200) return(NULL)
    
    data <- fromJSON(content(res, "text", encoding = "UTF-8"))
    if (is.null(data$meals) || length(data$meals) == 0) return(NULL)
    
    return(data$meals[1, ])
  }, error = function(e) {
    return(NULL)
  })
}

# Extract ingredients from TheMealDB recipe
extract_ingredients <- function(meal) {
  if (is.null(meal)) return(list())
  
  ingredients <- list()
  
  for (i in 1:20) {
    ing_col <- paste0("strIngredient", i)
    measure_col <- paste0("strMeasure", i)
    
    ingredient <- meal[[ing_col]]
    measure <- meal[[measure_col]]
    
    if (is.null(ingredient) || is.na(ingredient) || ingredient == "" || trimws(ingredient) == "") {
      break
    }
    
    measure <- trimws(measure)
    if (is.na(measure) || measure == "") {
      measure <- "to taste"
    }
    
    ingredients[[i]] <- list(
      name = ingredient,
      measure = measure
    )
  }
  
  return(ingredients)
}

# Parse measure string to extract numeric quantity
parse_measure <- function(measure) {
  if (is.null(measure) || is.na(measure) || measure == "" || measure == "to taste") {
    return(0)
  }
  
  # Convert to lowercase for easier parsing
  measure <- tolower(trimws(measure))
  
  # Try to extract numbers (including decimals and fractions)
  # Pattern matches: "200g", "1 cup", "2.5 tbsp", "1/2 cup", etc.
  
  # Handle fractions like "1/2", "1/4"
  if (grepl("^\\d+/\\d+", measure)) {
    parts <- strsplit(measure, "/")[[1]]
    if (length(parts) == 2) {
      numerator <- as.numeric(parts[1])
      denominator <- as.numeric(parts[2])
      if (!is.na(numerator) && !is.na(denominator) && denominator != 0) {
        # Convert to grams/ml estimate (e.g., 1/2 cup ≈ 120ml)
        base_amount <- (numerator / denominator) * 240  # 240ml per cup
        return(base_amount)
      }
    }
  }
  
  # Extract first number in the string
  num_match <- regmatches(measure, regexpr("\\d+\\.?\\d*", measure))
  
  if (length(num_match) > 0) {
    quantity <- as.numeric(num_match[1])
    
    if (!is.na(quantity)) {
      # Convert common measurements to grams/ml
      if (grepl("cup", measure)) {
        return(quantity * 240)  # 1 cup ≈ 240ml
      } else if (grepl("tbsp|tablespoon", measure)) {
        return(quantity * 15)   # 1 tbsp ≈ 15ml
      } else if (grepl("tsp|teaspoon", measure)) {
        return(quantity * 5)    # 1 tsp ≈ 5ml
      } else if (grepl("oz|ounce", measure)) {
        return(quantity * 28)   # 1 oz ≈ 28g
      } else if (grepl("lb|pound", measure)) {
        return(quantity * 454)  # 1 lb ≈ 454g
      } else if (grepl("kg|kilogram", measure)) {
        return(quantity * 1000) # 1 kg = 1000g
      } else if (grepl("ml|milliliter", measure)) {
        return(quantity)
      } else if (grepl("l|liter", measure) && !grepl("ml", measure)) {
        return(quantity * 1000) # 1 liter = 1000ml
      } else if (grepl("g|gram", measure) && !grepl("kg", measure)) {
        return(quantity)
      } else {
        # If no unit specified, assume it's already in grams/ml
        return(quantity)
      }
    }
  }
  
  # If we couldn't parse it, return 0 (will use default deduction)
  return(0)
}

# Helper function to sanitize input IDs
safe_id <- function(x) {
  gsub("[^a-zA-Z0-9]", "_", x)
}

# Login UI
login_ui <- fluidPage(
  tags$head(
    tags$title("🍳 Recipe Checker")  # <-- ADD THIS NEW LINE HERE
  ),
  tags$head(
    tags$style(HTML(sprintf("
      body {
        background: linear-gradient(135deg, %s 0%%, %s 100%%);
        min-height: 100vh;
        margin: 0;
        display: flex;
        justify-content: center;
        align-items: center;
        padding: 20px;
        box-sizing: border-box;
      }
      .login-container {
        background-color: %s;
        padding: 40px;
        border-radius: 15px;
        box-shadow: 0 10px 25px rgba(0,0,0,0.3);
        width: 100%%;
        max-width: 350px;
        margin: auto;
      }
      .login-title {
        color: %s;
        text-align: center;
        font-size: 28px;
        font-weight: bold;
        margin-bottom: 30px;
      }
      .login-btn {
        width: 100%%;
        padding: 12px;
        background-color: %s;
        color: white;
        border: none;
        border-radius: 8px;
        font-size: 16px;
        font-weight: bold;
        cursor: pointer;
        transition: background-color 0.3s;
      }
      .login-btn:hover {
        background-color: %s;
      }
      .error-message {
        color: #d32f2f;
        text-align: center;
        margin-top: 15px;
        font-size: 14px;
      }
    ", colors$sidebar, colors$accent, colors$panel, colors$header, 
                            colors$button, colors$header)))
  ),
  
  div(class = "login-container",
      h1("🍳", style = "font-size: 60px; text-align: center; margin: 0;"),
      p(""),
      div(class = "login-title", "Recipe Checker"),
      textInput("username", NULL, placeholder = "Username", width = "100%"),
      passwordInput("password", NULL, placeholder = "Password", width = "100%"),
      actionButton("login_btn", "Login", class = "login-btn"),
      uiOutput("login_error")
  )
)

# Main UI
main_ui <- fluidPage(
  tags$head(
    tags$title("🍳 Recipe Checker")  # <-- ADD THIS NEW LINE HERE
  ),
  tags$head(
    tags$style(HTML(sprintf("
      body {background-color: %s; color: %s; padding-top: 50px;}
      .shiny-input-container {background-color: %s; padding: 10px; border-radius: 5px;}
      .panel-title, h4 {color: %s; font-weight: bold;}
      .btn {background-color: %s; color: white; border: none;}
      .btn:hover {background-color: %s;}
      .well {background-color: %s;}
      .logout-btn {
        position: fixed;
        top: 10px;
        right: 10px;
        background-color: %s;
        color: white;
        padding: 8px 15px;
        border: none;
        border-radius: 5px;
        cursor: pointer;
        z-index: 1000;
      }
      .logout-btn:hover {
        background-color: %s;
      }
      .recipe-box {
        background-color: rgba(139, 94, 60, 0.1);
        padding: 15px;
        border-radius: 8px;
        margin-top: 10px;
        border-left: 4px solid %s;
      }
      .recipe-image {
        width: 100%%;
        max-width: 300px;
        border-radius: 8px;
        margin: 10px 0;
        box-shadow: 0 4px 6px rgba(0,0,0,0.1);
      }
      .instructions-box {
        background-color: white;
        padding: 15px;
        border-radius: 8px;
        margin-top: 10px;
        max-height: 400px;
        overflow-y: auto;
        line-height: 1.6;
      }
    ", colors$panel, colors$text, colors$panel, colors$header, colors$button, 
                            colors$accent, colors$panel, colors$sidebar, colors$accent, colors$accent)))
  ),
  
  actionButton("logout_btn", "Logout", class = "logout-btn", icon = icon("sign-out-alt")),
  
  titlePanel(tags$span(style=sprintf("color:%s;", colors$header2), 
                       "Recipe Checker 🍳")),
  
  fluidRow(
    column(width = 4,
           wellPanel(
             h4("Select Recipe"),
             pickerInput("recipe_input", "Choose a Recipe:",
                         choices = c("Loading recipes..." = ""),
                         options = list(`live-search`=TRUE, `actions-box`=TRUE)),
             actionButton("refresh_recipes_btn", "Refresh Recipe List", 
                          icon = icon("refresh"), style = "width: 100%; margin-bottom: 10px;"),
             hr(),
             h4("Recipe Ingredients"),
             p("Ingredients needed for this recipe:"),
             uiOutput("ingredients_list_ui"),
             hr(),
             h4("Shopping List"),
             p("Check what you're missing:"),
             actionButton("check_pantry_btn", "Check My Pantry", 
                          icon = icon("check-circle"), 
                          style = "width: 100%; margin-bottom: 10px;"),
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
             uiOutput("recipe_details_ui"),
             hr(),
             h4("How to Cook This Recipe"),
             uiOutput("cooking_instructions"),
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

# UI wrapper
ui <- uiOutput("page")

# Server
server <- function(input, output, session) {
  
  logged_in <- reactiveVal(FALSE)
  
  output$page <- renderUI({
    if (logged_in()) {
      main_ui
    } else {
      login_ui
    }
  })
  
  observeEvent(input$login_btn, {
    if (input$username == VALID_USERNAME && input$password == VALID_PASSWORD) {
      logged_in(TRUE)
    } else {
      output$login_error <- renderUI({
        div(class = "error-message", "Invalid username or password")
      })
    }
  })
  
  observeEvent(input$logout_btn, {
    logged_in(FALSE)
    output$login_error <- renderUI(NULL)
  })
  
  con <- reactiveVal(NULL)
  
  observe({
    if (logged_in() && is.null(con())) {
      db_con <- dbConnect(SQLite(), "pantry.db")
      
      dbExecute(db_con, "
        CREATE TABLE IF NOT EXISTS pantry (
          ingredient TEXT PRIMARY KEY,
          quantity REAL,
          unit TEXT
        )
      ")
      
      check_pantry <- dbGetQuery(db_con, "SELECT COUNT(*) as count FROM pantry")
      if(check_pantry$count == 0) {
        initial_stock <- data.frame(
          ingredient = ingredient_units$Ingredient,
          quantity = c(1000, 200, 150, 10, 5, 5, 500, 50, 100, 80, 50, 2, 200, 100, 
                       80, 80, 500, 5, 5, 200, 0, 0, 0, 0, 0, 0, 0, 0),
          unit = ingredient_units$Unit,
          stringsAsFactors = FALSE
        )
        dbWriteTable(db_con, "pantry", initial_stock, append = TRUE)
      }
      
      con(db_con)
    }
  })
  
  session$onSessionEnded(function() {
    if (!is.null(con())) {
      dbDisconnect(con())
    }
  })
  
  stock_data <- reactiveVal(NULL)
  available_recipes <- reactiveVal(NULL)
  
  load_stock <- function(){
    req(con())
    data <- dbGetQuery(con(), "SELECT ingredient as Ingredient, quantity as Quantity, unit as Unit FROM pantry")
    stock_data(data)
    return(data)
  }
  
  observe({
    if (logged_in() && !is.null(con())) {
      load_stock()
    }
  })
  
  # Load recipes from TheMealDB when app starts
  observe({
    if (logged_in() && is.null(available_recipes())) {
      withProgress(message = 'Loading recipes from TheMealDB...', value = 0, {
        recipes_list <- get_all_recipes()
        available_recipes(recipes_list)
        incProgress(1)
        
        updatePickerInput(session, "recipe_input", 
                          choices = recipes_list,
                          selected = recipes_list[1])
      })
    }
  })
  
  # Refresh recipes button
  observeEvent(input$refresh_recipes_btn, {
    withProgress(message = 'Refreshing recipe list...', value = 0, {
      recipes_list <- get_all_recipes()
      available_recipes(recipes_list)
      incProgress(1)
      
      updatePickerInput(session, "recipe_input", 
                        choices = recipes_list,
                        selected = recipes_list[1])
      showNotification("Recipe list updated!", type = "message")
    })
  })
  
  selected_recipe <- reactiveVal(NULL)
  recipe_data <- reactiveVal(NULL)
  missing_ingredients <- reactiveVal(NULL)
  pantry_checked <- reactiveVal(FALSE)
  
  # Fetch recipe from TheMealDB when recipe is selected
  observeEvent(input$recipe_input, {
    req(logged_in())
    req(input$recipe_input)
    req(input$recipe_input != "")
    
    # Reset pantry check when recipe changes
    pantry_checked(FALSE)
    missing_ingredients(NULL)
    
    withProgress(message = 'Fetching recipe details...', value = 0, {
      meal <- search_recipe(input$recipe_input)
      incProgress(0.5)
      
      if (is.null(meal)) {
        recipe_data(NULL)
        selected_recipe(NULL)
      } else {
        recipe_data(meal)
        
        ingredients <- extract_ingredients(meal)
        selected_recipe(list(
          name = meal$strMeal,
          ingredients = ingredients,
          servings = ifelse(!is.null(meal$strServings) && !is.na(meal$strServings), 
                            meal$strServings, "Unknown")
        ))
      }
      incProgress(1)
    })
  })
  
  # Check pantry for missing ingredients
  observeEvent(input$check_pantry_btn, {
    req(logged_in())
    req(selected_recipe())
    req(stock_data())
    
    recipe <- selected_recipe()
    current_stock <- stock_data()
    
    # Find missing ingredients by checking pantry
    missing <- list()
    
    for (ing in recipe$ingredients) {
      ingredient_name <- trimws(tolower(ing$name))
      
      # Check if ingredient exists in pantry (case-insensitive partial match)
      stock_match <- current_stock[tolower(current_stock$Ingredient) == ingredient_name, ]
      
      if (nrow(stock_match) == 0) {
        # Ingredient not in pantry at all
        missing[[length(missing) + 1]] <- list(
          name = ing$name,
          measure = ing$measure,
          in_pantry = FALSE
        )
      }
    }
    
    missing_ingredients(missing)
    pantry_checked(TRUE)
    
    if (length(missing) == 0) {
      showNotification("Great! You might have all ingredients!", 
                       type = "message", duration = 5)
    } else {
      showNotification(paste("Found", length(missing), "ingredient(s) not in your pantry"), 
                       type = "warning", duration = 5)
    }
  })
  
  # Shopping list UI
  output$shopping_list_ui <- renderUI({
    if (!pantry_checked()) {
      return(tags$p(style="color: #999; font-style: italic;", 
                    "Click 'Check My Pantry' to see what you need"))
    }
    
    missing <- missing_ingredients()
    
    if (is.null(missing) || length(missing) == 0) {
      return(tags$div(
        tags$p(style="color: green; font-weight: bold;", "✓ You have enough of all ingredients!")
      ))
    }
    
    shopping_items <- lapply(seq_along(missing), function(i) {
      ing <- missing[[i]]
      safe_name <- safe_id(ing$name)
      
      # Determine status message
      if (!is.null(ing$unparseable) && ing$unparseable) {
        status_msg <- tags$span(
          style="font-size: 11px; color: #ff9800;",
          paste("⚠ In stock:", ing$in_stock, "| Can't verify '", ing$measure, "'")
        )
        default_qty <- 100
      } else if (!ing$in_pantry) {
        status_msg <- tags$span(
          style="font-size: 11px; color: #d32f2f;",
          paste("✗ Not in pantry | Need:", ing$required)
        )
        default_qty <- ing$deficit
      } else {
        status_msg <- tags$span(
          style="font-size: 11px; color: #ff6f00;",
          paste("⚠ In stock:", ing$in_stock, "| Need:", ing$required, "| Short:", ing$deficit)
        )
        default_qty <- ing$deficit
      }
      
      fluidRow(
        style="margin-bottom: 10px; background-color: rgba(210, 105, 30, 0.1); padding: 8px; border-radius: 5px;",
        column(7, 
               checkboxInput(
                 inputId = paste0("shop_", safe_name),
                 label = tags$div(
                   tags$strong(ing$name),
                   tags$br(),
                   status_msg
                 ),
                 value = FALSE
               )
        ),
        column(5,
               numericInput(
                 inputId = paste0("shop_qty_", safe_name),
                 label = "Buy",
                 value = default_qty,
                 min = 0,
                 step = 10
               )
        )
      )
    })
    
    tagList(
      tags$p(style="font-size: 13px; color: #8B5E3C; margin-bottom: 10px;",
             paste("Need to restock", length(missing), "ingredient(s):")),
      shopping_items,
      actionButton("add_checked_btn", "Buy & Add Selected to Pantry",
                   icon = icon("shopping-cart"),
                   style = "width: 100%; margin-top: 10px;")
    )
  })
  
  # Add checked items to pantry
  observeEvent(input$add_checked_btn, {
    req(logged_in())
    missing <- missing_ingredients()
    
    if (is.null(missing) || length(missing) == 0) {
      showNotification("No missing ingredients to add", type = "warning")
      return()
    }
    
    added <- 0
    
    for (ing in missing) {
      safe_name <- safe_id(ing$name)
      checkbox_id <- paste0("shop_", safe_name)
      checkbox_value <- input[[checkbox_id]]
      
      if (!is.null(checkbox_value) && isTRUE(checkbox_value)) {
        ingredient <- ing$name
        
        # Default quantity and unit
        quantity <- 100
        unit <- "grams"
        
        existing <- dbGetQuery(con(), "SELECT quantity FROM pantry WHERE ingredient = ?", 
                               params = list(ingredient))
        
        if (nrow(existing) > 0) {
          new_qty <- existing$quantity + quantity
          dbExecute(con(), "UPDATE pantry SET quantity = ? WHERE ingredient = ?", 
                    params = list(new_qty, ingredient))
        } else {
          dbExecute(con(), "INSERT INTO pantry (ingredient, quantity, unit) VALUES (?, ?, ?)", 
                    params = list(ingredient, quantity, unit))
        }
        added <- added + 1
      }
    }
    
    if (added > 0) {
      showNotification(paste("Added", added, "ingredient(s) to pantry"), 
                       type = "message", duration = 5)
      load_stock()
      
      # Re-check pantry after adding
      if (input$check_pantry_btn > 0) {
        recipe <- selected_recipe()
        current_stock <- stock_data()
        
        missing_new <- list()
        for (ing in recipe$ingredients) {
          ingredient_name <- trimws(tolower(ing$name))
          stock_match <- current_stock[tolower(current_stock$Ingredient) == ingredient_name, ]
          
          required_qty <- parse_measure(ing$measure)
          
          if (nrow(stock_match) == 0) {
            missing_new[[length(missing_new) + 1]] <- list(
              name = ing$name,
              measure = ing$measure,
              required = required_qty,
              in_stock = 0,
              deficit = required_qty,
              in_pantry = FALSE
            )
          } else {
            current_qty <- stock_match$Quantity[1]
            
            if (required_qty > 0 && current_qty < required_qty) {
              deficit <- required_qty - current_qty
              missing_new[[length(missing_new) + 1]] <- list(
                name = ing$name,
                measure = ing$measure,
                required = required_qty,
                in_stock = current_qty,
                deficit = deficit,
                in_pantry = TRUE
              )
            } else if (required_qty == 0) {
              missing_new[[length(missing_new) + 1]] <- list(
                name = ing$name,
                measure = ing$measure,
                required = 0,
                in_stock = current_qty,
                deficit = 0,
                in_pantry = TRUE,
                unparseable = TRUE
              )
            }
          }
        }
        missing_ingredients(missing_new)
      }
    } else {
      showNotification("No items selected", type = "warning")
    }
  })
  
  # Cook button - deducts ingredients from pantry
  observeEvent(input$cook_btn, {
    req(logged_in())
    req(selected_recipe())
    req(stock_data())
    
    recipe <- selected_recipe()
    current_stock <- stock_data()
    
    # Track what was deducted
    deducted_items <- character(0)
    not_found_items <- character(0)
    insufficient_items <- character(0)
    
    for (ing in recipe$ingredients) {
      ingredient_name <- trimws(tolower(ing$name))
      
      # Find matching ingredient in pantry (case-insensitive)
      stock_match <- current_stock[tolower(current_stock$Ingredient) == ingredient_name, ]
      
      if (nrow(stock_match) > 0) {
        # Get the actual ingredient name from pantry
        actual_ingredient <- stock_match$Ingredient[1]
        current_qty <- stock_match$Quantity[1]
        
        # Parse the measure to extract quantity
        measure <- ing$measure
        deduct_amount <- parse_measure(measure)
        
        if (deduct_amount > 0) {
          if (current_qty >= deduct_amount) {
            new_qty <- current_qty - deduct_amount
            
            dbExecute(con(), "UPDATE pantry SET quantity = ? WHERE ingredient = ?", 
                      params = list(new_qty, actual_ingredient))
            
            deducted_items <- c(deducted_items, 
                                paste0(actual_ingredient, " (", deduct_amount, ")"))
          } else {
            # Not enough in pantry, deduct what's available
            dbExecute(con(), "UPDATE pantry SET quantity = ? WHERE ingredient = ?", 
                      params = list(0, actual_ingredient))
            
            insufficient_items <- c(insufficient_items,
                                    paste0(actual_ingredient, " (needed ", deduct_amount, 
                                           ", had ", current_qty, ")"))
          }
        } else {
          # Could not parse amount, deduct 50 as default
          new_qty <- max(0, current_qty - 50)
          dbExecute(con(), "UPDATE pantry SET quantity = ? WHERE ingredient = ?", 
                    params = list(new_qty, actual_ingredient))
          deducted_items <- c(deducted_items, paste0(actual_ingredient, " (50 - estimated)"))
        }
      } else {
        not_found_items <- c(not_found_items, ing$name)
      }
    }
    
    # Show notifications
    if (length(deducted_items) > 0) {
      msg <- paste("✓ Cooked", recipe$name, "! Deducted:", 
                   paste(deducted_items, collapse = ", "))
      showNotification(msg, type = "message", duration = 10)
    }
    
    if (length(insufficient_items) > 0) {
      msg <- paste("⚠ Insufficient quantities for:", 
                   paste(insufficient_items, collapse = ", "))
      showNotification(msg, type = "warning", duration = 10)
    }
    
    if (length(not_found_items) > 0) {
      msg <- paste("ℹ Not found in pantry:", 
                   paste(not_found_items, collapse = ", "))
      showNotification(msg, type = "warning", duration = 8)
    }
    
    # Reload stock
    load_stock()
    
    # Re-check pantry if it was checked before
    if (pantry_checked()) {
      missing_new <- list()
      updated_stock <- stock_data()
      
      for (ing in recipe$ingredients) {
        ingredient_name <- trimws(tolower(ing$name))
        stock_match <- updated_stock[tolower(updated_stock$Ingredient) == ingredient_name, ]
        
        required_qty <- parse_measure(ing$measure)
        
        if (nrow(stock_match) == 0) {
          missing_new[[length(missing_new) + 1]] <- list(
            name = ing$name,
            measure = ing$measure,
            required = required_qty,
            in_stock = 0,
            deficit = required_qty,
            in_pantry = FALSE
          )
        } else {
          current_qty <- stock_match$Quantity[1]
          
          if (required_qty > 0 && current_qty < required_qty) {
            deficit <- required_qty - current_qty
            missing_new[[length(missing_new) + 1]] <- list(
              name = ing$name,
              measure = ing$measure,
              required = required_qty,
              in_stock = current_qty,
              deficit = deficit,
              in_pantry = TRUE
            )
          } else if (required_qty == 0) {
            missing_new[[length(missing_new) + 1]] <- list(
              name = ing$name,
              measure = ing$measure,
              required = 0,
              in_stock = current_qty,
              deficit = 0,
              in_pantry = TRUE,
              unparseable = TRUE
            )
          }
        }
      }
      missing_ingredients(missing_new)
    }
  })
  
  # Render ingredients list
  output$ingredients_list_ui <- renderUI({
    recipe <- selected_recipe()
    
    if (is.null(recipe)) {
      return(tags$p(style="color: #999; font-style: italic;", 
                    "Select a recipe to see ingredients"))
    }
    
    ingredients_html <- lapply(recipe$ingredients, function(ing) {
      tags$li(paste0(ing$name, ": ", ing$measure))
    })
    
    tags$div(
      tags$ul(style="padding-left: 20px; line-height: 1.8;", ingredients_html)
    )
  })
  
  # Render recipe details
  output$recipe_details_ui <- renderUI({
    recipe <- selected_recipe()
    meal <- recipe_data()
    
    if (is.null(recipe)) {
      return(tags$p(style="color: #999; font-style: italic;", 
                    "No recipe selected"))
    }
    
    tagList(
      tags$h5(style="color: #8B5E3C; margin-top: 0;", recipe$name),
      tags$p(strong("Servings: "), recipe$servings),
      if (!is.null(meal)) {
        tagList(
          if(!is.null(meal$strCategory) && !is.na(meal$strCategory)) {
            tags$p(strong("Category: "), meal$strCategory)
          },
          if(!is.null(meal$strArea) && !is.na(meal$strArea)) {
            tags$p(strong("Cuisine: "), meal$strArea)
          }
        )
      },
      br(),
      actionButton("cook_btn", "🔥 Cook This Recipe!", 
                   icon = icon("fire"),
                   style = sprintf("background-color: %s; color: white; font-size: 16px; padding: 10px 20px; width: 100%%; font-weight: bold;", 
                                   colors$accent))
    )
  })
  
  # Render cooking instructions
  output$cooking_instructions <- renderUI({
    meal <- recipe_data()
    
    if (is.null(meal)) {
      return(div(class = "recipe-box",
                 tags$p(style="color: #999; font-style: italic;", 
                        "Select a recipe to see cooking instructions")))
    }
    
    instructions <- meal$strInstructions
    image_url <- meal$strMealThumb
    video_url <- meal$strYoutube
    
    tagList(
      div(class = "recipe-box",
          if(!is.null(image_url) && !is.na(image_url)) {
            tags$img(src = image_url, class = "recipe-image")
          },
          if(!is.null(video_url) && !is.na(video_url) && video_url != "") {
            tags$p(
              tags$a(href = video_url, target = "_blank",
                     icon("youtube"), " Watch Video Tutorial",
                     style = "color: #D2691E; font-weight: bold;")
            )
          }
      ),
      if(!is.null(instructions) && !is.na(instructions)) {
        div(class = "instructions-box",
            h5("Step-by-Step Instructions:", style = "color: #8B5E3C; margin-top: 0;"),
            HTML(gsub("\n", "<br><br>", instructions))
        )
      },
      tags$p(style="font-size: 11px; color: #888; margin-top: 10px;",
             "Recipe data from TheMealDB")
    )
  })
  
  output$unit_display <- renderUI({
    req(logged_in())
    if(input$ingredient_mode == "select") {
      req(input$add_ingredient)
      unit <- ingredient_units$Unit[ingredient_units$Ingredient == input$add_ingredient]
      tags$p(style="margin-top: 25px; font-weight: bold;", unit)
    } else {
      req(input$custom_unit)
      tags$p(style="margin-top: 25px; font-weight: bold;", input$custom_unit)
    }
  })
  
  output$stock_table <- DT::renderDataTable({
    req(logged_in())
    req(stock_data())
    current_stock <- stock_data()
    active_stock <- current_stock[current_stock$Quantity > 0, ]
    
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
  
  output$empty_stock_table <- DT::renderDataTable({
    req(logged_in())
    req(stock_data())
    current_stock <- stock_data()
    empty_stock <- current_stock[current_stock$Quantity == 0, ]
    
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
  
  observeEvent(input$stock_table_cell_edit, {
    req(logged_in())
    info <- input$stock_table_cell_edit
    if(info$col == 1) {
      current_stock <- stock_data()
      active_stock <- current_stock[current_stock$Quantity > 0, ]
      actual_ingredient <- active_stock[info$row + 1, "Ingredient"]
      new_quantity <- as.numeric(info$value)
      
      dbExecute(con(), "UPDATE pantry SET quantity = ? WHERE ingredient = ?", 
                params = list(new_quantity, actual_ingredient))
      load_stock()
    }
  })
  
  observeEvent(input$add_to_stock_btn, {
    req(logged_in())
    req(input$add_quantity)
    
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
    existing <- dbGetQuery(con(), "SELECT quantity FROM pantry WHERE ingredient = ?", 
                           params = list(ingredient))
    
    if(nrow(existing) > 0) {
      new_qty <- existing$quantity + quantity
      dbExecute(con(), "UPDATE pantry SET quantity = ? WHERE ingredient = ?", 
                params = list(new_qty, ingredient))
    } else {
      dbExecute(con(), "INSERT INTO pantry (ingredient, quantity, unit) VALUES (?, ?, ?)", 
                params = list(ingredient, quantity, unit))
    }
    
    showNotification(paste("Added", quantity, unit, "of", ingredient), type = "message")
    load_stock()
    
    if(input$ingredient_mode == "custom") {
      updateTextInput(session, "custom_ingredient", value = "")
    }
  })
}

shinyApp(ui, server)