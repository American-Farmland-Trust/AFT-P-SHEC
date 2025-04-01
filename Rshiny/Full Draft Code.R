library(shiny)
library(readxl)
library(dplyr)
library(DT)
library(tidyr)
library(ggplot2)
library(scales)

# Define UI
ui <- fluidPage(
  tabsetPanel(
    tabPanel("General Information",  
             fluidRow(
               column(12,
                      p("The Row Crop version of the P-SHEC tool is designed to evaluate the short-term 
             and long-term economic effects of the following soil health conservation practices:", 
                        strong("No-till or Reduced Tillage, Cover Cropping, and Nutrient Management"),
                        ". The tool can be used with 'soil health curious farmers' who are thinking about but 
            have not yet adopted any one or a combination of these practices. Soil practice options and underlying data are from ", 
                        a("COMET-Planner", href = "http://comet-planner.com/"), ".")
               )
             ),
             fluidRow(
               column(12, textInput("farm_name", "Enter Farm Name:", "")),
               column(4, selectInput("state", "Select State:", choices = NULL)),
               column(4, selectInput("county", "Select County:", choices = NULL))
             ),
             fluidRow(
               column(4, selectInput("tillage", "Select Tillage Practice:", choices = NULL)),
               column(4, selectInput("nutrient_mgmt", "Select Nutrient Management Practice:", choices = NULL)),
               column(4, selectInput("cover_cropping", "Select Cover Cropping Practice:", choices = NULL))
             ),
             fluidRow(
               column(6, h4("Crop Prices"), DTOutput("crop_prices")),
               column(6, h4("Fertilizer Prices"), DTOutput("fertilizer_prices"))
             ),
             fluidRow(
               column(12, h4("Rotation Information")),
               column(12, DTOutput("rotation_info_table")),
               column(4, actionButton("add_rotation_row", "Add Row")),
               column(4, actionButton("save_general_info", "Save General Information"))
             )),
    tabPanel("Current Management",
             fluidRow(
               uiOutput("current_management_headers")
             )
    ),
    tabPanel("Planned Management",
             fluidRow(
               uiOutput("planned_management_headers")
             )),
    
    tabPanel("Management Comparison Sheet",
             fluidRow(
               column(4, h3("General Information"),
                      uiOutput("general_info_recap")
               ),
               column(4, h3("Current Management At-A-Glance"),
                      uiOutput("current_mgmt_recap")
               ),
               column(4, h3("Planned Management At-A-Glance"),
                      uiOutput("planned_mgmt_recap")
               ))),
    
    tabPanel("Short-Term Outcomes",
             fluidRow(
               column(12, h3("Overview of Totals")),
               column(6, h4("Current Management Totals"),
                      DTOutput("short_term_outcomes_table")), 
               column(6, h4("Planned Management Totals"),
                      DTOutput("short_term_outcomes_table_planned")),
               column(12, h3("Partial Budget Analysis")),
               DTOutput("differences_table"),
               DTOutput("totals_table"),
               column(6, h3("Short-Term Result Graphics"),
                      plotOutput("stacked_bar_graph"), 
                      plotOutput("stacked_bar_graph_equipment")) 
             )),
    
    tabPanel("Long-Term Modelling",
             fluidRow(
               column(12, h3("Long-Term Modelling Overview")),
               column(12, tableOutput("long_term_som")), 
               column(12, uiOutput("som_change_report")) ,
               column(12, h3("Long-Term Modelling Estimates")),
               column(6, plotOutput("som_yield_plot_full")),
               column(6, plotOutput("som_yield_plot_zoomed")),
               column(12, DTOutput("som_yield_table")),
               column(6, uiOutput("ghg_reduction_report")),
               column(6, numericInput("price_per_ton", "Enter Price per Metric Ton of CO₂e ($):", value = 50, min = 0, step = 1))
             )),
    
    tabPanel("Drought Modelling",
             fluidRow(
               column(12, h3("Drought Modelling Overview")),
               column(12, h3("Drought Resilience Analysis")),
               column(12, textOutput("drought_probability_text")),  # Displays Probability of Drought
               column(12, tableOutput("get_drought_reduction_details")),
               column(6, plotOutput("drought_yield_change_plot") ) 
             )),
    
    tabPanel("Combined Outcomes",
             fluidRow(
               column(12, h3("Short-Term and Long-Term Combined Results")),
               column(12, h2("Short-Term Results Re-Cap")),
               column(12, tableOutput("short_term_table")),
               br(),
               textOutput("explanatory_text"),
               br(),
               h3("Per-Acre Cost Difference Over 10 Years"),
               tableOutput("combined_results"),
               numericInput("discount_rate", "Select Discount Rate (%)", value = 3, min = 0, max = 15, step = 1)
             ))
    #ENDING BRACKETS, DO NOT DELETE  
  )) 

# Define server logic
server <- function(input, output, session) {
  #Some datasets will be replaced by 'final versions' when outside analyses have been completed
  state_county_data <- read_excel("State_County.xlsx")
  comet_options <- read_excel("COMET_Options.xlsx")
  crop_prices <- read_excel("Prices.xlsx")
  equipment_data <- read_excel("Equipment.xlsx")
  pass_data <- read_excel("Pass_Options.xlsx")
  att_data <- read_excel("Attribution.xlsx")
  regionalequip_data <- read_excel("Regional_Equip.xlsx")
  gssurgo_base <- read_excel("SOM_County_Base.xlsx")  
  comet <- read_excel("COMET Planner Data_Concise.xlsx","All States Filtered")
  comet_ghg <- read_excel("COMET Planner Data_Concise.xlsx","Emissions Reductions")
  soybean_est<-read_excel("Soybean_Pred_Normal.xlsx")
  corn_est<-read_excel("corn_all_yield_predictions_intervals_machine_learning_model_update_N.xlsx")
  corn_drought<-read_excel("Drought_Example.xlsx")
  drought_prob<-read_excel("Drought_Probabilities_Example.xlsx")
  
  
  # Reactive values for the tables
  values <- reactiveValues(
    crop_prices = crop_prices %>%
      select(Crop, `Crop Price`, `Crop Unit`) %>%
      mutate(`User Price` = NA_real_),
    fertilizer_prices = crop_prices %>%
      select(Fertilizer, `Fertilizer Price`, `Fertilizer Unit`) %>%
      mutate(`User Price` = NA_real_),
    rotation_info = data.frame(Crop = character(), Acreage = numeric(), Year = integer(), Season = character(), stringsAsFactors = FALSE),
    current_management_tables = list(),   #Current Equipment Tables
    planned_management_tables = list(),   #Planned Equipment Tables
    cover_crop_tables = list(),
    cover_crop_tables_planned = list(),  
    nutrient_management_tables = list()
  )
  
  # Update state dropdown
  observe({
    state_choices <- unique(state_county_data$State)
    updateSelectInput(session, "state", choices = state_choices)
  })
  
  # Update county dropdown based on state selection
  observeEvent(input$state, {
    selected_state <- input$state
    county_choices <- state_county_data$County[state_county_data$State == selected_state]
    updateSelectInput(session, "county", choices = unique(county_choices))
  })
  
  # Update soil health practice dropdowns
  observe({
    updateSelectInput(session, "tillage", choices = unique(comet_options$`Tillage`))
    updateSelectInput(session, "nutrient_mgmt", choices = unique(comet_options$`Nutrient Management`))
    updateSelectInput(session, "cover_cropping", choices = unique(comet_options$`Cover Cropping`))
  })
  
  
  # Render the crop prices table
  output$crop_prices <- renderDT({
    datatable(values$crop_prices, editable = TRUE)
  }, server = FALSE)
  
  
  # Render the fertilizer prices table
  output$fertilizer_prices <- renderDT({
    datatable(values$fertilizer_prices, editable = TRUE)
  }, server = FALSE)
  
  # Render the rotation info table
  output$rotation_info_table <- renderDT({
    datatable(values$rotation_info, editable = TRUE, options = list(pageLength = 5))
  }, server = FALSE)
  
  # Observe cell edits in the crop prices table
  observeEvent(input$crop_prices_cell_edit, {
    info <- input$crop_prices_cell_edit
    str(info)
    
    # Update the reactive values with the edited "User Price" column
    values$crop_prices[info$row, info$col] <- info$value
  })
  
  # Observe cell edits in the fertilizer prices table
  observeEvent(input$fertilizer_prices_cell_edit, {
    info <- input$fertilizer_prices_cell_edit
    str(info)
    
    # Update the reactive values with the edited "User Price" column
    values$fertilizer_prices[info$row, info$col] <- info$value
  })
  
  # Observe cell edits in the rotation info table
  observeEvent(input$rotation_info_table_cell_edit, {
    info <- input$rotation_info_table_cell_edit
    str(info)
    
    # Update the reactive values with the edited columns
    values$rotation_info[info$row, info$col] <- info$value
  })
  
  # Save general information
  observeEvent(input$save_general_info, {
    if (nrow(values$crop_prices) > 0) {
      # show a notification to the user
      showNotification("User prices for crop prices have been updated successfully!", type = "message")
    } else {
      showNotification("No data available to update in crop prices.", type = "warning")
    }
    
    if (nrow(values$fertilizer_prices) > 0) {
      # show a notification to the user
      showNotification("User prices for fertilizer prices have been updated successfully!", type = "message")
    } else {
      showNotification("No data available to update in fertilizer prices.", type = "warning")
    }
    
    if (nrow(values$rotation_info) > 0) {
      # show a notification to the user
      showNotification("Rotation information has been updated successfully!", type = "message")
    } else {
      showNotification("No data available to update in rotation information.", type = "warning")
    }  })
  
  # Add row to rotation info table
  observeEvent(input$add_rotation_row, {
    new_row <- data.frame(Crop = NA, Acreage = NA, Year = NA, Season = NA, stringsAsFactors = FALSE)
    values$rotation_info <- rbind(values$rotation_info, new_row)
    values$current_management_tables[[nrow(values$rotation_info)]] <- data.frame(Equipment = character(), Dollar_Per_Acre = numeric(), stringsAsFactors = FALSE)
    values$planned_management_tables[[nrow(values$rotation_info)]] <- data.frame(Equipment = character(), Dollar_Per_Acre = numeric(), stringsAsFactors = FALSE)
    values$cover_crop_tables[[nrow(values$rotation_info)]] <- data.frame(Category = c("Establishment Cost", "Termination Cost"), Cost = as.numeric(c(NA, NA)), stringsAsFactors = FALSE) 
    values$cover_crop_tables_planned[[nrow(values$rotation_info)]] <- data.frame(Category = c("Establishment Cost", "Termination Cost"), Cost = as.numeric(c(NA, NA)), stringsAsFactors = FALSE) 
    values$nutrient_management_tables[[nrow(values$rotation_info)]] <- data.frame( Category = c("Current N application rate (lbs/Ac)","Current P application rate (lbs/Ac)", "Current K application rate (lbs/Ac)"),Value = as.numeric(c(NA, NA, NA)),stringsAsFactors = FALSE)
    values$nutrient_management_tables_planned[[nrow(values$rotation_info)]] <- data.frame( Category = c("% Reduction in N", "% Reduction in P", "% Reduction in K"),Value = as.numeric(c(NA, NA, NA)),stringsAsFactors = FALSE)  #new line, delete if you want NM tables on Current and planned mgmt to match. Make adjustment in render block of planned mgmt NM table, too
  })
  
  #########################HEADERS LINKED TO ROTATIONAL USER INFORMATION#############################
  
  output$current_management_headers <- renderUI({
    n_rows <- nrow(values$rotation_info)
    if (n_rows > 0) {
      fluidRow(lapply(1:n_rows, function(i) {
        crop_name <- values$rotation_info$Crop[i]  # Get crop name from rotation table
        header <- ifelse(!is.na(crop_name) & crop_name != "", crop_name, paste("Crop", i))  # Use crop name if available
        
        column(12,
               h4(header),
               uiOutput(paste0("equipment_choice_ui_", i)),
               selectInput(inputId = paste0("number_pass_", i), label = "Select the Number of Passes:", choices = pass_data$Passes, selected = NULL),
               selectInput(inputId = paste0("attribution_", i), label = "Select the Practice this Equipment is Used for:", choices = att_data$Attribution, selected = NULL),
               actionButton(paste0("add_equipment_row_", i), "Add Equipment Row"),
               DTOutput(paste0("equipment_table_", i)),
               h4("Total Equipment Cost"),
               textOutput(paste0("total_cost_", i)),
               h4("Cover Crop Costs"),
               DTOutput(paste0("cover_crop_table_", i)),
               actionButton(paste0("cover_crop_calc", i), "Calculate Cover Crop Costs"),
               h5("Total Cover Crop Costs:"),
               textOutput(paste0("total_cover_crop_costs_", i)),
               h4("Nutrient Management"),
               DTOutput(paste0("nutrient_management_table_", i)),
               actionButton(paste0("calculate_cost", i), "Calculate Total Nutrient Management Cost"),
               h5("Total Nutrient Management Costs:"),
               textOutput(paste0("total_nutrient_management_cost_", i))
        )
      }))
    }  })
  
  output$planned_management_headers <- renderUI({
    n_rows <- nrow(values$rotation_info)
    if (n_rows > 0) {
      fluidRow(lapply(1:n_rows, function(i) {
        crop_name <- values$rotation_info$Crop[i]  # Get crop name from rotation table
        header <- ifelse(!is.na(crop_name) & crop_name != "", crop_name, paste("Crop", i))  # Use crop name if available
        
        column(12,
               h4(header),
               uiOutput(paste0("equipment_choice_ui_planned_", i)),
               selectInput(inputId = paste0("number_pass_planned_", i), label = "Select the Number of Passes:", choices = pass_data$Passes, selected = NULL),
               selectInput(inputId = paste0("attribution_planned_", i), label = "Select the Practice this Equipment is Used for:", choices = att_data$Attribution, selected = NULL),
               actionButton(paste0("add_equipment_row_planned_", i), "Add Equipment Row"),
               DTOutput(paste0("equipment_table_planned_", i)),
               h4("Total Equipment Cost"),
               textOutput(paste0("total_cost_planned_", i)),
               h4("Cover Crop Costs"),
               DTOutput(paste0("cover_crop_table_planned_", i)),
               actionButton(paste0("cover_crop_calc_planned_", i), "Calculate Cover Crop Costs"),
               h5("Total Cover Crop Costs:"),
               textOutput(paste0("total_cover_crop_costs_planned_", i)),
               h4("Nutrient Management"),
               DTOutput(paste0("nutrient_management_table_planned_", i)),
               actionButton(paste0("calculate_cost_planned_", i), "Calculate Total Planned Nutrient Management Cost"),
               h5("Total Planned Nutrient Management Costs:"),
               textOutput(paste0("total_nutrient_management_cost_planned_", i))
        )
      }))
    }  })
  
  output$general_info_recap <- renderUI({
    # Ensure inputs are available
    req(input$state, input$county, input$tillage, input$nutrient_mgmt, input$cover_cropping)
    fluidRow(
      column(12,
             h4("General Information Recap"),
             tags$p(strong("State: "), input$state),
             tags$p(strong("County: "), input$county),
             tags$p(strong("Selected Tillage Practice: "), input$tillage),
             tags$p(strong("Selected Nutrient Management Practice: "), input$nutrient_mgmt),
             tags$p(strong("Selected Cover Cropping Practice: "), input$cover_cropping),
             
             h4("Crop Prices (User Input)"),
             if (nrow(values$crop_prices) > 0) {
               DT::dataTableOutput("recap_crop_prices")
             } else {
               tags$p("No crop prices entered yet.")
             },
             
             h4("Fertilizer Prices (User Input)"),
             if (nrow(values$fertilizer_prices) > 0) {
               DT::dataTableOutput("recap_fertilizer_prices")
             } else {
               tags$p("No fertilizer prices entered yet.")
             },
             
             h4("Rotation Information"),
             if (nrow(values$rotation_info) > 0) {
               DT::dataTableOutput("recap_rotation_info")
             } else {
               tags$p("No rotation information entered yet.")
             }
      )
    )  })
  
  output$current_mgmt_recap <- renderUI({
    # Ensure inputs are available
    req(input$state, input$county, input$tillage, input$nutrient_mgmt, input$cover_cropping)
    
    # Create a fluid row with the user-selected information
    fluidRow(
      column(12,
             
             h4("Current Management Equipment Tables"),
             if (length(values$current_management_tables) > 0) {
               # Dynamically create tables for each crop
               lapply(seq_along(values$current_management_tables), function(i) {
                 crop_name <- values$rotation_info$Crop[i]  # Get crop name
                 crop_label <- ifelse(!is.na(crop_name) & crop_name != "", crop_name, paste("Crop", i))  # Use crop name if available
                 
                 div(
                   h5(paste("Equipment Table for", crop_label)),
                   DT::dataTableOutput(paste0("recap_equipment_table_", i))
                 )
               })
             } else {
               tags$p("No equipment tables added yet.")
             },
             
             h4("Current Management Cover Crop Tables"),
             if (length(values$cover_crop_tables) > 0) {
               # Dynamically create cover crop tables for each crop
               lapply(seq_along(values$cover_crop_tables), function(i) {
                 crop_name <- values$rotation_info$Crop[i]  # Get crop name
                 crop_label <- ifelse(!is.na(crop_name) & crop_name != "", crop_name, paste("Crop", i))  # Use crop name if available
                 
                 div(
                   h5(paste("Cover Crop Table for", crop_label)),
                   DT::dataTableOutput(paste0("recap_cover_crop_table_", i))
                 )
               })
             } else {
               tags$p("No cover crop tables added yet.")
             },
             
             h4("Current Management Nutrient Management Tables"),
             if (length(values$nutrient_management_tables) > 0) {
               # Dynamically create nutrient management tables for each crop
               lapply(seq_along(values$nutrient_management_tables), function(i) {
                 crop_name <- values$rotation_info$Crop[i]  # Get crop name
                 crop_label <- ifelse(!is.na(crop_name) & crop_name != "", crop_name, paste("Crop", i))  # Use crop name if available
                 
                 div(
                   h5(paste("Nutrient Management Table for", crop_label)),
                   DT::dataTableOutput(paste0("recap_nutrient_management_table", i))
                 )
               })
             } else {
               tags$p("No nutrient management tables added yet.")
             }
      )
    )  })
  
  output$planned_mgmt_recap <- renderUI({
    # Ensure inputs are available
    req(input$state, input$county, input$tillage, input$nutrient_mgmt, input$cover_cropping)
    
    # Create a fluid row with the user-selected information
    fluidRow(
      column(12,
             h4("Planned Management Equipment Tables"),  
             if (length(values$planned_management_tables) > 0) {   
               # Dynamically create tables for each rotation
               lapply(seq_along(values$planned_management_tables), function(i) { 
                 crop_name <- values$rotation_info$Crop[i]  # Get user-entered crop name
                 crop_label <- ifelse(!is.na(crop_name) & crop_name != "", crop_name, paste("Crop", i))  # Use crop name if available
                 
                 div(
                   h5(paste("Equipment Table for", crop_label)),
                   DT::dataTableOutput(paste0("recap_equipment_table_planned", i))
                 )
               })
             } else {
               tags$p("No planned equipment tables added yet.")
             },
             
             h4("Planned Management Cover Crop Tables"),
             if (length(values$cover_crop_tables_planned) > 0) { 
               # Dynamically create cover crop tables for each planned rotation
               lapply(seq_along(values$cover_crop_tables_planned), function(i) { 
                 crop_name <- values$rotation_info$Crop[i]  # Get user-entered crop name
                 crop_label <- ifelse(!is.na(crop_name) & crop_name != "", crop_name, paste("Crop", i))  # Use crop name if available
                 
                 div(
                   h5(paste("Cover Crop Table for Planned", crop_label)),
                   DT::dataTableOutput(paste0("recap_cover_crop_table_planned_", i))
                 )
               })
             } else {
               tags$p("No planned cover crop tables added yet.")
             },
             
             h4("Planned Management Nutrient Management Tables"),
             if (length(values$nutrient_management_tables_planned) > 0) {
               # Dynamically create nutrient management tables for each crop
               lapply(seq_along(values$nutrient_management_tables_planned), function(i) {
                 crop_name <- values$rotation_info$Crop[i]  # Get user-entered crop name
                 crop_label <- ifelse(!is.na(crop_name) & crop_name != "", crop_name, paste("Crop", i))  # Use crop name if available
                 
                 div(
                   h5(paste("Planned Nutrient Management Table for", crop_label)),
                   DT::dataTableOutput(paste0("recap_nutrient_management_table_planned", i))
                 )
               })
             } else {
               tags$p("No planned nutrient management tables added yet.")
             }
      )
    )  })
  
  observe({
    n_rows <- nrow(values$rotation_info)
    lapply(1:n_rows, function(i) {
      output[[paste0("equipment_choice_ui_", i)]] <- renderUI({
        selected_state <- input$state
        equipment_choices <- regionalequip_data %>%
          filter(State == selected_state) %>%
          pull(Equipment)
        selectInput(inputId = paste0("equipment_choice_", i), label = "Select Equipment:", choices = equipment_choices, selected = NULL)
      })
    })  })
  
  observe({
    n_rows <- nrow(values$rotation_info)
    lapply(1:n_rows, function(i) {
      observeEvent(input[[paste0("add_equipment_row_", i)]], {
        # Use isolate to prevent reactivity from unnecessarily retriggering this block
        selected_equip <- isolate(input[[paste0("equipment_choice_", i)]])
        selected_equip_1 <- isolate(input[[paste0("number_pass_", i)]])
        selected_equip_2 <- isolate(input[[paste0("attribution_", i)]])
        selected_state <- isolate(input$state)
        
        # Check if all required inputs are provided and valid
        if (!is.null(selected_equip) && selected_equip != "" &&
            !is.null(selected_equip_1) && selected_equip_1 != "" &&
            !is.null(selected_equip_2) && selected_equip_2 != "" &&
            !is.null(selected_state) && selected_state != "") {
          
          # Prevent duplicate addition caused by multiple triggers
          existing_rows <- values$current_management_tables[[i]]
          new_row <- data.frame(
            Equipment = selected_equip,
            Passes = selected_equip_1,
            Dollar_Per_Acre = regionalequip_data %>%
              filter(State == selected_state, Equipment == selected_equip) %>%
              pull(Dollar_Per_Acre),
            Attribution = selected_equip_2
          )
          
          # Debugging statements
          print(paste("Existing rows:", nrow(existing_rows)))
          print(paste("New row:", new_row))
          
          # Add row only if it doesn't already exist in the table
          if (nrow(existing_rows) == 0 || !any(existing_rows %>% 
                                               mutate_all(as.character) %>% 
                                               rowwise() %>% 
                                               do.call(paste, .) == do.call(paste, new_row))) {
            values$current_management_tables[[i]] <- rbind(existing_rows, new_row)
          }
          
          # Reset input fields for this row
          updateSelectInput(session, paste0("equipment_choice_", i), selected = NULL)
          updateSelectInput(session, paste0("number_pass_", i), selected = NULL)
          updateSelectInput(session, paste0("attribution_", i), selected = NULL)
        }
      })
    })  })
  
  observe({
    n_rows <- nrow(values$rotation_info)
    lapply(1:n_rows, function(i) {
      output[[paste0("equipment_choice_ui_planned_", i)]] <- renderUI({
        selected_state <- input$state
        equipment_choices <- regionalequip_data %>%
          filter(State == selected_state) %>%
          pull(Equipment)
        selectInput(inputId = paste0("equipment_choice_planned_", i), label = "Select Equipment:", choices = equipment_choices, selected = NULL)
      })
    })  })
  
  observe({
    n_rows <- nrow(values$rotation_info)
    lapply(1:n_rows, function(i) {
      observeEvent(input[[paste0("add_equipment_row_planned_", i)]], {
        # Use isolate to prevent unnecessary reactive dependencies
        selected_equip <- isolate(input[[paste0("equipment_choice_planned_", i)]])
        selected_equip_1 <- isolate(input[[paste0("number_pass_planned_", i)]])
        selected_equip_2 <- isolate(input[[paste0("attribution_planned_", i)]])
        selected_state <- isolate(input$state)
        
        # Check if all required inputs are provided and valid
        if (!is.null(selected_equip) && selected_equip != "" &&
            !is.null(selected_equip_1) && selected_equip_1 != "" &&
            !is.null(selected_equip_2) && selected_equip_2 != "" &&
            !is.null(selected_state) && selected_state != "") {
          
          # Create the new row
          new_row <- data.frame(
            Equipment = selected_equip,
            Passes = selected_equip_1,
            Dollar_Per_Acre = regionalequip_data %>%
              filter(State == selected_state, Equipment == selected_equip) %>%
              pull(Dollar_Per_Acre),
            Attribution = selected_equip_2
          )
          
          # Check if the new row already exists in the table
          existing_rows <- values$planned_management_tables[[i]]
          # Add row only if it doesn't already exist in the table
          if (nrow(existing_rows) == 0 || !any(existing_rows %>% 
                                               mutate_all(as.character) %>% 
                                               rowwise() %>% 
                                               do.call(paste, .) == do.call(paste, new_row))) {
            values$planned_management_tables[[i]] <- rbind(existing_rows, new_row)
          }
          
          # Reset input fields for this row
          updateSelectInput(session, paste0("equipment_choice_planned_", i), selected = NULL)
          updateSelectInput(session, paste0("number_pass_planned_", i), selected = NULL)
          updateSelectInput(session, paste0("attribution_planned_", i), selected = NULL)
        }
      })
    })  })
  
  # Observe and calculate cover crop costs for Current management when the button is clicked
  observe({
    n_rows <- nrow(values$rotation_info)
    
    if (n_rows > 0) {
      lapply(1:n_rows, function(i) {
        # Listen for edits to the respective cover crop table
        observeEvent(input[[paste0("cover_crop_table_", i, "_cell_edit")]], {
          info <- input[[paste0("cover_crop_table_", i, "_cell_edit")]]
          if (!is.null(info)) {
            str(info)  # For debugging purposes 
            values$cover_crop_tables[[i]][info$row, info$col] <- as.numeric(info$value)
          }
        })
        
        # Calculate the total cover crop costs when the button is clicked
        observeEvent(input[[paste0("cover_crop_calc", i)]], {
          if (!is.null(values$cover_crop_tables[[i]])) {
            cover_crop_table <- values$cover_crop_tables[[i]]
            total_cover_crop_cost <- sum(cover_crop_table$Cost, na.rm = TRUE)
            values$total_cover_crop_cost[[i]] <- total_cover_crop_cost  # Store total cost as a reactive value
            
            output[[paste0("total_cover_crop_costs_", i)]] <- renderText({
              paste("$", format(total_cover_crop_cost, digits = 2))
            })
          }
        })
      })
    }  })
  
  # Observe and calculate cover crop costs for Planned management when the button is clicked
  observe({
    n_rows <- nrow(values$rotation_info)
    
    if (n_rows > 0) {
      lapply(1:n_rows, function(i) {
        # Listen for edits to the respective cover crop table
        observeEvent(input[[paste0("cover_crop_table_planned_", i, "_cell_edit")]], {
          info <- input[[paste0("cover_crop_table_planned_", i, "_cell_edit")]]
          if (!is.null(info)) {
            str(info)  # For debugging purposes; remove or comment out 
            values$cover_crop_tables_planned[[i]][info$row, info$col] <- as.numeric(info$value)
          }
        })
        # Calculate the total cover crop costs when the button is clicked
        observeEvent(input[[paste0("cover_crop_calc_planned_", i)]], {
          print(paste("Button clicked for row", i))  # Debugging statement
          if (!is.null(values$cover_crop_tables_planned[[i]])) {
            cover_crop_table_planned <- values$cover_crop_tables_planned[[i]]
            total_cover_crop_cost_planned <- sum(cover_crop_table_planned$Cost, na.rm = TRUE)
            values$total_cover_crop_cost_planned[[i]] <- total_cover_crop_cost_planned  
            
            print(paste("Total cover crop cost for row", i, ":", total_cover_crop_cost_planned))  # Debugging statement
            
            output[[paste0("total_cover_crop_costs_planned_", i)]] <- renderText({
              paste("$", format(total_cover_crop_cost_planned, digits = 2))
            })
          }
        })
      })
    }  })
  
  observe({
    n_rows <- nrow(values$rotation_info)
    if (n_rows > 0) {
      lapply(1:n_rows, function(i) {
        observeEvent(input[[paste0("nutrient_management_table_", i, "_cell_edit")]], {
          info <- input[[paste0("nutrient_management_table_", i, "_cell_edit")]]
          if (!is.null(info)) {
            str(info)
            values$nutrient_management_tables[[i]][info$row, info$col] <- as.numeric(info$value)
          }
        })
        
        # Calculate the total nutrient management costs when the button is clicked
        observeEvent(input[[paste0("calculate_cost", i)]], {
          print(paste("Button clicked for crop", i))  # Debugging statement
          if (!is.null(values$nutrient_management_tables[[i]])) {
            nutrient_table <- values$nutrient_management_tables[[i]]
            fertilizer_prices <- values$fertilizer_prices
            
            # Debugging statements to check the contents of the tables
            print("Nutrient Management Table:")
            print(nutrient_table)
            print("Fertilizer Prices:")
            print(fertilizer_prices)
            
            # Ensure the relevant columns are numeric
            nutrient_table[] <- lapply(nutrient_table, function(x) suppressWarnings(as.numeric(as.character(x))))
            fertilizer_prices$`Fertilizer Price` <- suppressWarnings(as.numeric(as.character(fertilizer_prices$`Fertilizer Price`)))
            fertilizer_prices$`User Price` <- suppressWarnings(as.numeric(as.character(fertilizer_prices$`User Price`)))
            
            # Use Fertilizer_Price if available, otherwise use Crop_Price
            user_price <- ifelse(!is.na(fertilizer_prices$`Fertilizer Price`), fertilizer_prices$`Fertilizer Price`, fertilizer_prices$`User Price`)
            
            # Calculate sum product for the current table
            total_nutrient_management_cost <- sum(nutrient_table * user_price, na.rm = TRUE)
            values$total_nutrient_management_cost[[i]] <- total_nutrient_management_cost
            
            print(paste("Total nutrient management cost for crop", i, ":", total_nutrient_management_cost))  # Debugging statement
            
            output[[paste0("total_nutrient_management_cost_", i)]] <- renderText({
              paste("$", format(total_nutrient_management_cost, digits = 2))
            })
          }
        })
      })
    }  })
  
  # Observe and store NM data for crop "i" for Planned management when the button is clicked
  observe({
    n_rows <- nrow(values$rotation_info)
    
    if (n_rows > 0) {
      lapply(1:n_rows, function(i) {
        # Listen for edits to the respective NM table
        observeEvent(input[[paste0("nutrient_management_table_planned_", i, "_cell_edit")]], {
          info <- input[[paste0("nutrient_management_table_planned_", i, "_cell_edit")]]
          if (!is.null(info)) {
            str(info)  # For debugging purposes; remove or comment out 
            values$nutrient_management_tables_planned[[i]][info$row, info$col] <- as.numeric(info$value)
          }
        })
        
        # Calculate the total nutrient management costs for planned changes when the button is clicked
        observeEvent(input[[paste0("calculate_cost_planned_", i)]], {
          print(paste("Button clicked for planned crop", i))  # Debugging statement
          if (!is.null(values$nutrient_management_tables[[i]]) && !is.null(values$nutrient_management_tables_planned[[i]])) {
            current_nutrient_table <- values$nutrient_management_tables[[i]]
            planned_nutrient_table <- values$nutrient_management_tables_planned[[i]]
            fertilizer_prices <- values$fertilizer_prices
            
            # Debugging statements to check the contents of the tables
            print("Current Nutrient Management Table:")
            print(current_nutrient_table)
            print("Planned Nutrient Management Table:")
            print(planned_nutrient_table)
            print("Fertilizer Prices:")
            print(fertilizer_prices)
            
            # Ensure the relevant columns are numeric
            current_nutrient_table[] <- lapply(current_nutrient_table, function(x) suppressWarnings(as.numeric(as.character(x))))
            planned_nutrient_table[] <- lapply(planned_nutrient_table, function(x) suppressWarnings(as.numeric(as.character(x))))
            fertilizer_prices$`Fertilizer Price` <- suppressWarnings(as.numeric(as.character(fertilizer_prices$`Fertilizer Price`)))
            fertilizer_prices$`User Price` <- suppressWarnings(as.numeric(as.character(fertilizer_prices$`User Price`)))
            
            # Use Fertilizer_Price if available, otherwise use Crop_Price
            user_price <- ifelse(!is.na(fertilizer_prices$`Fertilizer Price`), fertilizer_prices$`Fertilizer Price`, fertilizer_prices$`User Price`)
            
            # Calculate the total nutrient management costs for planned changes
            total_nutrient_management_cost_planned <- sum((current_nutrient_table - (current_nutrient_table * planned_nutrient_table)/100) * user_price, na.rm = TRUE)
            values$total_nutrient_management_cost_planned[[i]] <- total_nutrient_management_cost_planned
            
            print(paste("Total nutrient management cost for planned crop", i, ":", total_nutrient_management_cost_planned))  # Debugging statement
            
            output[[paste0("total_nutrient_management_cost_planned_", i)]] <- renderText({
              paste("$", format(total_nutrient_management_cost_planned, digits = 2))
            })
          }
        })
      })
    }  })
  
  #########################################################################RENDERING TABLES################################
  # Render equipment tables for each rotation row in Current Management
  observe({
    n_rows <- nrow(values$rotation_info)
    lapply(1:n_rows, function(i) {
      output[[paste0("equipment_table_", i)]] <- renderDT({
        datatable(values$current_management_tables[[i]], editable = FALSE, 
                  options = list(pageLength = 3))
      })
      
      output[[paste0("total_cost_", i)]] <- renderText({
        current_table <- values$current_management_tables[[i]]
        
        if (!is.null(current_table) && 
            all(c("Dollar_Per_Acre", "Passes", "Attribution") %in% colnames(current_table))) {
          
          # Ensure columns are numeric
          current_table$Dollar_Per_Acre <- as.numeric(current_table$Dollar_Per_Acre)
          current_table$Passes <- as.numeric(current_table$Passes)
          
          # Calculate total cost grouped by Attribute
          total_costs_by_attribute <- current_table %>%
            group_by(Attribution) %>%
            summarize(
              Total_Cost = sum(Dollar_Per_Acre * Passes, na.rm = TRUE),
              .groups = "drop"
            )
          # Store the results in the reactive value
          values$total_costs_by_attribute[[i]] <- as.data.frame(total_costs_by_attribute)
          
          # Calculate the sum product
          total_cost <- sum(current_table$Dollar_Per_Acre * current_table$Passes, na.rm = TRUE)
          # Store total cost as reactive value
          values$total_cost[[i]] <- total_cost  # Store total cost as a reactive value
          
          # Format the results for display
          attribute_text <- paste(
            apply(total_costs_by_attribute, 1, function(row) {
              paste(row["Attribution"], ": $", format(as.numeric(row["Total_Cost"]), nsmall = 2))
            }),
            collapse = "\n"
          )
          overall_text <- paste("Overall Total: $", format(total_cost, nsmall = 2))
          
          # Combine attribute totals and overall total for display
          paste(attribute_text, "\n\n", overall_text)
        } else {
          "Total Cost: $0.00 (No valid data available)"
        }
      })
    })  })
  
  # Render equipment tables for each rotation row in Planned Management
  observe({
    n_rows <- nrow(values$rotation_info)
    lapply(1:n_rows, function(i) {
      output[[paste0("equipment_table_planned_", i)]] <- renderDT({
        datatable(values$planned_management_tables[[i]], editable = FALSE, 
                  options = list(pageLength = 3))
      })
      
      output[[paste0("total_cost_planned_", i)]] <- renderText({
        planned_table <- values$planned_management_tables[[i]]
        
        if (!is.null(planned_table) && 
            all(c("Dollar_Per_Acre", "Passes", "Attribution") %in% colnames(planned_table))) {
          
          # Ensure columns are numeric
          planned_table$Dollar_Per_Acre <- as.numeric(planned_table$Dollar_Per_Acre)
          planned_table$Passes <- as.numeric(planned_table$Passes)
          
          # Calculate total cost grouped by Attribute
          total_costs_by_attribute <- planned_table %>%
            group_by(Attribution) %>%
            summarize(
              Total_Cost = sum(Dollar_Per_Acre * Passes, na.rm = TRUE),
              .groups = "drop"
            )
          # Store the results in the reactive value
          values$total_costs_by_attribute_planned[[i]] <- as.data.frame(total_costs_by_attribute)
          
          # Calculate the sum product
          total_cost <- sum(planned_table$Dollar_Per_Acre * planned_table$Passes, na.rm = TRUE)
          # Store the total cost as a reactive value
          values$total_cost_planned[[i]] <- total_cost  # Store total cost as a reactive value
          
          # Format the results for display
          attribute_text <- paste(
            apply(total_costs_by_attribute, 1, function(row) {
              paste(row["Attribution"], ": $", format(as.numeric(row["Total_Cost"]), nsmall = 2))
            }),
            collapse = "\n"
          )
          overall_text <- paste("Overall Total: $", format(total_cost, nsmall = 2))
          
          # Combine attribute totals and overall total for display
          paste(attribute_text, "\n\n", overall_text)
        } else {
          "Total Cost: $0.00 (No valid data available)"
        }
      })
    })
  })
  
  # Render cover crop tables for each rotation row in Current Management
  observe({
    n_rows <- nrow(values$rotation_info)
    lapply(1:n_rows, function(i) {
      output[[paste0("cover_crop_table_", i)]] <- renderDT({
        datatable(values$cover_crop_tables[[i]], editable = TRUE, options = list(pageLength = 2))
      })
      
    })  })
  
  # Render cover crop tables for each rotation row in Planned Management
  observe({
    n_rows <- nrow(values$rotation_info)
    lapply(1:n_rows, function(i) {
      output[[paste0("cover_crop_table_planned_", i)]] <- renderDT({
        datatable(values$cover_crop_tables_planned[[i]], editable = TRUE, options = list(pageLength = 2))
      })
    })
  })
  
  # Render nutrient management tables for each rotation row in Current Management
  observe({
    n_rows <- nrow(values$rotation_info)
    lapply(1:n_rows, function(i) {
      output[[paste0("nutrient_management_table_", i)]] <- renderDT({
        datatable(values$nutrient_management_tables[[i]], editable = TRUE, options = list(pageLength = 3))
      })
    })  })
  
  # Render nutrient management tables for each rotation row in Planned Management
  observe({
    n_rows <- nrow(values$rotation_info)
    lapply(1:n_rows, function(i) {
      output[[paste0("nutrient_management_table_planned_", i)]] <- renderDT({
        datatable(values$nutrient_management_tables_planned[[i]], editable = TRUE, options = list(pageLength = 3)) #was (reverts table to match current mgmt nut. mgmt table: values$nutrient_management_tables_
      })
    })  })
  
  # Crop Prices Table
  #Render price tables for current management recap 
  output$recap_crop_prices <- renderDT({
    datatable(values$crop_prices)
  }, server = FALSE)
  
  # Fertilizer Prices Table
  output$recap_fertilizer_prices <- DT::renderDataTable({
    req(values$fertilizer_prices)
    DT::datatable(values$fertilizer_prices, options = list(dom = "t", pageLength = 5))
  })
  
  # Rotation Information Table
  output$recap_rotation_info <- DT::renderDataTable({
    req(values$rotation_info)
    DT::datatable(values$rotation_info, options = list(dom = "t", pageLength = 5))
  })
  
  # Render each equipment table
  observe({
    lapply(seq_along(values$current_management_tables), function(i) {
      output[[paste0("recap_equipment_table_", i)]] <- DT::renderDataTable({
        req(values$current_management_tables[[i]])
        DT::datatable(values$current_management_tables[[i]], options = list(dom = "t", pageLength = 5))
      })
    })  })
  
  # Render each cover crop table
  observe({
    lapply(seq_along(values$cover_crop_tables), function(i) {
      output[[paste0("recap_cover_crop_table_", i)]] <- DT::renderDataTable({
        req(values$cover_crop_tables[[i]])
        DT::datatable(values$cover_crop_tables[[i]], options = list(dom = "t", pageLength = 5))
      })
    })  })
  
  
  # Render each current NM table
  observe({
    lapply(seq_along(values$nutrient_management_tables), function(i) {
      output[[paste0("recap_nutrient_management_table", i)]] <- DT::renderDataTable({
        req(values$nutrient_management_tables[[i]])
        DT::datatable(values$nutrient_management_tables[[i]], options = list(dom = "t", pageLength = 5))
      })
    })  })
  
  # Render each equipment table
  observe({
    lapply(seq_along(values$planned_management_tables), function(i) {
      output[[paste0("recap_equipment_table_planned", i)]] <- DT::renderDataTable({
        req(values$planned_management_tables[[i]])
        DT::datatable(values$planned_management_tables[[i]], options = list(dom = "t", pageLength = 5))
      })
    })  })
  
  # Render each planned management cover crop table
  observe({
    lapply(seq_along(values$cover_crop_tables_planned), function(i) {
      output[[paste0("recap_cover_crop_table_planned_", i)]] <- DT::renderDataTable({
        req(values$cover_crop_tables_planned[[i]])
        DT::datatable(
          values$cover_crop_tables_planned[[i]],
          options = list(dom = "t", pageLength = 5),
          rownames = FALSE
        )
      })
    })  })
  
  # Render each current NM table
  observe({
    lapply(seq_along(values$nutrient_management_tables_planned), function(i) {
      output[[paste0("recap_nutrient_management_table_planned", i)]] <- DT::renderDataTable({
        req(values$nutrient_management_tables_planned[[i]])
        DT::datatable(values$nutrient_management_tables_planned[[i]], options = list(dom = "t", pageLength = 5))
      })
    })  })
  
  ###################################################################### SHORT TERM RENDERING
  # Render the short term outcomes table for current management
  output$short_term_outcomes_table <- renderDT({
    datatable(rbind(
      data.frame(Attribution = bind_rows(values$total_costs_by_attribute)$Attribution, 
                 Total_Cost = format(round(bind_rows(values$total_costs_by_attribute)$Total_Cost, 2), nsmall = 2)),
      data.frame(Attribution = "Overall Total Equipment Cost", 
                 Total_Cost = format(round(sum(unlist(values$total_cost)), 2), nsmall = 2)), # Sum overall total cost
      data.frame(Attribution = "Total Cover Crop Cost", 
                 Total_Cost = format(round(sum(unlist(values$total_cover_crop_cost)), 2), nsmall = 2)),
      data.frame(Attribution = "Total Nutrient Management Cost", 
                 Total_Cost = format(round(sum(unlist(values$total_nutrient_management_cost)), 2), nsmall = 2))
    ))
  }, server = FALSE)
  
  # Render the short term outcomes table for planned management
  output$short_term_outcomes_table_planned <- renderDT({
    datatable(rbind(
      data.frame(Attribution = bind_rows(values$total_costs_by_attribute_planned)$Attribution, 
                 Total_Cost = format(round(bind_rows(values$total_costs_by_attribute_planned)$Total_Cost,2), nsmall=2))  ,
      data.frame(Attribution = "Overall Total Planned Equipment Cost",
                 Total_Cost = format(round(sum(unlist(values$total_cost_planned)),2), nsmall=2)) , # Sum overall total cost as a row
      
      data.frame(Attribution = "Total Planned Cover Crop Cost",
                 Total_Cost = format(round(sum(unlist(values$total_cover_crop_cost_planned)),2), nsmall = 2)),
      data.frame(Attribution = "Total Planned Nutrient Management Cost", 
                 Total_Cost = format(round(sum(unlist(values$total_nutrient_management_cost_planned)), 2), nsmall=2))
    ))
  }, server = FALSE)
  
  # Reactive values to store the differences
  differences <- reactiveValues(
    equipment_cost_difference = NULL,
    cover_crop_cost_difference = NULL,
    nutrient_management_cost_difference = NULL
  )
  
  # Server section to create the differences table
  output$differences_table <- renderDT({
    # Calculate the differences for total costs
    differences$equipment_cost_difference <- round(sum(unlist(values$total_cost)) - sum(unlist(values$total_cost_planned)), 2)
    differences$cover_crop_cost_difference <- round(sum(unlist(values$total_cover_crop_cost)) - sum(unlist(values$total_cover_crop_cost_planned)), 2)
    differences$nutrient_management_cost_difference <- round(sum(unlist(values$total_nutrient_management_cost)) - sum(unlist(values$total_nutrient_management_cost_planned)), 2)
    
    # Calculate the differences for total costs by attribute
    total_costs_by_attribute_difference <- bind_rows(values$total_costs_by_attribute) %>%
      full_join(bind_rows(values$total_costs_by_attribute_planned), by = "Attribution", suffix = c("_current", "_planned")) %>%
      mutate(
        Total_Cost_current = replace_na(Total_Cost_current, 0),
        Total_Cost_planned = replace_na(Total_Cost_planned, 0),
        Difference = round(Total_Cost_current - Total_Cost_planned, 2)
      )
    
    # Create a data frame with the results
    differences_df <- data.frame(
      Category = c("Equipment Costs", "Cover Crop Costs", "Nutrient Management Costs", total_costs_by_attribute_difference$Attribution),
      `Decrease in Cost` = c(
        ifelse(differences$equipment_cost_difference > 0, format(differences$equipment_cost_difference, nsmall = 2), NA),
        ifelse(differences$cover_crop_cost_difference > 0, format(differences$cover_crop_cost_difference, nsmall = 2), NA),
        ifelse(differences$nutrient_management_cost_difference > 0, format(differences$nutrient_management_cost_difference, nsmall = 2), NA),
        ifelse(total_costs_by_attribute_difference$Difference > 0, format(total_costs_by_attribute_difference$Difference, nsmall = 2), NA)
      ),
      `Increase in Cost` = c(
        ifelse(differences$equipment_cost_difference < 0, format(abs(differences$equipment_cost_difference), nsmall = 2), NA),
        ifelse(differences$cover_crop_cost_difference < 0, format(abs(differences$cover_crop_cost_difference), nsmall = 2), NA),
        ifelse(differences$nutrient_management_cost_difference < 0, format(abs(differences$nutrient_management_cost_difference), nsmall = 2), NA),
        ifelse(total_costs_by_attribute_difference$Difference < 0, format(abs(total_costs_by_attribute_difference$Difference), nsmall = 2), NA)
      )
    )
    
    # Render the table
    datatable(differences_df)
  })
  
  # Calculate the total for each column
  total_decrease_in_cost <- reactive({
    sum(
      na.omit(c(
        differences$equipment_cost_difference[differences$equipment_cost_difference > 0],
        differences$cover_crop_cost_difference[differences$cover_crop_cost_difference > 0],
        differences$nutrient_management_cost_difference[differences$nutrient_management_cost_difference > 0]
      ))
    )  })
  
  total_increase_in_cost <- reactive({
    sum(
      na.omit(c(
        abs(differences$equipment_cost_difference[differences$equipment_cost_difference < 0]),
        abs(differences$cover_crop_cost_difference[differences$cover_crop_cost_difference < 0]),
        abs(differences$nutrient_management_cost_difference[differences$nutrient_management_cost_difference < 0])
      ))
    )  })
  
  # Create a new data frame with the totals
  totals_df <- reactive({
    data.frame(
      Category = c("Total Decrease in Cost", "Total Increase in Cost"),
      Amount = c(format(total_decrease_in_cost(), nsmall = 2), format(total_increase_in_cost(), nsmall = 2))
    )
  })
  
  # Render the new table
  output$totals_table <- renderDT({
    datatable(totals_df())
  })
  
  ######################################################Short Term Graphics######################################## 
  #Create the stacked bar graph
  output$stacked_bar_graph <- renderPlot({
    current_costs <- data.frame(
      Category = c("Equipment Costs", "Cover Crop Costs", "Nutrient Management Costs (Chemical Only)"),
      Cost = c(sum(unlist(values$total_cost)), sum(unlist(values$total_cover_crop_cost)), sum(unlist(values$total_nutrient_management_cost))),
      Type = "Current"
    )
    
    planned_costs <- data.frame(
      Category = c("Equipment Costs", "Cover Crop Costs", "Nutrient Management Costs (Chemical Only)"),
      Cost = c(sum(unlist(values$total_cost_planned)), sum(unlist(values$total_cover_crop_cost_planned)), sum(unlist(values$total_nutrient_management_cost_planned))),
      Type = "Planned"
    )
    
    costs_data <- rbind(current_costs, planned_costs)
    
    ggplot(costs_data, aes(x = Type, y = Cost, fill = Category)) +
      geom_bar(stat = "identity", position = "stack") +
      labs(title = "Current vs Planned Costs", x = "Type of Management", y = "Total Cost ($/Ac)") +
      theme_minimal()
  })
  
  # Create the stacked bar graph for equipment attributes
  output$stacked_bar_graph_equipment <- renderPlot({
    current_equipment_attributes <- bind_rows(values$total_costs_by_attribute) %>%
      mutate(Type = "Current")
    
    planned_equipment_attributes <- bind_rows(values$total_costs_by_attribute_planned) %>%
      mutate(Type = "Planned")
    
    equipment_data <- rbind(current_equipment_attributes, planned_equipment_attributes)
    
    ggplot(equipment_data, aes(x = Type, y = Total_Cost, fill = Attribution)) +
      geom_bar(stat = "identity", position = "stack") +
      labs(title = "Current vs Planned Equipment Costs by Attribute", x = "Type of Management", y = "Total Cost ($/Ac)") +
      theme_minimal()
  })
  
  ####################LONG TERM SHEETS########################## 
  get_base_SOM <- reactive({
    req(input$state, input$county, values$rotation_info)  # Ensure inputs are available
    
    if (nrow(values$rotation_info) > 0) {
      # Convert `base_SOM` to numeric
      gssurgo_base <- gssurgo_base %>%
        mutate(base_SOM = as.numeric(base_SOM))  # Ensure `base_SOM` is numeric
      
      # Convert `Acreage` to numeric in `rotation_info` table
      values$rotation_info <- values$rotation_info %>%
        mutate(Acreage = as.numeric(Acreage))  # Ensure `Acreage` is numeric
      
      # Filter out rows with missing Crop or Acreage
      base_SOM_data <- values$rotation_info %>%
        filter(!is.na(Crop), !is.na(Acreage) & Acreage > 0) %>%  # Ensure Acreage > 0
        left_join(
          gssurgo_base %>%
            filter(State == input$state, County == input$county) %>%
            select(Crop, base_SOM),  # Ensure only relevant columns from gssurgo_base
          by = "Crop"
        ) %>%
        mutate(Weighted_SOM = base_SOM * Acreage)  # Multiply Base SOM by Acreage for weighting
      
      # Calculate the Acre-Weighted Average SOM
      total_acres <- sum(base_SOM_data$Acreage, na.rm = TRUE)
      total_weighted_SOM <- sum(base_SOM_data$Weighted_SOM, na.rm = TRUE)
      
      acre_weighted_SOM <- ifelse(total_acres > 0, total_weighted_SOM / total_acres, NA)
      
      # Add a row for the Acre-Weighted Average SOM at the bottom
      summary_row <- data.frame(Crop = "Acre-Weighted Average SOM", Base_SOM = acre_weighted_SOM, stringsAsFactors = FALSE)
      
      base_SOM_data <- base_SOM_data %>%
        select(Crop, Base_SOM = base_SOM)  # Keep only Crop and Base SOM columns
      
      return(bind_rows(base_SOM_data, summary_row))  # Bind the new summary row at the bottom
    } else {
      return(data.frame(Crop = character(), Base_SOM = numeric()))  # Return empty if no data
    }  })
  

  
  
  get_som_change_details <- reactive({
    req(input$state, input$county, input$tillage, input$nutrient_mgmt, input$cover_cropping)
    
    # Filter the comet dataset for the selected state and county
    selected_comet_data <- comet %>%
      filter(state == input$state,
             county == input$county)  # Filter by state and county
    
    # Get the SOM change for each practice selection based on `planner_implementation`
    
    # Check if "None" is selected for Tillage
    tillage_change <- if (input$tillage == "None") {
      0  # If "None", set the SOM change to 0
    } else {
      selected_comet_data %>%
        filter(planner_implementation == input$tillage) %>%
        select(SOM_Percent_Per_Year = `%SOM/yr`) %>%
        summarise(Tillage_SOM_Change = SOM_Percent_Per_Year * 10) %>%
        pull(Tillage_SOM_Change)
    }
    
    # Check if "None" is selected for Nutrient Management
    nutrient_mgmt_change <- if (input$nutrient_mgmt == "None") {
      0  # If "None", set the SOM change to 0
    } else {
      selected_comet_data %>%
        filter(planner_implementation == input$nutrient_mgmt) %>%
        select(SOM_Percent_Per_Year = `%SOM/yr`) %>%
        summarise(Nutrient_Mgmt_SOM_Change = SOM_Percent_Per_Year * 10) %>%
        pull(Nutrient_Mgmt_SOM_Change)
    }
    
    # Check if "None" is selected for Cover Cropping
    cover_cropping_change <- if (input$cover_cropping == "None") {
      0  # If "None", set the SOM change to 0
    } else {
      selected_comet_data %>%
        filter(planner_implementation == input$cover_cropping) %>%
        select(SOM_Percent_Per_Year = `%SOM/yr`) %>%
        summarise(Cover_Cropping_SOM_Change = SOM_Percent_Per_Year * 10) %>%
        pull(Cover_Cropping_SOM_Change)
    }
    
    # If any of the practice changes are NA, set them to 0 for clarity
    tillage_change <- ifelse(is.na(tillage_change), 0, tillage_change)
    nutrient_mgmt_change <- ifelse(is.na(nutrient_mgmt_change), 0, nutrient_mgmt_change)
    cover_cropping_change <- ifelse(is.na(cover_cropping_change), 0, cover_cropping_change)
    
    # Return the individual changes and the total 10-year change
    list(
      tillage_change = tillage_change,
      nutrient_mgmt_change = nutrient_mgmt_change,
      cover_cropping_change = cover_cropping_change,
      total_som_change = tillage_change + nutrient_mgmt_change + cover_cropping_change
    )
  })
  
  
  # Display the itemized report in the UI
  output$som_change_report <- renderUI({
    som_changes <- get_som_change_details()
    
    # Create an itemized report with each practice's contribution
    report <- tagList(
      h4("Itemized SOM Change Report:"),
      p(paste("Tillage: ", round(som_changes$tillage_change, 2), "% over 10 years")),
      p(paste("Nutrient Management: ", round(som_changes$nutrient_mgmt_change, 2), "% over 10 years")),
      p(paste("Cover Cropping: ", round(som_changes$cover_cropping_change, 2), "% over 10 years")),
      hr(),
      h4(paste("Total 10-Year SOM Change: ", round(som_changes$total_som_change, 2), "%"))
    )
    
    return(report)
  })
  
  
  
  
  output$long_term_som <- renderTable({
    get_base_SOM()
  }, rownames = FALSE)
  
  # Reactive function for full-range yield-SOM data
  filtered_yield_data_full <- reactive({
    req(input$state, input$county, values$rotation_info)
    
    user_crops <- unique(values$rotation_info$Crop)
    
    # Initialize an empty data frame with consistent column names
    combined_data <- data.frame(
      state_alpha = character(),
      county_name = character(),
      ssurgo_om_mean = numeric(),
      pred_yield = numeric(),
      Crop = character(),
      stringsAsFactors = FALSE
    )
    
    for (crop in user_crops) {
      if (crop == "Soybean") {
        crop_data <- soybean_est %>%
          filter(state_alpha == input$state, county_name == input$county)
      } else if (crop == "Corn") {
        crop_data <- corn_est %>%
          filter(state_alpha == input$state, county_name == input$county)
      } else {
        next  # Skip crops not in the datasets
      }
      
      # Ensure the selected crop data has the same structure as `combined_data`
      if (nrow(crop_data) > 0) {
        crop_data <- crop_data %>%
          select(state_alpha, county_name, ssurgo_om_mean, pred_yield) %>%  # Keep only necessary columns
          mutate(Crop = crop)  # Add a Crop column
        
        combined_data <- bind_rows(combined_data, crop_data)  # Use `bind_rows()` instead of `rbind()`
      }
    }
    
    return(combined_data)
  })
  
  # Reactive function to calculate the base SOM and projected SOM
  get_som_range <- reactive({
    req(get_base_SOM(), get_som_change_details())  # Ensure dependencies exist
    
    base_som <- get_base_SOM() %>%
      filter(Crop == "Acre-Weighted Average SOM") %>%
      pull(Base_SOM)  # Extract base SOM
    
    som_changes <- get_som_change_details()
    projected_som <- base_som + som_changes$total_som_change  # Calculate 10-year SOM gain
    
    return(list(base_SOM = base_som, projected_SOM = projected_som))
  })
  
  # Reactive function for zoomed-in yield-SOM data
  filtered_yield_data_zoomed <- reactive({
    req(input$state, input$county, values$rotation_info, get_som_range())
    
    user_crops <- unique(values$rotation_info$Crop)
    som_range <- get_som_range()
    
    # Initialize empty dataframe with correct structure
    combined_data <- data.frame(
      state_alpha = character(),
      county_name = character(),
      ssurgo_om_mean = numeric(),
      pred_yield = numeric(),
      Crop = character(),
      stringsAsFactors = FALSE
    )
    
    for (crop in user_crops) {
      if (crop == "Soybean") {
        crop_data <- soybean_est %>%
          filter(state_alpha == input$state, county_name == input$county)
      } else if (crop == "Corn") {
        crop_data <- corn_est %>%
          filter(state_alpha == input$state, county_name == input$county)
      } else {
        next  # Skip crops not in the datasets
      }
      
      # Filter the data within the SOM range
      crop_data <- crop_data %>%
        filter(ssurgo_om_mean >= som_range$base_SOM & ssurgo_om_mean <= som_range$projected_SOM) %>%
        select(state_alpha, county_name, ssurgo_om_mean, pred_yield) %>%  # Keep necessary columns
        mutate(Crop = crop)  # Add Crop column
      
      if (nrow(crop_data) > 0) {
        combined_data <- bind_rows(combined_data, crop_data)  # Use `bind_rows()` to avoid `rbind()` issues
      }
    }
    
    return(combined_data)
  })
  
  output$som_yield_plot_full <- renderPlot({
    yield_data <- filtered_yield_data_full()
    req(nrow(yield_data) > 0)  # Ensure data is available
    
    som_range <- get_som_range()
    req(!is.null(som_range$base_SOM), !is.null(som_range$projected_SOM))
    
    ggplot(yield_data, aes(x = ssurgo_om_mean, y = pred_yield, color = Crop)) +
      geom_line(size = 1.2) +
      geom_vline(xintercept = som_range$base_SOM, linetype = "dashed", color = "black") +
      geom_vline(xintercept = som_range$projected_SOM, linetype = "dashed", color = "blue") +
      annotate("text", x = som_range$base_SOM, 
               y = min(yield_data$pred_yield) + 6,  # Moves label downward
               label = "Base SOM", 
               color = "black", vjust = 1, hjust = -0.2, size = 5) +
      annotate("text", x = som_range$projected_SOM, 
               y = min(yield_data$pred_yield) + 6,  # Moves label downward
               label = "10-Year SOM", 
               color = "blue", vjust = 1, hjust = -0.2, size = 5) +
      labs(title = "Predicted Yield vs. Soil Organic Matter (Full Range)",
           x = "Soil Organic Matter (%)",
           y = "Predicted Yield (units)") +
      theme_minimal() +
      theme(legend.title = element_blank())
  })
  
  output$som_yield_plot_zoomed <- renderPlot({
    yield_data <- filtered_yield_data_zoomed()
    req(nrow(yield_data) > 0)  # Ensure data is available
    
    ggplot(yield_data, aes(x = ssurgo_om_mean, y = pred_yield, color = Crop)) +
      geom_line(size = 1.2) +
      labs(title = "Predicted Yield vs. Soil Organic Matter (Zoomed-In)",
           x = "Soil Organic Matter (%)",
           y = "Predicted Yield (units)") +
      theme_minimal() +
      theme(legend.title = element_blank())
  })
  
  
  # Reactive function for SOM increase and predicted yield per year for crops in the rotation
  generate_som_yield_table <- reactive({
    req(input$state, input$county, values$rotation_info, get_som_range())
    
    user_crops <- unique(values$rotation_info$Crop)  # Get user crops from rotation table
    som_range <- get_som_range()
    base_som <- som_range$base_SOM
    projected_som <- som_range$projected_SOM
    
    # Calculate the SOM increase per year based on 10-year total change
    som_increase_per_year <- (projected_som - base_som) / 10
    
    # Initialize empty dataframe for results
    results <- data.frame(
      Crop = character(),
      Year = integer(),
      Base_SOM = numeric(),
      Increased_SOM = numeric(),
      Pred_Yield = numeric(),
      Base_Yield = numeric(),
      difference_yield_revenue = numeric(),  # Updated column name
      stringsAsFactors = FALSE
    )
    
    # Loop over the crops and generate SOM and predicted yield for each year
    for (crop in user_crops) {
      # Fetch crop-specific yield data
      if (crop == "Soybean") {
        crop_data <- soybean_est %>%
          filter(state_alpha == input$state, county_name == input$county)
      } else if (crop == "Corn") {
        crop_data <- corn_est %>%
          filter(state_alpha == input$state, county_name == input$county)
      } else {
        next  # Skip crops not in the datasets
      }
      
      # Get crop price: Use user-provided price if available, otherwise use default price
      crop_price <- values$crop_prices %>%
        filter(Crop == crop) %>%
        mutate(Final_Price = ifelse(!is.na(`User Price`), `User Price`, `Crop Price`)) %>%
        pull(Final_Price)
      
      # Calculate the base yield once for each crop
      base_yield <- crop_data %>%
        filter(abs(ssurgo_om_mean - base_som) < 0.01) %>%
        summarise(base_yield = mean(pred_yield, na.rm = TRUE)) %>%
        pull(base_yield)
      
      # For each year, calculate the SOM increase and predicted yield
      for (year in 1:10) {
        increased_som <- base_som + (som_increase_per_year * year)
        
        # Find the predicted yield for the current crop and year
        pred_yield <- crop_data %>%
          filter(ssurgo_om_mean >= base_som & ssurgo_om_mean <= increased_som) %>%
          summarise(pred_yield = mean(pred_yield, na.rm = TRUE)) %>%
          pull(pred_yield)
        
        # Calculate Difference in Yield Revenue
        difference_yield_revenue <- (crop_price * pred_yield) - (crop_price * base_yield)
        
        # Add the results to the data frame with rounded values
        results <- bind_rows(results, data.frame(
          Crop = crop,
          Year = year,
          Base_SOM = round(base_som, 2),
          Increased_SOM = round(increased_som, 2),
          Pred_Yield = round(pred_yield, 2),
          Base_Yield = round(base_yield, 2),
          difference_yield_revenue = round(difference_yield_revenue, 2)  # Updated column name
        ))
        # Debugging: Print the column names and the first few rows to check if the column exists
        #print(names(results))
        # print(head(results))
      }    }
    
    return(results)
  })
  
  output$som_yield_table <- renderDT({
    som_yield_data <- generate_som_yield_table()
    req(nrow(som_yield_data) > 0)  # Ensure data is available
    
    datatable(som_yield_data, options = list(pageLength = 10))
  })
  
  # Reactive function to get individual GHG reductions for each practice
  get_ghg_reduction_details <- reactive({
    req(input$state, input$county, input$tillage, input$nutrient_mgmt, input$cover_cropping, input$price_per_ton)
    
    # Filter the COMET_ghg dataset for the selected state and county (normalize county case)
    selected_ghg_data <- comet_ghg %>%
      filter(state == input$state,
             toupper(county) == toupper(input$county))  # Normalize county case
    
    # Get the GHG reduction for each practice selection
    tillage_reduction <- selected_ghg_data %>%
      filter(planner_implementation == input$tillage) %>%
      summarise(Tillage_GHG_Reduction = sum(total_ghg_co2, na.rm = TRUE)) %>%
      pull(Tillage_GHG_Reduction)
    
    nutrient_mgmt_reduction <- selected_ghg_data %>%
      filter(planner_implementation == input$nutrient_mgmt) %>%
      summarise(Nutrient_Mgmt_GHG_Reduction = sum(total_ghg_co2, na.rm = TRUE)) %>%
      pull(Nutrient_Mgmt_GHG_Reduction)
    
    cover_cropping_reduction <- selected_ghg_data %>%
      filter(planner_implementation == input$cover_cropping) %>%
      summarise(Cover_Cropping_GHG_Reduction = sum(total_ghg_co2, na.rm = TRUE)) %>%
      pull(Cover_Cropping_GHG_Reduction)
    
    # Handle potential NA values
    tillage_reduction <- ifelse(is.na(tillage_reduction), 0, tillage_reduction)
    nutrient_mgmt_reduction <- ifelse(is.na(nutrient_mgmt_reduction), 0, nutrient_mgmt_reduction)
    cover_cropping_reduction <- ifelse(is.na(cover_cropping_reduction), 0, cover_cropping_reduction)
    
    # Calculate total GHG reduction
    total_ghg_reduction <- tillage_reduction + nutrient_mgmt_reduction + cover_cropping_reduction
    
    # Calculate cost savings based on user-entered price
    cost_savings <- total_ghg_reduction * input$price_per_ton
    
    # Return the values
    list(
      tillage_reduction = tillage_reduction,
      nutrient_mgmt_reduction = nutrient_mgmt_reduction,
      cover_cropping_reduction = cover_cropping_reduction,
      total_ghg_reduction = total_ghg_reduction,
      cost_savings = cost_savings
    )  })
  
  # Display the itemized GHG reduction report in the UI
  output$ghg_reduction_report <- renderUI({
    ghg_reductions <- get_ghg_reduction_details()
    
    # Create an itemized report with GHG reductions and cost savings
    report <- tagList(
      h4("Itemized GHG Reduction Report:"),
      p(paste("Tillage: ", round(ghg_reductions$tillage_reduction, 2), " metric tons CO2 eq./ac/yr")),
      p(paste("Nutrient Management: ", round(ghg_reductions$nutrient_mgmt_reduction, 2), " metric tons CO2 eq./ac/yr")),
      p(paste("Cover Cropping: ", round(ghg_reductions$cover_cropping_reduction, 2), " metric tons CO2 eq./ac/yr")),
      hr(),
      h4(paste("Total GHG Reduction: ", round(ghg_reductions$total_ghg_reduction, 2), " metric tons CO2 eq./ac/yr")),
      hr(),
      h4(paste("Total Potential Revenue: $/ac/yr", format(round(ghg_reductions$cost_savings, 2), big.mark = ",")))
    )
    
    return(report)
  }) 
  
  #Notes: carbon pricing calculator link with $/MT CO2e values under different policies: https://www.rff.org/publications/data-tools/carbon-pricing-calculator/
  
  ##################################### Drought Calculations ###################################
  #Display steady state probability for  drought conditions conditional upon State and County:
  get_drought_probability <- reactive({
    req(input$state, input$county)
    
    prob_data <- drought_prob %>%
      filter(State == input$state, County == input$county) %>%
      select(Prob_of_Drought) %>%
      pull()
    
    if (length(prob_data) == 0) {
      return("No data available")
    } else {
      return(paste0("Probability of Drought: ", round(prob_data * 100, 2), "%"))
    }
  })
  output$drought_probability_text <- renderText({
    get_drought_probability()
  })
  
  # Drought "Analysis" 
  get_drought_reduction_details <- reactive({
    req(input$state, input$county, values$rotation_info, get_som_range())
    
    # Get SOM range based on user selection
    som_range <- get_som_range()
    base_som <- som_range$base_SOM
    projected_som <- som_range$projected_SOM
    
    # Get the list of user-selected crops from rotation info
    user_crops <- unique(values$rotation_info$Crop)
    
    # Filter drought dataset for the selected state and county
    filtered_drought_data <- corn_drought %>%
      filter(state == input$state, county == input$county)
    
    # Ensure relevant data is available
    if (nrow(filtered_drought_data) == 0) {
      stop("No drought data available for this state and county.")
    }
    
    # Validate the organic matter column
    correct_col_name <- "om"  # Adjust based on actual column name
    if (!(correct_col_name %in% colnames(filtered_drought_data))) {
      stop(paste("Column", correct_col_name, "not found in filtered_drought_data. Check dataset."))
    }
    
    # Calculate annual SOM increase over 10 years
    som_increase_per_year <- (projected_som - base_som) / 10
    
    # Extract yield estimates from `generate_som_yield_table()`
    som_yield_data <- generate_som_yield_table() %>%
      filter(Crop %in% user_crops) %>%
      select(Crop, Year, Pred_Yield)  # Keep necessary columns
    
    # Create results dataframe
    results <- expand.grid(
      Crop = user_crops,
      Year = 1:10
    ) %>%
      mutate(
        Base_SOM = round(base_som, 2),
        Increased_SOM = round(base_som + (Year * som_increase_per_year), 2)
      )
    
    # Merge predicted yield from `generate_som_yield_table`
    results <- left_join(results, som_yield_data, by = c("Crop", "Year"))
    
    # Retrieve predicted yield change (%) from the drought dataset
    results <- results %>%
      rowwise() %>%
      mutate(
        Pred_Yield_Change_Pct = filtered_drought_data %>%
          filter(abs(om - Increased_SOM) == min(abs(om - Increased_SOM), na.rm = TRUE)) %>%
          slice(1) %>%  # Get the closest match
          pull(pred_yield_change_percentage) %>% 
          replace_na(0),  # Ensure no missing values
        
        # Adjusted predicted yield under drought
        Pred_Yield_Drought = Pred_Yield + (Pred_Yield * (Pred_Yield_Change_Pct / 100))
      ) %>%
      ungroup()
    
    # Compute difference between Year 1 and each subsequent year
    results <- results %>%
      group_by(Crop) %>%
      mutate(
        Drought_Yield_Diff = Pred_Yield_Drought - first(Pred_Yield_Drought)
      ) %>%
      ungroup()
    
    # Merge with crop price table to get prices
    crop_price_data <- values$crop_prices %>%
      select(Crop, `Crop Price`, `User Price`) %>%
      mutate(Effective_Price = ifelse(!is.na(`User Price`), `User Price`, `Crop Price`))  # Use user-entered price if available
    
    # Merge price data into results
    results <- left_join(results, crop_price_data, by = "Crop")
    
    # Compute the final economic difference value
    results <- results %>%
      mutate(
        Drought_Yield_Value_Diff = Drought_Yield_Diff * Effective_Price  # Multiply by the selected price
      )
    
    # Format all numeric values (except Year) to 2 decimal places
    results <- results %>%
      mutate(
        Base_SOM = round(Base_SOM, 2),
        Increased_SOM = round(Increased_SOM, 2),
        Pred_Yield = round(Pred_Yield, 2),
        Pred_Yield_Change_Pct = round(Pred_Yield_Change_Pct, 2),
        Pred_Yield_Drought = round(Pred_Yield_Drought, 2),
        Drought_Yield_Diff = round(Drought_Yield_Diff, 2),
        Effective_Price = round(Effective_Price, 2),
        Drought_Yield_Value_Diff = round(Drought_Yield_Value_Diff, 2)
      )
    
    # Split the data by crop to return a list of tables
    split(results, results$Crop)
  })
  
  output$get_drought_reduction_details <- renderUI({
    drought_tables <- get_drought_reduction_details()
    
    if (is.null(drought_tables)) {
      return(NULL)
    }
    
    output_list <- lapply(names(drought_tables), function(crop) {
      table_id <- paste0("drought_table_", crop)
      
      output[[table_id]] <- DT::renderDataTable({
        datatable(drought_tables[[crop]], 
                  options = list(pageLength = 10))  # Set page length to 10
      })
      
      tagList(
        h3(paste("Drought Impact for", crop)),
        DT::dataTableOutput(table_id)
      )     })
    
    do.call(tagList, output_list)
  })
  
  
  output$drought_yield_change_plot <- renderPlot({
    drought_data <- get_drought_reduction_details()
    
    if (is.null(drought_data)) {
      return(NULL)
    }
    
    combined_data <- bind_rows(drought_data, .id = "Crop")
    
    # Extract Year 1 Pred_Yield_Change_Pct for each crop
    baseline_values <- combined_data %>%
      filter(Year == 1) %>%
      select(Crop, Pred_Yield_Change_Pct) %>%
      distinct()
    
    ggplot(combined_data, aes(x = Increased_SOM, y = Pred_Yield_Change_Pct, color = Crop)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      geom_hline(data = baseline_values, aes(yintercept = Pred_Yield_Change_Pct, color = Crop), 
                 linetype = "dashed", size = 1) +  # Dashed horizontal line for yield penalty baseline
      labs(
        title = "Predicted Yield Change Percentage vs. Increased SOM",
        x = "Increased Soil Organic Matter (SOM)",
        y = "Predicted Yield Change (%)",
        color = "Crop"
      ) +
      theme_minimal()
  })
  
  
  #############Combined Results Tab
  
  # Reactive function to get Crop and Acreage
  get_short_term_recap <- reactive({
    req(nrow(values$rotation_info) > 0)
    
    # Ensure only complete rows with valid Crop and Acreage are captured
    valid_rows <- complete.cases(values$rotation_info$Crop, values$rotation_info$Acreage)
    
    # Sum the acreage for all crops in the rotation
    total_acreage <- sum(as.numeric(values$rotation_info$Acreage[valid_rows]), na.rm = TRUE)
    
    # Calculate the total decrease in cost
    total_decrease_in_cost_val <- total_decrease_in_cost()
    
    # Calculate the total increase in cost
    total_increase_in_cost_val <- total_increase_in_cost()
    
    # Calculate the difference between the increase and decrease in cost
    total_cost_difference <- total_decrease_in_cost_val - total_increase_in_cost_val
    
    # Calculate acreage multiplied by total_cost_difference
    acreage_cost_product <- total_acreage * total_cost_difference
    
    # Format the cost values with dollar signs
    formatted_cost_difference <- dollar(total_cost_difference)
    formatted_acreage_cost_product <- dollar(acreage_cost_product)
    
    # Create the data frame with total acreage and cost difference (formatted as currency)
    data.frame(
      Category = c("Total Acreage", "Cost Difference per Acre (Decrease-Increase)", "Study Area Cost Difference"),
      Value = c(total_acreage, formatted_cost_difference, formatted_acreage_cost_product),
      stringsAsFactors = FALSE
    )  })
  
  # Render the Short-Term PBA Results table in combined tab
  output$short_term_table <- renderTable({
    get_short_term_recap()
  })
  
  # Render the explanatory text below the table
  output$explanatory_text <- renderText({
    # Get the total cost difference
    total_cost_difference <- total_decrease_in_cost() - total_increase_in_cost()
    
    if (total_cost_difference < 0) {
      "If the difference between the decreased cost and increased cost is negative, this indicates that the planned management decisions are more expensive than current operations."
    } else {
      "If the difference between the decreased cost and increased cost is positive, this indicates that the planned management decisions are generating a cost savings compared to current operations."
    }  })
  
  combined_results_data <- reactive({
    short_term_data <- get_short_term_recap()
    formatted_cost_difference <- short_term_data$Value[short_term_data$Category == "Cost Difference per Acre (Decrease-Increase)"]
    
    som_yield_data <- generate_som_yield_table()
    # Get the user-selected discount rate (convert to decimal)
    discount_rate <- input$discount_rate / 100
    
    # Check column exists
    revenue_col <- "difference_yield_revenue"
    if (!revenue_col %in% names(som_yield_data)) {
      stop("Error: 'Difference_Yield_Revenue' column not found in SOM yield data.")
    }
    
    # Pivot to wide format: one column per crop
    yield_revenue_wide <- som_yield_data %>%
      select(Year, Crop, difference_yield_revenue) %>%
      tidyr::pivot_wider(
        names_from = Crop,
        values_from = difference_yield_revenue,
        names_glue = "{Crop} Revenue Difference ($/Acre)"
      )
    
    
    combined_results <- yield_revenue_wide %>%
      mutate(
        Total_Crop_Yield_Revenue = rowSums(select(., ends_with("Revenue Difference ($/Acre)")), na.rm = TRUE),
        Short_Term_Cost_Difference = as.numeric(gsub("[^0-9.-]", "", formatted_cost_difference)),
        Total_Dollar = (Short_Term_Cost_Difference + Total_Crop_Yield_Revenue)/(1+discount_rate)^Year
      )
    
    return(combined_results)
  })
  
  # Calculate the total of Total_Dollar column
  total_dollar_sum <- reactive({
    sum(combined_results_data()$Total_Dollar, na.rm = TRUE) %>%
      round(2)  # Round to 2 decimal places for clarity
  })
  
  output$combined_results <- renderUI({
    results_table <- renderTable({
      combined_results_data()
    })
    
    total_value <- total_dollar_sum()
    
    tagList(
      results_table,
      h3(paste("Total of Total_Dollar: $", total_value, "per acre accumulated over 10 years"))
    )  })
  
  ######Closing bracket, do not delete################
  
}

shinyApp(ui = ui, server = server)

