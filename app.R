library(shiny)
library(dplyr)
library(DT)
library(ggplot2)
library(shinydashboard)
library(plotly)
library(lpSolveAPI)
library(lpSolve)

# load files from Canada Nutrient File

nutr_files <- list.files(pattern = "*.rda")
lapply(nutr_files,load,.GlobalEnv)
# format quantities for ingredients

# take everything before parenthesis
ca_measure_name$units <- regmatches(ca_measure_name$MeasureDescription, regexpr("^([^(]+)", ca_measure_name$MeasureDescription, perl = T))
# only take what is in parenthesis
ca_measure_name$description <- gsub("^([^(]+)", "", ca_measure_name$MeasureDescription, perl = T)
# extract numeric values
r <- regexpr("^[[:digit:]]+[\\/\\.]*[[:digit:]]*", ca_measure_name$units)
out <- rep(NA,nrow(ca_measure_name))
out[r!=-1] <- regmatches(ca_measure_name$units, r)
ca_measure_name$numeric <- out
# convert fractions to decimal
fractions <- grep("\\/", ca_measure_name$numeric)
ca_measure_name$numeric[fractions] <- sapply(ca_measure_name$numeric[fractions], function(x) eval(parse(text=x)))
# fill in blank numeric values
ca_measure_name$numeric[is.na(ca_measure_name$numeric)] <- 1
# everything numberic
ca_measure_name$numeric <- round(as.numeric(ca_measure_name$numeric), 5)
# now remove numbers from units
ca_measure_name$units <- gsub("^[[:digit:]]+[\\/\\.]*[[:digit:]]*", "", ca_measure_name$units)


# format ingredient choices for Descriptive Analytics
ca_food_choices <- ca_food_name$FoodID
names(ca_food_choices) <- ca_food_name$FoodDescription

#####################################
#####################################
##### Code to standardize calories, protein, and fats for 100 ml servings of food
###### for Optimization section
###################################################################################
###################################################################################

protein = ca_food_name %>%
  left_join(ca_nutrient_amount) %>%
  left_join(ca_nutrient_name) %>%
  filter(NutrientID == 203) %>% ## Protein
  select(FoodID, FoodDescription, NutrientValue) %>%
  rename(Protein = NutrientValue, Food = FoodDescription)

satfats = ca_food_name %>%
  left_join(ca_nutrient_amount) %>%
  left_join(ca_nutrient_name) %>%
  filter(NutrientID == 606) %>% ## Total Sat Fats
  select(FoodID, NutrientValue) %>%
  rename(SatFats = NutrientValue)       

kcals = ca_food_name %>%
  left_join(ca_nutrient_amount) %>%
  left_join(ca_nutrient_name) %>%
  filter(NutrientID == 208) %>% ## Calories
  select(FoodID, NutrientValue) %>%
  rename(Calories = NutrientValue) 

## Only keeping foods that are of 100ml serving size
servings = ca_measure_name %>%
  inner_join(ca_conversion_factor) %>%
  filter(MeasureID == 341) %>% ## 100ml serving size
  select(FoodID, ConversionFactorValue)

food = protein %>%
  inner_join(satfats) %>%
  inner_join(kcals) %>%
  inner_join(servings) %>%
  mutate(Protein = ConversionFactorValue * Protein / 100, ## gives protein per 1 ml
         SatFats = ConversionFactorValue * SatFats / 100, ## gives sat fats per 1 ml
         Calories = ConversionFactorValue * Calories / 100 ) %>% ## gives calories per 1 ml
  select(FoodID, Food, Protein, SatFats, Calories) 

## format ingredient choices for Optimization
op_food_choices <- food$FoodID
names(op_food_choices) <- food$Food 

# format daily values

daily_value <- read.table("daily_values.txt", sep = "\t", header=T, stringsAsFactors = F)

ui <- navbarPage(title = span( "Fitness and Health App",color="black"),
                 
                 #############
                 #### First, the Descriptive Analytics Panel
                 ##############
                 
                 tabPanel('Descriptive Analytics', 
                          dashboardPage(skin='black',
                                        dashboardHeader(title = "Nutrition and Fitness"),
                                        dashboardSidebar(
                                          selectizeInput(
                                            'food_id', '1. Select your food', choices = ca_food_choices,
                                            options = list(
                                              placeholder = 'Type to search for food',
                                              onInitialize = I('function() { this.setValue(""); }')
                                            )
                                          ),
                                          conditionalPanel('input.food_id != ""', 
                                                           selectizeInput('measure_unit', '2. Unit of Measure', choices = c("Select an ingredient" = "")),
                                                           numericInput('quantity', '3. Quantity of Food', value = 100, min = 0, step = 1)),
                                          actionButton("add", "Add food"),
                                          actionButton("remove", "Remove food"),
                                          numericInput("serving", "Number of servings contained", min = 0.01, step = 1, value = 1),
                                          tags$p("Note: All nutrient information is based on the Canadian Nutrient File. Nutrient amounts do not account for variation in nutrient retention and yield losses of ingredients during preparation. % daily values (DV) are taken from the Table of Daily Values from the Government of Canada. This data should not be used for nutritional labeling.")
                                        ),
                                        dashboardBody(
                                          
                                          tags$head(tags$style(HTML('
                                                                    /* logo */
                                                                    .skin-grey .main-header .logo {
                                                                    background-color: #000000;
                                                                    }'))),

                                          tags$head(tags$style(HTML('
                                                                    /* logo */
                                                                    .skin-grey .main-header .navbar {
                                                                    background-color: #000000;
                                                                    }'))),       

                                          tags$head(tags$style(HTML('
                                                                    /* logo */
                                                                    .skin-grey .main-sidebar {
                                                                    background-color: #000000;
                                                                    }'))),
                                  
                                          fluidRow(
                                            valueBoxOutput("calories"),
                                            valueBoxOutput("over_nutrient"),
                                            valueBoxOutput("rich_nutrient")
                                          ),
                                          fluidRow(
                                            box(title = "Your Meal",
                                                solidHeader = T,
                                                width = 4,
                                                background = 'yellow',
                                                collapsible = T,
                                                div(DT::DTOutput("ing_df"), style = "font-size: 90%;")),
                                            box(title = "Your Macronutrients", solidHeader = T,
                                                width = 8, collapsible = T,
                                                background = 'yellow',
                                                plotlyOutput("macro_plot"))
                                          ), # row
                                          fluidRow(
                                            box(title = "Nutrition Table",
                                                solidHeader = T,
                                                width = 4, 
                                                background = 'yellow',
                                                collapsible = T,
                                                collapsed = F,
                                                tags$p(textOutput("serving", inline = T)),
                                                div(DT::DTOutput("nutrient_table"), style = "font-size: 90%;")),
                                            box(title = "Minerals", solidHeader = T,
                                                width = 8, collapsible = T,
                                                background = 'yellow',
                                                plotlyOutput("mineral_plot"))
                                          ),# row
                                          fluidRow(
                                            box(title = "Vitamins", solidHeader=T,
                                                width = 12, collapsible = T,
                                                background = 'yellow',
                                                plotlyOutput("vitamin_plot"))
                                          ) # row
                                          # body
                                          
                                          ))      
                          
                          
                          
                                          ),
                 
                 #############
                 #### Second, the Optimization Panel
                 ##############
                 
                 
                 tabPanel('Optimization', 
                          dashboardPage(
                            dashboardHeader(title = "Nutrition and Fitness"),
                            dashboardSidebar(
                              sliderInput(
                                'protein_goal', 
                                "1. What is your Protein Goal? (Minimum amount of protein you want in your meal)", 
                                min = 0, max = 300, value = 50, ticks = FALSE
                              ),
                              sliderInput(
                                'satfats_goal', 
                                "2. What is your Saturated Fats Goal? (Maximum amount of saturated fats in your meal)", 
                                min = 0, max = 300, value = 25, ticks = FALSE          
                              ),
                              selectizeInput(
                                'op_food_id', '3. Select the food you would like to eat for your meal.', choices = op_food_choices,
                                options = list(
                                  placeholder = 'Type to search for food',
                                  onInitialize = I('function() { this.setValue(""); }')
                                )
                              ),
                              actionButton("op_add", "Add food"),
                              actionButton("op_remove", "Remove food"),
                              sliderInput(
                                'cal', 
                                "4. What are the minimum calories that you want to consume for each food?", 
                                min = 0, max = 3000, value = 100, ticks = FALSE          
                              ),
                              actionButton('optimize', "Minimize my calories!"),
                              tags$p("Note: All nutrient information is based on the Canadian Nutrient File. Nutrient amounts do not account for variation in nutrient retention and yield losses of ingredients during preparation. % daily values (DV) are taken from the Table of Daily Values from the Government of Canada. This data should not be used for nutritional labeling.")
                            ),
                            dashboardBody(
                              
                              tags$head(tags$style(HTML('
                                                        /* logo */
                                                        .skin-blue .main-header .logo {
                                                        background-color: #000000;
                                                        }'))),
                        
                              tags$head(tags$style(HTML('
                                                        /* logo */
                                                        .skin-blue .main-header .navbar {
                                                        background-color: #000000;
                                                        }'))),       
                        
                              tags$head(tags$style(HTML('
                                                        /* logo */
                                                        .skin-blue .main-sidebar {
                                                        background-color: #000000;
                                                        }'))),
                        
                              tags$head(tags$style(HTML('
                                                        .box.box-solid.box-primary>.box-header {
                                                        color:000000;
                                                        background:#000000}'))),
                              
                              
                              box(title = "Instructions",
                                  status = "primary",
                                  solidHeader = F,
                                  collapsible = F,
                                  width = 12,
                                  background = 'yellow',
                                  textOutput('text1') ),
                              
                              
                              fluidRow(
                                box(title = "Your Meal's Nutrition Facts",
                                    solidHeader = T,
                                    width = 6,
                                    background = 'yellow',
                                    collapsible = T,
                                    textOutput('text2'),
                                    div(DT::DTOutput("op_df"), style = "font-size: 90%;"))
                                ,
                                
                                box(title = "Results",
                                    status = "primary",
                                    solidHeader = F,
                                    collapsible = F,
                                    width = 6,
                                    background = 'yellow',
                                    textOutput('text3') ),
                                
                                valueBoxOutput('kcal', width = 2),
                                valueBoxOutput('prot', width = 2),
                                valueBoxOutput('sf', width = 2)
                                
                              ),
                              fluidRow(
                                box(title = "Food Contribution",
                                    solidHeader = T,
                                    width = 12, 
                                    background = 'yellow',
                                    collapsible = T,
                                    collapsed = F,
                                    dataTableOutput('outcome'))
                              )
                              ))))





# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  ### Non UI Part for Descriptive Analytics
  #########################################
  #########################################
  
  # make reactive to store ingredients
  ing_df <- shiny::reactiveValues()
  ing_df$df <- data.frame("quantity" = numeric(), 
                          "units" = character(), 
                          "ingredient_name" = character(), 
                          "FoodID" = numeric(), 
                          stringsAsFactors = F)
  ing_df$measure <- data.frame("numeric" = numeric(),
                               "units" = character(),
                               "description" = character(),
                               "ConversionFactorValue" = numeric(),
                               "MeasureID" = numeric(),
                               "FoodID" = numeric(),
                               stringsAsFactors = F)
  # step 1 get singular ingredient
  measure_df <- eventReactive(input$food_id,{
    measure_df <- ca_food_name[ca_food_name$FoodID==input$food_id, "FoodID"] %>% 
      left_join(ca_conversion_factor) %>% 
      left_join(ca_measure_name) %>% 
      select(numeric, units, description, ConversionFactorValue, MeasureID, FoodID) 
    # measure_df <- rbind(measure_df, c(numeric = 100, units = "g", description = "", ConversionFactorValue = 1, MeasureID = ""))
    measure_df
  })
  # step 2 update the measure unit for singular ingredient
  observe({
    units <- unique(paste(measure_df()$units, measure_df()$description))
    updateSelectInput(session, "measure_unit", "2. Measure Unit", choices = units)
  })
  # step 3 update the ingredient dataframe
  observeEvent(input$remove, {
    isolate(ing_df$df<-ing_df$df[-(nrow(ing_df$df)),])
    isolate(ing_df$measure <- ing_df$measure[-nrow(ing_df$measure),])
  })
  observeEvent(input$add, {
    isolate(ing_df$df[nrow(ing_df$df) + 1,] <- c(input$quantity,
                                                 input$measure_unit, 
                                                 names(ca_food_choices[ca_food_choices == input$food_id]), 
                                                 as.numeric(input$food_id)))
    # get actual working ingredient dataframe for dplyr
    input_measure <- measure_df()
    input_measure <- input_measure[paste(measure_df()$units, measure_df()$description) == input$measure_unit, ]
    if(nrow(input_measure) > 1){
      input_measure <- input_measure[which(abs(input_measure$numeric-input$quantity)==min(abs(input_measure$numeric-input$quantity))),]
    }
    isolate(ing_df$measure[nrow(ing_df$measure) + 1, ] <- input_measure)
    # update choices
    updateNumericInput(session, 'quantity', '3. Quantity', 1)
    updateSelectizeInput(session, 'measure_unit', '2. Measure Unit')
    updateSelectInput(session, 'food_id', '1. Ingredient', choices = ca_food_choices)
  })
  # main nutrition data frame
  nutrition_df <- reactive({
    measure_food_df <- ing_df$measure
    ing_quant <- ing_df$df
    measure_food_df$quantity <- ing_quant$quantity
    measure_food_df <- measure_food_df %>%
      left_join(ca_nutrient_amount) %>%
      left_join(ca_nutrient_name) %>%
      # filter(NutrientID %in% select_nutrients) %>%
      mutate(NutrientName = tolower(NutrientName)) %>%
      mutate(NutrientValue = as.numeric(NutrientValue) * as.numeric(ConversionFactorValue) * as.numeric(quantity) / as.numeric(numeric) / input$serving) %>%
      select(NutrientName, NutrientValue, NutrientID, NutrientUnit, ConversionFactorValue, quantity, FoodID) %>% 
      group_by(NutrientName) %>% 
      summarize(Value = round(sum(NutrientValue, na.rm = T),2),
                Unit = first(NutrientUnit),
                NutrientID = first(NutrientID))
    
    measure_food_df
  })
  # display nutrients necessary for label
  nutrient_table <- reactive({
    select_nutrients <- c(208, 204, 606, 605, 601, 307, 205, 291, 269, 203, 814, 401, 301, 303)
    measure_food_df <- nutrition_df() %>% filter(NutrientID %in% select_nutrients)
    measure_food_df <- measure_food_df[order(match(measure_food_df$NutrientID, select_nutrients)),] %>%
      select(NutrientName, Value, Unit)
    measure_food_df
  })
  # df with dv%
  dv_df <- reactive({
    dv_df <- daily_value %>% left_join(nutrition_df())
    # hack for total sat fats and trans fats
    dv_df$Value[2] <- sum(nutrition_df()$Value[nutrition_df()$NutrientID %in% c(605, 606)], na.rm = T)
    dv_df$Unit[2] <- "g"
    dv_df$pct_dv <- round(dv_df$Value / dv_df$DV, 3) * 100
    dv_df
  })
  
  output$macro_plot <- renderPlotly({
    df_macro <- dv_df() %>% filter(Group == "macronutrients")
    plot_macro <- ggplot(df_macro) + 
      geom_point(aes(x = Nutrient, y = pct_dv, fill = pct_dv > 100, size=10, colour = pct_dv > 100)) +
      labs(x = "Nutrient", y = "% Daily Value") + 
      theme_gray() + 
      ylim(0, NA) +
      geom_hline(yintercept = 100) +
      scale_colour_manual(name = 'pct_dv > 100', values = setNames(c('red','green'),c(T, F))) +
      scale_fill_manual(name = 'pct_dv > 100', values = setNames(c('red','green'),c(T, F))) +
      theme(panel.background = element_rect(fill = "mintcream"), 
            legend.position = "none") +
      scale_x_discrete(labels = c("Cholesterol", "Fat", "Fibre", "Sodium",  "Sugars", "Saturated and \n Trans Fats"))
    ggplotly(plot_macro) 
  })
  output$mineral_plot <- renderPlotly({
    df_min <- dv_df() %>% filter(Group == "mineral")
    plot_min <- ggplot(df_min) + 
      geom_point(aes(x = Nutrient, y = pct_dv, fill = pct_dv > 100, size=10, colour = pct_dv > 100)) +
      labs(x = "Nutrient", y = "% Daily Value")  + 
      theme_gray() + 
      ylim(0, NA) +
      geom_hline(yintercept = 100) +
      scale_colour_manual(name = 'pct_dv > 100', values = setNames(c('red','green'),c(T, F))) +
      scale_fill_manual(name = 'pct_dv > 100', values = setNames(c('red','green'),c(T, F))) +
      theme(
        legend.position = "none",
        panel.background = element_rect(fill = "mintcream"))
    ggplotly(plot_min)
  })
  output$vitamin_plot <- renderPlotly({
    df_vit <- dv_df() %>% filter(Group == "vitamin")
    req(input$quantity)
    plot_vit <- ggplot(df_vit) + 
      geom_point(aes(x = Nutrient, y = pct_dv, fill = pct_dv > 100, size=10, colour = pct_dv > 100)) +
      geom_hline(yintercept = 100) +
      labs(x = "Nutrient", y = "% Daily Value")  + 
      theme_gray() + 
      ylim(0, NA) +
      scale_colour_manual(name = 'pct_dv > 100', values = setNames(c('red','green'),c(T, F))) +
      scale_fill_manual(name = 'pct_dv > 100', values = setNames(c('red','green'),c(T, F))) +
      theme(legend.position = "none",
            panel.background = element_rect(fill = "mintcream"),
            axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
    ggplotly(plot_vit)
  })
  # dt indicator
  output$ing_df <- DT::renderDataTable(ing_df$df[,1:3], 
                                       # colnames = c("Quantity", "Units", "Ingredient"), 
                                       rownames=F, options = list(pageLength = 5))
  output$nutrient_table <- DT::renderDataTable(nutrient_table())
  # value boxes
  output$calories <- renderValueBox({
    valueBox(paste0(nutrition_df()$Value[nutrition_df()$NutrientID == 208], "kcal"), 
             "Calories", icon = icon("fire"), color = "yellow")
  })
  output$over_nutrient <- renderValueBox({
    nutrition_df <- dv_df() %>% 
      # filter(NutrientID %in% c(601, 204, 307, 269, 0)) %>% 
      tidyr::drop_na(pct_dv) %>% filter(pct_dv > 100)
    if(nrow(nutrition_df) > 0){
      valueBox("Over Daily Value", HTML(paste0(nutrition_df$Nutrient, sep="<br>")), icon = icon("exclamation-triangle"), color = "black")
    } else {
      valueBox("All nutrients", "below recommended DV", icon = icon("exclamation-triangle"), color = "yellow")
    }
  })
  output$rich_nutrient <- renderValueBox({
    nutrition_df <- dv_df() %>% tidyr::drop_na(pct_dv) %>% filter(pct_dv >= 50) %>% filter(pct_dv < 100)
    if(nrow(nutrition_df) > 0){
      valueBox("High levels* of ", HTML(paste0(c(nutrition_df$Nutrient,"*above 50% recommended DV"),  sep="<br>")), icon = icon("exclamation-triangle"), color = "black")
    } else {
      valueBox(HTML("All nutrients"), "below 50% recommended DV", icon = icon("exclamation-triangle"), color = "yellow")
    }
    
  })
  output$serving <- renderText(paste("for 1 serving (", input$serving, "servings in recipe)"))
  
  #######
  ### Optimization
  #########
  
  ##################
  ### NON UI Optimization
  #######################
  # make reactive to store ingredients
  op_df <- shiny::reactiveValues()
  op_df$df <- data.frame( "FoodID" = numeric(),
                          "Food" = character(), 
                          "Protein" = numeric(),
                          "SaturatedFats" = numeric(),
                          "Calories" = numeric(),
                          stringsAsFactors = F)
  
  # step 1 get singular ingredient
  op_ing <- eventReactive(input$op_food_id,{
    op_ing <- food[food$FoodID==input$op_food_id, "FoodID"]
    op_ing
  })
  
  # step 2 update the ingredient dataframe
  observeEvent(input$op_remove, {
    isolate(op_df$df<-op_df$df[-(nrow(op_df$df)),])
    
  })
  observeEvent(input$op_add, {
    isolate(op_df$df[nrow(op_df$df) + 1,] <- c(input$op_food_id,
                                               food[food$FoodID == input$op_food_id, "Food"],
                                               food[food$FoodID == input$op_food_id, "Protein"],
                                               food[food$FoodID == input$op_food_id, "SatFats"],
                                               food[food$FoodID == input$op_food_id, "Calories"]
    ))
    updateSelectInput(session, 'op_food_id', '3. Select the food you would like to eat for your meal.', choices = op_food_choices)
  })
  
  output$text1 = renderText({ 'Our product serves our customers by matching protein and saturated fat fitness goals of our clients to the foods they would like to eat. Using this information, we select the optimal quantity of each food item to minimize caloric intake. First, select your protein goal (in grams). Second, select your saturated fats protein goal (in grams). Lastly, add the food items you would like to eat and the minimum number of calories that you want to consume for each food. Finally, click optimize, and you will receive the optimal quanity (in mL) of each food that will minimize your caloric intake.'})
  output$text2 = renderText({ 'Protein and saturated fat values are based in grams per mL while caloric values are based in kilocalories per mL.'})
  ## Rendering the data table
  output$op_df <- DT::renderDataTable(op_df$df[,2:5], 
                                      # colnames = c("Quantity", "Units", "Ingredient"), 
                                      rownames=F, options = list(pageLength = 5))
  
  ## Using lpSolve linear program
  
  observeEvent(input$optimize, {
    lps.model <- make.lp(nrow=0, ncol=nrow(op_df$df))
    # what kind of decision variables do you have? "real", "integer", or "binary"?
    for (col in 1:nrow(op_df$df)) {
      set.type(lps.model, columns=col, type="real")
    }
    
    # Give our model a name
    name.lp(lps.model, name="Fitness")
    # set objective function
    lp.control(lps.model, sense="min")
    set.objfn(lps.model, obj=op_df$df$Calories)
    
    # define constraints
    add.constraint(lps.model, op_df$df$Protein, ">=", input$protein_goal)
    add.constraint(lps.model, op_df$df$SaturatedFats, "<=", input$satfats_goal)
    
    for (item in 1:nrow(op_df$df)) {
      if (item == 1) {
        add.constraint(lps.model, c(as.numeric(op_df$df[item, 'Calories']), rep(0, nrow(op_df$df) - 1)), ">=", input$cal)
      } else if (item == nrow(op_df$df)) {
        add.constraint(lps.model, c(rep(0, nrow(op_df$df) - 1), as.numeric(op_df$df[item, 'Calories'])), ">=", input$cal)
      } else {
        add.constraint(lps.model, c(rep(0, item - 1), as.numeric(op_df$df[item, 'Calories']), rep(0, nrow(op_df$df) - item)), ">=", input$cal)
      }
      
    }
    
    # lets review our LP model
    print(lps.model)
    
    if (solve(lps.model) != 0) {
      ## If  there is no solution, then we need to tell the user
      output$text3 = renderText({ 'Based on your requirements, this meal will not work. Change either the protein goal, saturated fats goal, or minimum calories and try again!'})
    } else {
      ## Need reactive elements in here to notify user of how much they consume of each food, both in quantity, 
      ## protein, saturated fats, and calories
      
      print(get.variables(lps.model)) # optimal solution of d.v.'s (i.e. our decisions to make)
      
      quantity = as.matrix(get.variables(lps.model))
      colnames(quantity) = 'Quantity'
      values = t(as.matrix(get.variables(lps.model)))
      
      print(values)
      totals = values %*% as.matrix(op_df$df[, 3:5]) ## Give totals
      print(totals)
      m2 <- matrix(data = apply(quantity, 2, function(x) rep(x, 3)), ncol = 3)
      print(m2)
      
      foodspecific = m2 * as.matrix(op_df$df[, 3:5])
      
      foodspecific = cbind(op_df$df[, 2], as.data.frame(cbind(foodspecific, quantity)))
      
      colnames(foodspecific)[1] = 'Food'
      
      for (i in 2:5) {
        foodspecific[, i] = round(foodspecific[,i], 2)
      }
      
      output$outcome = renderDataTable({
        foodspecific
      })
      
      output$text3 = renderText({ 'Based on your requirements, these are your total calories, protein, and saturated fats for the meal!'})
      output$kcal = renderValueBox({
        valueBox(subtitle = 'Calories for Meal',
                 value = as.integer(totals[1, 3]), #calories
                 color = 'yellow',
                 icon = icon("fire"))
        # optimal obj. value (i.e. our maximum profit)
      })
      output$prot = renderValueBox({
        valueBox(subtitle = 'Protein for Meal',
                 value = as.integer(totals[1, 1]), #protein
                 color = 'yellow',
                 icon = icon("bacon"))
        # optimal obj. value (i.e. our maximum profit)
      })
      output$sf = renderValueBox({
        valueBox(subtitle = 'Saturated Fats for Meal',
                 value = as.integer(totals[1, 2]), #satfats
                 color = 'yellow',
                 icon = icon("weight"))
        # optimal obj. value (i.e. our maximum profit)
      })
      
    }
  })

  
}

# Run the application 
shinyApp(ui = ui, server = server)


  
  