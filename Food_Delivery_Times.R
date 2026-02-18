#---libraries---------------------------------------------->>>
library(dplyr)# to manipulate data
library(ggplot2)# to insights
library(readr)# for reading csv file
library(patchwork)#to concatenate plots
library(rpart)# decision tree
library(rpart.plot)# decision tree plot
library(shiny)# to made shiny app
library(bslib)# for slides

#---reading-data------------------------------------------->>>
df <- read_csv("Data_Set_Path")# to read & save our csv file in variable

#---Cleaning----------------------------------------------->>>
sum(duplicated(df))# to check if there duplicated rows

str(df)# to check our data structure

# check out lairs in each numeric column
boxplot(df$Courier_Experience_yrs)
boxplot(df$Preparation_Time_min)
boxplot(df$Distance_km)
boxplot(df$Order_ID)
# after checked we seven and removed out lairs
outliers<-boxplot(df$Delivery_Time_min)
df <- df[-which(df$Delivery_Time_min %in% outliers$out),]
boxplot(df$Delivery_Time_min)

# to check the NA's in each column
sum(is.na(df$Delivery_Time_min))
sum(is.na(df$Courier_Experience_yrs))
sum(is.na(df$Preparation_Time_min))
sum(is.na(df$Vehicle_Type))
sum(is.na(df$Time_of_Day))
sum(is.na(df$Traffic_Level))
sum(is.na(df$Weather))
sum(is.na(df$Distance_km))
sum(is.na(df$Order_ID))

# Function to get mode
get_mode <- function(x) {
  # to get each unique element at column and assigned to the variable uniq without NA's
  uniq <- unique(x[!is.na(x)])
  #the function is used to take out nas and then we take the unique values 
  uniq[which.max(tabulate(match(x, uniq)))]
}# the function is used to match between the values and the unique and then we take the numbers then we take the max and then we take the unique 

# handling NA's by apply our get_mode function in each category column
df$Weather[is.na(df$Weather)] <- get_mode(df$Weather)

df$Traffic_Level[is.na(df$Traffic_Level)] <- get_mode(df$Traffic_Level)

df$Time_of_Day[is.na(df$Time_of_Day)] <- get_mode(df$Time_of_Day)

# to handle NA's by applying median function to get the median value to numeric column 
df$Courier_Experience_yrs[is.na(df$Courier_Experience_yrs)] <- median(df$Courier_Experience_yrs, na.rm = TRUE)
 
#Alfateh

# calculate and adding speed column
df$Delivery_Speed <- c(as.numeric(df$Distance_km / (df$Delivery_Time_min/60)))

# adding total Time column
df$total_time <- c(as.numeric(df$Preparation_Time_min+df$Delivery_Time_min))

# adding Late_Delivery column
 df$Late_Delivery <- ifelse(df$Delivery_Time_min > 60, "Late", "On Time")

# adding customer_rate column
df$customer_rate <- with(df,ifelse(Delivery_Time_min <= 20 ,"5 stars",
                                  ifelse(Delivery_Time_min <= 30 ,"4 stars",
                                         ifelse(Delivery_Time_min <= 40,"3 stars",
                                                ifelse(Delivery_Time_min <= 60,"2 stars","1 stars")))))
#remove the first column
df<-df[,-c(1)]

# Summarize data
summary(df)

# Extract our cloned csv file
# I export my cleaned data frame to a CSV file
write.csv(df,
          file = "Cleaned_Data_Set_Path",
          row.names = FALSE)

#---Data-Visualization------------------------------------->>>
#---Overall-spread-of-delivery-time------------------------>>>
#  Histogram
p1 <- ggplot(df, aes(x = Delivery_Time_min + Preparation_Time_min)) +
  geom_histogram(fill = "skyblue") +
  labs(title = "Total_Time", x = "Total_Time")

#Boxplot
ggplot(df,
      aes(y=Delivery_Time_min+Preparation_Time_min))+
  geom_boxplot(fill = "pink")+
  labs(title = "Total_Time", y = "Total_Time" )

ggsave( "Overall-spread-of-delivery-time.png",
        plot = p1,
        path = "plots_Path",
        width = 10,
        height =7)

#---Delivery-performance-by-vehicle------------------------>>>
# Boxplot
p2<-ggplot(df,
       aes(x=Vehicle_Type, y = Delivery_Time_min),
)+geom_boxplot(fill = "orange")+
  labs(title = "Delivery-performance-by-vehicle")

ggsave( "Delivery-performance-by-vehicle.png",
        plot = p2,
        path = "plots_Path",
        width = 10,
        height =7) 
#---Rating-vs-delivery-time-------------------------------->>>
p3 <- ggplot(df, aes(x = customer_rate, y = Delivery_Time_min)) +
  geom_boxplot(fill = "pink") +
  labs(title = "Rating-vs-delivery-time")
ggsave( "Rating-vs-delivery-time.png",
        plot = p3,
        path = "plots_Path",
        width = 10,
        height =7)

#---Distribution-of-key-features--------------------------->>>
# Histogram
p4_1 <- (ggplot(df, aes(x = Distance_km)) +
           geom_histogram(fill = "orange")) +
        (ggplot(df, aes(x = Preparation_Time_min)) +
           geom_histogram(fill = "skyblue"))

ggsave( "Distance_km&Preparation_Time_min.png",
        plot = p4_1,
        path = "plots_Path",
        width = 10,
        height =7)

p4_2 <- (ggplot(df, aes(x = Courier_Experience_yrs)) +
           geom_histogram(fill = "skyblue")) +
        (ggplot(df, aes(x = Delivery_Speed)) +
           geom_histogram(fill = "orange"))

ggsave( "Courier_Experience_yrs&Delivery_Speed.png",
        plot = p4_2,
        path = "plots_Path",
        width = 10,
        height =7)

#---Unusual-points-per-category---------------------------->>>
# Boxplot
p5<-ggplot(data = df, aes(x = Traffic_Level,
                          y = Delivery_Time_min, 
                          fill = Weather))+
  geom_boxplot()+
  labs(title = "Traffic_Level VS Delivery_Time_min", x= "Traffic_Level", y = "Delivery_Time_mi")

ggsave("Traffic_Level_VS_Delivery_Time_min.png",
       plot = p5,
       path = "plots_Path",
       width = 10,
       height = 5)

#---Orders-per-category------------------------------------>>>
# Barplot
p6 <- ggplot(data = df, aes(x = Weather, fill = Traffic_Level)) +
  geom_bar() +
  labs(title = "Weather")

ggsave("Weather&Traffic_Level.png",
       plot = p6,
       path = "plots_Path",
       width = 10,
       height = 5)
#---Distance-vs-time-relationship-------------------------->>>
# SmoothLine
p7=ggplot(df,
       aes(x=Distance_km,y=Delivery_Time_min))+
  geom_point()+
  geom_smooth(method = lm,fill = "skyblue")
ggsave( "Distance-vs-time-relationship.png",
        plot = p7,
        path = "plots_Path",
        width = 10,
        height =7)
#---Delivery-time-by-time-of-day--------------------------->>>
# Boxplot
p8=ggplot(df,
       aes(x=Time_of_Day,y=Delivery_Time_min, fill = Time_of_Day))+
  geom_boxplot()

ggsave("Delivery-time-by-time-of-day.png",
        plot = p8,
        path = "plots_Path",
        width = 10,
        height =7)
#---K-means------------------------------------------------>>>
numeric_data = df[,-c(2,3,4,5,10,11,12)]

scale_data = scale(numeric_data)


kmeans_model = kmeans(scale_data, centers = 3)

kmeans_model

df$clusters <- kmeans_model$cluster

agr<-aggregate(df,cbind(Distance_km,
                Delivery_Time_min,
                Preparation_Time_min,
                Courier_Experience_yrs,
                Delivery_Speed) ~ clusters,
          FUN = mean)

write.csv(scale_data,
          file = "Scale_data_Path",
          row.names = FALSE)

write.csv(numeric_data,
          file = "numeric_data_Path",
          row.names = FALSE)

write.csv(agr,
          file = "aggregate_Path",
          row.names = FALSE)

#---K-means-Plot------------------------------------------->>>
plot(df$Distance_km,
     df$Delivery_Time_min,
     col=df$clusters,
     xlab="Distance_km",
     ylab="Delivery_Time_min")

#---Decision-Tree------------------------------------------>>>
## 1) I split the data into training and test sets -----------------------
n <- nrow(df)
train_idx <- sample(1:n, size = 0.7 * n)   # I take 70% of the rows for training

train <- df[train_idx, ]
test  <- df[-train_idx, ]

write.csv(train,
          file = "Training_Data_Set_Path",
          row.names = FALSE)
write.csv(test,
          file = "Testing_Data_Set_Path",
          row.names = FALSE)

# I convert Late_Delivery to a factor for classification
train$Late_Delivery <- factor(train$Late_Delivery,
                              levels = c("Late", "On Time"))
test$Late_Delivery  <- factor(test$Late_Delivery,
                              levels = c("Late", "On Time"))

## 2) Classification tree: predict late vs on-time ----------------------

tree_class <- rpart(Late_Delivery ~ Preparation_Time_min +
                      Courier_Experience_yrs +
                      Weather + Traffic_Level +
                      Time_of_Day + Vehicle_Type,
                    data   = train,
                    method = "class",   # I use a classification tree
                    minsplit = 20)      # I require at least 20 records to split

# I visualize the classification tree
rpart.plot(tree_class, main = "Classification Tree: Late vs On-time")

# I predict Late_Delivery on the test set and compute accuracy
pred_class <- predict(tree_class, newdata = test, type = "class")
accuracy  <- mean(pred_class == test$Late_Delivery)
accuracy   # I check how accurate my classification tree is

## 3) Regression tree: predict delivery time (minutes) ------------------

tree_reg <- rpart(Delivery_Time_min ~ Courier_Experience_yrs +
                    Weather + Traffic_Level + Time_of_Day +
                    Vehicle_Type + Preparation_Time_min +
                    Distance_km,
                  data   = train,
                  method = "anova",   # I use a regression tree
                  minsplit = 20)

# I visualize the regression tree
rpart.plot(tree_reg, main = "Regression Tree: Delivery Time (min)")

# I predict delivery time on the test set and compute RMSE
pred_time <- predict(tree_reg, newdata = test)
rmse <- sqrt(mean((pred_time - test$Delivery_Time_min)^2))
rmse      # I use RMSE to measure my regression error

# ========================= UI =========================
ui <- page_navbar(
  title = "Interactive Data Tool",
  theme = bs_theme(bootswatch = "cosmo"),
  
  
  # ====================== TAB 1 ========================
  
  nav_panel(
    "Upload Data",
    layout_sidebar(
      sidebar = sidebar(
        title = "Upload Options",
        open = "closed",
        width = "300px",
        
        fileInput("file_tab1", "Upload CSV:"),
        
        selectInput(
          "disp",
          "Display:",
          choices = c("Head" = "head", "All" = "all")
        ),
        hr()
      ),
      
      mainPanel(
        h3("Dataset Preview"),
        tableOutput("data_head")
      )
    )
  ),
  
  
  # ====================== TAB 2 ========================
  
  nav_panel(
    "Visualizations",
    
    layout_sidebar(
      sidebar = sidebar(
        title = "Plot Options",
        open = "closed",
        width = "300px",
        
        fileInput("file_tab2", "Upload CSV for Visualization:"),
        
        selectInput(
          "vis_plot",
          "Choose Visualization:",
          choices = c(
            "Delivery Time Distribution (Boxplot)" = "p1",
            "Delivery Time Histogram" = "p1_hist",
            "Vehicle Type vs Delivery Time" = "p2",
            "Distance Distribution" = "p4_dist",
            "Preparation Time Distribution" = "p4_prep",
            "Traffic Level vs Delivery Time" = "p5",
            "Weather & Traffic Level Counts" = "p6",
            "Distance vs Delivery Time (Smooth Line)" = "p7",
            "Delivery Time vs Time of Day" = "p8"
          )
        ),
        
        selectInput(
          "plot_color",
          "Choose Color:",
          choices = c("Blue" = "blue", "Red" = "red", "Green" = "green",
                      "Orange" = "orange", "Purple" = "purple")
        ),
        
        
        conditionalPanel(
          condition = "input.vis_plot == 'p1_hist' ||
                       input.vis_plot == 'p4_dist' ||
                       input.vis_plot == 'p4_prep'",
          sliderInput("bins", "Number of bins:", min = 5, max = 100, value = 30)
        ),
        
        conditionalPanel(
          condition = "input.vis_plot == 'p7'",
          checkboxInput("add_smooth", "Add Smooth Line", TRUE)
        ),
        
        hr()
      ),
      
      mainPanel(
        h3("Visualization Output"),
        plotOutput("vis_plot_output", height = "500px")
      )
    )
  ),
  
  # ====================== TAB 3 ========================
  nav_panel(
    "Clustering Results",
    
    layout_sidebar(
      sidebar = sidebar(
        title = "Cluster Settings",
        open = "closed",
        width = "300px",
        
        fileInput("file_tab3", "Upload CSV for Clustering:"),
        
        selectInput("cluster_k", "K Clusters:", choices = c(2, 3, 4, 5), selected = 3),
        
        hr()
      ),
      
      mainPanel(
        h3("Cluster Plot"),
        plotOutput("cluster_plot"),
        tableOutput("cluster_table")
      )
    )
  ),
  
  # ====================== TAB 4 ========================
  
  nav_panel(
    "Decision Tree",
    
    layout_sidebar(
      sidebar = sidebar(
        title = "Decision Tree Options",
        open = "closed",
        width = "300px",
        
        fileInput("file_tab4", "Upload CSV for Decision Tree:"),
        
        uiOutput("tree_target_ui"),
        uiOutput("tree_features_ui"),
        
        sliderInput("cp", "Complexity Parameter (cp):",
                    min = 0, max = 0.1, value = 0.01, step = 0.005)
      ),
      
      mainPanel(
        h3("Decision Tree Model"),
        plotOutput("treePlot"),
        verbatimTextOutput("treeSummary")
      )
    )
  )
)

# ========================= SERVER =========================
server <- function(input, output, session) {
  
  
  # ====================== TAB 1 ========================
  
  data1 <- reactive({
    req(input$file_tab1)
    read_csv(input$file_tab1$datapath)
  })
  
  output$data_head <- renderTable({
    req(data1())
    df <- data1()
    if (input$disp == "head") head(df) else df
  })
  
  
  # ====================== TAB 2 ========================
  
  data2 <- reactive({
    req(input$file_tab2)
    read_csv(input$file_tab2$datapath)
  })
  
  output$vis_plot_output <- renderPlot({
    req(data2())
    df <- data2()
    
    # safely convert to factors 
    if ("Weather" %in% names(df)) df$Weather <- as.factor(df$Weather)
    if ("Traffic_Level" %in% names(df)) df$Traffic_Level <- as.factor(df$Traffic_Level)
    if ("Time_of_Day" %in% names(df)) df$Time_of_Day <- as.factor(df$Time_of_Day)
    if ("Vehicle_Type" %in% names(df)) df$Vehicle_Type <- as.factor(df$Vehicle_Type)
    
    # =================== plots ============================
    
    if (input$vis_plot == "p1") {
      validate(need("Delivery_Time_min" %in% names(df), "Delivery_Time_min column required for this plot."))
      ggplot(df, aes(y = Delivery_Time_min)) +
        geom_boxplot(fill = input$plot_color) +
        labs(title = "Delivery Time Distribution", y = "Delivery_Time_min")
    }
    
    else if (input$vis_plot == "p1_hist") {
      validate(need("Delivery_Time_min" %in% names(df), "Delivery_Time_min column required for this plot."))
      ggplot(df, aes(x = Delivery_Time_min)) +
        geom_histogram(bins = input$bins, fill = input$plot_color) +
        labs(title = "Delivery Time Histogram", x = "Delivery_Time_min")
    }
    
    else if (input$vis_plot == "p2") {
      validate(need(all(c("Vehicle_Type","Delivery_Time_min") %in% names(df)), "Vehicle_Type and Delivery_Time_min required."))
      ggplot(df, aes(x = Vehicle_Type, y = Delivery_Time_min)) +
        geom_boxplot(fill = input$plot_color) +
        labs(title = "Vehicle Type vs Delivery Time")
    }
    
    else if (input$vis_plot == "p4_dist") {
      validate(need("Distance_km" %in% names(df), "Distance_km column required for this plot."))
      ggplot(df, aes(x = Distance_km)) +
        geom_histogram(bins = input$bins, fill = input$plot_color) +
        labs(title = "Distance Distribution", x = "Distance_km")
    }
    
    else if (input$vis_plot == "p4_prep") {
      validate(need("Preparation_Time_min" %in% names(df), "Preparation_Time_min column required for this plot."))
      ggplot(df, aes(x = Preparation_Time_min)) +
        geom_histogram(bins = input$bins, fill = input$plot_color) +
        labs(title = "Preparation Time Distribution", x = "Preparation_Time_min")
    }
    
    else if (input$vis_plot == "p5") {
      validate(need(all(c("Traffic_Level","Delivery_Time_min") %in% names(df)), "Traffic_Level and Delivery_Time_min required."))
      ggplot(df, aes(x = Traffic_Level, y = Delivery_Time_min, fill = Weather)) +
        geom_boxplot() +
        labs(title = "Traffic Level vs Delivery Time")
    }
    
    else if (input$vis_plot == "p6") {
      validate(need("Weather" %in% names(df), "Weather column required for this plot."))
      ggplot(df, aes(x = Weather, fill = Traffic_Level)) +
        geom_bar() +
        labs(title = "Weather & Traffic Level")
    }
    
    else if (input$vis_plot == "p7") {
      validate(need(all(c("Distance_km","Delivery_Time_min") %in% names(df)), "Distance_km and Delivery_Time_min required."))
      p <- ggplot(df, aes(x = Distance_km, y = Delivery_Time_min)) +
        geom_point(color = input$plot_color)
      if (input$add_smooth) p <- p + geom_smooth(method = "lm")
      p + labs(title = "Distance vs Delivery Time")
    }
    
    else if (input$vis_plot == "p8") {
      validate(need(all(c("Time_of_Day","Delivery_Time_min") %in% names(df)), "Time_of_Day and Delivery_Time_min required."))
      ggplot(df, aes(x = Time_of_Day, y = Delivery_Time_min, fill = Time_of_Day)) +
        geom_boxplot() +
        labs(title = "Delivery Time vs Time of Day")
    }
  })
  
  
  # ====================== TAB 3 =================
  
  data3 <- reactive({
    req(input$file_tab3)
    read_csv(input$file_tab3$datapath)
  })
  
  # cleaned numeric data
  numeric_clean <- reactive({
    df <- data3()
    df_num <- df %>% dplyr::select(where(is.numeric))
    df_num <- na.omit(df_num)
    
    validate(
      need(ncol(df_num) >= 2, "You need at least TWO numeric columns for clustering.")
    )
    
    df_num
  })
  
  
  use_distance_time <- reactive({
    df_num <- numeric_clean()
    all(c("Distance_km", "Delivery_Time_min") %in% colnames(df_num))
  })
  
  output$cluster_plot <- renderPlot({
    req(numeric_clean())
    
    df_num <- numeric_clean()
    k <- as.integer(input$cluster_k)
    
    km <- kmeans(df_num, centers = k)
    
    if (use_distance_time()) {
      plot(df_num$Distance_km,
           df_num$Delivery_Time_min,
           col = km$cluster,
           pch = 19,
           xlab = "Distance_km",
           ylab = "Delivery_Time_min",
           main = paste0("K-means (k=", k, ")"))
      
      # centers: 
      centers_approx <- aggregate(cbind(Distance_km, Delivery_Time_min) ~ km$cluster, data = df_num, FUN = mean)
      points(centers_approx$Distance_km, centers_approx$Delivery_Time_min,
             pch = 8, cex = 2, col = 1:nrow(centers_approx))
      
      legend("topright", legend = paste0("C", 1:k), col = 1:k, pch = 19, bty = "n")
    } else {
      # fallback: 
      cols <- colnames(df_num)[1:2]
      plot(df_num[[cols[1]]], df_num[[cols[2]]],
           col = km$cluster, pch = 19,
           xlab = cols[1],
           ylab = cols[2],
           main = paste0("K-means (k=", k, ")"))
      points(km$centers[, 1], km$centers[, 2],
             pch = 8, cex = 2, col = 1:k)
      legend("topright", legend = paste0("C", 1:k), col = 1:k, pch = 19, bty = "n")
    }
  })
  
  output$cluster_table <- renderTable({
    req(numeric_clean())
    df_num <- numeric_clean()
    k <- as.integer(input$cluster_k)
    km <- kmeans(df_num, centers = k)
    
    if (use_distance_time()) {
      res <- data.frame(Distance_km = df_num$Distance_km,
                        Delivery_Time_min = df_num$Delivery_Time_min,
                        Cluster = km$cluster)
    } else {
      res <- data.frame(df_num, Cluster = km$cluster)
    }
    head(res, 1000)
  })
  
  
  # ====================== TAB 4 ========================
  data4 <- reactive({
    req(input$file_tab4)
    read_csv(input$file_tab4$datapath)
  })
  
  output$tree_target_ui <- renderUI({
    req(data4())
    selectInput("tree_target", "Target Variable:", names(data4()))
  })
  
  output$tree_features_ui <- renderUI({
    req(data4())
    selectInput("tree_features", "Features:", names(data4()), multiple = TRUE)
  })
  
  output$treePlot <- renderPlot({
    req(data4(), input$tree_target, input$tree_features)
    
    df <- data4()
    
    formula <- as.formula(
      paste(input$tree_target, "~", paste(input$tree_features, collapse = "+"))
    )
    
    model <- rpart(formula, data = df,
                   control = rpart.control(cp = input$cp))
    
    rpart.plot(model)
  })
  
  output$treeSummary <- renderPrint({
    req(data4(), input$tree_target, input$tree_features)
    
    df <- data4()
    
    formula <- as.formula(
      paste(input$tree_target, "~", paste(input$tree_features, collapse = "+"))
    )
    
    model <- rpart(formula, data = df,
                   control = rpart.control(cp = input$cp))
    
    summary(model)
  })
  
}

# ========================= Run App =========================
shinyApp(ui, server)