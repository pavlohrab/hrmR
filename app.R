# Load required libraries

library(shiny)
library(MBmca)
library(ggplot2)
library(dplyr)
library(reshape2)
library(mclust)
library(plotly)
library(xtable)
library(shinyjs)
library(tidyverse)
library(factoextra)
library(cluster)
library(dbscan)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("hrmR - HRM in R"),

    # Sidebar 
    useShinyjs(),
    sidebarLayout(
        # Uploading the file 
        sidebarPanel(
            fileInput("hmr",
                      "Upload melting dataset"),
            # Set the curve parameters for data cleaning 
            h3("Curve parameters"),
            sliderInput("n",
                        "Resolution:",
                        min = 1,
                        max = 5,
                        value=1,
                        step=0.1),
            sliderInput("smooth",
                        "Smooth the curves (lower better):",
                        min = 0.6,
                        max = 1.1,
                        value=0.95,
                        step = 0.05),
            # Input for temperature trimming
            numericInput("min_tm", "Min T for melt curve analysis", value = 0),
            numericInput("max_tm", "Max T for melt curve analysis", value = 100),
            actionButton("trim", "Trim temps"),
            # Input to exclude column data from the analysis
            textInput("drop_txt", "Enter ID of columns to exclude via comma or via - range"),
            actionButton("drop", "Exclude"),
            actionButton("reset", "Reset data"),
            # Some clustering options. Can choose an algorithm and their specific arguments
            h3("Clustering options"),
            checkboxInput("cluster", "Enable clustering values"),
            selectInput("clust_type", "Choose clustering algoritm:",
                        list("K-Means" =0, "Hierarchical"=1, "DBScan"=3,"Model-based"=2), selected = 2),
            h4("Clustering would not be done until you specify number of clusters below", id="kmeans_help"),
            numericInput("kmeans_clusters", "Enter number of groups to cluster data in", value = 2),
            numericInput("eps", "Enter eps value", value = 0.2),
            actionButton("kmeans_cluster_action", "Cluster"),
            # Analysis options for HMR. More data trimming and actually can cluster on whole row, not only melting temps
            h3("HMR analysis options"),
            h4(strong("Note: All clustering will be based on used in diff plot data, not on Tm")),
            checkboxInput("enable_hrm", "Enable HRM analysis mode"),
            textInput("ref_column", "Enter reference column to center diff plot"),
            actionButton("diff_plot", "Plot"),
            numericInput("min_tm_hrm", "Min T for HRM analysis", value = 0),
            numericInput("max_tm_hrm", "Max T for HRM analysis", value = 100),
            actionButton("trim_hrm", "Trim temps")
            
        ),

        # All inputs are in the tabsets
        mainPanel(
            tabsetPanel(tabPanel("Melting curves", value = 1, plotlyOutput("melt_curve")),
                        tabPanel("Melting temps", value = 2, uiOutput("tmps"), uiOutput("ind_tmps")),
                        tabPanel("HMR analysis",  value = 3, plotlyOutput("norm_curve"), plotlyOutput("diffplot")),
                        tabPanel("Clustering",  value = 4, plotlyOutput("wss"), plotlyOutput("silhouette"), plotlyOutput("gap_stat")),
                        tabPanel("Used data",  value = 1, uiOutput("used_data")),
                        type = "tabs", id="main"
                        )
           
        )
    )
)

# Define server logic 
server <- function(input, output) {
    
    # Define rective values.
    vals <- reactiveValues(df = NULL, scaled_df_der=NULL, scaled_df=NULL,  plot_df = NULL, clust_class = NULL, 
                           diff_cluster_action = FALSE, clust_temps = NULL ,matr = NULL, trim_hrm = FALSE, 
                           diffdata_cluster = NULL, diff_plot = FALSE, tm_min= 0, tm_max=100, diffdata = NULL,
                           clust_data = FALSE, kmeans_cluster_actions = FALSE, cluster_data = NULL, trim = FALSE
                           )
    # Define master reactive value, which is basically a list of inputs. Then it will pass to the observeEvent().
    # Therefore changes on multiple occasions are possible
    toListen <- reactive({
        list(input$hmr,input$smooth,input$n,
             input$trim, input$drop, input$reset, input$kmeans_cluster_action)
    })
    # Define listen function for diffplot ploting
    DiffplotListen <- reactive({
        list(input$diff_plot,input$drop, input$reset,
             vals$scaled_df, input$kmeans_cluster_action, input$trim_hrm)
    })
    
    # Logic id the ref column is selected for diffplot
    observeEvent(input$ref_column, {
        vals$diff_plot == TRUE
    })
    
    # Logic if trim button is pressed. Process input values to the reactive ones
    observeEvent(input$trim,{
        req(input$hmr)
        vals$kmeans_cluster_actions <-  TRUE
        vals$tm_min <- input$min_tm
        vals$tm_max <- input$max_tm
    })
    
    # Logic if the trim button in HRM mode is pressed. Pass TRUE to the reactive value
    observeEvent(input$trim_hrm,{
        vals$trim_hrm = TRUE
    })
    # Logic if the HRM mode is triggered, pass TRUE/FALSE to the reactive values 
    observeEvent(input$enable_hrm,{
        if (input$enable_hrm == TRUE){
            vals$clust_data <-  TRUE
        }else{
            vals$clust_data <- FALSE
        }
    })
    # Logic if the if the button cluster is pressed
    observeEvent(input$kmeans_cluster_action,{
        vals$kmeans_cluster_actions <-  TRUE
        vals$diff_cluster_action <- TRUE
    })
    # Hide/Show cluster_type selector if cluster checkbox is triggered
    observeEvent(input$cluster,{
        if (input$cluster == FALSE){
            hideElement(selector = "#clust_type")
            hideElement(selector = "#kmeans_cluster_action")
        } else{
            showElement(selector = "#clust_type")
            showElement(selector = "#kmeans_cluster_action")
        }
        
    })
    # Hide/Show cluster options, based on cluster_type selector in the UI
    observeEvent(input$clust_type, {
        if ((as.numeric(input$clust_type) == 0) | (as.numeric(input$clust_type) == 1)){
            showElement(selector = "#kmeans_help")
            showElement(selector = "#kmeans_clusters")
            showElement(selector = "#kmeans_cluster_action")
            hideElement(selector = "#eps")
            showTab("main", 4)
        } else if (as.numeric(input$clust_type) == 3) {
            hideElement(selector = "#kmeans_help")
            hideElement(selector = "#kmeans_clusters")
            showElement(selector = "#eps")
            showElement(selector = "#kmeans_cluster_action")
            hideTab("main", 4)
        } else {
            hideElement(selector = "#kmeans_help")
            hideElement(selector = "#kmeans_clusters")
            hideElement(selector = "#eps")
            hideTab("main", 4)
        }
        })
    # Hide/Show UI elements if enable_hrm checkbox is triggered
    observeEvent(input$enable_hrm,{
        if (input$enable_hrm == FALSE){
            hideElement(selector = "#ref_column")
            hideElement(selector = "#diff_plot")
            hideElement(selector = "#min_tm_hrm")
            hideElement(selector = "#max_tm_hrm")
            hideElement(selector = "#trim_hrm")
        } else{
            showElement(selector = "#ref_column")
            showElement(selector = "#diff_plot")
            showElement(selector = "#min_tm_hrm")
            showElement(selector = "#max_tm_hrm")
            showElement(selector = "#trim_hrm")
        }
    })
    
    # The computing part of an app
    # Look at file upload, As soon as file is uploaded prepare the data and save it
    observeEvent(input$hmr,{
        # Read the first line and define the delimiter. Then read the whole data
        L <- readLines(input$hmr$datapath, n = 1)
        if (grepl(";", L)) vals$df <- read.csv2(input$hmr$datapath) else vals$df <- read.csv(input$hmr$datapath)
        # Drop NA columns
        vals$df <- vals$df[,colSums(is.na(vals$df))<nrow(vals$df)]
        write.csv(vals$df, file = "data.csv", row.names = F)
        
         })
    # Drop data from master dataframe (columns - individual results)
    observeEvent(input$drop,{
        to_drop <- input$drop_txt
        # First split the input by comma, then by range
        commas <- strsplit(to_drop, ",")
        ranges_drop <- trimws(unlist(strsplit(unlist(commas)[grepl("-", unlist(commas))], "-")))
        ind_col_drop <- trimws(unlist(commas)[!grepl("-", unlist(commas))])
        # Logic on how to split the data - like an individual column, or by range of columns. First go by ranges
        if (length(ranges_drop) > 0) {
            vals$df <- vals$df%>%
            select(!ranges_drop[1]:ranges_drop[2])
        }
        if (length(ind_col_drop) > 0){
            vals$df <- vals$df%>%
            select(-one_of(ind_col_drop))
        }
        # Set recluster to true
        vals$kmeans_cluster_actions <-  TRUE
    })
    # If reset button is triggered -> reset the daatframe (TO-DO: Add additional values change to recompute everything)
    observeEvent(input$reset,{
        vals$df <- read.csv("data.csv")
    })
    # Compute the data for diffplot
    observeEvent(DiffplotListen(),{
        req(input$hmr)
        req(input$ref_column)
        df <- vals$scaled_df
        # If need to trim, then redo the dataframes
        if (vals$trim_hrm ==TRUE){
            if (input$min_tm_hrm < min(df['Temperature']) ){
                min_tm_hrm = min(df['Temperature'])
            } else {
                min_tm_hrm <- input$min_tm_hrm
            }
            if (input$max_tm_hrm > max(df['Temperature']) ){
                max_tm_hrm = max(df['Temperature'])
            }else{
                max_tm_hrm <- input$max_tm_hrm
            }
            df <- df %>%
                dplyr::filter(Temperature>min_tm_hrm) %>%
                dplyr::filter(Temperature<max_tm_hrm)
            hold_tmp <- df['Temperature']
        }
        # Make a difference dataframe from given reference column
        hold_tmp <- df['Temperature']
        df <- df %>%
            select(-Temperature)
        
        df_ref <- df %>%
            select(input$ref_column) %>%
            c()
        
        df <- df - df_ref
        df['Temperature'] <- hold_tmp
        # Pass computed dataframe to the reactive one
        vals$diffdata_cluster <- df 
    })
    # MAIN COMPUTATION. Make a melting peak plot 
    observeEvent(toListen(),{
        req(input$hmr)
        df <- vals$df
        # Some logic to trim the temperature data. If values are greater/smaller that the min/max in dataframe ->
        # min/max used to trim (so no trim at all)
        if (vals$tm_min < min(df['Temperature']) ){
            vals$tm_min = min(df['Temperature'])
        } 
        if (vals$tm_max > max(df['Temperature']) ){
            vals$tm_max = max(df['Temperature'])
        }
        
        
        tmp_df <- df %>% dplyr::select(-Temperature)
        # Define min_max matrix -> it will hold the melting peak temperature. Fill with NAs
        min_max_matrix <- matrix(NA,(dim(df)[2]-1),4,
                                 dimnames = list(colnames(tmp_df[, 1L:(dim(tmp_df)[2])]),
                                                 c("Fluo", "Tm", "Tm1D2", "Tm2D2")))
        
        # For every column in dataframe make data smoother and compute melting peak via main function
        for (i in 1L:(dim(tmp_df)[2])) {
            # Smooth the data
            tmp <- mcaSmoother(df$Temperature,
                               tmp_df[, i],
                               n=input$n,
                               df.fact = input$smooth,
                               minmax = TRUE,
                               Trange = c(vals$tm_min, vals$tm_max)
            )
            # Compute the melting peak inpormation. Will store as a list
            tmpTM <- diffQ2(tmp, fct = max, verbose = TRUE, warn = F)
            min_max_matrix[i, 1] <- max(tmp$y)
            min_max_matrix[i, 2] <- tmpTM$TmD1$Tm
            min_max_matrix[i, 3] <- tmpTM$Tm1D2$Tm
            min_max_matrix[i, 4] <- tmpTM$Tm2D2$Tm
            # Make some new dataframes to fill. At first iteration only. Initial fill is with NA, then they will be 
            # populated 
            if (i == 1L){
                # Dataframe to hold the derivative information
                new_df <- matrix(NA,length(tmpTM$TmD1$xy[,1]),(dim(df)[2]-1),
                                 dimnames = list(tmpTM$TmD1$xy[,1],
                                                 colnames(tmp_df[, 1L:(dim(tmp_df)[2])])))
                # Dataframe to store the basic, input info. Then will scale
                scaled_df <-matrix(NA,length(tmp[,1]),(dim(df)[2]-1),
                                        dimnames = list(tmp[,1],
                                                        colnames(tmp_df[, 1L:(dim(tmp_df)[2])])))
            }
            # Populate the dataframes
            new_df[, i] <- tmpTM$TmD1$xy[,2]
            scaled_df[, i] <- tmp[,2]
        }
        # Make as th ematrics , that were populated, an actual dataframes
        min_max_matrix <-  data.frame(min_max_matrix)
        new_df <- data.frame(new_df)
        scaled_df <- data.frame(scaled_df)
        # Store the min_max matrix for further visualization in reactive value
        vals$matr <- min_max_matrix
        # Add Temperatures as a column to the dataframes
        new_df['Temperature'] <- as.numeric(rownames(new_df))
        scaled_df['Temperature'] <- as.numeric(rownames(scaled_df))
        # Scaled df have 2 data entries more, than the derivative one. Delete the first oen and the last one
        scaled_df[1,] <- NA
        scaled_df[dim(scaled_df)[1],] <- NA
        scaled_df <- scaled_df[complete.cases(scaled_df),]
        # Store them as reactive values
        vals$scaled_df_der <- new_df
        vals$scaled_df <- scaled_df
        # Make a plot_df, which is melted derivative df. Also add class column. Store as reactive value 
        plot_df <- melt(data = new_df, id.vars = "Temperature", variable.name = "x", value.name = "y" ) 
        plot_df['class'] <- NA
        vals$plot_df <- plot_df
    })
    # CLUSTERING COMPUTATION.
    observeEvent(toListen(),{
        req(vals$plot_df)
        # Begin computation, only if cluster checkbox is checked
        if (input$cluster == TRUE){
            # Define which dataframe use for clustering, (TO-DO: Make a selector out of it). So cluster on melting 
            # temperatures or on the whole range?
            if (input$enable_hrm == FALSE){
                cluster_data <- vals$matr %>% select(Tm) %>% as.matrix()
            }else if (length(input$ref_column) != 0){
                cluster_data <- vals$diffdata_cluster %>%
                    select(-Temperature)
                cluster_data <- t(cluster_data)
            }else{
                cluster_data <- vals$matr %>% select(Tm) %>% as.matrix()
            }
            # Clustering. Different options for different algorithms. From every algorithm store the mean Tm for further
            # visualization. If no mean Tm is provided by an algoritm - compute, based on data.
            # Store the class (so the clustering result) vector for columns in reactive value. 
            if (input$clust_type == 2){
                if (vals$kmeans_cluster_actions == TRUE){
                    # TO-DO: Strange values with HRM mode on
                    m_clust <- Mclust(cluster_data)
                    if (input$enable_hrm == TRUE){
                        cluster_data <- data.frame(cluster_data)
                        cluster_data['Tm'] <- vals$matr %>% select(Tm)
                        vals$clust_temps <- as.matrix(tapply(cluster_data$Tm, m_clust$classification, mean))
                    }else{
                        vals$clust_temps <- m_clust$parameters$mean
                    }
                    vals$clust_class <- m_clust$classification
                    
                }
            } else if (input$clust_type == 0){
                if (vals$kmeans_cluster_actions == TRUE){
                    kmeans_clust <- kmeans(cluster_data, input$kmeans_clusters , n=nrow(cluster_data)-1)
                    if (input$enable_hrm == TRUE){
                        cluster_data <- data.frame(cluster_data)
                        cluster_data['Tm'] <- vals$matr %>% select(Tm)
                        vals$clust_temps <- as.matrix(tapply(cluster_data$Tm, kmeans_clust$cluster, mean))
                    }else{
                        vals$clust_temps <- kmeans_clust$centers
                    }
                    vals$clust_class <- kmeans_clust$cluster
                    
                }
            } else if (input$clust_type == 1) {
                if (vals$kmeans_cluster_actions == TRUE){
                    h_clust <- hclust(dist(cluster_data, method = "euclidian"), method = "ward.D")
                    vals$clust_class <- cutree(h_clust, input$kmeans_clusters)
                    cluster_data <- data.frame(cluster_data)
                    cluster_data['class'] <- vals$clust_class
                    if (input$enable_hrm == TRUE){
                        cluster_data <- data.frame(cluster_data)
                        cluster_data['Tm'] <- vals$matr %>% select(Tm)
                    }
                    vals$clust_temps <- as.matrix(tapply(cluster_data$Tm, cluster_data$class, mean))
                   
                }
            } else if (input$clust_type == 3) {
                if (vals$kmeans_cluster_actions == TRUE){
                    db_clust <- dbscan(cluster_data, input$eps)
                    vals$clust_class <- db_clust$cluster
                    cluster_data <- data.frame(cluster_data)
                    cluster_data['class'] <- vals$clust_class
                    if (input$enable_hrm == TRUE){
                        cluster_data <- data.frame(cluster_data)
                        cluster_data['Tm'] <- vals$matr %>% select(Tm)
                    }
                    vals$clust_temps <- as.matrix(tapply(cluster_data$Tm, cluster_data$class, mean))
                    
                }
            }
            vals$cluster_data <- cluster_data
            
        }
        # Get the plot df (melted derivative df) and min_max matrix. Store them locally to make changes
        plot_df <- vals$plot_df
        min_max_matrix <- vals$matr
        # Add class value to the ploting df and to the min_max matrix. Make the plots colorful
        if (input$cluster == TRUE){
            if (vals$kmeans_cluster_actions == TRUE){
                for (i in seq(1:dim(plot_df)[1])){
                    plot_df[i,'class'] <- vals$clust_class[plot_df[i,'x']]
                }
                min_max_matrix['class'] <- vals$clust_class   
                # Clustering completed. Now the flag is FALSE. Press the button to make it TRUE
                vals$kmeans_cluster_actions <- FALSE
            } else if (length(vals$clust_class) != 0) {
                # Here pass class values if the vector itseld is not empty (so the clustering was done previously)
                for (i in seq(1:dim(plot_df)[1])){
                    plot_df[i,'class'] <- vals$clust_class[plot_df[i,'x']]
                }
                min_max_matrix['class'] <- vals$clust_class 
            }
        } else {
            # If no clustering occured -> each column is an individual class
            plot_df['class'] <- plot_df['x']
            min_max_matrix['class'] <- NA
            min_max_matrix <- min_max_matrix[,colSums(is.na(min_max_matrix))<nrow(min_max_matrix)]
        }
        # Make the processed data reactive once again. (if clustering is off, then just preserve the data)
        vals$matr <- min_max_matrix
        vals$plot_df <- plot_df
    })
    # Make the Norm curve curve plot, Be in the diffplot tab
    output$norm_curve <- renderPlotly({
        req(input$hmr)
        req(vals$scaled_df)
        
        plot_df <- vals$scaled_df
        plot_df <- melt(data = plot_df, id.vars = "Temperature", variable.name = "x", value.name = "y" ) 
        plot_df['class'] <- vals$plot_df['class']
        
        ggplotly(plot_df %>%
                     ggplot(aes(x=as.numeric(Temperature), y=y)) +
                     geom_line(aes(line = x, colour = as.factor(class)))+
                     xlab("Melting Temperature") +
                     ylab("Scaled RFU") + 
                     scale_color_hue(l=53, c=60)
        )
    })
    # Make the melting curve plot
    output$melt_curve <- renderPlotly({
        req(input$hmr)
        req(vals$plot_df)
        
        plot_df <- vals$plot_df
        
        ggplotly(plot_df %>%
                     ggplot(aes(x=as.numeric(Temperature), y=y)) +
                     geom_line(aes(line = x, colour = as.factor(class)))+
                     xlab("Melting Temperature") +
                     ylab("Scaled -d(RFU)/dT") + 
                     scale_color_hue(l=53, c=60)
        )
    })
    # Make the diffplot
    output$diffplot <- renderPlotly({
        req(input$hmr)
        req(input$diff_plot)
        req(vals$diffdata_cluster)
        
        plot_data <- vals$diffdata_cluster
        plot_data <- melt(data = plot_data, id.vars = "Temperature", variable.name = "x", value.name = "y" ) 
        if (input$cluster == TRUE){
            if (vals$diff_cluster_action == TRUE) {
            for (i in seq(1:dim(plot_data)[1])){
                plot_data[i,'class'] <- vals$clust_class[plot_data[i,'x']]
            }} else{
                plot_data['class'] <- vals$plot_df['x']
            }
        }else{
            plot_data['class'] <- vals$plot_df['x']
        }
        ggplotly(plot_data %>%
                     ggplot(aes(x=as.numeric(Temperature), y=y)) +
                     geom_line(aes(line = x, colour = as.factor(class))) +
                     xlab("Melting Temperature") +
                     ylab(paste("Difference from", input$ref_column)) + 
                     scale_color_hue(l=53, c=60)
                 )
        
    })
    # Render the table of Melting temps cluster-based. If clustering is off, then the table will not be shown 
    output$tmps <-  renderUI({
        req(input$hmr)
        req(input$cluster)
        M <- as.matrix(vals$clust_temps)
        M <- print(xtable(M, align=rep("c", ncol(M)+1)), 
                   floating=FALSE, tabular.environment="array", comment=FALSE, print.results=FALSE)
        html <- paste0("$$", M, "$$")
        list(
            withMathJax(HTML(html))
        )
        
    })
    # Render the table of melting temps for an individual curves
    output$ind_tmps <-  renderUI({
        req(input$hmr)
        M <- as.matrix(vals$matr)
        M <- print(xtable(M, align=rep("c", ncol(M)+1)), 
                   floating=FALSE, tabular.environment="array", comment=FALSE, print.results=FALSE)
        html <- paste0("$$", M, "$$")
        list(
            withMathJax(HTML(html))
        )
    })
    # Make the used data table. Mainly is used for debugging purposed, but left in case someone will need it 
    output$used_data <-  renderUI({
        req(input$hmr)
        req(vals$cluster_data)
        M <- as.matrix(vals$cluster_data)
        M <- print(xtable(M, align=rep("c", ncol(M)+1)), 
                   floating=FALSE, tabular.environment="array", comment=FALSE, print.results=FALSE)
        html <- paste0("$$", M, "$$")
        list(
            withMathJax(HTML(html))
        )
    })
    # Best number of clusters computation and visualization for k-means and h-clust clustering with wss method
    output$wss <- renderPlotly({
        req(vals$cluster_data)
        req(input$cluster)
        req(input$hmr)
        
        df <- vals$cluster_data
        
        if (as.numeric(input$clust_type) == 0){
            fviz_nbclust(df, FUN = kmeans, method = "wss", k.max = dim(df)[1]-1)
        } else if (as.numeric(input$clust_type) == 1) {
            fviz_nbclust(df, FUN = hcut, method = "wss", k.max = dim(df)[1]-1)
        }
    })
    # Best number of clusters computation and visualization for k-means and h-clust clustering with silhouette method
    output$silhouette <- renderPlotly({
        req(vals$cluster_data)
        req(input$cluster)
        req(input$hmr)
        
        df <- vals$cluster_data
        
        if (as.numeric(input$clust_type) == 0){
            fviz_nbclust(df, FUN = kmeans, method = "silhouette", k.max = dim(df)[1]-1)
        } else if (as.numeric(input$clust_type) == 1) {
            fviz_nbclust(df, FUN = hcut, method = "silhouette", k.max = dim(df)[1]-1)
        }
    })
    # Best number of clusters computation and visualization for k-means and h-clust clustering with gap statistics
    # method
    output$gap_stat <- renderPlotly({
        req(vals$cluster_data)
        req(input$cluster)
        req(input$hmr)
        
        df <- as.matrix(vals$cluster_data)
        
        if (as.numeric(input$clust_type) == 0){
            gap_stat <- clusGap(df, FUN = kmeans, nstart = 25, K.max = dim(df)[1]-1, B = 50)
            fviz_gap_stat(gap_stat)
        } else if (as.numeric(input$clust_type) == 1) {
            gap_stat <- clusGap(df, FUN = hcut, nstart = 25, K.max = dim(df)[1]-1, B = 50)
            fviz_gap_stat(gap_stat)
        }
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
