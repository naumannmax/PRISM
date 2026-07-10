#------------------------------------------------------------------------------#
# Shiny App - PRISM - Phasor-based Raman Image Segmentation Manager ####
#------------------------------------------------------------------------------#
library(shiny)
library(shinyFiles)
library(plotly)
library(hyperSpec)
library(tidyverse)
library(fs)
library(shinyjs)

preprocess_spc <- function(spc, input) {
  # PUT YOUR PREPROCESSING LOGIC HERE
  return(spc)
}

# Increase limit to 500MB for RData uploads
options(shiny.maxRequestSize = 500 * 1024^2)

## UI ####
ui <- fluidPage(
  shinyjs::useShinyjs(),
  tags$head(tags$style(HTML("
    /* Freeze columns */
    .sticky-column {
      position: -webkit-sticky;
      position: sticky;
      top: 10px;
      height: calc(100vh - 20px);
      overflow-y: auto;
    }
    
    .plot-container {
      border: 1px solid #d3d3d3; border-radius: 8px;
      padding: 15px; margin-bottom: 20px; background-color: #ffffff;
      box-shadow: 0 2px 4px rgba(0,0,0,0.05);
    }
    .plot-header {
      font-weight: bold; font-size: 16px; text-align: center;
      margin-bottom: 10px; color: #34495e; border-bottom: 1px solid #f0f0f0; padding-bottom: 5px;
    }
    .scan-manager-box { 
      background: #fff3cd; padding: 15px; border: 1px solid #ffeeba; 
      border-radius: 8px; height: 100%; min-height: 400px;
      display: flex; flex-direction: column;
    }
    .control-panel { padding: 15px; border-radius: 5px; margin-bottom:15px; }
    summary { cursor: pointer; }
    
    .export-container {
      display: flex;
      gap: 10px;
      justify-content: flex-end;
      margin-top: 5px;
    }
    
    /* Conditional styling for the Export Matrix button */
    #downloadFullData:not(.disabled) {
      background-color: #28a745 !important;
      color: white !important;
      border: none !important;
    }
    
    /* Header Styling */
    .app-header {
      display: flex; 
      align-items: center; 
      justify-content: space-between; 
      padding: 15px 30px; 
      background-color: #ffffff; 
      border-bottom: 1px solid #eee; 
      margin-bottom: 20px;
    }
    
    .nav-buttons {
      margin-top: auto;
      padding-top: 10px;
      display: flex;
      gap: 5px;
    }
  "))),
  
  #--- BRANDED HEADER ---#
  div(class = "app-header",
      div(style = "flex: 1; display: flex; align-items: center; gap: 20px;",
          img(src = "LPI.png", height = "50px"),
          img(src = "LeibnizIPHT.png", height = "45px")
      ),
      div(style = "flex: 2; text-align: center;",
          h1("PRISM", style = "font-weight: 900; margin: 0; color: #2c3e50; letter-spacing: 1px;"),
          p("Phasor-based Raman Image Segmentation Manager", 
            style = "font-weight: 300; color: #7f8c8d; font-size: 1.1em; margin-bottom: 0;")
      ),
      div(style = "flex: 1; text-align: right; font-size: 0.9em; color: #95a5a6; line-height: 1.4;",
          tags$b("Developed by: Max Naumann"), br(),
          HTML("&copy; 2026 All Rights Reserved")
      )
  ),
  
  fluidRow(
    ### LEFT COLUMN: Controls ####
    column(3,
           tags$details(open = "open",
                        tags$summary(style="font-size:1.2em; margin-bottom:10px; color:#2c3e50; font-weight:bold;", "Data Import"),
                        div(class = "control-panel", style = "background: #f0f7ff; border: 1px solid #cce5ff;",
                            shinyFilesButton("file_btn", "Browse Files...", "Select WITec Header (.txt) or .RData", multiple = FALSE, class = "btn-info", style="width:100%"),
                            br(), br(),
                            verbatimTextOutput("selected_file_path", placeholder = TRUE),
                            actionButton("load_btn", "Load Data", class = "btn-primary", style="width:100%; margin-top:10px;")
                        )
           ),
           hr(),
           tags$details(open = "open",
                        tags$summary(style="font-size:1.2em; margin-bottom:10px; color:#2c3e50; font-weight:bold;", "Data Preprocessing"),
                        div(class = "control-panel", style = "background: #fdf2f2; border: 1px solid #f8d7da;",
                            checkboxInput("do_baseline", "Apply Baseline Correction", value = TRUE),
                            numericInput("snip_iter", "SNIP Iterations", value = 50, min = 1),
                            hr(style = "margin: 10px 0;"),
                            checkboxInput("do_norm", "Apply Vector Normalization", value = TRUE),
                            br(),
                            actionButton("process_btn", "Apply & Process", class = "btn-warning", style="width:100%"),
                            actionButton("reset_btn", "Reset", class = "btn-link", style="width:100%; color: #dc3545;")
                        )
           ),
           hr(),
           tags$details(open = "open",
                        tags$summary(style="font-size:1.2em; margin-bottom:10px; color:#2c3e50; font-weight:bold;", "Cluster Selection"),
                        tags$details(
                          tags$summary(style="font-size:1.1em; font-weight:bold;", "> Grouping by Metadata"),
                          div(style="padding: 10px; border-left: 3px solid #2980b9; margin-bottom: 10px;",
                              uiOutput("meta_selector_ui")
                          )
                        ),
                        br(),
                        tags$details(
                          tags$summary(style="font-size:1.1em; font-weight:bold;", "> Manual Cluster Selection"),
                          br(),
                          textInput("roi_manual_name", "Base Name", "Cluster"),
                          selectInput("roi_color", "Initial Color", choices = c("black","red", "blue", "green", "magenta", "orange", "cyan")),
                          actionButton("add_roi", "Save Current Selection", class = "btn-primary", style="width:100%"),
                          actionButton("deselect_lasso", "Delete Current Selection", class = "btn-default", style="width:100%; margin-top:5px;")
                        ),
                        br(),
                        tags$details(
                          tags$summary(style="font-size:1.1em; font-weight:bold;", "> Automatic Cluster Selection"),
                          br(),
                          numericInput("k_clusters", "Number of Clusters (k):", 3, min = 2, max = 10),
                          actionButton("run_kmeans", "Run Auto-Clustering", class = "btn-success", style="width:100%")
                        )
           ),
           hr(),
           tags$details(open = "open",
                        tags$summary(style="font-size:1.2em; margin-bottom:10px; color:#2c3e50; font-weight:bold;", "Cluster Management"),
                        div(class = "control-panel", style = "background: #f8f9fa; border: 1px solid #dee2e6;",
                            uiOutput("roi_selector_ui"), 
                            hr(),
                            actionButton("clear_rois", "Clear All Clusters", class = "btn-danger", style="width:100%")
                        )
           )
    ),
    
    ### MIDDLE COLUMN: Plots ####
    column(7, class = "sticky-column",
           fluidRow(
             column(6, 
                    div(class = "plot-container", 
                        div(class = "plot-header", "Spectral Phasor Plot"), 
                        plotlyOutput("phasorPlot", height = "380px")
                    )
             ),
             column(6, 
                    div(class = "plot-container", 
                        div(class = "plot-header", "False-color Raman Image"), 
                        uiOutput("ramanImage_container"),
                        div(class = "export-container", 
                            # Swapped Positions and updated button ID/classes
                            downloadButton("downloadImage", "Export Image (PNG)", class = "btn-sm"),
                            downloadButton("downloadFullData", "Export Spectral Matrix (CSV)", class = "btn-sm")
                        )
                    )
             )
           ),
           div(class = "plot-container",
               div(class = "plot-header", "Mean Raman Spectra of selected Clusters"),
               plotOutput("meanSpectrum", height = "300px"),
               div(class = "export-container", 
                   downloadButton("downloadSpectra", "Export Spectra (PNG)", class = "btn-sm"),
                   downloadButton("downloadCSV", "Export Spectra (CSV)", class = "btn-sm", style = "background-color: #28a745; color: white; border: none;")
               )
           )
    ),
    
    ### RIGHT COLUMN: Scan Manager ####
    column(2, class = "sticky-column",
           tags$details(open = "open",
                        tags$summary(style="font-size:1.2em; margin-bottom:10px; color:#2c3e50; font-weight:bold;", "Data Management"),
                        div(class = "scan-manager-box",
                            style = "padding-top: 5px;",
                            helpText("Active Scan(s):"),
                            uiOutput("scan_selector_ui"),
                            div(class = "nav-buttons",
                                actionButton("prev_scan", "↑ Up", class = "btn-default", style="width:50%"),
                                actionButton("next_scan", "↓ Down", class = "btn-default", style="width:50%")
                            )
                        )
           )
    )
  )
)

## SERVER ####
server <- function(input, output, session) {
  
  raw_data_list <- reactiveVal(list())
  all_scans <- reactiveVal(list())
  rois <- reactiveVal(list())
  roots <- getVolumes()() 
  
  shinyFileChoose(input, "file_btn", roots = roots, session = session, filetypes = c("txt", "RData"))
  
  target_file_path <- reactive({
    req(input$file_btn)
    parseFilePaths(roots, input$file_btn)$datapath
  })
  
  output$selected_file_path <- renderText({
    if (is.integer(input$file_btn)) "No file selected" else target_file_path()
  })
  
  observeEvent(input$load_btn, {
    path <- target_file_path()
    req(path)
    withProgress(message = 'Loading Data...', value = 0.5, {
      tryCatch({
        spc_list <- list()
        ext <- tools::file_ext(path)
        if(ext == "txt") {
          folder <- dirname(path)
          txt_files <- list.files(path = folder, pattern = "\\(Header).txt$", full.names = TRUE)
          for (f in txt_files) spc_list[[basename(f)]] <- read.txt.Witec.Graph(f, type = "map")
        } else if(ext == "RData") {
          env <- new.env(); load(path, envir = env)
          for (obj_name in ls(env)) {
            if (inherits(env[[obj_name]], "hyperSpec")) {
              spc_list[[paste0(basename(path), " [", obj_name, "]")]] <- env[[obj_name]]
            }
          }
        }
        if(length(spc_list) == 0) stop("No valid hyperSpec objects found.")
        for(n in names(spc_list)) { spc_list[[n]]@data$filename <- n }
        raw_data_list(spc_list)
        all_scans(spc_list) 
        showNotification("Data loaded raw.", type = "message")
      }, error = function(e) { showNotification(paste("Error:", e$message), type = "error") })
    })
  })
  
  observeEvent(input$next_scan, {
    scans <- names(all_scans()); req(length(scans) > 0)
    curr <- input$selected_scan_names
    if(length(curr) != 1) return()
    idx <- which(scans == curr)
    next_idx <- if(idx == length(scans)) 1 else idx + 1
    updateCheckboxGroupInput(session, "selected_scan_names", selected = scans[next_idx])
  })
  
  observeEvent(input$prev_scan, {
    scans <- names(all_scans()); req(length(scans) > 0)
    curr <- input$selected_scan_names
    if(length(curr) != 1) return()
    idx <- which(scans == curr)
    prev_idx <- if(idx == 1) length(scans) else idx - 1
    updateCheckboxGroupInput(session, "selected_scan_names", selected = scans[prev_idx])
  })
  
  observe({
    if(length(input$selected_scan_names) != 1) {
      shinyjs::disable("prev_scan"); shinyjs::disable("next_scan")
    } else {
      shinyjs::enable("prev_scan"); shinyjs::enable("next_scan")
    }
  })
  
  observeEvent(input$process_btn, {
    raw_list <- raw_data_list(); req(length(raw_list) > 0)
    withProgress(message = 'Preprocessing...', value = 0, {
      tryCatch({
        processed <- lapply(raw_list, function(spc) preprocess_spc(spc, input))
        all_scans(processed)
        showNotification("Preprocessing applied.", type = "message")
      }, error = function(e) { showNotification(paste("Error:", e$message), type = "error") })
    })
  })
  
  observeEvent(input$reset_btn, {
    req(raw_data_list())
    all_scans(raw_data_list())
    showNotification("Reset to raw data.", type = "message")
  })
  
  metadata_cols <- reactive({
    d <- current_data(); req(d)
    all_cols <- colnames(d@data)
    meta <- setdiff(all_cols, c("spc", "x", "y", "id", "row", "col"))
    return(meta)
  })
  
  output$meta_selector_ui <- renderUI({
    cols <- metadata_cols()
    if(length(cols) == 0) return(helpText("No metadata columns found in this scan."))
    tagList(
      selectInput("selected_meta_col", "Choose Variable:", choices = cols),
      actionButton("apply_meta_clustering", "Generate Clusters from Groups", class = "btn-info", style="width:100%")
    )
  })
  
  observeEvent(input$apply_meta_clustering, {
    d <- current_data(); df <- phasor_base()
    req(input$selected_meta_col, d)
    groups <- as.character(d@data[[input$selected_meta_col]])
    unique_groups <- unique(groups)
    palette <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf")
    new_rois <- list()
    for(i in seq_along(unique_groups)) {
      g_name <- unique_groups[i]
      indices <- which(groups == g_name)
      new_rois[[paste0("meta_", i)]] <- list(
        ids = df$id[indices], name = g_name, color = palette[((i-1) %% length(palette)) + 1]
      )
    }
    rois(new_rois)
    showNotification(paste("Success: Applied", length(new_rois), "groups."), type = "message")
  })
  
  output$scan_selector_ui <- renderUI({
    scans <- all_scans(); req(length(scans) > 0)
    checkboxGroupInput("selected_scan_names", NULL, choices = names(scans), selected = names(scans)[1])
  })
  
  current_data <- reactive({
    req(input$selected_scan_names)
    scans_list <- all_scans()[input$selected_scan_names]
    if(length(scans_list) == 1) {
      return(scans_list[[1]])
    } else {
      return(do.call(hyperSpec::collapse, scans_list))
    }
  })
  
  phasor_base <- reactive({
    d <- current_data(); req(d)
    Nc <- ncol(d$spc); phase_term <- 2 * pi * (1:Nc) / Nc
    sum_I <- rowSums(d$spc); sum_I[sum_I == 0] <- 1 
    G <- (d$spc %*% cos(phase_term)) / sum_I
    S <- (d$spc %*% sin(phase_term)) / sum_I
    data.frame(G = as.numeric(G), S = as.numeric(S), x = d$x, y = d$y, 
               id = 1:nrow(d), filename = d@data$filename)
  })
  
  output$roi_selector_ui <- renderUI({
    roi_list <- rois()
    # Define your 10 safe color options
    safe_colors <- c("black", "red", "blue", "green", "magenta", 
                     "orange", "cyan", "purple", "brown", "grey")
    
    if(length(roi_list) == 0) return(p("No Clusters saved."))
    
    tagList(lapply(names(roi_list), function(id) {
      div(class = "roi-control", style = "margin-bottom: 10px; border-bottom: 1px solid #eee; padding-bottom: 5px;",
          fluidRow(
            column(1, checkboxInput(paste0("vis_", id), NULL, value = TRUE)),
            column(6, textInput(paste0("name_", id), NULL, value = roi_list[[id]]$name)),
            column(5, selectInput(paste0("hex_", id), NULL, 
                                  choices = safe_colors, 
                                  selected = roi_list[[id]]$color))
          )
      )
    }))
  })
  
  observe({
    roi_list <- rois()
    req(length(roi_list) > 0)
    changed <- FALSE
    
    for(id in names(roi_list)) {
      new_color <- input[[paste0("hex_", id)]]
      new_name <- input[[paste0("name_", id)]]
      
      # Ensure inputs exist before checking
      if(!is.null(new_color) && new_color != roi_list[[id]]$color) { 
        roi_list[[id]]$color <- new_color
        changed <- TRUE 
      }
      if(!is.null(new_name) && new_name != "" && new_name != roi_list[[id]]$name) { 
        roi_list[[id]]$name <- new_name
        changed <- TRUE 
      }
    }
    
    if(changed) {
      # Use isolate to prevent infinite reactive loops
      isolate({ rois(roi_list) })
    }
  })
  
  observeEvent(input$deselect_lasso, {
    plotlyProxy("phasorPlot", session) %>% plotlyProxyInvoke("restyle", list(selectedpoints = list(NULL)))
    shinyjs::runjs("Shiny.setInputValue('plotly_selected-phasorPlot', null);")
  })
  
  observeEvent(input$add_roi, {
    sel <- event_data("plotly_selected", source = "phasorPlot"); req(sel)
    current <- rois(); new_id <- paste0("roi_", length(current) + 1)
    current[[new_id]] <- list(ids = as.numeric(sel$key), name = paste0(input$roi_manual_name, " ", length(current) + 1), color = input$roi_color)
    rois(current); shinyjs::click("deselect_lasso")
  })
  
  observeEvent(input$run_kmeans, {
    df <- phasor_base(); set.seed(123)
    km <- kmeans(df[, c("G", "S")], centers = input$k_clusters)
    palette <- c("black", "green", "blue", "red", "lightblue", "#ffff33")
    new_rois <- list()
    for(i in 1:input$k_clusters) {
      new_rois[[paste0("roi_", i)]] <- list(ids = df$id[km$cluster == i], name = paste("Cluster", i), color = palette[((i-1) %% length(palette)) + 1])
    }
    rois(new_rois)
  })
  
  observeEvent(input$clear_rois, { rois(list()) })
  observeEvent(input$selected_scan_names, { rois(list()) })
  
  output$phasorPlot <- renderPlotly({
    df <- phasor_base(); active_rois <- rois()
    p <- plot_ly(source = "phasorPlot") %>% 
      add_markers(data = df, x = ~G, y = ~S, key = ~id, marker = list(size = 4, color = "grey", opacity = 0.1), name = "Unassigned")
    for(id in names(active_rois)) {
      if(isTRUE(input[[paste0("vis_", id)]])) {
        p <- p %>% add_markers(data = df[df$id %in% active_rois[[id]]$ids, ], x = ~G, y = ~S, marker = list(size = 5, color = active_rois[[id]]$color, opacity = 0.8), name = active_rois[[id]]$name)
      }
    }
    p %>% layout(dragmode = "lasso", margin = list(t=30), xaxis = list(title = "G"), yaxis = list(title = "S", scaleanchor="x"))
  })
  
  output$ramanImage_container <- renderUI({
    if(length(input$selected_scan_names) == 1) {
      plotOutput("ramanImage", height = "380px")
    } else {
      div(style="height: 380px; display: flex; align-items: center; justify-content: center; background: #f8f9fa; color: #95a5a6; border: 1px dashed #ddd; text-align: center;",
          p("Spatial image hidden for multiple scans."))
    }
  })
  
  image_plot <- reactive({
    req(length(input$selected_scan_names) == 1)
    df <- phasor_base(); active_rois <- rois(); sel <- event_data("plotly_selected", source = "phasorPlot")
    p <- ggplot(df, aes(x = x, y = y)) + geom_tile(fill = "grey92") + coord_fixed() + theme_void()
    for(id in names(active_rois)) if(isTRUE(input[[paste0("vis_", id)]])) p <- p + geom_tile(data = df[df$id %in% active_rois[[id]]$ids, ], fill = active_rois[[id]]$color)
    if(!is.null(sel)) p <- p + geom_tile(data = df[df$id %in% as.numeric(sel$key), ], fill = "yellow", alpha = 0.6)
    p
  })
  output$ramanImage <- renderPlot({ image_plot() })
  
  spectra_plot <- reactive({
    d <- current_data(); req(d); active_rois <- rois(); sel <- event_data("plotly_selected", source = "phasorPlot")
    plt_df <- data.frame(wavenumber = wl(d)); colors_map <- c()
    for(id in names(active_rois)) if(isTRUE(input[[paste0("vis_", id)]])) {
      plt_df[[active_rois[[id]]$name]] <- colMeans(d$spc[active_rois[[id]]$ids, , drop=FALSE])
      colors_map[active_rois[[id]]$name] <- active_rois[[id]]$color
    }
    if(!is.null(sel)) { plt_df[["Selection"]] <- colMeans(d$spc[as.numeric(sel$key), , drop=FALSE]); colors_map["Selection"] <- "gold" }
    if(ncol(plt_df) < 2) return(NULL)
    plt_df %>% pivot_longer(-wavenumber, names_to = "ROI", values_to = "Int") %>%
      ggplot(aes(x = wavenumber, y = Int, color = ROI)) + geom_line(linewidth = 1) + 
      scale_color_manual(values = colors_map) + theme_minimal() + labs(x = "Wavenumber (cm-1)", y = "Intensity (norm)")
  })
  output$meanSpectrum <- renderPlot({ spectra_plot() })
  
  #--- EXPORT LOGIC ---#
  
  observe({
    if (length(rois()) == 0) shinyjs::disable("downloadFullData") else shinyjs::enable("downloadFullData")
    if (length(input$selected_scan_names) != 1) shinyjs::disable("downloadImage") else shinyjs::enable("downloadImage")
  })
  
  output$downloadFullData <- downloadHandler(
    filename = function() { "SPCMatrix_PRISM-Analysis.csv" },
    content = function(file) {
      d <- current_data(); req(d); df_phasor <- phasor_base(); active_rois <- rois()
      spc_df <- as.data.frame(d$spc); colnames(spc_df) <- as.character(wl(d))
      final_df <- data.frame(x = d$x, y = d$y, G = df_phasor$G, S = df_phasor$S, 
                             Scan = df_phasor$filename, Cluster_Label = "Unassigned")
      for(id in names(active_rois)) {
        indices <- which(df_phasor$id %in% active_rois[[id]]$ids)
        final_df$Cluster_Label[indices] <- active_rois[[id]]$name
      }
      write.csv(cbind(final_df, spc_df), file, row.names = FALSE)
    }
  )
  
  output$downloadImage <- downloadHandler(
    filename = function() { "Map_Export.png" },
    content = function(file) { ggsave(file, plot = image_plot(), device = "png", width = 8, height = 6) }
  )
  output$downloadSpectra <- downloadHandler(
    filename = function() { "Spectra_Export.png" },
    content = function(file) { ggsave(file, plot = spectra_plot(), device = "png", width = 10, height = 6) }
  )
  
  output$downloadCSV <- downloadHandler(
    filename = function() { "Mean_Spectra_Export.csv" },
    content = function(file) {
      d <- current_data(); req(d); active_rois <- rois(); sel <- event_data("plotly_selected", source = "phasorPlot")
      export_df <- data.frame(Wavenumber = wl(d))
      for(id in names(active_rois)) {
        if(isTRUE(input[[paste0("vis_", id)]])) {
          export_df[[active_rois[[id]]$name]] <- colMeans(d$spc[active_rois[[id]]$ids, , drop=FALSE])
        }
      }
      if(!is.null(sel)) export_df[["Current_Selection"]] <- colMeans(d$spc[as.numeric(sel$key), , drop=FALSE])
      write.csv(export_df, file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)

#UPDATE ####