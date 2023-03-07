server <- function(input, output, session) {

    slider_vals <- reactiveValues(
      "Boreal" = list(),
      "Central" = list(),
      "Southern" = list(),
      "Northern" = list()
    )
    isolate({
      wc <- if (is.null(input[["slider_Boreal_custom"]]))
          0 else input$slider_Boreal_custom
      l <- list(
          w_B4BNorm = input$slider_Boreal_1,
          w_CUNorm = input$slider_Boreal_2,
          w_Custom = wc)
      slider_vals[["Boreal"]] <- l
    })
    isolate({
      wc <- if (is.null(input[["slider_Central_custom"]]))
          0 else input$slider_Central_custom
      l <- list(
          w_B4BNorm = input$slider_Central_1,
          w_CUNorm = input$slider_Central_2,
          w_Custom = wc)
      slider_vals[["Central"]] <- l
    })
    isolate({
      wc <- if (is.null(input[["slider_Southern_custom"]]))
          0 else input$slider_Southern_custom
      l <- list(
          w_B4BNorm = input$slider_Southern_1,
          w_NrmPl20 = input$slider_Southern_2,
          w_NormCor = input$slider_Southern_3,
          w_UWRNorm = input$slider_Southern_4,
          w_Custom = wc)
      slider_vals[["Southern"]] <- l
    })
    isolate({
      wc <- if (is.null(input[["slider_Northern_custom"]]))
          0 else input$slider_Northern_custom
      l <- list(
          w_B4BNorm = input$slider_Northern_1,
          w_NrmPl20 = input$slider_Northern_2,
          w_NormCor = input$slider_Northern_3,
          w_UWRNorm = input$slider_Northern_4,
          w_Custom = wc)
      slider_vals[["Northern"]] <- l
    })

    output$map_All <- renderLeaflet({
        map_all(extra = isolate(custom_spatial()))
    })
    observeEvent(custom_spatial(), {
      leafletProxy("map_All") |>
        map_all_update(extra = custom_spatial())
    }, ignoreInit = TRUE)

    output$map_Boreal <- renderLeaflet({
        map_one_base(dat, "Boreal",
                     custom_var = isolate(custom_var()[["Boreal"]]),
                     extra = isolate(custom_spatial()))
    })
    observeEvent(c(input$btn_Boreal, input$custom_upload, custom_spatial()), {
        wc <- if (is.null(input[["slider_Boreal_custom"]]))
            0 else input$slider_Boreal_custom
        l <- list(
            w_B4BNorm = input$slider_Boreal_1,
            w_CUNorm = input$slider_Boreal_2,
            w_Custom = wc)
        if (sum(unlist(l)) == 0) {
            showNotification("Weights cannot be all 0.", type = "error")
        } else {
            slider_vals[["Boreal"]] <- l
            str(slider_vals[["Boreal"]])
            leafletProxy("map_Boreal") |>
            clearControls() |>
            clearPopups() |>
            clearShapes() |>
            map_one(dat, "Boreal", inputs = l,
                    custom_var = custom_var()[["Boreal"]],
                    opacity = input$alpha_Boreal,
                    extra = custom_spatial())
        }
    }, ignoreInit = TRUE)

    output$map_Central <- renderLeaflet({
        map_one_base(dat, "Central",
                     custom_var = isolate(custom_var()[["Central"]]),
                     extra = isolate(custom_spatial()))
    })
    observeEvent(c(input$btn_Central, input$custom_upload, custom_spatial()), {
        wc <- if (is.null(input[["slider_Central_custom"]]))
            0 else input$slider_Central_custom
        l <- list(
            w_B4BNorm = input$slider_Central_1,
            w_CUNorm = input$slider_Central_2,
            w_Custom = wc)
        if (sum(unlist(l)) == 0) {
            showNotification("Weights cannot be all 0.", type = "error")
        } else {
            slider_vals[["Central"]] <- l
            str(slider_vals[["Central"]])
            leafletProxy("map_Central") |>
            clearControls() |>
            # clearPopups() |>
            clearShapes() |>
            map_one(dat, "Central", inputs = l,
                    custom_var = custom_var()[["Central"]],
                    opacity = input$alpha_Central,
                    extra = custom_spatial())
        }
    }, ignoreInit = TRUE)

    output$map_Southern <- renderLeaflet({
        map_one_base(dat, "Southern",
                     custom_var = isolate(custom_var()[["Southern"]]),
                     extra = isolate(custom_spatial()))
    })
    observeEvent(c(input$btn_Southern, input$custom_upload, custom_spatial()), {
        wc <- if (is.null(input[["slider_Southern_custom"]]))
            0 else input$slider_Southern_custom
        l <- list(
            w_B4BNorm = input$slider_Southern_1,
            w_NrmPl20 = input$slider_Southern_2,
            w_NormCor = input$slider_Southern_3,
            w_UWRNorm = input$slider_Southern_4,
            w_Custom = wc)
        if (sum(unlist(l)) == 0) {
            showNotification("Weights cannot be all 0.", type = "error")
        } else {
            slider_vals[["Southern"]] <- l
            str(slider_vals[["Southern"]])
            leafletProxy("map_Southern") |>
            clearControls() |>
            # clearPopups() |>
            clearShapes() |>
            map_one(dat, "Southern", inputs = l,
                    custom_var = custom_var()[["Southern"]],
                    opacity = input$alpha_Southern,
                    extra = custom_spatial())
        }
    }, ignoreInit = TRUE)

    output$map_Northern <- renderLeaflet({
        map_one_base(dat, "Northern",
                     custom_var = isolate(custom_var()[["Northern"]]),
                     extra = isolate(custom_spatial()))
    })
    observeEvent(c(input$btn_Northern, input$custom_upload, custom_spatial()), {
        wc <- if (is.null(input[["slider_Northern_custom"]]))
            0 else input$slider_Northern_custom
        l <- list(
            w_B4BNorm = input$slider_Northern_1,
            w_NrmPl20 = input$slider_Northern_2,
            w_NormCor = input$slider_Northern_3,
            w_UWRNorm = input$slider_Northern_4,
            w_Custom = wc)
        if (sum(unlist(l)) == 0) {
            showNotification("Weights cannot be all 0.", type = "error")
        } else {
            slider_vals[["Northern"]] <- l
            str(slider_vals[["Northern"]])
            leafletProxy("map_Northern") |>
            clearControls() |>
            # clearPopups() |>
            clearShapes() |>
            map_one(dat, "Northern", inputs = l,
                    custom_var = custom_var()[["Northern"]],
                    opacity = input$alpha_Northern,
                    extra = custom_spatial())
        }
    }, ignoreInit = TRUE)

    # Shapefile download buttons
    output$btn_dl_Central <- downloadHandler(
      filename = function() {
        return("BC-Priority-Support-Tool_CENTRAL.zip")
      },
      content = function(file) {
        reg <- "Central"
        print(paste("Download:", reg))
        shpzip <- prepare_region_shp(dat, 
          region =  reg, 
          inputs = slider_vals[[reg]], 
          custom_var = custom_var()[[reg]],
          crs = DOWNLOAD_CRS)
        file.copy(shpzip, file)
      }
    )
    output$btn_dl_Boreal <- downloadHandler(
      filename = function() {
        return("BC-Priority-Support-Tool_BOREAL.zip")
      },
      content = function(file) {
        reg <- "Boreal"
        print(paste("Download:", reg))
        shpzip <- prepare_region_shp(dat, 
          region =  reg, 
          inputs = slider_vals[[reg]], 
          custom_var = custom_var()[[reg]],
          crs = DOWNLOAD_CRS)
        file.copy(shpzip, file)
      }
    )
    output$btn_dl_Southern <- downloadHandler(
      filename = function() {
        return("BC-Priority-Support-Tool_SOUTHERN.zip")
      },
      content = function(file) {
        reg <- "Southern"
        print(paste("Download:", reg))
        shpzip <- prepare_region_shp(dat, 
          region =  reg, 
          inputs = slider_vals[[reg]], 
          custom_var = custom_var()[[reg]],
          crs = DOWNLOAD_CRS)
        file.copy(shpzip, file)
      }
    )
    output$btn_dl_Northern <- downloadHandler(
      filename = function() {
        return("BC-Priority-Support-Tool_NORTHERN.zip")
      },
      content = function(file) {
        reg <- "Northern"
        print(paste("Download:", reg))
        shpzip <- prepare_region_shp(dat, 
          region =  reg, 
          inputs = slider_vals[[reg]], 
          custom_var = custom_var()[[reg]],
          crs = DOWNLOAD_CRS)
        file.copy(shpzip, file)
      }
    )


    # Customization - Values -------------------------------------------
    output$custom_dl <- downloadHandler(
      filename = function() {
        return("BC-Prioritization-Tool.xlsx")
        },
      content = function(file) {
        file.copy("www/BC-Prioritization-Tool.xlsx", file)
      }
    )

    custom_data <- reactive({

      # let dependencies know that there is nothing here
      if (is.null(input$custom_upload))
        return(NULL)

      req(!is.null(input$custom_upload))

      # Check that Excel format
      feedbackDanger("custom_upload",
                     show = is.na(excel_format(input$custom_upload$datapath)),
                     text = "Selected file is not in Excel format")
      req(!is.na(excel_format(input$custom_upload$datapath)),
          cancelOutput = TRUE)

      # Check sheet names
      sheets <- readxl::excel_sheets(input$custom_upload$datapath)
      good <- all(names(dat) %in% sheets)
      missing <- names(dat)[!names(dat) %in% sheets]
      feedbackDanger(
        "custom_upload",
        show = !good,
        text = paste0("Missing at least one regional work sheet in Excel file (",
                      paste0(missing, collapse = ", "), ")"))
      req(good)

      # Upload to list
      d <- lapply(
        names(dat),
        function(x) {
          z <- read_excel(input$custom_upload$datapath, sheet = x)
          for (i in seq_len(ncol(z)))
            if (is.numeric(z[[i]]))
              z[[i]] <- round(z[[i]], 4)
          z
        })
      names(d) <- names(dat)

      # Check that we have the same columns in each sheet
      good <- sapply(names(d),
                     function(x) all(names(d[[x]]) %in% names(dat[[x]])),
                     USE.NAMES = TRUE)
      which_bad <- names(good)[!good]
      good <- all(good)
      feedbackDanger("custom_upload",
                     show = !good,
                     text = paste0("Column names in some sheets don't match ",
                                   "the original data (",
                                   paste0(which_bad, collapse = ", "),")"))

      req(all(unlist(good)))

      # Check that custom data column is filled at least somewhere
      good <- any(sapply(
        names(d), function(x) !all(is.na(d[[x]][["CustomVariable"]]))))
      feedbackDanger("custom_upload",
                     show = !good,
                     text = "No sheet has custom values")
      req(good)

      # Warn if missing many values
      good <- sapply(d, function(x) sum(is.na(x$CustomVariable))/nrow(x) < 0.5,
                     USE.NAMES = TRUE)
      which_bad <- names(good)[!good]
      feedbackWarning("custom_upload",
                      show = any(!good),
                      text = paste0("Some sheets are missing more than 50% ",
                                    "of the custom values (",
                                    paste0(which_bad, collapse = ", "), ")"))

      d
    })

    # Return just the custom variable columns
    custom_var <- reactive({
      if(!is.null(custom_data())) {
        l <- list()
        for(x in names(custom_data())) {
          # Ensure that the user hasn't changed the order of the data
          # l[[x]] <- custom_data()[[x]]$CustomVariable[order(dat[[x]]$UID)]
          # match by UID just to be sure: it can be chr or int !!!
          l[[x]] <- custom_data()[[x]]$CustomVariable[
            match(dat[[x]]$UID, custom_data()[[x]]$UID)]
        }
      } else {
        # return NULL, the default for wght_zone()
        l <- as.list(rep(list(NULL), length(dat))) |> setNames(names(dat))
      }
      l
    })

    output$custom_data_preview <- renderUI({

      validate(need(!is.null(input$custom_upload), "No data uploaded"))
      req(custom_data())

      tagList(
        tabBox(tabPanel("Boreal", renderReactable(reactable(custom_data()[["Boreal"]]))),
               tabPanel("Central", renderReactable(reactable(custom_data()[["Central"]]))),
               tabPanel("Southern", renderReactable(reactable(custom_data()[["Southern"]]))),
               tabPanel("Northern", renderReactable(reactable(custom_data()[["Northern"]]))),
               width = 12
        )
      )
    })

    lapply(names(dat), function(x) {
      output[[paste0("ui_slider_", x, "_custom")]] <- renderUI({
        req(!is.null(input$custom_upload))

        sliderInput(paste0("slider_", x, "_custom"),
                    "Custom variable weight",
                    min = 0, max = 1, value = 0, step = 0.01)
      })
    })

    # Observer to add warning to controls when they do nothing
    lapply(names(dat), function(x) {
      observe({
        req(input[[paste0("slider_", x, "_custom")]], custom_var()[[x]])
        if(all(is.na(custom_var()[[x]]))) {
          showFeedbackDanger(paste0("slider_", x, "_custom"),
                             text = "No custom data available for this region")
          disable(paste0("slider_", x, "_custom"))
        }
      })
    })

    # Customization - Spatial -------------------------------------------
    custom_spatial <- reactive({

      # let dependencies know that there is nothing here
      if (is.null(input$custom_spatial_upload))
        return(NULL)

      req(!is.null(input$custom_spatial_upload))

      # Check file types
      ext <- tools::file_ext(input$custom_spatial_upload$datapath)
      ext <- tolower(ext)

      # Check for shape files
      bad <- ext %in% c("shp", "shx", "dbf", "sbn")
      feedbackDanger("custom_spatial_upload",
                     show = bad,
                     text = paste0("Shape files should be first compressed in ",
                                   "a 'zip' file before uploading"))
      req(!bad)

      # Check for correct file type
      good <- ext %in% c("tiff", "tif", "zip", "gpkg")
      feedbackDanger("custom_spatial_upload",
                     show = !good,
                     text = "Selected file is not tif, zip, or gpkg")
      req(good)

      # Check for shape file inside zip

      if(ext == "zip") {
        f <- try(unzip(input$custom_spatial_upload$datapath, list = TRUE)$Name,
                 silent = TRUE)

        bad <- inherits(f, "try-error")
        feedbackDanger(
          "custom_spatial_upload",
          show = bad,
          text = extract_error(f, "Error unzipping file"))
        req(!bad)

        f <- tools::file_ext(f)
        good <- all(c("shp", "shx", "dbf") %in% f)
        feedbackDanger(
          "custom_spatial_upload",
          show = !good,
          text = paste0("Zip file does not contain required file types for ",
                        "Shape format: 'shp', 'shx', and 'dbf'"))
        req(good)
      }


      # Upload
      p <- try(read_spatial_file(input$custom_spatial_upload$datapath),
               silent = TRUE)

      bad <- inherits(p, "try-error")
      feedbackDanger(
        "custom_spatial_upload",
        show = bad,
        text = extract_error(p, "Error loading spatial data file"))
      req(!bad)

      # add random colors
      if(!inherits(p, "RasterLayer")) {
        p[["polyCol"]] <- sample(hcl.colors(25, "RdYlBu"), nrow(p), replace=TRUE)
      }

      p
    })

    output$custom_spatial_preview <- renderLeaflet({
      req(!is.null(custom_spatial()))

      m <- map_base()
      m <- try(map_custom(m, custom_spatial()), silent = TRUE)

      validate(need(
        !inherits(m, "try-error"),
        extract_error(m, "Error plotting spatial data")))

      m
    })

    # Formulae -----------
    observeEvent(input$custom_upload, {
      shinyjs::html("math_boreal", ui_formula("boreal", custom_var = TRUE))
      shinyjs::html("math_central", ui_formula("central", custom_var = TRUE))
      shinyjs::html("math_northern", ui_formula("northern", custom_var = TRUE))
      shinyjs::html("math_southern", ui_formula("southern", custom_var = TRUE))
    })


    # Glossary ----------------------------------------
    output$glossary_table <- renderReactable({
        meta <- rbind(
            attr(dat[[1]], "metadata"),
            attr(dat[[2]], "metadata"),
            attr(dat[[3]], "metadata"),
            attr(dat[[4]], "metadata"))
        meta <- meta[!duplicated(meta$Name),-1]
        reactable(
            meta,
            columns = list(
                Name = colDef(minWidth = 100),
                Title = colDef(minWidth = 200),
                Description = colDef(minWidth = 200)
            ),
            highlight = TRUE,
            defaultPageSize = nrow(meta),
            striped = TRUE)
    })

}
