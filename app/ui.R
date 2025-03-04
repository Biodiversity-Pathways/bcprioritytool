# tables in map popup: https://github.com/r-spatial/leafpop

# Navigation --------------------------------------------------------------
header <- dashboardHeader(
    title = TITLE,
    titleWidth = 300
)

sidebar <- dashboardSidebar(
    sidebarMenu(
        id = "tabs",
        menuItem("Home", tabName = "home", icon = icon("home")),
        menuItem("Regions", icon = icon("map"),
            menuSubItem("Boreal", tabName = "region1"),
            menuSubItem("SMC-Northern group", tabName = "region4"),
            menuSubItem("SMC-Central group", tabName = "region2"),
            menuSubItem("SMC-Southern group", tabName = "region3")
        ),
        menuItem("Customization", tabName = "customization",
                 icon = icon("globe-americas"),
                 menuSubItem("Custom Values", tabName = "custom_values"),
                 menuSubItem("Custom Spatial Data", tabName = "custom_spatial")
        ),
        menuItem("Glossary", tabName = "glossary", icon = icon("table")),
        menuItem("Reports", tabName = "reports", icon = icon("book"),
                 menuSubItem("Priority Zones", tabName = "report_zones"),
                 menuSubItem("SCRIP Report", tabName = "report_scrip"))
    )
)


# Regions ------------------------------------------------------------------

ui_region <- function(region_name) {
    sliders <- switch(region_name,
        "Boreal" = tagList(
            sliderInput(paste0("slider_", region_name, "_1"), "Normalized bang for buck",
                min = 0, max = 1, value = 1, step = 0.01),
            sliderInput(paste0("slider_", region_name, "_2"), "Normalized caribou use index",
                min = 0, max = 1, value = 1, step = 0.01),
            uiOutput(paste0("ui_slider_", region_name, "_custom"))
        ),
        "Central" = tagList(
            sliderInput(paste0("slider_", region_name, "_1"), "Normalized bang for buck",
                min = 0, max = 1, value = 1, step = 0.01),
            sliderInput(paste0("slider_", region_name, "_2"), "Normalized caribou use index",
                min = 0, max = 1, value = 1, step = 0.01),
            uiOutput(paste0("ui_slider_", region_name, "_custom"))
        ),
        tagList(
            sliderInput(paste0("slider_", region_name, "_1"), "Normalized bang for buck",
                min = 0, max = 1, value = 1, step = 0.01),
            sliderInput(paste0("slider_", region_name, "_2"), "Normalized percent disturbed (<20 years)",
                min = 0, max = 1, value = 1, step = 0.01),
            sliderInput(paste0("slider_", region_name, "_3"), "Normalized core caribou habitat index",
                min = 0, max = 1, value = 1, step = 0.01),
            sliderInput(paste0("slider_", region_name, "_4"), "Normalized ungulate winter range",
                min = 0, max = 1, value = 1, step = 0.01),
            uiOutput(paste0("ui_slider_", region_name, "_custom"))
        )
    )
    fluidRow(
        column(width = 9,
            leafletOutput(paste0("map_", region_name), width = "100%", height = MAP_HEIGHT),
            fluidRow(
                column(8, align = "center",
                    HTML(ui_formula(region_name))
                ),
                column(4, align = "center",
                    HTML("<p>Download results as Shapefile (<a href=\"https://epsg.io/4269\" target=\"_blank\">EPSG:4269</a>) based on the last applied slider settings:</p>"),
                    tags$span(
                        downloadButton(paste0("btn_dl_", region_name), "Download Shapefile",
                                class = "btn-primary")
                    )
                )
            )
        ),
        # use switch for regions sliders
        column(width = 3,
            box(
                title = "Controls", width = NULL, solidHeader = TRUE,
                sliderInput(paste0("alpha_", region_name), "Opacity",
                    min = 0, max = 1, value = ALPHA, step = 0.05),
                if (region_name == "Central") {
                    sliderInput(paste0("alpha_", region_name, "_scrip"), "SCRIP Layer Opacity",
                        min = 0, max = 1, value = ALPHA, step = 0.05)
                } else {
                    NULL
                },
                hr(),
                h4("Zone weights"),
                sliders,
                actionButton(paste0("btn_", region_name), "Update",
                    class = "btn-primary")
            )
        )
    )
}

ui_all <- fluidRow(
    column(width = 12,
        h3("Welcome to the BC Prioritization Support Tool!"),
        p("Here you'll find the results of a prioritization process to guide habitat restoration within southern mountain caribou (SMC) and boreal caribou populations. The process directs restoration towards areas with higher gains in undisturbed caribou habitat relative to the cost of conducting restoration, while building on existing habitat protection and focusing in areas more heavily used by caribou. For the Central group of SMC, we also provide priority areas developed through the Strategic Caribou Restoration Implementation Plan (SCRIP), which was developed by West Moberly First Nations, Saulteau First Nations, Environment and Climate Change Canada, and the Province of British Columbia, via the Partnership Agreement. The SCRIP priorities are founded on shared values and aims to include Indigenous knowledge, and newly established conservation and protection measures that have been instituted through the Partnership Agreement. Please find reports for the two processes under the \"Reports\" tab."),
        leafletOutput("map_All", width = "100%", height = MAP_HEIGHT),
        p(),
        HTML("<p class=\"text-right\">Application web design by <a href=\"https://analythium.io\" target=\"_blank\">Analythium Solutions Inc.</a></p>")
    )
)


# Custom Values -----------------------------------------------------------

# Match upload button to 'btn-primary' style
custom_style <- HTML("
      .btn {
        font-size: 16px;
      }

      .btn-default.btn-file {
        background-color: #3c8dbc;
        border-color: #367fa9;
        color: white;
      }

      .form-control {
        height: 36px;
        font-size: 16px;
      }

      #custom_upload_progress .progress-bar {
        font-size: 16px;
        color: black;
      }
      #custom_spatial_upload_progress .progress-bar {
        font-size: 16px;
      }
    ")

ui_custom_values <- fluidRow(

  column(
    width = 12,
    tags$head(tags$style(custom_style)),
    box(h2("Custom Values"), width = 12, style = "font-size: 130%",
        "Here you may add custom values to the data to be included in ",
        "the models. Once you have uploaded your custom variable, ",
        "you can go back into the regions to plot priority zones and ",
        "explore the new data.",
        p(),

        "1. First download the data set ", br(),
        downloadButton("custom_dl", label = "Download Data",
                       class = "btn-primary", style = "color:white"),
        p(),
        "2. Next, modify the column ", code("CustomVariable"), " and fill ",
        "with your values of interest.",
        br(),
        p(style = "font-size: 90%; padding-left: 20px",
          HTML("<strong>Note</strong>:"),
          "Blank and 'NA' values are treated as missing",
          br(),
          HTML("<strong>Note</strong>:"),
          "There are 4 worksheets, one for each region",
          br(),
          HTML("<strong>Note</strong>: Values should be in the 0&ndash;1 range")),
        p(),
        "3. Now, upload your new data (Excel format)",
        p(),

        fileInput("custom_upload", label = NULL, buttonLabel = "Upload Data", width = "50%")
    ),
    box(h3("Data Preview"), width = 12,
        tableOutput("custom_data_preview"))

  )
)


# Custom Spatial data ---------------------------------------------------------

ui_custom_spatial <- fluidRow(
  column(
    width = 12,
    box(h2("Custom Spatial Data"), width = 12, style = "font-size: 130%",
        "Here you may add custom spatial data to the maps displayed in the ",
        "'Regions' tabs.",
        p(),
        "1. First upload your spatial data file", br(),
        p(style = "font-size: 90%; padding-left: 20px",
          HTML("<strong>Note</strong>: Acceptible formats are <em>.tif</em>, <em>.shp</em>",
          "(compressed to <em>.zip</em>) and <em>.gpkg</em>."),
          br(),
          HTML("<strong>Note</strong>:"),
          "Only raster files will be coloured by values.",
          br(),
          HTML("<strong>Note</strong>:"),
          "Maximum upload file size is 25 MB."
          ),
        fileInput("custom_spatial_upload", label = NULL,
                  buttonLabel = "Upload Spatial Data", width = "100%"),
        p(),
        "2. Next, check the preview below to ensure your spatial data looks ",
        "as it should.", br(),
        "If it does, you're ready to check out the 'Regions' tabs."
    ),
    box(h3("Map Preview"), width = 12,
        leafletOutput("custom_spatial_preview", width = "100%", height = MAP_HEIGHT))
  )
)


body <- dashboardBody(
    autoWaiter(),
    shinyFeedback::useShinyFeedback(),
    shinyjs::useShinyjs(),
    tabItems(
        tabItem("home", ui_all),
        tabItem("region1", ui_region("Boreal")),
        tabItem("region2", ui_region("Central")),
        tabItem("region3", ui_region("Southern")),
        tabItem("region4", ui_region("Northern")),
        tabItem("custom_values", ui_custom_values),
        tabItem("custom_spatial", ui_custom_spatial),
        tabItem("glossary", 
            fluidRow(column(width=12, reactableOutput("glossary_table")))
        ),
        tabItem("report_zones", 
            fluidRow(column(width=12, includeMarkdown("report_zones.md")))
        ),
        tabItem("report_scrip", 
            fluidRow(column(width=12, includeMarkdown("report_scrip.md")))
        )
    )
)


dashboardPage(
    header = header,
    sidebar = sidebar,
    body =   body,
    title = TITLE,
    skin = "green"
)
