map_base <- function() {
    leaflet() |>
    addTiles(
        urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga",
        group = "Google",
        options = leaflet::providerTileOptions(zIndex = 200)) |>
    addProviderTiles(
        provider = "CartoDB.Positron",
        group = "CartoDB",
        options = leaflet::providerTileOptions(zIndex = 200)) |>
    addProviderTiles(
        provider = "OpenStreetMap",
        group = "Open Street Map",
        options = leaflet::providerTileOptions(zIndex = 200)) |>
    addProviderTiles(
        provider = 'Esri.WorldImagery',
        group = "ESRI",
        options = leaflet::providerTileOptions(zIndex = 200))
}

map_region <- function(map, dat, region, opacity = 0.8) {
    p <- dat[[region]]
    map |>
        addPolygons(
            data = p,
            group = region,
            fillColor = pal(factor(p$Zone, 1:5)),
            # popup = as.character(p$Zone),
            weight = 0.5,
            opacity = opacity,
            color = "white",
            fillOpacity = opacity) |>
        addPolylines(
            data = h[h$region == region,],
            group = region,
            fillColor = NA,
            weight = HERD_BORDER,
            opacity = 1,
            fillOpacity = 0,
            color = "black")
}

map_all <- function(opacity = 0.8, extra = NULL, collapsed = TRUE) {
    if (is.null(extra)) {
        map_base() |>
        map_region(dat, "Boreal", opacity = opacity) |>
        map_region(dat, "Central", opacity = opacity) |>
        map_region(dat, "Southern", opacity = opacity) |>
        map_region(dat, "Northern", opacity = opacity) |>
        addLegend(
            position = "bottomleft",
            pal = pal,
            values = as.factor(1:5),
            title = "Zone",
            opacity = opacity) |>
        addLayersControl(
            baseGroups = c("CartoDB", "ESRI", "Open Street Map", "Google"),
            overlayGroups = c("Boreal", "Central", "Southern", "Northern"),
            position = "topright",
            options = leaflet::layersControlOptions(collapsed = collapsed))
    } else {
        if (inherits(extra, "sf")) {
            map_base() |>
            map_region(dat, "Boreal", opacity = opacity) |>
            map_region(dat, "Central", opacity = opacity) |>
            map_region(dat, "Southern", opacity = opacity) |>
            map_region(dat, "Northern", opacity = opacity) |>
            addMapPane("custom_layer", zIndex = 300) |>
            addPolygons(
                data = extra,
                group = "Custom Layer",
                options = pathOptions(pane = "custom_layer"),
                # popup = popupTable(extra[,colnames(extra) != "polyCol"], row.numbers = FALSE, feature.id = FALSE),
                # fillColor = NA,
                fillColor = ~polyCol,
                # color = "red",
                color = "#666666",
                weight = 1,
                opacity = opacity,
                fillOpacity = 0.5*opacity) |>
            addLegend(
                position = "bottomleft",
                pal = pal,
                values = as.factor(1:5),
                title = "Zone",
                opacity = opacity) |>
            addLayersControl(
                baseGroups = c("CartoDB", "ESRI", "Open Street Map", "Google"),
                overlayGroups = c("Boreal", "Central", "Southern", "Northern", "Custom Layer"),
                position = "topright",
                options = leaflet::layersControlOptions(collapsed = collapsed))
        } else {
            map_base() |>
            map_region(dat, "Boreal", opacity = opacity) |>
            map_region(dat, "Central", opacity = opacity) |>
            map_region(dat, "Southern", opacity = opacity) |>
            map_region(dat, "Northern", opacity = opacity) |>
            addLegend(
                position = "bottomleft",
                pal = pal,
                values = as.factor(1:5),
                title = "Zone",
                opacity = opacity) |>
            addRasterImage(extra,
                colors = "Spectral",
                group = "Custom Layer",
                opacity = opacity) |>
            addLayersControl(
                baseGroups = c("CartoDB", "ESRI", "Open Street Map", "Google"),
                overlayGroups = c("Boreal", "Central", "Southern", "Northern", "Custom Layer"),
                position = "topright",
                options = leaflet::layersControlOptions(collapsed = collapsed))
        }
    }
}

map_all_update <- function(map, opacity = 0.8, extra = NULL, collapsed = TRUE) {
  map <- clearGroup(map, "Custom Layer")

  if (inherits(extra, "sf")) {
    map <- map |>
      addMapPane("custom_layer", zIndex = 300) |>
      addPolygons(
        data = extra,
        group = "Custom Layer",
        options = pathOptions(pane = "custom_layer"),
        # popup = popupTable(extra[,colnames(extra) != "polyCol"], row.numbers = FALSE, feature.id = FALSE),
        # fillColor = NA,
        fillColor = ~polyCol,
        # color = "red",
        color = "#666666",
        weight = 1,
        opacity = opacity,
        fillOpacity = 0.5*opacity) |>
      addLayersControl(
        baseGroups = c("CartoDB", "ESRI", "Open Street Map", "Google"),
        overlayGroups = c("Boreal", "Central", "Southern", "Northern", "Custom Layer"),
        position = "topright",
        options = leaflet::layersControlOptions(collapsed = collapsed))
  } else if(inherits(extra, "SpatRaster")) {
    map <- map |>
      addRasterImage(extra,
                     colors = "Spectral",
                     group = "Custom Layer",
                     opacity = opacity) |>
      addLayersControl(
        baseGroups = c("CartoDB", "ESRI", "Open Street Map", "Google"),
        overlayGroups = c("Boreal", "Central", "Southern", "Northern", "Custom Layer"),
        position = "topright",
        options = leaflet::layersControlOptions(collapsed = collapsed))
  }
  map
}



map_region_popup <- function(map, p, region, opacity = 0.8) {
    md <- attr(p, "meta")
    up <- as.data.frame(p)[,md$Name]
    up$Zone <- as.character(up$Zone)
    for (i in seq_len(ncol(up)))
        if (is.numeric(up[[i]]))
            up[[i]] <- round(up[[i]], 3)
    names(up) <- md$Title
    # map |>
    #     addPolygons(
    #         data = p,
    #         group = region,
    #         fillColor = pal(factor(p$Zone, 1:5)),
    #         popup = popupTable(up, row.numbers = FALSE, feature.id = FALSE),
    #         weight = 0.5,
    #         opacity = opacity,
    #         color = "white",
    #         fillOpacity = opacity) |>
    #     addPolygons(
    #         data = h[h$region == region,],
    #         group = region,
    #         fillColor = NA,
    #         weight = 1,
    #         opacity = opacity,
    #         color = "blue")
    hs <- h[h$region == region,]
    for (j in unique(hs$HERD_NA)) {
        pj <- p[p$HERD_NA == j,]
        upj <- up[p$HERD_NA == j,]
        map <- map |>
            addMapPane("herds", zIndex = 420) |>  
            addPolygons(
                data = pj,
                group = j,
                fillColor = pal(factor(pj$Zone, 1:5)),
                popup = popupTable(upj, row.numbers = FALSE, feature.id = FALSE),
                weight = 0.5,
                options = pathOptions(pane = "herds"),
                opacity = opacity,
                color = "white",
                fillOpacity = opacity) |>
            addPolylines(
                data = hs[hs$HERD_NA == j,],
                group = j,
                options = list(zIndex = 400),
                fillColor = NA,
                weight = HERD_BORDER,
                opacity = 1,
                fillOpacity = 0,
                color = "black")
    }
    map
}

map_one <- function(map, dat, region, inputs = NULL, custom_var = NULL,
                    opacity = 0.8, extra = NULL, 
                    scrip_layer = NULL, scrip_opacity = NULL,
                    collapsed = TRUE) {

    if (is.null(scrip_opacity))
        scrip_opacity <- opacity

    p <- wght_zone(dat, region, inputs = inputs, custom_var = custom_var)

    if (region == "Central" && !is.null(scrip_layer)) {
        map <- map |>
        addMapPane("scrip", zIndex = 410) |>  
        addPolygons(
            data = s,
            group = "SCRIP",
            fillColor = pal2(factor(s$PriorityCl, c("High", "Medium", "Low"))),
            weight = 0.5,
            options = pathOptions(pane = "scrip"),
            opacity = scrip_opacity,
            color = "black",
            fillOpacity = scrip_opacity) |>
        addLegend(
            position = "bottomright",
            pal = pal2,
            values = factor(c("High", "Medium", "Low"), c("High", "Medium", "Low")),
            title = "SCRIP",
            opacity = scrip_opacity)
        grp <- c(unique(p$HERD_NA), "SCRIP")
    } else {
        grp <- unique(p$HERD_NA)
    }

    if (is.null(extra)) {
        map |>
        map_region_popup(p, region, opacity = opacity) |>
        addLegend(
            position = "bottomleft",
            pal = pal,
            values = as.factor(1:5),
            title = "Zone",
            opacity = opacity) |>
        addLayersControl(
            baseGroups = c("CartoDB", "ESRI", "Open Street Map", "Google"),
            overlayGroups = grp,
            position = "topright",
            options = leaflet::layersControlOptions(collapsed = collapsed))
    } else {
        if (inherits(extra, "sf")) {
            map |>
            map_region_popup(p, region, opacity = opacity) |>
            addLegend(
                position = "bottomleft",
                pal = pal,
                values = as.factor(1:5),
                title = "Zone",
                opacity = opacity) |>
            addMapPane("custom_layer", zIndex = 300) |>
            addPolygons(
                data = extra,
                group = "Custom Layer",
                options = pathOptions(pane = "custom_layer"),
                popup = popupTable(extra[,colnames(extra) != "polyCol"], row.numbers = FALSE, feature.id = FALSE),
                # fillColor = NA,
                fillColor = ~polyCol,
                # color = "red",
                color = "#666666",
                weight = 1,
                opacity = opacity,
                fillOpacity = 0.5*opacity) |>
            addLayersControl(
                baseGroups = c("CartoDB", "ESRI", "Open Street Map", "Google"),
                overlayGroups = c(grp, "Custom Layer"),
                position = "topright",
                options = leaflet::layersControlOptions(collapsed = collapsed))
        } else {
            map |>
            map_region_popup(p, region, opacity = opacity) |>
            addLegend(
                position = "bottomleft",
                pal = pal,
                values = as.factor(1:5),
                title = "Zone",
                opacity = opacity) |>
            addRasterImage(extra,
                colors = "Spectral",
                group = "Custom Layer",
                opacity = opacity) |>
            addLegend(position = "bottomright",
                pal = colorNumeric("Spectral", 
                    # range(raster::values(extra), na.rm=TRUE), 
                    range(terra::values(extra)[,1L], na.rm=TRUE), 
                    na.color = "transparent"),
                # values = raster::values(extra),
                values = terra::values(extra)[,1L],
                group = "Custom Layer",
                opacity = opacity,
                title = "Custom Layer") |>
            addLayersControl(
                baseGroups = c("CartoDB", "ESRI", "Open Street Map", "Google"),
                overlayGroups = c(grp, "Custom Layer"),
                position = "topright",
                options = leaflet::layersControlOptions(collapsed = collapsed))
        }
    }
}

map_one_base <- function(dat, region, inputs = NULL, custom_var = NULL,
                         opacity = 0.8, extra = NULL, 
                         scrip_layer = NULL, scrip_opacity = NULL,
                         collapsed = TRUE) {

    if (is.null(scrip_opacity))
        scrip_opacity <- opacity

    p <- wght_zone(dat, region, inputs = inputs, custom_var = custom_var)

    if (region == "Central" && !is.null(scrip_layer)) {
        m <- map_base() |>
        addMapPane("scrip", zIndex = 410) |>  
        addPolygons(
            data = s,
            group = "SCRIP",
            fillColor = pal2(factor(s$PriorityCl, c("High", "Medium", "Low"))),
            weight = 0.5,
            options = pathOptions(pane = "scrip"),
            opacity = scrip_opacity,
            color = "black",
            fillOpacity = scrip_opacity) |>
        addLegend(
            position = "bottomright",
            pal = pal2,
            values = factor(c("High", "Medium", "Low"), c("High", "Medium", "Low")),
            title = "SCRIP",
            opacity = scrip_opacity)
        grp <- c(unique(p$HERD_NA), "SCRIP")
    } else {
        m <- map_base()
        grp <- unique(p$HERD_NA)
    }

    if (is.null(extra)) {
        m |>
        map_region_popup(p, region, opacity = opacity) |>
        addLegend(
            position = "bottomleft",
            pal = pal,
            values = as.factor(1:5),
            title = "Zone",
            opacity = opacity) |>
        addLayersControl(
            baseGroups = c("CartoDB", "ESRI", "Open Street Map", "Google"),
            overlayGroups = grp,
            position = "topright",
            options = leaflet::layersControlOptions(collapsed = collapsed))
    } else {
        if (inherits(extra, "sf")) {
            map_base() |>
            map_region_popup(p, region, opacity = opacity) |>
            addLegend(
                position = "bottomleft",
                pal = pal,
                values = as.factor(1:5),
                title = "Zone",
                opacity = opacity) |>
            addMapPane("custom_layer", zIndex = 300) |>
            addPolygons(
                data = extra,
                group = "Custom Layer",
                options = pathOptions(pane = "custom_layer"),
                popup = popupTable(extra[,colnames(extra) != "polyCol"], row.numbers = FALSE, feature.id = FALSE),
                # fillColor = NA,
                fillColor = ~polyCol,
                # color = "red",
                color = "#666666",
                weight = 1,
                opacity = opacity,
                fillOpacity = 0.5*opacity) |>
            addLayersControl(
                baseGroups = c("CartoDB", "ESRI", "Open Street Map", "Google"),
                overlayGroups = c(grp, "Custom Layer"),
                position = "topright",
                options = leaflet::layersControlOptions(collapsed = collapsed))
        } else {
            map_base() |>
            map_region_popup(p, region, opacity = opacity) |>
            addLegend(
                position = "bottomleft",
                pal = pal,
                values = as.factor(1:5),
                title = "Zone",
                opacity = opacity) |>
            addRasterImage(extra,
                colors = "Spectral",
                group = "Custom Layer",
                opacity = opacity) |>
            addLegend(position = "bottomright",
                pal = colorNumeric("Spectral", 
                    # range(raster::values(extra), na.rm=TRUE), 
                    range(terra::values(extra)[,1L], na.rm=TRUE), 
                    na.color = "transparent"),
                # values = raster::values(extra),
                values = terra::values(extra)[,1L],
                group = "Custom Layer",
                opacity = opacity,
                title = "Custom Layer") |>
            addLayersControl(
                baseGroups = c("CartoDB", "ESRI", "Open Street Map", "Google"),
                overlayGroups = c(grp, "Custom Layer"),
                position = "topright",
                options = leaflet::layersControlOptions(collapsed = collapsed))
        }

    }
}

# map_base() |> map_region_popup(p, "Boreal")
# map_one(dat, "Boreal")
map_custom <- function(map, custom, opacity = 0.8, collapsed = TRUE) {

    if(inherits(custom, "SpatRaster")) {
        m <- map |>
        addRasterImage(custom,
            colors = "Spectral",
            group = "Custom Layer",
            opacity = opacity) |>
        addLegend(position = "bottomright",
            pal = colorNumeric("Spectral", 
                # range(raster::values(custom), na.rm=TRUE), 
                range(terra::values(extra)[,1L], na.rm=TRUE),
                na.color = "transparent"),
            # values = raster::values(custom),
            values = terra::values(extra)[,1L],
            opacity = opacity,
            title = "Custom Layer")
    } else {
        m <- map |>
        addPolygons(
            data = custom,
            group = "Custom Layer",
            popup = popupTable(custom[,colnames(custom) != "polyCol"], row.numbers = FALSE, feature.id = FALSE),
            # fillColor = "white",
            fillColor = custom$polyCol,
            weight = 1,
            opacity = opacity,
            color = "#666666",
            # color = "red",
            fillOpacity = opacity)
        }

    m <- m |>
        addLayersControl(
            baseGroups = c("CartoDB", "ESRI", "Open Street Map", "Google"),
            overlayGroups = "Custom Layer",
            position = "topright",
            options = leaflet::layersControlOptions(collapsed = collapsed))
    m
}


## determine supported file formats
file_format <- function(file) {
    ext <- tools::file_ext(file) |>
      tolower()
    switch(ext,
        "tif" = "geotiff",
        "tiff" = "geotiff",
        "gpkg" = "geopackage",
        "zip" = "shapefile",
        stop("Unsupported file format: ", ext))
}

## load supported file formats
read_spatial_file <- function(file, dTolerance = 10) {
    ff <- file_format(file) |>
      tolower()
    if (ff == "geotiff") {
        # x <- stars::read_stars(file)
        # x <- stars::st_warp(x, crs = 4326)
        x <- terra::rast(file)
        # x <- terra::project(x, "epsg:4326", method = "near")
        # x <- terra::as.raster(x)
        # x <- raster::raster(file)
#        crs(r) <- CRS("+init=epsg:4326")
#        x <- raster::resample(x, CRS("+init=epsg:4326"))
        if (dim(x)[3L] > 1)
            stop("Raster file can only have 1 channel/layer.")
    }
    if (ff == "geopackage") {
        x <- sf::st_read(file)
        x <- sf::st_transform(x, crs = 4326)
        x <- st_zm(x, drop = TRUE, what = "ZM")
        x <- st_simplify(x, dTolerance = dTolerance, preserveTopology = TRUE)
    }
    if (ff == "shapefile") {
        fp <- file.path(tempdir(), "shapefile")
        unzip(file, exdir = fp)
        fl <- list.files(fp)
        x <- sf::st_read(file.path(fp, fl[tools::file_ext(fl) == "shp"]))
        on.exit(unlink(fp, recursive = TRUE))
        x <- sf::st_transform(x, crs = 4326)
        x <- st_zm(x, drop = TRUE, what = "ZM")
        x <- st_simplify(x, dTolerance = dTolerance, preserveTopology = TRUE)
    }
    x
}

extract_error <- function(e, message) {
 if(inherits(e, "try-error")) {
   e <- paste0(message, ": ", conditionMessage(attr(e, "condition")))
 } else e <- ""
 e
}

## prepare zipped shapefile based on regional custom weighted results
## returns the zip file path & name
prepare_region_shp <- function(dat, region, inputs = NULL, custom_var = NULL, dir = tempdir(), crs = 4269) {
    p <- wght_zone(dat, region, inputs = inputs, custom_var = custom_var)
    p <- sf::st_transform(p, crs = crs)

    tmp_dir <- file.path(dir, region)
    if (dir.exists(tmp_dir))
        unlink(tmp_dir, recursive = TRUE, force = TRUE)
    dir.create(tmp_dir)
    on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE), add = TRUE)
    sf::st_write(p, file.path(tmp_dir, paste0("BC-Priority-Support-Tool_", toupper(region), ".shp")))
    fl <- list.files(tmp_dir)
    out <- file.path(dir, paste0("BC-Priority-Support-Tool_", toupper(region), ".zip"))
    zip(out, list.files(tmp_dir, full.names = TRUE), flags = "-j") # junk path
    out
}
