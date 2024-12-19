#' @import leaflet
#' @importFrom stats quantile
#' @importFrom dplyr mutate ntile
#' @importFrom rlang sym
NULL

# R/color_functions.R
#' Create Default Color Matrix for Bivariate Choropleth
#' 
#' @description Creates a 3x3 color matrix with default colors for bivariate choropleth maps
#' @return A matrix of color hex codes
#' @export
create_default_color_matrix <- function() {
  matrix(c(
    "#64acbe", "#627f8c", "#574249",
    "#b0d5df", "#ad9ea5", "#985356",
    "#e8e8e8", "#e4acac", "#c85a5a"
  ), nrow = 3, byrow = TRUE)
}

#' Calculate Tertiles for Bivariate Variables
#' 
#' @param data A data frame containing the variables to analyze
#' @param var_1 Character string naming the first variable
#' @param var_2 Character string naming the second variable
#' @return A data frame with added tertile columns
#' @export
calculate_tertiles <- function(data, var_1, var_2) {
  if (!all(c(var_1, var_2) %in% names(data))) {
    stop("One or both of the specified variables are not in the dataset.")
  }
  
  var1_sym <- rlang::sym(var_1)
  var2_sym <- rlang::sym(var_2)
  
  if (length(unique(data[[var_1]])) < 3) {
    warning(paste(var_1, "has fewer than 3 unique values; tertiles may not be meaningful."))
  }
  if (length(unique(data[[var_2]])) < 3) {
    warning(paste(var_2, "has fewer than 3 unique values; tertiles may not be meaningful."))
  }
  
  data$var_1_tertile <- dplyr::ntile(data[[var_1]], 3)
  data$var_2_tertile <- dplyr::ntile(data[[var_2]], 3)
  
  return(data)
}

#' Assign Colors Based on Tertiles
#' 
#' @param data A data frame containing tertile columns
#' @param color_matrix A 3x3 matrix of colors
#' @importFrom dplyr %>%
#' @return A data frame with added color column
#' @export
assign_colors <- function(data, color_matrix) {
  if (!all(c("var_1_tertile", "var_2_tertile") %in% names(data))) {
    stop("The dataset must contain 'var_1_tertile' and 'var_2_tertile' columns.")
  }
  
  adjusted_color_matrix <- color_matrix[nrow(color_matrix):1, ]
  
  data$color <- mapply(
    function(t1, t2) adjusted_color_matrix[t1, t2],
    data$var_1_tertile, data$var_2_tertile
  )
  
  return(data)
}

# R/legend_functions.R
#' Calculate Tertile Breaks
#' 
#' @param data A data frame
#' @param variable Character string naming the variable
#' @return A numeric vector of break points
#' @export
calculate_tertile_breaks <- function(data, variable) {
  if (!variable %in% names(data)) {
    stop("The specified variable is not in the dataset.")
  }
  
  stats::quantile(data[[variable]], probs = seq(0, 1, length.out = 4), na.rm = TRUE)
}

#' Generate Legend HTML
#' 
#' @param var_1 Character string naming first variable
#' @param var_2 Character string naming second variable
#' @param var_1_breaks Numeric vector of breaks for first variable
#' @param var_2_breaks Numeric vector of breaks for second variable
#' @param color_matrix Matrix of colors
#' @return HTML string for legend
#' @export
generate_legend_html <- function(var_1, var_2, var_1_breaks, var_2_breaks, color_matrix) {
  # Initialize the legend container
  legend_html <- paste0(
    '<div style="background-color: #FFFFFF; padding-top: 10px; padding-left: 70px; ',
    'padding-bottom: 20px; padding-right: 20px; margin: 2px; position: relative;">',
    '<h4>Map Legend</h4>',
    '<div style="display: flex; align-items: center;">'
  )
  
  # Add vertical axis with tick marks and values
  legend_html <- paste0(legend_html,
                        '<div style="position: relative; margin-right: 10px;">',
                        '<hr style="margin: 0; border: none; border-left: 1px solid #000000; height: 120px; position: relative; top: 0px;">',
                        '<div style="position: absolute; left: -5px; top: 0%; border-top: 1px solid #000000; width: 12px;"></div>',
                        '<div style="position: absolute; left: -5px; top: 33%; border-top: 1px solid #000000; width: 12px;"></div>',
                        '<div style="position: absolute; left: -5px; top: 66%; border-top: 1px solid #000000; width: 12px;"></div>',
                        '<div style="position: absolute; left: -5px; top: 100%; border-top: 1px solid #000000; width: 12px;"></div>',
                        sprintf('<div style="position: absolute; right: 20px; top: 0%%; transform: translateY(-50%%); font-size: 0.7em;">%s</div>', round(var_1_breaks[4], 1)),
                        sprintf('<div style="position: absolute; right: 20px; top: 33%%; transform: translateY(-50%%); font-size: 0.7em;">%s</div>', round(var_1_breaks[3], 1)),
                        sprintf('<div style="position: absolute; right: 20px; top: 66%%; transform: translateY(-50%%); font-size: 0.7em;">%s</div>', round(var_1_breaks[2], 1)),
                        sprintf('<div style="position: absolute; right: 20px; top: 100%%; transform: translateY(-50%%); font-size: 0.7em;">%s</div>', round(var_1_breaks[1], 1)),
                        '</div>'
  )
  
  # Add color matrix
  legend_html <- paste0(legend_html, '<table style="border-collapse: collapse;">')
  for (i in 1:nrow(color_matrix)) {
    legend_html <- paste0(legend_html, "<tr>")
    for (j in 1:ncol(color_matrix)) {
      legend_html <- paste0(
        legend_html,
        sprintf('<td style="width: 40px; height: 40px; background-color: %s;"></td>', color_matrix[i, j])
      )
    }
    legend_html <- paste0(legend_html, "</tr>")
  }
  legend_html <- paste0(legend_html, "</table></div>")
  
  # Add horizontal axis with tick marks and values
  legend_html <- paste0(legend_html,
                        '<div style="position: relative; text-align: center; margin-top: 10px;">',
                        '<hr style="margin: 0; border: none; border-top: 1px solid #000000; left: 14px; width: 124px; position: relative;">',
                        '<div style="position: absolute; top: -5px; left: 10%; border-left: 1px solid #000000; height: 12px;"></div>',
                        '<div style="position: absolute; top: -5px; left: 39%; border-left: 1px solid #000000; height: 12px;"></div>',
                        '<div style="position: absolute; top: -5px; left: 69%; border-left: 1px solid #000000; height: 12px;"></div>',
                        '<div style="position: absolute; top: -5px; left: 100%; border-left: 1px solid #000000; height: 12px;"></div>',
                        sprintf('<div style="position: absolute; top: 12px; left: 10%%; transform: translateX(-50%%); font-size: 0.7em;">%s</div>', round(var_2_breaks[1], 1)),
                        sprintf('<div style="position: absolute; top: 12px; left: 39%%; transform: translateX(-50%%); font-size: 0.7em;">%s</div>', round(var_2_breaks[2], 1)),
                        sprintf('<div style="position: absolute; top: 12px; left: 69%%; transform: translateX(-50%%); font-size: 0.7em;">%s</div>', round(var_2_breaks[3], 1)),
                        sprintf('<div style="position: absolute; top: 12px; left: 100%%; transform: translateX(-50%%); font-size: 0.7em;">%s</div>', round(var_2_breaks[4], 1)),
                        '</div>'
  )
  
  # Add axis labels
  legend_html <- paste0(legend_html,
                        sprintf('<div style="text-align: center; margin-top: 30px;"><span style="font-size: 0.8em;">%s &#8594;</span></div>', var_2),
                        sprintf('<div style="position: absolute; top: 50%%; left: -30px; transform: translateY(-50%%) rotate(-90deg); text-align: center;"><span style="font-size: 0.8em;">%s &#8594;</span></div>', var_1),
                        '</div>'
  )
  
  return(legend_html)
}

#' Create Bivariate Choropleth Map
#' 
#' @param data A spatial data frame
#' @param var_1 Character string naming first variable
#' @param var_2 Character string naming second variable
#' @param color_matrix Optional custom color matrix
#' @param custom_labels Optional vector of custom HTML labels for tooltips
#' @return A leaflet map object
#' @import leaflet
#' @importFrom sf st_transform
#' @export
create_bivariate_map <- function(data, var_1, var_2, color_matrix = NULL, custom_labels = NULL) {
  # Ensure data is in the right projection for Leaflet (WGS84)
  if (sf::st_crs(data) != 4326) {
    data <- sf::st_transform(data, 4326)
  }
  
  if (is.null(color_matrix)) {
    color_matrix <- create_default_color_matrix()
  }
  
  # Calculate tertiles and assign colors
  processed_data <- data %>%
    calculate_tertiles(var_1, var_2) %>%
    assign_colors(color_matrix)
  
  # Calculate breaks
  var_1_breaks <- calculate_tertile_breaks(processed_data, var_1)
  var_2_breaks <- calculate_tertile_breaks(processed_data, var_2)
  
  # Generate legend
  legend_html <- generate_legend_html(
    var_1, var_2, var_1_breaks, var_2_breaks, color_matrix
  )
  
  # Create default labels if custom_labels not provided
  if (is.null(custom_labels)) {
    labels <- sprintf(
      "<strong>%s:</strong> %g<br/><strong>%s:</strong> %g",
      var_1, processed_data[[var_1]],
      var_2, processed_data[[var_2]]
    ) %>% lapply(htmltools::HTML)
  } else {
    labels <- lapply(custom_labels, htmltools::HTML)
  }
  
  # Create map
  leaflet(processed_data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolygons(
      fillColor = ~color,
      color = "#BDBDC3",
      weight = 1,
      opacity = 1,
      fillOpacity = 0.9,
      label = labels
    ) %>%
    addControl(html = legend_html, position = "bottomright")
}
