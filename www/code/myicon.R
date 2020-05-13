icon <- function(name, class = NULL, lib = "font-awesome") {
  prefixes <- list(
    "font-awesome" = "fa",
    "glyphicon" = "glyphicon"
  )
  prefix <- prefixes[[lib]]
  # determine stylesheet
  if (is.null(prefix)) {
    stop("Unknown font library '", lib, "' specified. Must be one of ",
         paste0('"', names(prefixes), '"', collapse = ", "))
  }
  # build the icon class (allow name to be null so that other functions
  # e.g. buildTabset can pass an explicit class value)
  iconClass <- ""
  if (!is.null(name)) {
    prefix_class <- prefix
    # if (prefix_class == "fa" && name %in% font_awesome_brands) {
    #   prefix_class <- "fab"
    # }
    # The classes fab, fas, far and fal are special prefix classes.
    # We detect them and "pull them to the front", replacing the default prefix class
    # This allows e.g. icon("address-book", "far") to function properly.
    if (!is.null(class) && grepl("\\b(fab|fas|far|fal)\\b", class)) {
      match <- regexpr("\\b(fab|fas|far|fal)\\b", class)
      prefix_class <- regmatches(class, match)
      class <- trimws(paste(regmatches(class, match, invert = TRUE)[[1]], collapse = ""))
    }
    iconClass <- paste0(prefix_class, " ", prefix, "-", name)
  }
    if (!is.null(class) && class != ""){
      iconClass <- paste(iconClass, class)
    }
  iconTag <- tags$i(class = iconClass)
  # font-awesome needs an additional dependency (glyphicon is in bootstrap)
  if (lib == "font-awesome") {
    htmlDependencies(iconTag) <- htmlDependency(
      "font-awesome", "5.9.0", "./www/shared/fontawesome", package = "shiny",
      stylesheet = c(
        "css/all.min.css",
        "css/v4-shims.min.css"
      )
    )
  }
  htmltools::browsable(iconTag)
}
# Helper funtion to extract the class from an icon
iconClass <- function(icon) {
  if (!is.null(icon)) icon$attribs$class
}