#' print.hydrus_model
#'
#' @param x object of class 'hydrus_model
#' @param indent indentation width
#' @param color logical, default TRUE
#'
#' @returns prints list in pretty way
#' @export
#'
#' @examples print
print.hydrus_model <- function(x, indent = 0, color = TRUE) {
  # Ensure input is a list
  if (!is.list(x)) {
    stop("Input must be a list.")
  }

  # Detect if colors are supported
  color_supported <- color && crayon::has_color()

  # Define a palette for different nesting levels
  palette <- list(
    crayon::green$bold,
    crayon::blue$bold,
    crayon::magenta$bold,
    crayon::yellow$bold,
    crayon::cyan$bold
  )

  # Function to pick color based on level
  get_color <- function(level) {
    if (!color_supported) return(identity) # No color
    palette[[ (level %% length(palette)) + 1 ]]
  }

  # Loop through each element
  for (name in names(x)) {
    display_name <- ifelse(is.null(name) || name == "", "<unnamed>", name)

    # Add $ prefix to match R's list element style
    display_name <- paste0("$", display_name)

    # Apply color for the key based on nesting level
    name_str <- get_color(indent)(display_name)

    # Print the name with indentation
    cat(strrep("  ", indent), name_str, ": ", sep = "")

    # If element is another list, recurse
    if (is.list(x[[name]])) {
      cat("\n")
      print.hydrus_model(x[[name]], indent + 1, color_supported)
    } else {
      # Convert element to string safely
      val <- tryCatch(
        paste(capture.output(print(x[[name]])), collapse = " "),
        error = function(e) "<unprintable>"
      )
      # Apply value color (white if colors supported)
      val_str <- if (color_supported) crayon::white(val) else val
      cat(val_str, "\n")
    }
  }
}
