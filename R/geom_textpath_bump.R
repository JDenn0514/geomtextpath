
# utility functions -------------------------------------------------------

#  Convert unique values to ordered sequence of integers
discretise <- function(x) {
  match(x, unique(x))
}

# Shorthand for `vapply(split(x, group), ...)`
gapply <- function(x, group, FUN, FUN.VALUE, ..., USE.NAMES = FALSE) {
  vapply(
    split(x, group),
    FUN = FUN, FUN.VALUE = FUN.VALUE,
    ...,
    USE.NAMES = USE.NAMES
  )
}

# Gives the indices of a vector where a run length starts
run_start <- function(x) {
  x <- discretise(x)
  c(0, which(diff(x) != 0)) + 1
}

# Allows information about co-ordinate system to be passed to grob
get_polar_params <- function(coord) {
  if (inherits(coord, "CoordPolar")) {
    list(x = 0.5, y = 0.5, theta = coord$theta)
  } else  {
    NULL
  }
}

# Parse characters as expressions with validity checks
safe_parse <- function(text) {
  if (!is.character(text)) stop("`text` must be a character vector")
  out <- vector("expression", length(text))
  for (i in seq_along(text)) {
    expr     <- parse(text = text[[i]])
    out[[i]] <- if (length(expr) == 0) NA else expr[[1]]
  }
  out
}

# Convert internal ggplot-type aesthetic data frame to gp object for text
data_to_text_gp <- function(data) {
  grid::gpar(
    col        = alpha(data$textcolour %||% data$colour, data$alpha),
    fontsize   = data$size * .pt,
    fontface   = data$fontface %||% data$font,
    fontfamily = data$family,
    lineheight = data$lineheight,
    tracking   = data$spacing
  )
}

# Convert internal ggplot-type aesthetic data frame to gp object for lines
data_to_path_gp <- function(
    data,
    lineend   = "butt",
    linejoin  = "round",
    linemitre = 10) {

  if (all(data$linetype %in% c("0", "blank", NA))) {
    grid::gpar(lty = 0)
  } else {
    grid::gpar(
      col       = alpha(data$linecolour %||% data$colour, data$alpha),
      fill      = alpha(data$linecolour %||% data$colour, data$alpha),
      lwd       = data$linewidth * .pt,
      lty       = data$linetype,
      lineend   = lineend,
      linejoin  = linejoin,
      linemitre = linemitre
    )
  }
}

# This function is to check that user input is what we would expect it to be.
# It checks `value` for being of a particular class `type` and have `length`
# length. Optionally, one can allow NAs or NULLs.
assert <- function(
    value,
    type,
    length     = 1L,
    allow_NAs  = FALSE,
    allow_NULL = FALSE,
    argname    = deparse(substitute(value))
) {

  if (is.null(value) && allow_NULL) {
    return(NULL)
  }
  force(argname)
  message <- character()
  if (!inherits(value, type)) {
    message <- c(
      message,
      paste0("`", argname, "` must be a `", type, "` vector.")
    )
  }
  if (length(value) != length) {
    message <- c(
      message,
      paste0("`", argname, "` must be of length ", length, ".")
    )
  }
  if (isFALSE(allow_NAs) && anyNA(value)) {
    message <- c(
      message,
      paste0("`", argname, "` contains NAs whereas it cannot.")
    )
  }
  if (length(message)) {
    message <- c(
      "Unexpected input:",
      message
    )
    abort(message)
  }
  value
}


# This sets parameters for text on a path that aren't expected to change
# during the construction of a grob.
static_text_params <- function(
    .type          = "text",
    text_only      = FALSE,
    gap            = NULL,
    upright        = TRUE,
    halign         = "center",
    offset         = NULL,
    parse          = FALSE,
    straight       = FALSE,
    padding        = unit(0.05, "inch"),
    text_smoothing = 0,
    rich           = FALSE,
    remove_long    = FALSE
) {

  if (is.null(gap)) {
    gap <- switch(.type, text = NA, FALSE)
  }
  halign <- rlang::arg_match0(halign, c("center", "left", "right"))
  if (!isFALSE(rich) && !isFALSE(parse)) {
    warn(paste0("Plotmath expressions are incompatible with rich text.\n",
                "Setting `rich = FALSE`. for now."))
    rich <- FALSE
  }

  list(
    text_only      = assert(text_only,      "logical"),
    gap            = assert(gap,            "logical", allow_NAs = TRUE),
    upright        = assert(upright,        "logical"),
    parse          = assert(parse,          "logical"),
    straight       = assert(straight,       "logical", allow_NULL = TRUE),
    padding        = assert(padding,        "unit"),
    offset         = assert(offset,         "unit", allow_NULL = TRUE),
    text_smoothing = assert(text_smoothing, "numeric"),
    rich           = assert(rich,           "logical"),
    remove_long    = assert(remove_long,    "logical"),
    halign         = halign
  )
}

# Automatically capture static text parameters
set_params <- function(...) {

  params      <- list(...)
  text_names  <- names(formals(static_text_params))
  text_names  <- intersect(text_names, names(params))
  text_params <- do.call(static_text_params, params[text_names])
  params      <- params[setdiff(names(params), text_names)]
  params$text_params <- text_params
  params
}

update_params <- function(params, type = "text") {

  text_params <- params$text_params %||% static_text_params(.type = type)
  text_names  <- names(formals(static_text_params))
  text_names  <- intersect(text_names, names(params))
  for (i in text_names) {
    text_params[[i]] <- params[[i]]
    params[[i]] <- NULL
  }
  params$text_params <- text_params
  params
}





# geom function -----------------------------------------------------------

textpathGrob <- function(
    label,
    x              = 0.5,
    y              = 0.5,
    id             = 1L,
    just           = "centre",
    hjust          = NULL,
    vjust          = NULL,
    halign         = "left",
    angle          = 0,
    straight       = FALSE,
    rich           = FALSE,
    gp_text        = gpar(),
    gp_path        = gpar(),
    gp_box         = gpar(),
    gap            = NA,
    upright        = TRUE,
    text_smoothing = 0,
    polar_params   = NULL,
    padding        = unit(0.05, "inch"),
    label.padding  = unit(0.25, "lines"),
    label.r        = unit(0.15, "lines"),
    remove_long    = FALSE,
    arrow          = NULL,
    default.units  = "npc",
    name           = NULL,
    vp             = NULL,
    as_label       = FALSE
) {

  cl <- if (as_label) "labelpath" else "textpath"

  if (missing(label)) return(gTree(name = name, vp = vp, cl = cl))

  n_label <- length(label)
  id      <- discretise(id)

  check_grob_input(x, y, id, n_label, angle)

  # Match justification to labels length
  hjust  <- rep_len(resolveHJust(just, hjust), n_label)
  vjust  <- rep_len(resolveVJust(just, vjust), n_label)
  halign <- rep_len(halign, n_label)

  label  <- measure_label(label, gp = gp_text, vjust = vjust,
                          halign = halign, straight = straight, rich = rich)

  x <- as_unit(x, default.units)
  y <- as_unit(y, default.units)

  if (!is.null(polar_params)) {
    polar_params$x <- unit(polar_params$x, default.units)
    polar_params$y <- unit(polar_params$y, default.units)
  }

  path <- data_frame(x = x, y = y, id = id, line_x = x, line_y = y)

  if (text_smoothing != 0) path <- path_smoother(path, text_smoothing)

  gTree(
    textpath = list(
      data          = path,
      label         = label,
      gp_text       = attr(label, "gp"),
      gp_path       = gp_path,
      gp_box        = gp_box,
      arrow         = arrow,
      params        = list(
        upright       = upright,
        polar_params  = polar_params,
        angle         = angle,
        padding       = padding,
        label.padding = label.padding,
        label.r       = label.r,
        hjust         = hjust,
        vjust         = vjust,
        halign        = halign,
        gap           = gap,
        remove_long   = remove_long)
    ),
    name = name,
    vp = vp,
    cl = cl
  )
}

check_grob_input <- function(x, y, id, n_label, angle) {

  stopifnot(
    "`x` is not of the same length as `id`" =
      length(x) == length(id),
    "`y` is not the same length as `x`" =
      length(x) == length(y),
    "Cannot match labels to paths." =
      n_label == max(id),
    "`angle` must be length 1 or the same length as `x`." =
      (length(x) == length(angle)) || length(angle) == 1
  )
}

# make the geomtextpath object
GeomTextpath <- ggproto("GeomTextpath", Geom,

                        required_aes = c("x", "y", "label"),

                        # These aesthetics will all be available to the draw_panel function
                        default_aes = aes(
                          colour     = "black",
                          size       = 3.88,
                          hjust      = 0.5,
                          vjust      = 0.5,
                          family     = "",
                          fontface   = 1,
                          lineheight = 1.2,
                          alpha      = 1,
                          linewidth  = 0.5,
                          linetype   = 1,
                          spacing    = 0,
                          linecolour = NULL,
                          textcolour = NULL,
                          angle      = 0
                        ),

                        extra_params = c("na.rm", names(formals(static_text_params))[-1]),

                        setup_params = function(data, params) {
                          update_params(params, type = "text")
                        },

                        setup_data = function(data, params) {
                          if (isTRUE(params$text_params$text_only)) {
                            data$linetype <- 0
                          }
                          if (all(data$group == -1) && !is.null(data$label)) {
                            data$group <- discretise(data$label)
                          }
                          data
                        },

                        # Do we want this draw_key?
                        draw_key = draw_key_text,

                        # The main draw_panel function is where we process our aesthetic data frame
                        # into a tree of grobs for plotting.
                        draw_panel = function(
    data, panel_params, coord,
    lineend = "butt", linejoin = "round", linemitre = 10,
    text_params = static_text_params("text"), arrow = NULL
                        ) {

                          # We need to change groups to numeric to order them appropriately
                          data$group <- discretise(data$group)

                          # If there is more than one text string associated with any of the groups,
                          # we warn that only the first is used
                          if (!all(gapply(data$label, data$group,
                                          function(x) all(x == x[1]), logical(1)))) {
                            warn(paste("geom_textpath: Multiple strings found in at",
                                       "least one group. Only the first will be used."))
                          }

                          # Now we can sort the data by group
                          data <- data[order(data$group), , drop = FALSE]

                          # All our transformations occur after the coord transform:
                          data <- coord_munch(coord, data, panel_params)

                          # Get first observation of each group
                          first <- run_start(data$group)

                          text_gp <- data_to_text_gp(data[first, , drop = FALSE])
                          path_gp <- data_to_path_gp(
                            data[first, , drop = FALSE],
                            lineend = lineend, linejoin = linejoin, linemitre = linemitre
                          )

                          safe_labels <- if (text_params$parse) {
                            safe_parse(as.character(data$label[first]))
                          } else {
                            data$label[first]
                          }

                          geomtextpath::textpathGrob(
                            label          = safe_labels,
                            x              = data$x,
                            y              = data$y,
                            id             = data$group,
                            hjust          = data$hjust[first],
                            vjust          = text_params$offset %||% data$vjust[first],
                            halign         = text_params$halign,
                            gap            = text_params$gap,
                            gp_text        = text_gp,
                            gp_path        = path_gp,
                            straight       = text_params$straight,
                            upright        = text_params$upright,
                            text_smoothing = text_params$text_smoothing,
                            default.units  = "npc",
                            angle          = data$angle,
                            polar_params   = get_polar_params(coord),
                            padding        = text_params$padding,
                            rich           = text_params$rich,
                            arrow          = arrow,
                            remove_long    = text_params$remove_long
                          )
                        }
)


#' @export
#' @rdname geom_textpath
geom_textline_bump <- function(
    mapping     = NULL,
    data        = NULL,
    stat        = "identity",
    position    = "identity",
    na.rm       = FALSE,
    orientation = NA,
    show.legend = NA,
    inherit.aes = TRUE,
    ...,
    lineend     = "butt",
    linejoin    = "round",
    linemitre   = 10,
    arrow       = NULL
) {

  layer(
    data        = data,
    mapping     = mapping,
    stat        = StatBump,
    geom        = GeomTextline,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params      = set_params(
      na.rm     = na.rm,
      lineend   = lineend,
      linejoin  = linejoin,
      linemitre = linemitre,
      arrow     = arrow,
      ...
    )
  )
}




GeomTextline <- ggproto("GeomTextline", GeomTextpath,

                        setup_params = function(data, params) {
                          params$flipped_aes <- has_flipped_aes(data, params, ambiguous = TRUE)
                          update_params(params, type = "text")
                        },

                        extra_params = c("na.rm", "orientation"),

                        setup_data = function(data, params) {
                          data$flipped_aes <- params$flipped_aes
                          data             <- flip_data(data, params$flipped_aes)
                          data             <- data[order(data$PANEL, data$group, data$x), ]

                          flip_data(data, params$flipped_aes)
                        }
)
