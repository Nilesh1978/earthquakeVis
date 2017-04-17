
geom_timeline <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", na.rm = FALSE,
                          show.legend = NA, inherit.aes = TRUE, ...) {

     ggplot2::layer(
          geom = GeomTimeline, mapping = mapping,
          data = data, stat = stat, position = position,
          show.legend = show.legend, inherit.aes = inherit.aes,
          params = list(na.rm = na.rm, ...)
     )
}

GeomTimeline <-
     ggplot2::ggproto(
          "GeomTimeline", ggplot2::Geom,
          required_aes = c("x"),
          default_aes = ggplot2::aes(colour = "grey", size = 1.5, alpha = 0.5,
                                     shape = 21, fill = "grey", stroke = 0.5),
          draw_key = ggplot2::draw_key_point,
          draw_panel = function(data, panel_scales, coord) {

               if (!("y" %in% colnames(data))) {
                    data$y <- 0.15
               }

               coords <- coord$transform(data, panel_scales)

               points <- grid::pointsGrob(
                    coords$x, coords$y,
                    pch = coords$shape, size = unit(coords$size / 4, "char"),
                    gp = grid::gpar(
                         col = scales::alpha(coords$colour, coords$alpha),
                         fill = scales::alpha(coords$colour, coords$alpha)
                    )
               )
               y_lines <- unique(coords$y)
               lines <- lapply(y_lines,
                               function(y) {
                                    grid::linesGrob(
                                         y = unit(c(y, y), "npc"),
                                         gp = grid::gpar(col = "grey",
                                                         lwd = .pt)
                                    )
                               })

               all <- append(list(points), lines)

               do.call(grid::gList, all)
          }
     )


geom_timeline_label <- function(mapping = NULL, data = NULL, stat = "identity",
                                position = "identity", ..., na.rm = FALSE,
                                n_max = NULL, show.legend = NA,
                                inherit.aes = TRUE) {

     ggplot2::layer(
          geom = GeomTimelineLabel, mapping = mapping,
          data = data, stat = stat, position = position,
          show.legend = show.legend, inherit.aes = inherit.aes,
          params = list(na.rm = na.rm, n_max = n_max, ...)
     )
}

GeomTimelineLabel <-
     ggplot2::ggproto(
          "GeomTimelineLabel", ggplot2::Geom,
          required_aes = c("x", "label"),
          draw_key = ggplot2::draw_key_blank,
          setup_data = function(data, params) {
               if (!is.null(params$n_max)) {
                    if (!("size" %in% colnames(data))) {
                         stop(paste("'size' aesthetics needs to be",
                                    "provided when 'n_max' is defined."))
                    }
                    data <- data %>%
                         dplyr::group_by(group) %>%
                         dplyr::top_n(params$n_max, size) %>%
                         dplyr::ungroup()
               }
               data
          },
          draw_panel = function(data, panel_scales, coord, n_max) {

               if (!("y" %in% colnames(data))) {
                    data$y <- 0.15
               }

               coords <- coord$transform(data, panel_scales)
               n_grp <- length(unique(data$group))
               offset <- 0.2 / n_grp

               lines <-
                    lapply(1:nrow(data),
                           function(i) {
                                grid::linesGrob(
                                     x = unit(c(coords$x[i], coords$x[i]),
                                              "npc"),
                                     y = unit(c(coords$y[i], coords$y[i] +
                                                     offset),
                                              "npc"),
                                     gp = grid::gpar(
                                          col = "grey"
                                     )
                                )
                           })

               names <-
                    lapply(1:nrow(data),
                           function(i) {
                                grid::textGrob(
                                     label = coords$label[i],
                                     x = unit(coords$x[i], "npc"),
                                     y = unit(coords$y[i] + offset, "npc"),
                                     just = c("left", "bottom"),
                                     rot = 45
                                )
                           })

               all <- append(lines, names)
               do.call(grid::gList, all)
          }
     )
