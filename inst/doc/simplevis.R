## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = FALSE,
  warning = FALSE,
  fig.height = 3.5, 
  fig.width = 6
)

## -----------------------------------------------------------------------------
library(dplyr)
library(simplevis)
library(palmerpenguins)
library(ggplot2)
library(patchwork)
set.seed(123456789)

## ---- echo=FALSE, fig.width=8, fig.height=3-----------------------------------
plot_data <- penguins %>%
  group_by(species, sex) %>%
  summarise(body_mass_g = mean(body_mass_g, na.rm = TRUE))

p1 <- gg_bar_col(
  plot_data,
  y_var = body_mass_g,
  x_var = species,
  col_var = sex,
  col_na_rm = TRUE,
  size_width = 0.6,
  y_labels = function(x)
    x / 1000,
  y_title = "Body mass kg",
  title = "gg_bar*()"
)

p2 <- gg_boxplot_col(
  penguins,
  x_var = species,
  y_var = body_mass_g,
  col_var = sex,
  col_na_rm = TRUE,
  size_point = 0.5,
  y_labels = function(x)
    x / 1000,
  y_title = "Body mass kg",
  title = "gg_boxplot*()"
)

p1 + p2

## ---- echo=FALSE, fig.width=8, fig.height=3-----------------------------------
p3 <- gg_density_col(
  penguins,
  x_var = body_mass_g,
  col_var = species,
  col_na_rm = TRUE,
  y_labels = scales::label_number(accuracy = 0.0001),
  x_breaks_n = 2,
  x_labels = function(x)
    x / 1000,
  x_title = "Body mass kg",
  title = "gg_density*()"
)

p4 <- gg_histogram_col(
  penguins,
  x_var = body_mass_g,
  col_var = species,
  col_na_rm = TRUE,
  x_breaks_n = 3,
  x_labels = function(x)
    x / 1000,
  y_labels = scales::label_comma(accuracy = 1),
  x_title = "Body mass kg",
  title = "gg_histogram*()"
)

p3 + p4

## ---- echo=FALSE, fig.width=8, fig.height=3-----------------------------------
p5 <- gg_hbar_col(
  plot_data,
  x_var = body_mass_g,
  y_var = species,
  col_var = sex,
  col_na_rm = TRUE,
  size_width = 0.6,
  x_breaks_n = 3,
  x_labels = function(x)
    x / 1000,
  x_title = "Body mass kg",
  title = "gg_hbar*()"
)

p6 <- gg_hboxplot_col(
  penguins,
  x_var = body_mass_g,
  y_var = species,
  col_var = sex,
  col_na_rm = TRUE,
  size_point = 0.5,
  x_breaks_n = 2,
  x_labels = function(x)
    x / 1000,
  x_title = "Body mass kg",
  title = "gg_hboxplot*()"
)

p5 + p6

## ---- echo=FALSE, fig.width=8, fig.height=3-----------------------------------
plot_data <- penguins %>%
  group_by(sex, species) %>%
  summarise(
    middle = median(body_mass_g, na.rm = TRUE),
    lower = quantile(body_mass_g, probs = 0.25, na.rm = TRUE),
    upper = quantile(body_mass_g, probs = 0.75, na.rm = TRUE)
  )

p7 <- gg_hpointrange_col(
  plot_data,
  xmiddle_var = middle,
  xmin_var = lower,
  xmax_var = upper,
  y_var = species,
  col_var = sex,
  col_na_rm = TRUE,
  x_breaks_n = 3,
  x_labels = function(x)
    x / 1000,
  x_title = "Body mass kg",
  size_point = 1,
  y_dodge = 0.3,
  title = "gg_hpointrange*()"
)

p8 <- gg_hviolin_col(
  penguins,
  x_var = body_mass_g,
  y_var = species,
  col_var = sex,
  x_breaks_n = 2,
  x_labels = function(x)
    x / 1000,
  x_title = "Body mass kg",
  col_na_rm = TRUE, 
  title = "gg_hviolin*()"
)

p7 + p8

## ---- echo=FALSE, fig.width=8, fig.height=3-----------------------------------
plot_data <- penguins %>%
  group_by(year, species) %>% 
  summarise(body_mass_g = mean(body_mass_g, na.rm = T))

p9 <- gg_line_col(
  plot_data,
  x_var = year,
  y_var = body_mass_g,
  col_var = species,
  x_labels = scales::label_number(accuracy = 1, big.mark = "")
)

p10 <- gg_point_col(
  penguins,
  x_var = bill_length_mm,
  y_var = body_mass_g,
  col_var = species,
  x_breaks_n = 3,
  x_labels = scales::label_comma(accuracy = 1),
  y_labels = function(x) x / 1000,
  y_title = "Body mass kg",
  size_point = 0.5,
  title = "gg_point*()"
)

p9 + p10

## ---- echo=FALSE, fig.width=8, fig.height=3-----------------------------------
plot_data <- penguins %>%
  group_by(sex, species) %>%
  summarise(
    middle = median(body_mass_g, na.rm = TRUE),
    lower = quantile(body_mass_g, probs = 0.25, na.rm = TRUE),
    upper = quantile(body_mass_g, probs = 0.75, na.rm = TRUE)
  )

p11 <- gg_pointrange_col(
  plot_data,
  x_var = sex,
  ymiddle_var = middle,
  ymin_var = lower,
  ymax_var = upper,
  col_var = species,
  x_na_rm = TRUE,
  x_dodge = 0.3,
  size_point = 1,
  y_labels = function(x) x / 1000,
  y_title = "Body mass kg",
  title = "gg_pointrange*()"
)

p12 <- gg_smooth_col(
  penguins,
  x_var = bill_length_mm,
  y_var = body_mass_g,
  col_var = species,
  size_point = 0.5,
  x_breaks_n = 3,
  x_labels = scales::label_comma(accuracy = 1),
  y_labels = function(x) x / 1000,
  y_title = "Body mass kg",
  title = "gg_smooth*()"
) 

p11 + p12

## ---- echo=FALSE, fig.width=8, fig.height=3-----------------------------------
plot_data <- penguins %>%
  group_by(species, sex) %>%
  summarise(body_mass_g = mean(body_mass_g, na.rm = TRUE))

p13 <- gg_tile_col(
  plot_data,
  x_var = sex,
  y_var = species,
  col_var = body_mass_g,
  x_na_rm = TRUE,
  col_labels = scales::label_comma(accuracy = 1),
  title = "gg_tile*()"
)

p14 <- gg_violin_col(
  penguins,
  x_var = species,
  y_var = body_mass_g,
  col_var = sex,
  col_na_rm = TRUE,
  title = "gg_violin*()", 
  y_labels = function(x) x / 1000,
  y_title = "Body mass kg",
)

p13 + p14

## ---- echo=FALSE, fig.width=8, fig.height=3-----------------------------------
p15 <- gg_sf_col(
  example_point,
  col_var = trend_category,
  borders = example_borders,
  size_point = 0.5,
  title = "gg_sf*()"
)

p16 <- gg_stars_col(
  example_stars,
  col_var = nitrate,
  col_na_rm = TRUE,
  borders = example_borders,
  col_labels = scales::label_comma(accuracy = 1),
  title = "gg_stars*()"
)

p15 + p16

## ---- fig.width = 5-----------------------------------------------------------
gg_point(penguins, 
         x_var = bill_length_mm, 
         y_var = body_mass_g)

## -----------------------------------------------------------------------------
gg_point_col(penguins, 
             x_var = bill_length_mm, 
             y_var = body_mass_g, 
             col_var = sex)

## -----------------------------------------------------------------------------
gg_point_facet(penguins, 
               x_var = bill_length_mm, 
               y_var = body_mass_g, 
               facet_var = species)

## ---- fig.width = 7-----------------------------------------------------------
gg_point_col_facet(penguins, 
                   x_var = bill_length_mm, 
                   y_var = body_mass_g, 
                   col_var = sex, 
                   facet_var = species)

## -----------------------------------------------------------------------------
gg_point_col(penguins, 
             x_var = bill_length_mm, 
             y_var = body_mass_g, 
             col_var = species, 
             pal = c("#da3490", "#9089fa", "#47e26f"))

## -----------------------------------------------------------------------------
gg_point_col(penguins, 
         x_var = bill_length_mm, 
         y_var = body_mass_g, 
         col_var = flipper_length_mm, 
         col_method = "continuous",
         subtitle = "col_method = 'continuous'")

## ---- echo=FALSE, fig.width=8, fig.height=3-----------------------------------
p1 <- gg_point_col(penguins, 
             x_var = bill_length_mm, 
             y_var = body_mass_g, 
             col_var = flipper_length_mm, 
             col_method = "bin",
             size_point = 0.5,
             x_breaks_n = 3,
             x_labels = scales::label_comma(accuracy = 1),
             subtitle = "col_method = 'bin'")

p2 <- gg_point_col(penguins, 
             x_var = bill_length_mm, 
             y_var = body_mass_g, 
             col_var = flipper_length_mm, 
             col_method = "quantile",
             size_point = 0.5,
             x_breaks_n = 3,
             x_labels = scales::label_comma(accuracy = 1),
             col_labels = scales::label_comma(accuracy = 1),
             subtitle = "col_method = 'quantile'")
p1 + p2

## -----------------------------------------------------------------------------
gg_point_col(penguins, 
             x_var = bill_length_mm, 
             y_var = body_mass_g, 
             col_var = species, 
             title = "A nice title", 
             subtitle = "And a subtitle",
             theme = ggplot2::theme_grey())

## -----------------------------------------------------------------------------
custom_theme <- gg_theme(
  pal_body = "white",
  pal_title = "white",
  pal_subtitle = "white",
  pal_background = c("#232323", "black"),
  pal_gridlines = "black", 
  gridlines_h = TRUE,
  gridlines_v = TRUE)

gg_point_col(penguins,
             species,
             x_var = bill_length_mm,
             y_var = body_mass_g, 
             theme = custom_theme)

## -----------------------------------------------------------------------------
plot_data <- storms %>%
  group_by(year, status) %>%
  summarise(wind = mean(wind))

gg_line_col(plot_data, 
           x_var = year, 
           y_var = wind, 
           col_var = status,
           x_breaks_n = 10,
           x_labels = function(x) stringr::str_sub(x, 3, 4),
           y_labels = scales::label_dollar(accuracy = 1), 
           col_labels = c("H", "TD", "TS"),
           y_zero = TRUE, 
           y_breaks_n = 10,
           y_expand = ggplot2::expansion(add = c(0, 10)))

## -----------------------------------------------------------------------------
gg_point_col(penguins, 
             x_var = bill_length_mm, 
             y_var = body_mass_g, 
             col_var = species, 
             title = "Adult penguin mass by bill length and species",
             subtitle = "Palmer station, Antarctica",
             x_title = "Bill length (mm)", 
             y_title = "Body mass (g)",
             col_title = "Species")

## -----------------------------------------------------------------------------
gg_sf_col(example_point, 
          col_var = trend_category, 
          borders = example_borders)

## -----------------------------------------------------------------------------
gg_stars_col(example_stars,
             col_var = nitrate,
             col_method = "quantile",
             col_cuts = c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1),
             col_na_rm = TRUE,
             borders = example_borders)

## ---- fig.width = 4-----------------------------------------------------------
plot_data <- penguins %>%
  filter(!is.na(body_mass_g)) %>% 
  group_by(species) %>%
  summarise_boxplot_stats(body_mass_g) 

gg_bar_col(plot_data,
           x_var = species,
           y_var = middle,
           col_var = species,
           col_legend_none = TRUE,
           col_na_rm = TRUE,
           y_title = "Body mass g") +
  ggplot2::geom_errorbar(ggplot2::aes(x = species, ymin = lower, ymax = upper),
                         width = 0.2) +
  ggplot2::geom_text(ggplot2::aes(x = species, y = lower - 500, label = middle),
                     col = "white") +
  ggplot2::scale_y_continuous(
    name = "Body mass g",
    breaks = function(x) pretty(x, 5),
    limits = function(x) c(min(pretty(x, 5)), max(pretty(x, 5))),
    expand = c(0, 0)
  )            

## -----------------------------------------------------------------------------
library(patchwork)

p1 <- gg_point(penguins, 
               x_var = species, 
               y_var = body_mass_g, 
               x_jitter = 0.2, 
               alpha_point = 0.5) 

p2 <- gg_boxplot(penguins, 
                 x_var = species, 
                 y_var = body_mass_g) 

p1 + p2

## ---- eval = FALSE------------------------------------------------------------
#  plot <- gg_point_col(penguins,
#                       x_var = bill_length_mm,
#                       y_var = body_mass_g,
#                       col_var = species)
#  
#  plotly::ggplotly(plot) %>%
#    plotly_camera()

## ---- echo = FALSE, message = FALSE, warning = FALSE, fig.width = 7-----------
tibble::tribble(
  ~family, ~data, ~x_var, ~y_var, ~col_var, ~facet_var, 
  "bar", "tibble or data.frame", "Any*", "Numeric", "Categorical or numeric", "Categorical",
  "boxplot", "tibble or data.frame", "Categorical", "Numeric", "Categorical", "Categorical",
  "density", "tibble or data.frame", "Numeric", NA, "Categorical",  "Categorical", 
  "histogram", "tibble or data.frame", "Numeric", NA, "Categorical",  "Categorical", 
  "line", "tibble or data.frame", "Any", "Numeric", "Categorical", "Categorical",
  "point", "tibble or data.frame", "Any", "Numeric", "Categorical or numeric", "Categorical",
  "pointrange", "tibble or data.frame", "Any", "Numeric", "Categorical or numeric", "Categorical",
  "sf", "sf", NA, NA, "Categorical or numeric", "Categorical",
  "smooth", "tibble or data.frame", "Numeric", "Numeric", "Categorical", "Categorical",
  "stars", "stars", NA, NA, "Categorical or numeric", NA,
  "tile",  "tibble or data.frame", "Categorical", "Categorical", "Categorical or numeric", "Categorical",
  "violin", "tibble or data.frame", "Categorical", "Numeric", "Categorical", "Categorical", 
  "hbar", "tibble or data.frame", "Numeric", "Any*", "Categorical or numeric", "Categorical",
  "hboxplot", "tibble or data.frame", "Numeric", "Categorical", "Categorical", "Categorical",
  "hviolin", "tibble or data.frame", "Numeric", "Categorical", "Categorical", "Categorical", 
  "hpointrange", "tibble or data.frame", "Numeric", "Any", "Categorical or numeric", "Categorical"
  ) %>%
  DT::datatable(options = list(pageLength = 20, lengthChange = FALSE))
    

