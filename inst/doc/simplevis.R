## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = FALSE,
  warning = FALSE,
  fig.height = 3.5, 
  fig.width = 6
)

## ---- message=FALSE, warning=FALSE--------------------------------------------
library(dplyr)
library(simplevis)
library(palmerpenguins)
library(ggplot2)
library(patchwork)
set.seed(123456789)

## -----------------------------------------------------------------------------
gg_point(penguins,
         x_var = bill_length_mm,
         y_var = body_mass_g)

## -----------------------------------------------------------------------------
plot_data <- storms %>%
  group_by(year) %>%
  summarise(wind = mean(wind))

gg_bar(plot_data,
       x_var = year,
       y_var = wind)

## -----------------------------------------------------------------------------
plot_data <- storms %>%
  group_by(year) %>%
  summarise(wind = mean(wind))

gg_hbar(plot_data, 
        x_var = wind, 
        y_var = year)

## -----------------------------------------------------------------------------
plot_data <- storms %>%
  group_by(year) %>%
  summarise(wind = mean(wind))

gg_line(plot_data, 
        x_var = year, 
        y_var = wind)

## -----------------------------------------------------------------------------
gg_boxplot(penguins, 
           x_var = species, 
           y_var = body_mass_g)

## -----------------------------------------------------------------------------
gg_density(penguins, 
           x_var = body_mass_g)

## -----------------------------------------------------------------------------
plot_data <- penguins %>%
   group_by(sex) %>%
   summarise(middle = median(body_mass_g, na.rm = TRUE),
             lower = quantile(body_mass_g, probs = 0.25, na.rm = TRUE),
             upper = quantile(body_mass_g, probs = 0.75, na.rm = TRUE))

 gg_pointrange(
   plot_data,
   x_var = sex,
   y_var = middle,
   ymin_var = lower,
   ymax_var = upper,
   y_title = "Body mass g")

## -----------------------------------------------------------------------------
gg_smooth(penguins,  
          x_var = bill_length_mm, 
          y_var = body_mass_g)

## -----------------------------------------------------------------------------
plot_data <- penguins %>% 
  group_by(species, sex) %>% 
  summarise(bill_length_mm = round(mean(bill_length_mm, na.rm = TRUE), 1))

gg_tile_col(plot_data, 
            x_var = sex, 
            y_var = species, 
            col_var = bill_length_mm, 
            label_var = bill_length_mm) 

## -----------------------------------------------------------------------------
gg_violin(penguins, 
           x_var = species, 
           y_var = body_mass_g)

## -----------------------------------------------------------------------------
gg_sf_col(example_point, 
          col_var = trend_category, 
          borders = example_borders)

## -----------------------------------------------------------------------------
gg_stars_col(example_stars,
             col_var = nitrate,
             col_na_rm = TRUE,
             borders = example_borders)

## -----------------------------------------------------------------------------
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

## -----------------------------------------------------------------------------
gg_point_col_facet(penguins, 
                   x_var = bill_length_mm, 
                   y_var = body_mass_g, 
                   col_var = sex, 
                   facet_var = species)

## -----------------------------------------------------------------------------
gg_point_col(penguins, 
             x_var = bill_length_mm, 
             y_var = body_mass_g, 
             col_var = species)

## -----------------------------------------------------------------------------
gg_point_col(penguins, 
             x_var = bill_length_mm, 
             y_var = body_mass_g, 
             col_var = species, 
             title = "Adult penguin mass by bill length and species",
             subtitle = "Palmer station, Antarctica",
             x_title = "Bill length (mm)", 
             y_title = "Body mass (g)",
             col_title = "Penguin species")

## -----------------------------------------------------------------------------
gg_point(iris, 
         x_var = Sepal.Width, 
         y_var = Sepal.Length, 
         pal = "#e7298a")

## -----------------------------------------------------------------------------
gg_point_col(penguins, 
             x_var = bill_length_mm, 
             y_var = body_mass_g, 
             col_var = species, 
             pal = c("#da3490", "#9089fa", "#47e26f"))

## -----------------------------------------------------------------------------
plot_data <- storms %>%
  group_by(year, status) %>%
  summarise(wind = mean(wind))

gg_bar_col(plot_data, 
        x_var = year, 
        y_var = wind, 
        col_var = status,
        stack = TRUE,
        x_breaks_n = 4,
        x_labels = function(x) stringr::str_sub(x, 3, 4),
        y_labels = scales::label_comma(accuracy = 0.1), 
        col_labels = c("H", "TD", "TS"),
        y_zero = T, 
        y_breaks_n = 10,
        y_expand = ggplot2::expansion(mult = c(0.025, 0.025)))

## -----------------------------------------------------------------------------
gg_point_col(penguins, 
             x_var = bill_length_mm, 
             y_var = body_mass_g, 
             col_var = sex, 
             col_na_rm = TRUE)

## -----------------------------------------------------------------------------
plot_data <- penguins %>% 
  group_by(species) %>% 
  summarise(body_mass_g = mean(body_mass_g, na.rm = TRUE))  

gg_bar_col(plot_data, 
       x_var = species, 
       y_var = body_mass_g, 
       col_var = species, 
       col_legend_none = TRUE,
       size_width = 0.5)

## -----------------------------------------------------------------------------
gg_point_col(penguins, 
             x_var = bill_length_mm, 
             y_var = body_mass_g, 
             col_var = species, 
             title = "A nice long title", 
             subtitle = "And a subtitle",
             theme = ggplot2::theme_grey())

## -----------------------------------------------------------------------------
gg_sf_col(example_polygon, 
          col_var = density, 
          col_method = "continuous",
          borders = example_borders)

## -----------------------------------------------------------------------------
gg_sf_col(example_polygon, 
          col_var = density, 
          col_method = "bin",
          borders = example_borders)

## -----------------------------------------------------------------------------
gg_stars_col(example_stars,
             col_var = nitrate,
             col_method = "quantile",
             col_cuts = c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1),
             col_na_rm = TRUE,
             borders = example_borders)

## ---- echo = FALSE, message = FALSE, warning = FALSE, fig.width = 7-----------
tibble::tribble(
  ~family, ~data, ~x_var, ~y_var, ~col_var, ~facet_var, 
  "bar", "tibble or data.frame", "Any*", "Numeric", "Categorical or numeric", "Categorical",
  "boxplot", "tibble or data.frame", "Categorical", "Numeric", "Categorical", "Categorical",
  "density", "tibble or data.frame", "Numeric", NA, "Categorical",  "Categorical", 
  "line", "tibble or data.frame", "Any", "Numeric", "Categorical", "Categorical",
  "point", "tibble or data.frame", "Any", "Numeric", "Categorical or numeric", "Categorical",
  "pointrange", "tibble or data.frame", "Any", "Numeric", "Categorical or numeric", "Categorical",
  "sf", "sf", NA, NA, "Categorical or numeric", "Categorical",
  "smooth", "tibble or data.frame", "Numeric", "Numeric", "Categorical or numeric", "Categorical",
  "stars", "stars", NA, NA, "Categorical or numeric", NA,
  "tile",  "tibble or data.frame", "Categorical", "Categorical", "Categorical or numeric", "Categorical",
  "violin", "tibble or data.frame", "Categorical", "Numeric", "Categorical", "Categorical", 
  "hbar", "tibble or data.frame", "Numeric", "Any*", "Categorical or numeric", "Categorical",
  "hboxplot", "tibble or data.frame", "Numeric", "Categorical", "Categorical", "Categorical",
  "hviolin", "tibble or data.frame", "Numeric", "Categorical", "Categorical", "Categorical", 
  "hpointrange", "tibble or data.frame", "Numeric", "Any", "Categorical or numeric", "Categorical"
  ) %>%
  DT::datatable(options = list(pageLength = 20, lengthChange = FALSE))
    

## -----------------------------------------------------------------------------
penguins %>% 
  gg_density_col(x_var = body_mass_g, 
                 col_var = sex,
                 col_na_rm = TRUE)

## -----------------------------------------------------------------------------
gg_point_col(penguins,
             x_var = bill_length_mm,
             y_var = body_mass_g,
             col_var = species) +
  ggplot2::scale_y_log10(
    name = "Bill length mm",
    breaks = function(x) pretty(x, 4),
    limits = function(x) c(min(pretty(x, 4)), max(pretty(x, 4))),
    expand = c(0, 0)
  ) 


## ---- fig.height = 4.5--------------------------------------------------------
plot_data <- penguins %>% 
  group_by(species, sex, island) %>% 
  summarise(body_mass_g = mean(body_mass_g, na.rm = TRUE))

gg_bar(plot_data, 
       x_var = sex, 
       y_var = body_mass_g, 
       size_width = 0.66, 
       x_na_rm = TRUE, 
       y_breaks_n = 3) +
  facet_grid(rows = vars(species), 
             cols = vars(island), 
             labeller = as_labeller(snakecase::to_sentence_case))


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

