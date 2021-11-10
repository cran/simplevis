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

## -----------------------------------------------------------------------------
plot_data <- storms %>%
  group_by(year) %>%
  summarise(wind = mean(wind))

gg_bar(plot_data, 
       x_var = year, 
       y_var = wind)

## -----------------------------------------------------------------------------
gg_point(iris, 
         x_var = Sepal.Width, 
         y_var = Sepal.Length)

## -----------------------------------------------------------------------------
plot_data <- storms %>%
  group_by(year) %>%
  summarise(wind = mean(wind))

gg_line(plot_data, 
        x_var = year, 
        y_var = wind)

## -----------------------------------------------------------------------------
plot_data <- ggplot2::diamonds %>%
  group_by(cut) %>%
  summarise(price = mean(price))

gg_hbar(plot_data, 
        x_var = price, 
        y_var = cut)

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
gg_boxplot(storms, 
           x_var = year, 
           y_var = wind)

## -----------------------------------------------------------------------------
gg_density(penguins, 
           x_var = body_mass_g)

## -----------------------------------------------------------------------------
gg_sf_col(example_sf_point, 
          col_var = trend_category, 
          borders = nz)

## -----------------------------------------------------------------------------
library(stars)

gg_stars_col(example_stars,
             col_var = nitrate,
             col_na_rm = TRUE,
             borders = nz)

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
plot_data <- storms %>%
  group_by(year, status) %>%
  summarise(wind = mean(wind))

gg_bar_col(plot_data, 
        x_var = year, 
        y_var = wind, 
        col_var = status,
        position = "stack",
        x_pretty_n = 4,
        x_labels = function(x) stringr::str_sub(x, 3, 4),
        y_labels = function(x) scales::comma(x, accuracy = 0.1), 
        y_zero = T, 
        y_pretty_n = 10,
        y_gridlines_minor = T,
        y_expand = ggplot2::expansion(mult = c(0.025, 0.025)))

## -----------------------------------------------------------------------------
gg_point_col(penguins, 
             x_var = bill_length_mm, 
             y_var = body_mass_g, 
             col_var = sex, 
             col_na_rm = TRUE)

## -----------------------------------------------------------------------------
plot_data <- penguins %>% 
  group_by(sex, species) %>% 
  summarise(count = n())

gg_hbar_col(plot_data, 
        x_var = count, 
        y_var = species, 
        col_var = sex,
        position = "stack")

## -----------------------------------------------------------------------------
gg_sf_col(example_sf_point, 
          col_var = median, 
          col_method = "quantile",
          col_cuts = c(0, 0.25, 0.5, 0.75, 1),
          borders = nz)

## -----------------------------------------------------------------------------
gg_stars_col(example_stars,
             col_var = nitrate,
             col_method = "quantile",
             col_cuts = c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1),
             col_na_rm = TRUE,
             borders = nz)

## -----------------------------------------------------------------------------
leaflet_sf_col(example_sf_point, 
               col_var = trend_category)

## -----------------------------------------------------------------------------
library(stars)

leaflet_stars_col(example_stars, 
                  col_var = nitrate, 
                  col_method = "quantile", 
                  col_cuts = c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1),
                  col_na_rm = TRUE)


## ---- echo = FALSE, message = FALSE, warning = FALSE, fig.width = 7-----------
tibble::tribble(
  ~family, ~data, ~x_var, ~y_var, ~col_var, ~facet_var, ~stat,
  "bar", "tibble or data.frame", "Any*", "Numeric", "Categorical or numeric", "Categorical", "identity",
  "hbar", "tibble or data.frame", "Numeric", "Any*", "Categorical or numeric", "Categorical", "identity",
  "line", "tibble or data.frame", "Any", "Numeric", "Categorical", "Categorical", "identity",
  "point", "tibble or data.frame", "Any", "Numeric", "Categorical or numeric", "Categorical", "identity",
  "density", "tibble or data.frame", "Numeric", NA, "Categorical",  "Categorical", "density",
  "boxplot", "tibble or data.frame", "Any*", "Numeric", "Categorical", "Categorical", "boxplot (or identity)",
  "tile",  "tibble or data.frame", "Categorical", "Categorical", "Categorical or numeric", "Categorical", "identity",
  "sf", "sf", NA, NA, "Categorical or numeric", "Categorical", "identity",
  "stars", "stars", NA, NA, "Categorical or numeric", NA, "identity",
  ) %>% 
  DT::datatable()

## -----------------------------------------------------------------------------
penguins %>% 
  gg_density_col(x_var = body_mass_g, 
                 col_var = species)

## -----------------------------------------------------------------------------
gg_point_col(penguins, 
             x_var = bill_length_mm, 
             y_var = body_mass_g, 
             col_var = species) +
  geom_smooth(aes(x = bill_length_mm, y = body_mass_g, col = species))


## ---- fig.height = 4.5--------------------------------------------------------
plot_data <- penguins %>% 
  group_by(species, sex, island) %>% 
  summarise(body_mass_g = mean(body_mass_g, na.rm = TRUE))

gg_bar(plot_data, 
       x_var = sex, 
       y_var = body_mass_g, 
       width = 0.66, 
       x_na_rm = TRUE, 
       y_pretty_n = 3) +
  facet_grid(rows = vars(species), 
             cols = vars(island), 
             labeller = as_labeller(snakecase::to_sentence_case))


## ---- eval = FALSE------------------------------------------------------------
#  plot <- gg_point_col(penguins,
#                       x_var = bill_length_mm,
#                       y_var = body_mass_g,
#                       col_var = species)
#  
#  plotly::ggplotly(plot) %>%
#    plotly_camera()

## ---- eval = FALSE------------------------------------------------------------
#  plot <- gg_point_col(penguins,
#                       x_var = bill_length_mm,
#                       y_var = body_mass_g,
#                       col_var = species)
#  
#  plotly::ggplotly(plot) %>%
#    plotly_camera()
#  
#  plot_data <- penguins %>%
#    mutate_text()
#  
#  plot <- gg_point_col(plot_data,
#                       x_var = bill_length_mm,
#                       y_var = body_mass_g,
#                       col_var = species,
#                       text_var = text,
#                       font_family = "Helvetica")
#  
#  plotly::ggplotly(plot, tooltip = "text") %>%
#    plotly_camera()

