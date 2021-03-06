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

## ---- fig.height = 3----------------------------------------------------------
plot_data <- ggplot2::diamonds %>%
  group_by(cut) %>%
  summarise(price = mean(price))

gg_hbar(plot_data, 
        x_var = price, 
        y_var = cut)

## -----------------------------------------------------------------------------
gg_density(penguins, 
           x_var = body_mass_g)

## -----------------------------------------------------------------------------
gg_boxplot(storms, 
           x_var = year, 
           y_var = wind)

## -----------------------------------------------------------------------------
gg_sf(example_sf_point, 
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
             col_title = "Penguin species",
             caption = "Source: Gorman KB, Williams TD, Fraser WR (2014)")

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
        y_labels = scales::comma_format(accuracy = 0.1), 
        y_zero = T, 
        y_pretty_n = 10,
        y_gridlines_minor = T,
        y_expand = ggplot2::expansion(mult = c(0.025, 0.025)))

## -----------------------------------------------------------------------------
gg_point_col(penguins, 
             x_var = bill_length_mm, 
             y_var = body_mass_g, 
             col_var = sex, 
             col_na = FALSE)

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
          col_var = trend_category)

## -----------------------------------------------------------------------------
gg_sf_col(example_sf_point, 
          col_var = trend_category,
          borders = nz)


## -----------------------------------------------------------------------------
leaflet_sf_col(example_sf_point, 
               col_var = trend_category)

## ---- message = FALSE, warning = FALSE, fig.width = 7-------------------------
tibble::tribble(
  ~family, ~data, ~x_var, ~y_var, ~col_var, ~facet_var, ~stat,
  "bar", "tibble or data.frame", "Any*", "Numeric", "Categorical or numeric", "Categorical", "identity",
  "hbar", "tibble or data.frame", "Numeric", "Any*", "Categorical or numeric", "Categorical", "identity",
  "line", "tibble or data.frame", "Any", "Numeric", "Categorical or numeric", "Categorical", "identity",
  "point", "tibble or data.frame", "Any", "Numeric", "Categorical or numeric", "Categorical", "identity",
  "density", "tibble or data.frame", "Numeric", NA, "Categorical",  "Categorical", "density",
  "boxplot", "tibble or data.frame", "Any*", "Numeric", "Categorical", "Categorical", "boxplot (or identity)",
  "sf", "sf", NA, NA, "Categorical or numeric", "Categorical", "identity",
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
       x_na = FALSE, 
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

