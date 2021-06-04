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

gg_bar(plot_data, year, wind)

## -----------------------------------------------------------------------------
gg_point(iris, Sepal.Width, Sepal.Length)

## -----------------------------------------------------------------------------
plot_data <- storms %>%
  group_by(year) %>%
  summarise(wind = mean(wind))

gg_line(plot_data, year, wind)

## -----------------------------------------------------------------------------
gg_boxplot(storms, year, wind)

## ---- fig.height = 3----------------------------------------------------------
plot_data <- ggplot2::diamonds %>%
  group_by(cut) %>%
  summarise(price = mean(price))

gg_hbar(plot_data, price, cut)

## -----------------------------------------------------------------------------
gg_sf(example_sf_point, borders = nz)

## -----------------------------------------------------------------------------
gg_point(penguins, bill_length_mm, body_mass_g)

## -----------------------------------------------------------------------------
gg_point_col(penguins, bill_length_mm, body_mass_g, sex)

## -----------------------------------------------------------------------------
gg_point_facet(penguins, bill_length_mm, body_mass_g, species)

## -----------------------------------------------------------------------------
gg_point_col_facet(penguins, bill_length_mm, body_mass_g, sex, species)

## -----------------------------------------------------------------------------
gg_point_col(penguins, bill_length_mm, body_mass_g, species, 
             title = "Adult penguin mass by bill length and species",
             subtitle = "Palmer station, Antarctica",
             x_title = "Bill length (mm)", 
             y_title = "Body mass (g)",
             col_title = "Penguin species",
             caption = "Source: Gorman KB, Williams TD, Fraser WR (2014)")

## -----------------------------------------------------------------------------
gg_point_col(penguins, bill_length_mm, body_mass_g, species)

## -----------------------------------------------------------------------------
gg_point(iris, Sepal.Width, Sepal.Length, pal = "#e7298a")


## -----------------------------------------------------------------------------
plot_data <- storms %>%
  group_by(year) %>%
  summarise(wind = mean(wind))

gg_line(plot_data, year, wind, 
        x_pretty_n = 4,
        x_labels = function(x) stringr::str_sub(x, 3, 4),
        y_labels = scales::comma_format(accuracy = 0.1), 
        y_zero = T, 
        y_pretty_n = 10,
        y_expand = c(0, 2))

## -----------------------------------------------------------------------------
gg_point_col(penguins, bill_length_mm, body_mass_g, sex, col_na = FALSE)

## -----------------------------------------------------------------------------
gg_sf_col(example_sf_point, trend_category, 
          size_point = 0.5,
          pal = c("#4575B4", "#D3D3D3", "#D73027"))

## -----------------------------------------------------------------------------
gg_sf_col(example_sf_point, trend_category,
          borders = nz, 
          size_point = 0.5,
          pal = c("#4575B4", "#D3D3D3", "#D73027"))


## -----------------------------------------------------------------------------
leaflet_sf_col(example_sf_point, trend_category,
          pal = c("#4575B4", "#D3D3D3", "#D73027"))

## ---- message = FALSE, warning = FALSE, fig.width = 7-------------------------
tibble::tribble(
  ~type, ~data, ~x_var, ~y_var, ~col_var, ~facet_var, ~stat,
  "bar", "tibble or data.frame", "Any*", "Numeric", "Categorical or numeric", "Categorical", "Identity",
  "hbar", "tibble or data.frame", "Numeric", "Any*", "Categorical or numeric", "Categorical", "Identity",
  "line", "tibble or data.frame", "Any*", "Numeric", "Categorical or numeric", "Categorical", "Identity",
  "point", "tibble or data.frame", "Any*", "Numeric", "Categorical or numeric", "Categorical", "Identity",
  "boxplot", "tibble or data.frame", "Any*", "Numeric", "Categorical", "Categorical", "Boxplot or identity",
  "sf", "sf", NA, NA, "Categorical or numeric", "Categorical", "Identity",
  ) %>% 
  DT::datatable()

## ---- eval = FALSE------------------------------------------------------------
#  plot <- gg_point_col(penguins, bill_length_mm, body_mass_g, species)
#  
#  plotly::ggplotly(plot) %>%
#    plotly_camera()

## ---- eval = FALSE------------------------------------------------------------
#  plot <- gg_point_col(penguins, bill_length_mm, body_mass_g, species)
#  
#  plotly::ggplotly(plot) %>%
#    plotly_camera()
#  
#  plot_data <- penguins %>%
#    mutate_text()
#  
#  plot <- gg_point_col(plot_data, bill_length_mm, body_mass_g, species,
#                       text_var = text,
#                       font_family = "Helvetica")
#  
#  plotly::ggplotly(plot, tooltip = "text") %>%
#    plotly_camera()

## -----------------------------------------------------------------------------
gg_point_col(penguins, bill_length_mm, body_mass_g, species) +
  geom_smooth(aes(bill_length_mm, body_mass_g, col = species))


## ---- fig.height = 4.5--------------------------------------------------------
plot_data <- penguins %>% 
  group_by(species, sex, island) %>% 
  summarise(body_mass_g = mean(body_mass_g, na.rm = TRUE)) %>% 
  filter(!is.na(sex))

gg_bar(plot_data, sex, body_mass_g, width = 0.66, y_pretty_n = 3, x_na = FALSE) +
  facet_grid(rows = vars(species), 
             cols = vars(island), 
             labeller = as_labeller(snakecase::to_sentence_case))


