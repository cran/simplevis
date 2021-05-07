## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- message=FALSE, warning=FALSE--------------------------------------------
library(dplyr)
library(simplevis)

## ---- message = FALSE, warning = FALSE, fig.height = 3, fig.width = 6---------
plot_data <- ggplot2::diamonds %>% 
  slice_sample(prop = 0.05) %>% 
  tibble::as_tibble()

plot_data

ggplot_point(plot_data, carat, price)

## ---- message = FALSE, warning = FALSE, fig.height = 3, fig.width = 6---------
plot_data <- ggplot2::diamonds %>%
  group_by(cut) %>%
  summarise(average_price = mean(price)) %>%
  mutate(average_price = average_price / 1000) %>%  
  mutate(cut = stringr::str_to_sentence(cut)) %>% 
  tibble::as_tibble()

plot_data

ggplot_hbar(plot_data, 
            x_var = average_price, 
            y_var = cut,
            title = "Average diamond price by cut", 
            x_title = "Average price ($US thousands)", 
            y_title = "Cut")

## ---- message = FALSE, warning = FALSE, fig.height = 3, fig.width = 6---------
plot_data <- storms %>%
  group_by(year) %>%
  summarise(average_wind = mean(wind)) %>% 
  tibble::as_tibble()

plot_data

ggplot_vbar(plot_data, 
            x_var = year, 
            y_var = average_wind)

## ---- message = FALSE, warning = FALSE, fig.height = 3, fig.width = 6---------
plot_data <- storms %>%
  group_by(year) %>%
  summarise(wind = mean(wind)) %>% 
  tibble::as_tibble()

plot_data 

ggplot_line(plot_data, 
            x_var = year, 
            y_var = wind)

## ---- message = FALSE, warning = FALSE, fig.height = 3, fig.width = 6---------
plot_data <- iris %>%
  mutate(Species = stringr::str_to_sentence(Species)) %>% 
  tibble::as_tibble()

plot_data

ggplot_boxplot(plot_data, 
               x_var = Species, 
               y_var = Petal.Length)

## ---- message = FALSE, warning = FALSE, fig.height = 3.5, fig.width = 6-------
plot_data <- ggplot2::diamonds %>%
  group_by(cut, clarity) %>%
  summarise(average_price = mean(price)) %>% 
  mutate(average_price = average_price / 1000) %>% 
  mutate(cut = stringr::str_to_sentence(cut)) %>% 
  tibble::as_tibble()

plot_data

ggplot_hbar_col(plot_data, 
                x_var = average_price, 
                y_var = cut, 
                col_var = clarity)

## ---- message = FALSE, warning = FALSE, fig.height = 5, fig.width = 6---------
plot_data <- ggplot2::diamonds %>%
  mutate(cut = stringr::str_to_sentence(cut)) %>%
  group_by(cut, clarity) %>%
  summarise(average_price = mean(price)) %>%
  mutate(average_price = average_price / 1000) %>% 
  tibble::as_tibble()

plot_data

ggplot_hbar_facet(plot_data, 
                  x_var = average_price, 
                  y_var = cut, 
                  facet_var = clarity)

## ---- message = FALSE, warning = FALSE, fig.height = 4.5, fig.width = 6-------
plot_data <- ggplot2::diamonds %>%
  mutate(cut = stringr::str_to_sentence(cut)) %>%
  group_by(cut, clarity, color) %>%
  summarise(average_price = mean(price)) %>%
  mutate(average_price = round(average_price / 1000, 1))

ggplot_hbar_col_facet(plot_data, 
                      x_var = average_price, 
                      y_var = color, 
                      col_var = clarity, 
                      facet_var = cut)

## ---- message = FALSE, warning = FALSE, fig.height = 3, fig.width = 7---------
example_sf_point

ggplot_sf(example_sf_point, 
          size_point = 0.25)

## ---- message = FALSE, warning = FALSE, fig.height = 3, fig.width = 7---------
nz

ggplot_sf(example_sf_point, 
          borders = nz, 
          size_point = 0.25)

## ---- message = FALSE, warning = FALSE, fig.height = 3, fig.width = 7---------
example_sf_polygon

ggplot_sf_col(example_sf_polygon, 
              col_var = density, 
              borders = nz)

## ---- message = FALSE, warning = FALSE, fig.height = 3, fig.width = 7---------
ggplot_sf_facet(example_sf_point, 
                facet_var = trend_category, 
                borders = nz, 
                size_point = 0.5)

## ---- message = FALSE, warning = FALSE, fig.height = 3, fig.width = 7---------
ggplot_sf_col_facet(example_sf_point, 
                    col_var = trend_category, 
                    facet_var = trend_category,
                    borders = nz, 
                    size_point = 0.5, 
                    pal = c("#4575B4", "#D3D3D3", "#D73027"),
                    title = "Site trends, 1990\u201317")

## ---- message = FALSE, warning = FALSE, fig.width = 7-------------------------
tibble::tribble(
  ~group, ~x_var, ~y_var, ~col_var, ~facet_var, ~stat,
  "vbar", "numeric, date or categorical", "numeric", "categorical or numeric", "categorical", "identity",
  "hbar", "numeric", "categorical", "categorical or numeric", "categorical", "identity",
  "line", "numeric or date", "numeric", "categorical or numeric", "categorical", "identity",
  "point", "numeric", "numeric", "categorical or numeric", "categorical", "identity",
  "boxplot", "Categorical", "numeric", "categorical", "categorical", "boxplot or identity",
  "sf", NA, NA, "categorical or numeric", "categorical", "identity",
  ) %>% 
  DT::datatable()

## ---- eval = FALSE------------------------------------------------------------
#  plot <- ggplot_point(iris, Sepal.Width, Sepal.Length)
#  
#  plotly::ggplotly(plot)

## ---- message = FALSE, warning = FALSE, fig.height = 4, fig.width = 7---------
leaflet_sf_col(example_sf_point, 
               col_var = trend_category, 
               basemap = "light",
               pal = c("#0D94A3", "#C4C4C7", "#AE4E51"), 
               title = "Monitored trends, 2008\u201317")

## ---- message = FALSE, warning = FALSE, fig.height = 4, fig.width = 7---------
ggplot_point(iris, Sepal.Width, Sepal.Length) +
  ggplot2::geom_smooth(ggplot2::aes(Sepal.Width, Sepal.Length), col = viridis::viridis(4)[2])

## ---- message = FALSE, warning = FALSE, fig.height = 4, fig.width = 7---------
ggplot_point(iris, Sepal.Width, Sepal.Length, pal = "#e7298a")

## ---- message = FALSE, warning = FALSE, fig.height = 4, fig.width = 7---------
ggplot_point_col(iris, Sepal.Width, Sepal.Length, Species, 
                 pal = c("#1B9E77", "#D95F02", "#7570B3"))


