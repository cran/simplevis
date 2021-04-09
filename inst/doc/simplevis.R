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
  mutate(cut = stringr::str_to_sentence(cut)) %>%
  group_by(cut) %>%
  summarise(average_price = mean(price)) %>%
  mutate(average_price = round(average_price / 1000, 1)) %>%
  mutate(cut = factor(cut, levels = c("Fair", "Good", "Very good", "Premium", "Ideal"))) 

ggplot_hbar(plot_data, average_price, cut,
            title = "Average diamond price by cut", 
            x_title = "Average price ($US thousands)", 
            y_title = "Cut")


## ---- message = FALSE, warning = FALSE, fig.height = 4, fig.width = 6---------
plot_data <- ggplot2::diamonds %>%
  mutate(cut = stringr::str_to_sentence(cut)) %>%
  group_by(cut, clarity) %>%
  summarise(average_price = mean(price)) %>%
  mutate(average_price = round(average_price / 1000, 1))

ggplot_hbar_col(plot_data, average_price, cut, clarity, 
                title = "Average diamond price by cut and clarity", 
                x_title = "Average price ($US thousands)", 
                y_title = "Cut")


## ---- message = FALSE, warning = FALSE, fig.height = 4, fig.width = 7---------
plot_data <- ggplot2::diamonds %>%
  mutate(cut = stringr::str_to_sentence(cut)) %>%
  group_by(cut, clarity) %>%
  summarise(average_price = mean(price)) %>%
  mutate(average_price = round(average_price / 1000, 1))

ggplot_hbar_facet(plot_data, average_price, cut, clarity,
                  facet_ncol = 4,
                  title = "Average diamond price by cut and clarity", 
                  x_title = "Average price ($US thousands)", 
                  y_title = "Cut")


## ---- message = FALSE, warning = FALSE, fig.height = 5, fig.width = 7---------
plot_data <- ggplot2::diamonds %>%
  mutate(cut = stringr::str_to_sentence(cut)) %>%
  group_by(cut, clarity, color) %>%
  summarise(average_price = mean(price)) %>%
  mutate(average_price = round(average_price / 1000, 1))

ggplot_hbar_col_facet(plot_data, average_price, color, clarity, cut,
                      title = "Average diamond price by colour, clarity and cut", 
                      x_title = "Average price ($US thousands)", 
                      y_title = "Colour")


## ---- echo = TRUE, eval = FALSE, message = FALSE, warning = FALSE, fig.height = 4, fig.width = 7----
#  plot_data <- storms %>%
#    group_by(year) %>%
#    summarise(average_wind = round(mean(wind), 2))
#  
#  plot <- ggplot_vbar(data = plot_data,
#                      x_var = year,
#                      y_var = average_wind,
#                      title = "Average wind speed of Atlantic storms, 1975\u20132015",
#                      x_title = "Year",
#                      y_title = "Average maximum sustained wind speed (knots)")
#  
#  plotly::ggplotly(plot) %>%
#    plotly_camera()

## ---- echo = FALSE------------------------------------------------------------
tibble::tribble(
  ~group, ~x_var, ~y_var, ~col_var, ~facet_var, ~stat,
  "vbar", "numeric, date or categorical", "numeric", "categorical or numeric", "categorical", "identity",
  "hbar", "numeric", "categorical", "categorical or numeric", "categorical", "identity",
  "line", "numeric or date", "numeric", "categorical or numeric", "categorical", "identity",
  "point", "numeric", "numeric", "categorical or numeric", "categorical", "identity",
    "boxplot", "categorical", "numeric", NA, "categorical", "boxplot or identity"
  ) %>% 
  DT::datatable()

## ---- message = FALSE, warning = FALSE, fig.height = 4, fig.width = 7---------
ggplot_sf(example_sf_point, 
          borders = nz, 
          point_size = 0.25,
          title = "Site trends, 2008\u201317",
          title_wrap = 40)

## ---- message = FALSE, warning = FALSE, fig.height = 4, fig.width = 7---------
pal <- c("#4575B4", "#D3D3D3", "#D73027")

ggplot_sf_col(example_sf_point, trend_category, 
              borders = nz, 
              point_size = 0.25, 
              pal = pal, 
              title = "Site trends, 2008\u201317",
              title_wrap = 40)

## ---- message = FALSE, warning = FALSE, fig.height = 4, fig.width = 7---------
ggplot_sf_facet(example_sf_point, trend_category, 
                borders = nz, 
                point_size = 0.25,
                title = "Site trends, 2008\u201317")

## ---- message = FALSE, warning = FALSE, fig.height = 5, fig.width = 7---------
pal <- c("#4575B4", "#D3D3D3", "#D73027")

ggplot_sf_col_facet(example_sf_point, trend_category, trend_category,
                    borders = nz, 
                    point_size = 0.25, 
                    pal = pal,
                    title = "Site trends, 1990\u201317")

## ---- echo = TRUE, results = 'hide', message = FALSE, warning = FALSE, fig.height = 4, fig.width = 7----
leaflet_sf(data = example_sf_polygon)

## ---- echo = TRUE, results = 'hide', message = FALSE, warning = FALSE, fig.height = 4, fig.width = 7----
leaflet_sf_col(example_sf_polygon, density, 
               col_method = "bin", 
               col_cuts = c(0, 10, 50, 100, 150, 200, Inf), 
               col_labels_dp = 0,
               title = "Modelled density, 2013-2017")

## ---- eval=F------------------------------------------------------------------
#  run_template("template1") # a graph and table
#  run_template("template2") # a leaflet map, as well as graph and table
#  

