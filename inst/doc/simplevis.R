## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, warning = FALSE, message = FALSE----------------------------------
library(simplevis)
library(dplyr)

## ---- message = FALSE, warning = FALSE, fig.height = 4, fig.width = 7---------
plot_data <- ggplot2::diamonds %>%
  mutate(cut = stringr::str_to_sentence(cut)) %>%
  group_by(cut) %>%
  summarise(average_price = mean(price)) %>%
  ungroup() %>% 
  mutate(average_price_thousands = round(average_price / 1000, 1)) %>%
  mutate(cut = factor(cut, levels = c("Fair", "Good", "Very good", "Premium", "Ideal"))) 

plot <- ggplot_hbar(data = plot_data, 
                    x_var = average_price_thousands, 
                    y_var = cut,
                    title = "Average diamond price by cut", 
                    x_title = "Average price ($US thousands)", 
                    y_title = "Cut")

plot 

## ---- message = FALSE, warning = FALSE, fig.height = 4, fig.width = 7---------
plot_data <- ggplot2::diamonds %>%
  mutate(cut = stringr::str_to_sentence(cut)) %>%
  group_by(cut, clarity) %>%
  summarise(average_price = mean(price)) %>%
  mutate(average_price_thousands = round(average_price / 1000, 1)) %>%
  ungroup()

plot <- ggplot_hbar_col(data = plot_data, 
                        x_var = average_price_thousands, 
                        y_var = cut, 
                        col_var = clarity, 
                        legend_ncol = 4,
                        title = "Average diamond price by cut and clarity", 
                        x_title = "Average price ($US thousands)", 
                        y_title = "Cut")

plot

## ---- message = FALSE, warning = FALSE, fig.height = 4, fig.width = 7---------
plot_data <- ggplot2::diamonds %>%
  mutate(cut = stringr::str_to_sentence(cut)) %>%
  group_by(cut, clarity) %>%
  summarise(average_price = mean(price)) %>%
  mutate(average_price_thousands = round(average_price / 1000, 1)) %>%
  ungroup()

plot <- ggplot_hbar_facet(data = plot_data, 
                          x_var = average_price_thousands, 
                          y_var = cut, 
                          facet_var = clarity,
                          title = "Average diamond price by cut and clarity", 
                          x_title = "Average price ($US thousands)", 
                          y_title = "Cut")

plot

## ---- message = FALSE, warning = FALSE, fig.height = 5, fig.width = 7---------
plot_data <- ggplot2::diamonds %>%
  mutate(cut = stringr::str_to_sentence(cut)) %>%
  group_by(cut, clarity, color) %>%
  summarise(average_price = mean(price)) %>%
  mutate(average_price_thousands = round(average_price / 1000, 1)) %>%
  ungroup()

plot <- ggplot_hbar_col_facet(data = plot_data, 
                              x_var = average_price_thousands, 
                              y_var = color, 
                              col_var = clarity, 
                              facet_var = cut,
                              legend_ncol = 4, 
                              title = "Average diamond price by colour, clarity and cut", 
                              x_title = "Average price ($US thousands)", 
                              y_title = "Colour")

plot

## ---- echo = TRUE, eval = FALSE, message = FALSE, warning = FALSE, fig.height = 4, fig.width = 7----
#  plot_data <- storms %>%
#    group_by(year) %>%
#    summarise(average_wind = round(mean(wind), 2)) %>%
#    ungroup()
#  
#  plot <- ggplot_vbar(data = plot_data,
#                      x_var = year,
#                      y_var = average_wind,
#                      title = "Average wind speed of Atlantic storms, 1975\u20132015",
#                      x_title = "Year",
#                      y_title = "Average maximum sustained wind speed (knots)")
#  
#  plotly::ggplotly(plot, tooltip = "text") %>%
#    plotly_remove_buttons()
#  
#  plotly::ggplotly(plot, tooltip = c("average_wind")) %>%
#    plotly_remove_buttons()

## ---- message = FALSE, warning = FALSE, fig.height = 4, fig.width = 7---------
map_data <- example_sf_nz_river_wq %>%
  dplyr::filter(period == "1998-2017", indicator == "Nitrate-nitrogen") 

ggplot_sf(data = map_data, 
          coastline = nz, 
          size = 0.25,
          title = "Monitored river nitrate-nitrogen trend sites, 2008\u201317",
          wrap_title = 40)

## ---- message = FALSE, warning = FALSE, fig.height = 4, fig.width = 7---------
map_data <- example_sf_nz_river_wq %>%
  filter(period == "1998-2017", indicator == "Nitrate-nitrogen") 

pal <- c("#4575B4", "#D3D3D3", "#D73027")

ggplot_sf_col(data = map_data, 
              col_var = trend_category, 
              coastline = nz, 
              size = 0.25, 
              pal = pal, 
              title = "Monitored river nitrate-nitrogen trends, 2008\u201317",
              wrap_title = 40)

## ---- message = FALSE, warning = FALSE, fig.height = 4, fig.width = 7---------
map_data <- example_sf_nz_river_wq %>%
 filter(period == "1998-2017", indicator == "Nitrate-nitrogen") 

ggplot_sf_facet(data = map_data, 
                facet_var = trend_category, 
                coastline = nz, 
                size = 0.25,
                title = "Monitored river nitrate-nitrogen trends, 2008\u201317")

## ---- message = FALSE, warning = FALSE, fig.height = 5, fig.width = 7---------
map_data <- example_sf_nz_river_wq %>%
 filter(period == "1998-2017", indicator %in% c("Nitrate-nitrogen", "Dissolved reactive phosphorus")) 

pal <- c("#4575B4", "#D3D3D3", "#D73027")

ggplot_sf_col_facet(data = map_data, 
                    col_var = trend_category, 
                    facet_var = indicator,
                    coastline = nz, 
                    size = 0.25, 
                    pal = pal,
                    title = "Monitored river nitrate-nitrogen trends, 2008\u201317")

## ---- message = FALSE, warning = FALSE, fig.height = 4, fig.width = 7---------
ggplot_stars_col(data = example_stars_nz_no3n, 
                 coastline = nz,
                 col_method = "quantile", quantile_cuts = c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1),
                 title = "River modelled median nitrate-nitrogen concentrations, 2013\u201317",
                 wrap_title = 40, 
                 legend_digits = 1)

## ---- message = FALSE, warning = FALSE, fig.height = 4, fig.width = 7---------
map_data1 <- example_stars_nz_no3n %>%
  rlang::set_names("NO3N")

map_data2 <- example_stars_nz_drp %>%
  rlang::set_names("DRP")

map_data <- c(map_data1, map_data2)

ggplot_stars_col_facet(data = map_data, 
                       coastline = nz,
                       col_method = "quantile", quantile_cuts = c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1),
                       title = "River modelled nutrient concentrations, 2013\u201317")

## ---- echo = TRUE, results = 'hide', message = FALSE, warning = FALSE, fig.height = 4, fig.width = 7----
leaflet_sf(data = example_sf_nz_livestock)

## ---- echo = TRUE, results = 'hide', message = FALSE, warning = FALSE, fig.height = 4, fig.width = 7----
map_data <- example_sf_nz_livestock %>%
  mutate(dairydens = round(dairydens, 2))

leaflet_sf_col(data = map_data, 
               col_var = dairydens, 
               col_method = "bin", 
               bin_cuts = c(0, 10, 50, 100, 150, 200, Inf), 
               legend_digits = 0,
               title = "Dairy density in count per km\u00b2, 2017")

## ---- echo = TRUE, results = 'hide', fig.height = 4, fig.width = 7------------
leaflet_stars_col(data = example_stars_nz_no3n,
  col_method = "quantile", quantile_cuts = c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1),
   title = "River modelled median nitrate-nitrogen concentrations in g/m\u00b3, 2013\u201317")

## ---- message = FALSE, warning = FALSE, fig.height = 4, fig.width = 7---------
plot_data <- ggplot2::diamonds %>%
  mutate_at(vars("cut"), ~stringr::str_to_sentence(.)) %>%
  group_by_at(vars("cut")) %>%
  summarise_at(vars("price"), ~mean(.)) %>%
  ungroup() %>% 
  mutate_at(vars("price"), ~round(. / 1000, 2)) %>%
  mutate_at(vars("cut"), ~factor(., levels = c("Fair", "Good", "Very good", "Premium", "Ideal"))) 

x_var <- "price"
y_var <- "cut"

plot <- ggplot_hbar(data = plot_data, 
                    x_var = !!sym(x_var), 
                    y_var = !!sym(y_var),
                    title = "Average diamond price by cut", 
                    x_title = "Average price ($US thousands)", 
                    y_title = "Cut")

plot 

## ---- eval=F------------------------------------------------------------------
#  run_template("template1") # a graph and table
#  run_template("template2") # a leaflet map, as well as graph and table
#  

