## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- message = FALSE, warning = FALSE----------------------------------------
library(simplevis)
library(dplyr)
library(ggplot2)

data <- tibble::tribble(
  ~pet, ~count,
  "Cat", 567,
  "Dog", 443,
  "Horse", 11)  

data

## ---- message = FALSE, warning = FALSE, fig.height = 4, fig.width = 6---------
ggplot(data) +
  geom_col(aes(pet, count)) +
  labs(title = "Wellington pets, 2020", x = "Pet", y = "Count")

## ---- message = FALSE, warning = FALSE, fig.height = 3, fig.width = 6---------
ggplot(data) +
  geom_col(aes(pet, count), fill = "#085C75", width = 0.75) +
  labs(title = "Wellington pets, 2020", x = "Pet", y = "Count") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 600)) + #hardcoded = bad!
  theme_minimal() +
  theme(plot.title = element_text(family = "Helvetica", hjust = 0.5, size = 11, face = "bold", colour = "#000000")) +
  theme(axis.title = element_text(family = "Helvetica", hjust = 0.5, size = 10, face = "plain", colour = "#323232")) +
  theme(axis.text = element_text(family = "Helvetica", hjust = 0.5, size = 10, face = "plain", colour = "#323232")) +
  theme(panel.grid.major.x = element_blank()) +
  theme(panel.grid.minor.y = element_blank()) +
  theme(axis.line = element_line(colour = "#323232", size = 0.3)) +
  theme(axis.ticks = element_line(colour = "#323232", size = 0.3)) +
  theme(axis.title.x = element_text(margin = margin(t = 10))) +
  theme(axis.title.y = element_text(margin = margin(r = 10)))

## ---- message = FALSE, warning = FALSE, fig.height = 3, fig.width = 6---------
ggplot_vbar(data, pet, count, 
            title = "Wellington pets, 2020", 
            x_title = "Pet", 
            y_title = "Count")

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
                legend_ncol = 4,
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
                      legend_ncol = 4, 
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
    "box", "categorical", "numeric", "categorical or numeric", "categorical", "boxplot or identity"
  ) %>% 
  DT::datatable()

## ---- message = FALSE, warning = FALSE, fig.height = 4, fig.width = 7---------
ggplot_sf(example_sf_point, 
          boundary = nz, 
          size = 0.25,
          title = "Site trends, 2008\u201317",
          title_wrap = 40)

## ---- message = FALSE, warning = FALSE, fig.height = 4, fig.width = 7---------
pal <- c("#4575B4", "#D3D3D3", "#D73027")

ggplot_sf_col(example_sf_point, trend_category, 
              boundary = nz, 
              size = 0.25, 
              pal = pal, 
              title = "Site trends, 2008\u201317",
              title_wrap = 40)

## ---- message = FALSE, warning = FALSE, fig.height = 4, fig.width = 7---------
ggplot_sf_facet(example_sf_point, trend_category, 
                boundary = nz, 
                size = 0.25,
                title = "Site trends, 2008\u201317")

## ---- message = FALSE, warning = FALSE, fig.height = 5, fig.width = 7---------
pal <- c("#4575B4", "#D3D3D3", "#D73027")

ggplot_sf_col_facet(example_sf_point, trend_category, trend_category,
                    boundary = nz, 
                    size = 0.25, 
                    pal = pal,
                    title = "Site trends, 1990\u201317")

## ---- message = FALSE, warning = FALSE, fig.height = 4, fig.width = 7---------
ggplot_stars_col(example_stars, 
                 col_method = "quantile", col_cuts = c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1),
                 title = "Site medians, 2013-2017",
                 title_wrap = 40, 
                 legend_digits = 1)

## ---- message = FALSE, warning = FALSE, fig.height = 4, fig.width = 7---------
map_data1 <- example_stars %>% rlang::set_names("A")

map_data2 <- example_stars_2 %>% rlang::set_names("B")

map_data <- c(map_data1, map_data2)

ggplot_stars_col_facet(map_data, 
                       col_method = "quantile", col_cuts = c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1),
                       title = "Site medians, 2013-2017")

## ---- echo = TRUE, results = 'hide', message = FALSE, warning = FALSE, fig.height = 4, fig.width = 7----
leaflet_sf(data = example_sf_polygon)

## ---- echo = TRUE, results = 'hide', message = FALSE, warning = FALSE, fig.height = 4, fig.width = 7----
leaflet_sf_col(example_sf_polygon, density, 
               col_method = "bin", 
               col_cuts = c(0, 10, 50, 100, 150, 200, Inf), 
               legend_digits = 0,
               title = "Modelled density, 2013-2017")

## ---- echo = TRUE, results = 'hide', fig.height = 4, fig.width = 7------------
leaflet_stars_col(example_stars,
  col_method = "quantile", col_cuts = c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1),
   title = "Modelled density, 2013-2017")

## ---- eval=F------------------------------------------------------------------
#  run_template("template1") # a graph and table
#  run_template("template2") # a leaflet map, as well as graph and table
#  

