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
  width = 0.6,
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
  y_labels = scales::label_number(accuracy = 0.0001),
  x_breaks_n = 2,
  x_labels = function(x)
    x / 1000,
  x_title = "Body mass kg",
  title = "gg_density*()"
)

p4 <- gg_hbar_col(
  plot_data,
  x_var = body_mass_g,
  y_var = species,
  col_var = sex,
  col_na_rm = TRUE,
  width = 0.6,
  x_breaks_n = 3,
  x_labels = function(x)
    x / 1000,
  x_title = "Body mass kg",
  title = "gg_hbar*()"
)

p3 + p4

## ---- echo=FALSE, fig.width=8, fig.height=3-----------------------------------
p5 <- gg_hboxplot_col(
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

p6 <- gg_histogram_col(
  penguins,
  x_var = body_mass_g,
  col_var = species,
  x_breaks_n = 3,
  x_labels = function(x)
    x / 1000,
  y_labels = scales::label_comma(accuracy = 1),
  x_title = "Body mass kg",
  title = "gg_histogram*()"
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
  x_var = middle,
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
  summarise(body_mass_g = mean(body_mass_g, na.rm = TRUE))

p9 <- gg_line_col(
  plot_data,
  x_var = year,
  y_var = body_mass_g,
  col_var = species,
  size_point = 1,
  x_breaks_n = 2,
  x_labels = scales::label_number(accuracy = 1, big.mark = ""),
  title = "gg_line*()"
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
  x_var = species,
  y_var = middle,
  ymin_var = lower,
  ymax_var = upper,
  col_var = sex,
  col_na_rm = TRUE,
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

## ---- fig.width = 7-----------------------------------------------------------
penguins %>% 
  gg_point_col_facet(bill_length_mm, body_mass_g, sex, species)

## -----------------------------------------------------------------------------
gg_point(penguins, 
         x_var = bill_length_mm,  
         y_var = body_mass_g,  
         pal = "#da3490")

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

col_labels <- c("Hcane", "TDep", "TSt")
names(col_labels) <- sort(unique(plot_data$status))
col_labels

gg_line_col_facet(plot_data, 
                  x_var = year, 
                  y_var = wind, 
                  col_var = status, 
                  facet_var = status, 
                  title = "US storms storm wind speed, 1975-2020",
                  x_title = "Storm year",
                  y_title = "Average storm wind speed",
                  col_title = "Storm status",
                  x_labels = function(x) stringr::str_sub(x, 3, 4),
                  y_labels = scales::label_comma(accuracy = 0.1),
                  col_labels = col_labels,
                  facet_labels = ~ stringr::str_to_sentence(stringr::str_wrap(.x, 5)),
                  y_zero = TRUE,
                  y_breaks_n = 6,
                  y_expand = ggplot2::expansion(add = c(0, 10)),
                  size_point = 1)

## -----------------------------------------------------------------------------
custom_theme <- gg_theme(
  pal_body = "white",
  pal_title = "white",
  pal_subtitle = "white",
  pal_background = c("#232323", "black"),
  pal_grid = "black", 
  y_grid = TRUE,
  x_grid = TRUE)

gg_point_col(penguins,
             species,
             x_var = bill_length_mm,
             y_var = body_mass_g, 
             theme = custom_theme)

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
leaf_sf_col(example_point, 
            col_var = trend_category)

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

## ---- echo = FALSE, fig.align='left'------------------------------------------
df <- tibble::tribble(
  ~family, ~data, ~x_var, ~y_var, ~col_var, ~facet_var, 
  "bar", "dataframe", "Any*", "Numeric", "Categorical or numeric", "Categorical",
  "boxplot", "dataframe", "Categorical", "Numeric", "Categorical", "Categorical",
  "density", "dataframe", "Numeric", NA, "Categorical",  "Categorical", 
  "histogram", "dataframe", "Numeric", NA, "Categorical",  "Categorical", 
  "line", "dataframe", "Any", "Numeric", "Categorical", "Categorical",
  "point", "dataframe", "Any", "Numeric", "Categorical or numeric", "Categorical",
  "pointrange", "dataframe", "Any", "Numeric", "Categorical or numeric", "Categorical",
  "sf", "sf", NA, NA, "Categorical or numeric", "Categorical",
  "smooth", "dataframe", "Numeric", "Numeric", "Categorical", "Categorical",
  "stars", "stars", NA, NA, "Categorical or numeric", NA,
  "tile",  "dataframe", "Categorical", "Categorical", "Categorical or numeric", "Categorical",
  "violin", "dataframe", "Categorical", "Numeric", "Categorical", "Categorical", 
  "hbar", "dataframe", "Numeric", "Any*", "Categorical or numeric", "Categorical",
  "hboxplot", "dataframe", "Numeric", "Categorical", "Categorical", "Categorical",
  "hviolin", "dataframe", "Numeric", "Categorical", "Categorical", "Categorical", 
  "hpointrange", "dataframe", "Numeric", "Any", "Categorical or numeric", "Categorical"
) 

df %>% 
  gt::gt() %>% 
  gt::tab_options(table.font.size = "13px")

