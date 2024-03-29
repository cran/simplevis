# simplevis 7.1.0

* Added add_tooltip function.

# simplevis 7.0.0

* Removed all ggplot2 wrapper functions. Users should use {ggblanket} instead.

# simplevis 6.4.0

* Deprecated essentially all ggplot2 wrapper functions. Users should use {ggblanket} instead.

# simplevis 6.3.0

* Deprecated: renamed theme v_gridlines/h_gridlines arguments to x_grid/y_grid. 
* Breaking: renamed size_width argument to width.
* Breaking: renamed *_balance argument to _zero_mid.
* Breaking: Renamed default pals.
* Breaking: Changed col_intervals_right argument to col_intervals_left.
* Breaking: Changed pointrange/hpointrange ymiddle_var/x_middle_var to x_var/y_var.
* Removed shiny_demo functions.
* Titles left aligned.
* Themes and guides tweaked.
* Fixed direction of numeric legends in all gg*() legends.
* Fixed bug in gg_hpointrange y scale.
* Add numeric colouring to gg_pointrange*().
* Fixed hpointrange bug where legend was vertical. 
* Fixed bug where legend appearing in mobile, despite col_legend_none = TRUE.

# simplevis 6.2.0

* Added gg_histogram*() functions.
* In leaf_sf*(), updated the map_id default to leaf
* Updated app demos.
* Adjusted default alpha_fill values.
* Added kimisc to imports.
* Changed default bar oob back to ggplot2 default.
* Changed gg_pointrange and gg_hpointrange family y_var and x_var to ymiddle_var and xmiddle_var. 

# simplevis 6.1.0

* In leaf_*, removed side-effect of clearing the map, and added a new function to do this.
* Created leaf_clear() for use in shiny.
* In leaf_*, added group_id and legend_id arguments. 
* Made theme outputs from gg_theme a gg and theme object, rather than a list.
* Changed the gg_theme() family argument to font.
* Changed the gg_theme() gridlines arguments to gridlines_h and gridlines_v.
* Changed gg_theme() gridlines default to no gridlines.
* Moved the gg_theme gridlines arguments to the front.
* In theme, added arguments to modify background colours.
* In theme, changed face_ arguments to style_.
* Added functionality to allow different font for titles to body.
* Dropped gg_theme_void, and merged this functionality into gg_theme with void argument.
* Dropped gg_theme_mobile_extra_map and merged this functionality into gg_theme_mobile_extra with void argument.
* In density, updated default to gridlines each way.
* Updated size_point default to 1.5.
* Fixed leaf_sf*() functions to wotk with longlat CRS's that are not 4326 (e.g. 4167).

# simplevis 6.0.0

* Continuous colouring across gradients for point, bar, hbar, tile, sf, stars and tile.
* Theme flexibility for all gg functions.
* gg_theme function to build a quick clean theme.
* New Smooth functions.
* New violin & hviolin functions.
* New hboxplot functions.
* New pointrange functions.
* more flexibility and precision with adjusting alpha & size.
* Updated title and categorical label default transformations to use snakecase::to_sentence_case.
* Updated numeric label default transformations to generally use scales::label_comma().
* Redesigned col_labels argument in leaf functions to accept a function.
* Deprecated leaflet_ functions in favour of new leaf_ functions.
* Removed trans arguments from functions. 
* In density functions, renamed density_ prefixed arguments to model_.
* Updated mutate_text to provide numeric_format argument. Removed comma argument from mutate_text.
* In leaf_sf functions, supported layer id's.
* In leaf_sf, added arguments to modify popup and label numeric format.
* In leaf_sf functions, defaulted label and popup numeric vars to non-scientific numbers with labels having a comma. 
* In bar, changed position argument to stack = FALSE.
* Renamed x_pretty_n argument to x_breaks_n & likewise for equivalent y & col arguments.
* Renamed var arguments and structure for boxplot where stat = "identity".
* Renamed col_right_closed to col_intervals_right.
* Removed *_digits arguments from gg functions.
* Renamed example objects to remove unnecessary words.
* Removed *_gridlines_minor arguments.
* Removed font_family, font_size_title and font_size_body arguments.
* Removed comma argument from mutate_text.
* In mutate_text, renamed text_vars_vctr to vars_vctr. 
* Tidied code.
* Numerous bug fixes and tweaks.

# simplevis 5.0.0

* Renamed *_na arguments to _na_rm.
* Renamed leaflet_basemap top_layer argument to basemap for consistency.
* Renamed leaflet_* title arguments to col_title.
* Renamed borders_behind argument to borders_on_top.
* Renamed pal_borders to borders_pal.
* Fixed the borders sf to transform the borders to the data CRS where they are different.
* Added gg_stars and gg_stars_col functions.
* Added leaflet_stars_col function.
* Changed leaflet col_title default behaviour to convert the variable name to sentence case.
* Added col_label_digits argument to all functions with numeric colour variable options.
* Removed kimisc dependency.
* Removed leaflet.extras dependency.
* Updated app templates to make height of vis easier to adjust.
* Added reactive points into app template2.
* Added minimap into app template2.
* Fixed mobileDetect bug in app template 2.
* Updated arguments in default pals.
* Added reset button into leaflet for when shiny equals FALSE.
* Updated method to estimate numeric colour label digits, when not specified.

# simplevis 4.6.0

* Rearranged default pal_d3_reorder palette slightly.
* Updated wrapping defaults.
* Updated shiny app templates.
* Updated col_labels argument to work with functions or vectors for numeric col_var's.
* Updated and renamed interval label functions.
* Updated default alpha for polygons.
* Renamed bin_cuts_to_interval_labels to interval_labels.
* Updated bar code to make *_reorder arguments work better.
* Updated hbar code to make *_reorder arguments work better.
* Updated tile width code.
* Remove unnecesary *_na arguments for point and boxplot families.
* Adjust width so that it defaults to NULL for date or datetime vars.
* Fix leaflet popup bug. 
* Make quick style improvements to tile function.
* Adjusted the tiles colours.
* Reverse y variable in tile functions.

# simplevis 4.5.0 

* Removed col_legend_ncol and col_legend_nrow
* Updated facet_labels method for consistency with other labels arguments.
* Added tile family
* Added support for colouring numeric variables for bar and hbar.
* Added col_pretty_n argument for functions with numeric colour variables.
* Changed interval default to be right closed, but allowed this to be modified for functions with numeric colour variables. 
* Changed numeric col_method default to "bin".
* Added pal_na argument to allow the user to specify the NA colour.
* Added stringr::str_to_sentence default for all categorical variables.
* Added scales::comma default for all numeric x, y and colour variables.
* Added theme_no_gridlines.
* Updated themes to be complete.

# simplevis 4.4.0

* Renamed leaflet text_var to label_var.
* Removed col_var2 from point tooltip.

# simplevis 4.3.0

* Made legend fill background white for all functions. 
* Updated vignette and arrticles to discuss density functions, and make vars more explicit.
* Added minor gridline arguments.
* Added gg_density_col_facet() function.
* Made all gg_density* functions only support position = "identity".
* Added gg_density_facet() function.
* Added arguments to modify density stat within gg_density().
* Added gg_density_col() function.
* Created first density function.
* Updated bar help to note that only stack and dodge positions are supported.
* For point, added position and alpha arguments.
* Updated bar position code to provide more flexibility.
* Consolidated and renamed themes.

# simplevis 4.2.0

* For point*, fixed bug to support use of the same col_var as x or y.
* Changed gg_sf* family alpha default to 0.9.
* Fixed point* family bug to support log10 x axis with x_zero equals TRUE.
* Updated pal_na to add a col_n argument for consistency with other pal functions.
* Add article on titles.
* Added pal_na function.
* Updated colour article.
* Changed label default conversion to use stringr::str_to_sentence.
* Fixed gg_point_col bug to make the zero lines work.
* Updated articles and vignettes.

# simplevis 4.1.0

* Add categorical colour palette.
* Adjusted NA colour.
* Increase title and subtitle wrap defaults to 100.
* Updated hbar col reversing and legend order.

# simplevis 4.0.0

* Updated app templates.
* Updated defaults for facetted pretty breaks.
* Added code to ensure logical variables work.
* Changed vbar family to hbar.
* Removed col_rev from gg_boxplot family.
* Modified group by statement for col stack bars, so that zeros are not dropped.
* Updated bar x_var and y_var help for the specific.
* Fixed col_labels default bug.
* Updated col_labels default. 
* Added facet_labels argument. 
* Updated leaflet_sf_col col_label_dp defaults.
* Replaced internal function with rlang::set_names(~snakecase::to_sentence_case(.))
* Added new col_labels code for gg_sf family.
* Added new col_labels code for gg_point family.
* Added default sentence case x and y labels for categorical variables.
* Fixed bugs with default titles.
* Changed more Helvetica font defaults to "".
* Removed discrete horizontal scale mobile wrapping.
* Updated code to use snakecase::to_sentence_case where possible.
* Removed hbar reversing of breaks, as not required.
* Updated boxplot x_var scale.
* Removed flipping of scales for boxplot on mobile.
* Fixed bar scales.
* Reverted bar expand default to c(0, 0).
* Added datetime support to hbar.
* Added support for datetime for point, boxplot and vbar.
* Removed x_trans from line as unnecessary.
* Added support for datetime to the other gg_line functions.
* Added support for datetime to gg_line().
* Added in arguments to filter out NA values.
* For bar, removed the ability to reverse x and y scales for numeric variables. 
* Fixed bar reorder direction.
* Updated boxplot code for how it works with stat = "identity".
* Corrected the hbar and vbar x_var and y_var help.
* Changed position default to "dodge" from "stack".
* Moved palmerpenguins to suggests.
* Removed col_na argument.
* Updated all statement to remove NAs.
* Tweaked hbar title wrapping defaults.
* Renamed all ggplot_ prefixes with gg_.
* Update x_rev and col_rev to support logical variables.
* Updated titles logic.
* Fixed bug with hbar x_title and y_title around the wrong way.
* Updated support for x logical variables.
* Updated col_title wrapping for mobile.
* Updated default colours for 3 values.
* Fixed hbar mobile scales.
* Rewrote hbar_col_facet based on vbar code to expand variable types and arguments available.
* Rewrote hbar_facet based on vbar code to expand variable types and arguments available.
* Rewrote hbar_col based on vbar code to expand variable types and arguments available.
* For hbar, fixed ordering of date or numeric on y scale.
* For vbar and hbar, added x_reorder and y_reorder argument.
* Rewrote hbar based on vbar code to expand variable types and arguments available.
* Fixed vbar x_limits defaults. 
* Fixed x_rev for if character in vbar, point and line.
* Corrected vbar_col_facet labels bug.
* Fixed x_rev in point and line.
* Updated breaks functions for better speed.
* For ggplot functions, updated col_labels.
* For ggplot functions, updated help for x and y_labels arguments.
* For vbar, added support for all variables on the x scale.
* For vbar, added an x_rev argument. 
* Removed x_trans from vbar and boxplot.
* Removed col_quantile_by_facet argument from point.
* Update all boxplot, point, and line x scales to be consistent.
* Added x_rev argument to all boxplot, point, and line x scales.
* Update boxplot x scales to be more flexible.
* Update vbar x scales to be more flexible.
* Removed group_var argument, and updated grouping code.
* Added support for x categorical variables in point.
* Added support for x categorical and date variables in boxplot.
* Added support for x date variables in vbar.
* Added support for x categorical variables in line.
* Updated col_rev code to fix colouring of factors where rev = TRUE.

# simplevis 3.1.0

* Minor change: Removed rnaturalearth from suggests.
* Corrected is_null bug in leaflet_sf.
* Corrected hbar y_var reversing bug.
* Changed mutate_text vars_vctr argument to text_vars_vctr.
* Changed col_labels_nrow and col_labels_ncol to col_legend_ncol and col_legend_nrow.
* Added plotly_col_legend function, and removed plotly_legend_rev and plotly_legend_order.
* leaflet popup_var has been removed, and replaced with a popup_vars_vctr argument.

# simplevis 3.0.0

* Corrected vbar default reversing of col_var.
* Removed vbar x_rev argument, as unsure of logic rules.
* Corrected vbar default reversing of x_var.
* Update templates.
* Added ggplot_boxplot_col_facet function. 
* Make handling of all zero values pretty for all plots other than point.
* Changed the isMobile argument to mobile for snakecase consistency across the package.
* Fixed where trans was equal to log or log10, and zero was selected
* Added ggplot_boxplot_col
* Updated scale_x_date to remove oob.
* Updated website vignette and articles.
* Removed x_na_inf and y_na_inf arguments.
* Export breaks functions.
* Corrected bug fix with x_na_inf and y_na_inf.
* Added col_na argument to all functions lacking it.
* Added x_rev and col_rev to vbar functions.
* Modularised all x_zero adjustments and automatic x_zero line components, and likewise the y_ ones.
* Modularised all x and y numeric breaks.
* Added balance, trans and zero arguments to all numeric arguments.
* Changed x_zero and y_zero defaults for non-bar graph numeric scales to FALSE.
* Underlying code change of hbar to not use `coord_flip`.
* Rebuilt y numeric breaks.
* Rebuilt default font size code.
* Added to all themes legend.direction = "vertical" to make titles always be above legends.
* Added shiny for mobile article.
* Changed leaflet default alpha to 0.9.
* Added scales article.
* Changed boxplot for outlines to be always coloured black, and alpha defaulted to 1. 
* Changed point_size and line_size to size_point and size_line.
* Changed leaflet_sf default point_size to 2.
* Added text_var arguments to ggplot sf functions to fully support plotly interactive maps. 
* Fix mutate_text, so that it does not add Not available to non-NA charcter values with NA in them. 
* Make ggplot sf functions not adjust alpha of outlines.
* Remove key_glyph, as it is not implemented in ggplotly.  
* Updated vignette and articles.

# simplevis 2.6.0

* Added template zip files.
* Changed point_size to default to 1 for all functions.
* Removed line_alpha from leaflet functions, as not required.
* Removed col_drop from leaflet function.
* Removed col_quantile_by_facet function from ggplot_sf_col_facet.
* In leaflet_sf functions, changed opacity to fill_alpha.
* In leaflet_sf functions, changed weight to line_size.
* In leaflet_sf functions, changed radius to point_size.
* In ggplot_line functions, removed points and lines argument. 
* In ggplot_sf functions, changed size argument to point_size and line_size for consistency.
* For boxplot, added line_size argument and alpha. 
* Default colour changed for where 2 values. 
* Changed x_na_bar/y_na_bar to x_na_inf/y_na_inf. 
* Changed output of mutate_text to name the new column text.
* Removed all stars functions, as these need more development work.
* Rename tip_var to text_var to align with ggplot2.
* Rename add_tip to mutate_text to align with ggplot2 and dplyr.
* Rename boundary with borders to align with ggplot2.
* Rename ggplot_box with ggplot_boxplot to align with ggplot2.
* Bug fix: Make ggplot_vbar and vbar_facet scales pretty.
* Change default point_size to 1.5.
* Add line_size argument to hbar and vbar functions.
* Make ggplot_vbar function x scales pretty, where max or min equals the x limit.
* Change default of ggplots to legend on right with 1 column.
* Add facet_ncol argument.
* Add col_labels_ncol argument.
* Rename col_label_digits argument to col_labels_dp.
* Rename leaflet_sf label_var to tip_var to avoid confusion.
* Rename size arguments in ggplot_point and ggplot_line functions.
* Fixed vbar legend elements being reversed. 
* In leaflet_sf, rename col_na_remove for consistency.
* In leaflet_sf functions, rename popup as popup_var and improve help.
* Rename col_digits argument to col_label_digits.
* In line functions, rename size argument to size_point for clarity.
* In line functions, add size_line argument.
* Rename all legend_ arguments to col_ arguments for internal consistency.
* Rename wrap_col_title to col_title_wrap for internal consistency.
* Defaulted colours to viridis for all functions.
* Improved ggplot_point and ggplot_sf colouring code.
* Added col_na arguments to ggplot_point and ggplot_sf functions to show na col_var values or not.  
* Added alpha argument for the fill of hbar and vbar.
* Removed size argument in line functions.
* Renamed point_size as size in line functions.
* Add alpha argument into hbar and vbar.
* Changed quantile_by_facet argument to col_quantile_by_facet for internal consistency.
* Remove leaflet_basemap_nz.
* Updated leaflet_basemap to include an argument for bounds.
* Removed rnaturalearth wrapper functions, but referenced the package in help, examples and articles instead.
* Add the ability to set bounds in leaflet basemap for country boundaries from the rnaturalearth package.
* Add wrapper functions to easily extract sf boundaries and bounds from the rnaturalearth package.

# simplevis 2.4.0

* Reduce the size of sf example objects.
* Add vignette for making maps of sf objects.
* Removed support for boundary arguments in ggplot_stars, as it was not working.
* Updated help for boundary_behind arguments to specify correct default.
* For ggplot_sf and stars functions, added boundary_size argument in.
* Added nz_region as a helpful example boundary.
* Renamed example objects to focus on their object class.
* For line, renamed rev_pal to pal_rev for consistency. 
* For vbar, added in the pal_rev argument for consistency.
* For hbar & vbar, renamed arguments to x_na_bar and y_na_bar.
* Add na_bar argument to vbar_col.
* For hbar_col, fix na_bar so that is works on negative data.
* For hbar and vbar, fix na_bar so that it works for negative and positive data with ggplotly(plot, tooltip = "text").
* For line graphs on mobile, make x axis labels just the minimum and maximum (and zero if applicable). 
* Added y_balance arguments to vbar, line and point functions.
* Defaulted zero reference lines to be on by default if there are positive and negative values in the data. 
* Peplace all superceded scoped functions with across.
* Renamed ggplot_scatter to ggplot_point, and likewise for theme_scatter.  
* Renamed coastline argument to boundary.
* Moved pals to er.helpers package.
* Changed wrap text arguments to prefix with what is being wrapped.
* Renamed x_na_bar and y_na_bar arguments to na_bar.
* Corrected hbar error messages for faceted plots.
* Adjusted mobile hbar x label justification.
* Ungrouped data in sf functions.
* Removed support for mobile in `_facet` and `_col_facet` functions.
* Updated functions for a more reliable mobile experience.
* Updated app templates css and default table rows.
* For hbar, add mobile wrapping for x labels.
* For hbar, update the mobile x breaks, so always only min and max, and zero if relevant.
* For hbar, made plots x aim for 1 interval of breaks for mobile, and left-align labels.
* For hbar, added x_balance argument to hbar functions.
* Changed name of compare2 pals to alpha2 pals.
* Made ggplot line functions default to expanding by zero on x.
* Changed hbar and vbar na_grey arguments to be x_na_bar or y__na_bar.
* Added y_na_na_bar argument to ggplot_hbar_col.
* Removed col_drop and col_remove_na arguments.
* Added two new palettes for graphs that compare a current year against a previous year.
* Added left-align to hover values with the plotly_camera function.
* Added expand arguments to all other plots.
* Changed zero_lines to default off.
* Removed automatic zero lines, so now they must be manually turned off and on.
* Added expand arguments to scatterplots.
* Corrected the direction of bar legend labels if someone manually adds them in.
* Update the leaflet sf popup so that it does not load if there is only geometry.
* Reverted default col_title = "" rather than NULL, as this works better with ggplotly.
* Adjusted add_tip to default to putting in all variables, and work with sf objects.
* Added support for using logical variables to colour in hbar.
* Added pal_rev argument in hbar.
* Removed feature id and row number from popup.
* Updated add_tip so that it can also work with sf objects.
* Removed geometry from defaulting into the leaflet popup.
* Fixed bug with scatter not working with NAs appropriately.

# simplevis 2.0.0

* Update shiny templates.
* Drop region, TA and sea-draining catchments from nz basemap stack.
* Removed code to automate isMobile, as it was not working.
* Removed na_tip as was not working.
* Renamed leaflet_basemap_stack functions to remove the word stack.
* Removed automatic addition of tooltip text into functions.
* Added support for the user to add a tooltip variable into plot functions.
* Created add_tip function to easily create a tooltip text column within a dataset.
* In scatter plots, changed quantile_cuts and bin_cuts arguments to col_cuts.
* Renamed plotly functions.
* Added a size argument to lines in line plots.
* Added a group var to boxplot functions.
* Removed requirement for categorical x var for boxplot functions.
* Added a width argument to boxplot functions.
* Renamed na_grey_hover_value. 
* Fixed bug in leaflet functions with colouring by bin col_method.
* Fixed bug in ggplot_vbar to support making a plot when all values are zero.
* Renamed all plot x_scale_ prefixed arguments with x_ and likewise for y_scale_ and col_scale_.
* Renamed remove_na argument in scatter and sf plots as col_na_remove.
* Renamed rev_pal as pal_rev in scatter, sf and stars functions.
* Renamed bin_cuts and quantile_cuts as col_cuts.
* Renamed quantile_by_facet as col_quantile_by_facet.
* Added col_na_remove argument to leaflet_sf_col.
* Fixed bug with legend key in line plots not displaying the line and point in the key.
* Removed dplyr 1.0.0 dependency by replacing across function in stars plots with _at functions.
* Moved all aesthetics from ggplot() function to geoms.
* Fixed bug with line hover_var.
* Update template apps, and vignette.
* Made isMobile default to NULL, which selects input$isMobile in apps and FALSE otherwise.
* Removed shiny logical argument from leaflet functions.
* Added argument to modify pretty n algorathim for numeric breaks.
* Fixed bug where hbar and hbar_col were not handling all zero values appropriately.
* Fixed bug where line and line_col were not handling all zero values appropriately.
* Fixed bug where plot functions ability were not able to deal with NA values.
* Fixed bug where vbar and vbar_col were not handling all zero values appropriately.

# simplevis 1.5.0

* Added default zero lines to plots for where there are negative values.
* Updated plot scales to deal with negative values better.
* Added x_scale_labels and y_scale_labels for arguments to all plots.
* Removed x_scale_date_format from plots.
* Removed wrap_y_labels argument from plots.
* Update line functions with lines = FALSE argument.
* Default the na_grey argument to FALSE.
* Improved the tooltip for NA values in vbar and hbar plots.
* Deleted remove_na argument from leaflet_sf.
* Updated vbar x_scale code to start and stop at the minimum and maximum x bar.
* Added na_grey argument to vbar and hbar non-coloured functions.
* Updated y scale in bar and line non-facetted plots for improved handling of all zero values.
* Updated templates to help users learn an easy workflow method.

# simplevis 1.4.0

* Updated vbar x scale to accomodate edge bars.
* Added argument to select a label variable in leaflet sf.

# simplevis 1.3.0

* Updated colour in pal_point_trend3 and pal_point_trend5.
* Added x_scale_labels and y_scale_labels argument to all numeric x and y scales in ggplot functions.
* Updated app templates.

# simplevis 1.2.0

* Replaced dplyr and tidyr superceded and retired functions with maturing functions.
* Added support for hover_var's being adding manually as ggplotly tooltips.
* Added support for categoical x variables on vbar ggplots.
* Dropped nz_region shape.
* Added plotly_reverse_legend and plotly_order_legend functions.
* Added width argument to hbar and vbar functions.
* Updated css plot minimum height.
* Added a req statement in the observe function of template2 to ensure the basemap is output before points are tried to be plotted.
* Adjusted ggplot wrapper functions to default to nice sizes for mobile or desktop.
* Removed run_example functions.
* Removed template3 from run_template functions.
* Adjusted vbar expand on the x scale to be zero.
* Updated all maps to default to quantile col_method with quartiles.

# simplevis 1.1.0 

* Renamed pal_trend3, pal_trend5, and pal_set1 to have pal_point prefixes.
* Updated scales for all ggplot wrapper functions to have better limits and breaks.
* Added plotly_remove_buttons function to remove buttons other than camera.
* Updated app templates to add theme(plot.title.position = "plot") to mobile plots.
* Updated css to app templates.
* Updated the default wrapping for titles.
* Updated vertical bar functions to support numeric or date variables on the x axis.
* Updated line graphs to move text to geom_point so that hover is on points.
* Updated bar code to fix free_y and free_x facet_scales, which were around the wrong way.
* Updated bar code to allow for graphs with scale_zero = FALSE & position equals "stack".
* Updated plot margins in graphs.
* Updated plot label wrapping in graphs.
* Updated caption position and wrapping length.

# simplevis 1.0.0

Initial release to CRAN.