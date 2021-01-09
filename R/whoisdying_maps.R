## map of incidence vs CFR

## load packages
library("rio")
library("epichecks") # github package from https://github.com/R4IDSR/epichecks
library("sf")
library("ggplot2")
library("dplyr")
library("janitor")
library("stringr")
library("forcats")
library("ggspatial")
library("patchwork")


## import alice's count dataset
counts <- rio::import("https://docs.google.com/spreadsheets/d/1P8Y18wxhntPVjt8bYstBcXfZIAgHfLei/edit#gid=1946857273") %>%
  filter(country_iso != "sum")

## read in UN population data
population <- rio::import("https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/EXCEL_FILES/1_Population/WPP2019_POP_F01_1_TOTAL_POPULATION_BOTH_SEXES.xlsx")



## clean up the population data

## fix names
colnames(population) <- population[12, ]
population <- clean_names(population)

## drop extras at top
population <- population[-c(1:12), ]

## back up original
og_population <- population

## create iso code variable
population$country_iso <- countrycode::countrycode(population$country_code,
                                                   origin = "un",
                                                   destination = "iso3c")



## only keep countries listed in counts
population <- population %>%
  filter(country_iso %in% counts$country_iso) %>%
  select(region_subregion_country_or_area, country_iso, x2020)



## merge counts with population
counts <- left_join(counts, population, by = "country_iso")


## correct population as is in thousands
counts <- mutate(counts,
                 x2020 = as.numeric(x2020) * 1000)


## add in important vars
counts <- mutate(counts,
                 ## calculate incidence per 10k
                 incidence = confirmed / x2020 * 10000,
                 ## create groups of incidence
                 incidence_grp =  cut(incidence,
                                      breaks = c(0, 0.01, 0.5, 1.5, 2.5, 3.5, Inf),
                                      include.lowest = TRUE,
                                      right = TRUE,
                                      labels = c("0", "<0.5", "0.5-1.4", "1.5-2.4", "2.5-3.4", "3.5+")),
                 ## create CFR groups
                 cfr_grp = cut(CFR,
                               breaks = c(0, 0.01, 0.9, 3.5, 5.5, 7.5, Inf),
                               include.lowest = TRUE,
                               labels = c("0","<1","1-3.4", "3.5-5.4", "5.5-7.5", "7.5+")),
                 ## create an id for joining colour defs
                 merger = str_glue("{as.numeric(incidence_grp)-1}-{as.numeric(cfr_grp)-1}")
                 )






## get file path
shp_path <- system.file("extdata", "AFRO.shp", package = "epichecks")

## read in shapefile
afro_shp <- read_sf(shp_path)

## get list of islands to subset
islands <- c("Cape Verde",
             "Comoros",
             "Mauritius",
             "Seychelles",
             "Sao Tome and Principe")




## Prepare data for plotting ---------------------------------------------------
## define your colour palette (need to have one more to include 0?)
cols2 <- make_colours(num1 = 6,
                      num2 = 6)

## minus 1 from the colour numbering to include 0 counts
cols2 <- cols2 %>%
  mutate_at(.vars = c("rws", "cls"),
            .funs = funs(. - 1))

## create a unique identifier by combining the row and columnn counts
## this will match the counts of reports after
cols2 <- cols2 %>%
  mutate(merger = str_glue("{rws}-{cls}"))

## join the colours to the counts
map_counts <- left_join(
  ## only keep certain columns from the counts dataset
  select(counts,
         country_iso,
         incidence_grp,
         cfr_grp,
         merger
         ),
  cols2, by = "merger")

## join counts with colours to the shapefile
afro_shp <- left_join(afro_shp, map_counts, by = c("ISO_3_CODE" = "country_iso"))

## set missings (i.e. non-afro) to grey
afro_shp <- afro_shp %>%
  mutate(clrs = fct_explicit_na(clrs, na_level = "Grey90"))

## Plot map --------------------------------------------------------------------
## first plot the map as a choropleth
base_map <- ggplot(data = afro_shp) +
  ## fill shape with the colours column
  geom_sf(aes(fill = clrs),
          color = "black",
          size = 0.1) +
  ## fill countries by the colours as they are named clrs column
  scale_fill_identity(drop = FALSE,
                      na.value = "grey90") +
  ## add a scale bar
  annotation_scale(location = "br") +
  ## get rid of axes and extras
  theme_void(base_size = 18)

## plot each of the islands seperately
island_plot <- purrr::map(islands,
                          function(x) {
                            ggplot(data = filter(afro_shp, country == x)) +
                              ## fill shape with the colours column
                              geom_sf(aes(fill = clrs),
                                      color = "black",
                                      size = 0.1) +
                              ## fill countries by the colours as
                              ## they are named clrs column
                              scale_fill_identity(drop = FALSE,
                                                  na.value = "grey90") +
                              ## add a scale bar
                              # annotation_scale(location = "br") +
                              ## get rid of axes and extras
                              theme_void(base_size = 12) +
                              theme(legend.position = "none",
                                    panel.border = element_rect(colour = "black", fill = NA),
                                    plot.margin = margin(10,
                                                         10,
                                                         10,
                                                         10)) +
                              ggtitle(x)
                          }
)

## plot the legend seperately
## use the cols2 dataset (colour scheme generated by make_colours function)
## use the counts for rows and columns and fill by the clrs columns
legend <- ggplot(
  data = cols2,
  mapping = aes(
    x = cls,
    y = rws,
    fill = clrs)) +
  ## create a tile plot (boxes)
  geom_tile() +
  ## fill tiles by the colour names in clr
  scale_fill_identity() +
  ## label the axis ticks from 0 to the number of groups
  scale_x_continuous(breaks = c(0:5),
                     labels = levels(counts$cfr_grp)) +
  scale_y_continuous(breaks = c(0:5),
                     labels = levels(counts$incidence_grp)) +
  ## make tiles boxes
  coord_fixed() +
  ## label axes
  labs(x = "CFR (%)", y = "Incidence (per 10k)") +
  theme_minimal(base_size = 18) +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.75, vjust = 1))


## using {patchwork} - set the plot layout area
## The map goes from top left at 1,1 to bottom right at 10,10
## the legend sits within that as a smaller box
## (think of this as upside-down cartesian coordinates)
layout <- c(
  area(t = 1, l = 1, b = 10, r = 10),
  area(t = 7, l = 2, b = 9, r = 4),
  area(t = 11, l = 1, b = 12, r = 10)
)

## combine the map and the legend with the above layout
full_map <- base_map +
  legend +
  wrap_plots(island_plot, nrow = 1) +
  plot_layout(design = layout)


ggsave(plot = full_map,
       filename = here::here("included_map.jpg"),
       height = 7,
       width = 7*1.25,
       units = "in")




