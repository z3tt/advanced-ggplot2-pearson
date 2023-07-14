################################################################################
#                                                                              #
# "Hands-On Guide to Advanced Data Visualization with ggplot2: Custom Design"  #
#          Pearson Live Training Session for O'Reilly, July 14 2023            #
#                                                                              #
#                             Dr Cédric Scherer                                #
#                             cedricscherer.com                                #
#                                                                              #
################################################################################


##------------------------------------------------------------------------------
## PREPARATION
##------------------------------------------------------------------------------

## This session makes use of the following packages:

##  - ggplot2
##  - dplyr
##  - forcats
##  - stringr
##  - scales
##  - systemfonts
##  - gapminder
##  - rcartocolor
##  - prismatic
##  - ggrepel
##  - ggforce
##  - ggtext
##  - patchwork
##  - magick
##  - ggdist
##  - ggridges
##  - plotly
##  - ggiraph
##  - echarts4r
##  - gganimate

## Please install the missing packages by running the following:
pkgs <- c("ggplot2", "dplyr", "forcats", "stringr", "scales", "systemfonts", "gapminder", 
          "rcartocolor", "prismatic", "ggrepel", "ggforce", "ggtext",  "patchwork", 
          "magick", "ggdist", "ggridges", "plotly", "ggiraph", "echarts4r", "gganimate")
unavailable <- setdiff(pkgs, rownames(installed.packages()))
install.packages(unavailable)

## We are using a non-default fonts in this session. If you want to run all code
## "as it is", please install the following typefaces which are available 
## for free via Google Fonts: 
##   - Roboto Condensed
##   - Open Sans
##   - Hepta Slab
##
## You find the files in the ./fonts folder. Install them by double-clicking.
##
## If you don't want to or can't install the fonts, it is still possible to run
## the code. Just make sure to replace the font families in the code with one 
## that is installed on your system--or remove the respective rows.

## For Mac Users: If you want to save your visualization to PDF, Please make 
## sure that XQuartz is installed which is needed to use the cairo pdf device:
## https://www.xquartz.org/


## That's it---let's start!


##------------------------------------------------------------------------------
## LOAD PACKAGES
##------------------------------------------------------------------------------

library(ggplot2)
library(dplyr)
library(forcats)
library(stringr)
library(scales)
library(systemfonts)
library(gapminder)
library(rcartocolor)
library(prismatic)
library(ggrepel)
library(ggforce)
library(ggtext)
library(patchwork)
library(magick)
library(ggdist)
library(ggridges)
library(plotly)
library(ggiraph)
library(echarts4r)
library(gganimate)


##------------------------------------------------------------------------------
## DATA + BASICS
##------------------------------------------------------------------------------

gapminder

(gm2007 <- filter(gapminder, year == 2007))


## A BASIC GGPLOT

ggplot(data = gm2007) +                              ## data
  aes(x = log10(gdpPercap), y = lifeExp,             ## mapping
      color = continent, size = pop) +
  geom_point(alpha = .5)                             ## geometry

ggplot(data = gm2007) +                              ## data
  aes(x = log10(gdpPercap), y = lifeExp,             ## mapping
      color = continent, size = pop) +
  geom_point(alpha = .5)                             ## geometry

ggplot(data = gm2007,                                ## data + mapping
       aes(x = log10(gdpPercap), y = lifeExp,
           color = continent, size = pop)) +
 geom_point(alpha = .5)                             ## geometry

ggplot(data = gm2007) +                              ## data
  geom_point(aes(x = log10(gdpPercap), y = lifeExp,  ## geometry + mapping
                 color = continent, size = pop),
             alpha = .5)


## A POLISHED GGPLOT

ggplot(gm2007, aes(x = gdpPercap, y = lifeExp,
                   color = continent, size = pop)) +
  geom_point(alpha = .5, stroke = .5) +
  coord_cartesian(expand = FALSE, clip = "off") +
  scale_x_log10(breaks = c(500, 1000, 2000, 4000, 8000, 16000, 32000),
                labels = scales::label_dollar(accuracy = 1), name = "GDP per capita") +
  scale_color_manual(values = continent_colors, name = "Region:",
                     guide = guide_legend(override.aes = list(size = 5))) +
  scale_size(range = c(3, 30), name = "Population:", breaks = c(10, 100, 1000)*1000000, 
             labels = scales::label_comma(scale = 1 / 10^6, suffix = "M")) +
  labs(y = "Life expectancy", title = "Health & Income of Nations in 2007", caption = "Source: Gapminder project") +
  theme_minimal(base_family = "Roboto Condensed", base_size = 20) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", hjust = .5, size = rel(1.7), family = "Hepta Slab"),
        plot.title.position = "plot",
        legend.position = c(.9, .35),
        legend.background = element_rect(color = "grey87"),
        legend.box.just = "right",
        plot.margin = margin(25, 40, 25, 25))


##------------------------------------------------------------------------------
## SETUP
##------------------------------------------------------------------------------

g <-
  ggplot(gm2007, aes(x = gdpPercap, y = lifeExp, size = pop)) +
  scale_x_log10(breaks = c(500, 2000, 8000, 32000),
                labels = scales::label_dollar(accuracy = 1)) +
  scale_size(range = c(1, 12), name = "Population:", breaks = c(10, 100, 1000)*1000000, 
             labels = scales::label_comma(scale = 1 / 10^6, suffix = "M")) +
  labs(x = "GDP per capita", y = "Life expectancy") 

g

gp <- g + 
  geom_point( 
    aes(color = continent), alpha = .5
  ) +
  ggtitle("gapminder 2007") +
  scale_color_manual(
    values = continent_colors, 
    name = "Region:"
  )

gp


##------------------------------------------------------------------------------
## WORKING WITH THEMES
##------------------------------------------------------------------------------

gp + theme_minimal()

gp + theme_classic()

system_fonts() %>%
  filter(stringr::str_detect(family, "Open")) %>%
  pull(family) %>%
  unique()

gp +
  theme_bw(
    base_family = "Open Sans",
    base_size = 15
  )

system_fonts() %>%
  filter(family == "Open Sans") %>%
  pull(name) %>%
  sort()

system_fonts() %>%
  filter(family == "Open Sans") %>%
  pull(name) %>%
  sort()

register_variant(
  name = "Open Sans Semibold S1",
  family = "Open Sans",
  weight = "semibold",
  features = font_feature(letters = "stylistic")
)

gp + 
  theme_bw(
    base_family = "Open Sans Semibold S1",
    base_size = 14
  )

g1 <- gp + theme_bw(base_family = "Open Sans", base_size = 20)
g2 <- gp + theme_bw(base_family = "Open Sans Semibold S1", base_size = 20)

g1 + plot_spacer() + g2 + plot_layout(widths = c(1, .05, 1))

theme_set(theme_light(base_family = "Roboto Condensed", base_size = 14))

theme_update(panel.grid.minor = element_blank(),
             plot.title = element_text(face = "bold", size = rel(1.5)),
             plot.title.position = "plot")

gp

theme_grey

theme_bw

theme_open <- function(base_size = 14, base_family = "Open Sans", 
                       base_line_size = base_size/22, base_rect_size = base_size/22) {
  
  theme_bw(base_size = base_size, base_family = base_family, 
           base_line_size = base_line_size, base_rect_size = base_rect_size) 
}

theme_open <- function(base_size = 14, base_family = "Open Sans", 
                       base_line_size = base_size/22, base_rect_size = base_size/22) {
  
  theme_bw(base_size = base_size, base_family = base_family, 
           base_line_size = base_line_size, base_rect_size = base_rect_size) %+replace% 
    theme(
      plot.title = element_text(size = base_size * 1.5, margin = margin(b = base_size / 2),
                                family = "Open Sans Extrabold", hjust = .5),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      panel.background = element_rect(fill = "white", colour = "grey20"), 
      plot.background = element_rect(fill = "grey85", color = NA), 
      legend.background = element_rect(fill = "grey85", color = NA), 
      legend.key = element_rect(fill = "grey85", color = NA), 
      panel.grid.minor = element_blank(), 
      complete = TRUE
    )
}

gp + 
  theme_open()

gp + 
  theme_open(
    base_size = 9,
    base_family = "Hepta Slab"
  )

theme_open <- function(base_size = 14, base_family = "Open Sans", 
                       title_family = "Open Sans Extrabold",
                       base_line_size = base_size/22, base_rect_size = base_size/22) {
  
  if (title_family == "Open Sans Extrabold") {
    register_variant(name = "Open Sans Extrabold",
                     family = "Open Sans",
                     weight = "ultrabold")
  }
  
  theme_bw(base_size = base_size, base_family = base_family, 
           base_line_size = base_line_size, base_rect_size = base_rect_size) %+replace% 
    theme(
      plot.title = element_text(size = base_size * 1.5, margin = margin(b = base_size / 2),
                                family = title_family, hjust = .5),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      panel.background = element_rect(fill = "white", colour = "grey20"), 
      plot.background = element_rect(fill = "grey85", color = NA), 
      legend.background = element_rect(fill = "grey85", color = NA), 
      legend.key = element_rect(fill = "grey85", color = NA), 
      panel.grid.minor = element_blank(), 
      complete = TRUE
    )
}

gp +
  theme_open(
    base_size = 10,
    base_family = "Hepta Slab",
    title_family = "Hepta Slab"
  )

theme_open <- function(base_size = 14, base_family = "Open Sans", 
                       base_line_size = base_size/22, base_rect_size = base_size/22) {
  out <- 
    theme_bw(base_size = base_size, base_family = base_family, 
             base_line_size = base_line_size, base_rect_size = base_rect_size) + 
    theme( 
      plot.title = element_text(size = base_size * 1.5, margin = margin(b = base_size / 2),
                                family = "Open Sans Extrabold", hjust = .5),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      panel.background = element_rect(fill = "white", colour = "grey20"), 
      plot.background = element_rect(fill = "grey85", color = NA), 
      legend.background = element_rect(fill = "grey85", color = NA), 
      legend.key = element_rect(fill = "grey85", color = NA), 
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
  
  return(out)
}

theme_open_grid <- function(base_size = 14, base_family = "Open Sans", grid = "xy", 
                            base_line_size = base_size/22, base_rect_size = base_size/22) {
  out <- 
    theme_bw(base_size = base_size, base_family = base_family, 
             base_line_size = base_line_size, base_rect_size = base_rect_size) + 
    theme(
      plot.title = element_text(size = base_size * 1.5, margin = margin(b = base_size / 2),
                                family = "Open Sans Extrabold", hjust = .5),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      panel.background = element_rect(fill = "white", colour = "grey20"), 
      plot.background = element_rect(fill = "grey85", color = NA), 
      legend.background = element_rect(fill = "grey85", color = NA), 
      legend.key = element_rect(fill = "grey85", color = NA), 
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
  
  if (stringr::str_detect(grid, "x|X")) {
    out <- out + theme(panel.grid.major.x = element_line(color = "grey87"),
                       axis.ticks.x = element_blank())
  }
  if (stringr::str_detect(grid, "y|Y")) {
    out <- out + theme(panel.grid.major.y = element_line(color = "grey87"),
                       axis.ticks.y = element_blank())
  }
  
  return(out)
}

gp + 
  theme_open_grid(
    grid = "none"
  )

gp + 
  theme_open_grid(
    grid = "y"
  )


##------------------------------------------------------------------------------
## WORKING WITH COLORS
##------------------------------------------------------------------------------

g + 
  geom_point( 
    aes(color = continent), alpha = .5
  ) +
  scale_color_viridis_d()

g + 
  geom_point( 
    aes(color = continent), alpha = .5
  ) +
  scale_color_viridis_d(
    direction = -1
  )

g + 
  geom_point( 
    aes(color = gdpPercap), alpha = .5
  ) +
  scale_color_viridis_c(
    direction = -1,
    end = .9
  )

g + 
  geom_point( 
    aes(color = gdpPercap), alpha = .5
  ) +
  scale_color_viridis_c(
    option = "turbo"
  )

g + 
  geom_point( 
    aes(color = continent), alpha = .5
  ) +
  rcartocolor::scale_color_carto_d()

g + 
  geom_point( 
    aes(color = continent), alpha = .5
  ) +
  rcartocolor::scale_color_carto_d(
    palette = "Bold"
  )

pal <- rcartocolor::carto_pal(
  name = "Bold", n = 5
)

names(pal) <- unique(gm2007$continent)

g + 
  geom_point( 
    aes(color = continent), alpha = .5
  ) +
  scale_color_manual(
    values = pal
  )

g + 
  geom_point( 
    data = filter(
      gm2007, 
      continent %in% c("Asia", "Europe")
    ),
    aes(color = continent), alpha = .5
  ) +
  scale_color_manual(
    values = pal
  )

pal <- rcartocolor::carto_pal(
  name = "Bold", n = 6
)

g + 
  geom_point( 
    aes(color = continent), alpha = .5
  ) +
  scale_color_manual(
    values = pal
  )

pal <- rcartocolor::carto_pal(
  name = "Bold", n = 8
)[c(1:3,5,7)]

g + 
  geom_point( 
    aes(color = continent), alpha = .5
  ) +
  scale_color_manual(
    values = pal
  )

pal_dark <- clr_darken(pal, .4)

g + 
  geom_point( 
    aes(color = continent), alpha = .5
  ) +
  scale_color_manual(
    values = pal_dark
  )

g +
  geom_point(
    aes(color = continent,
        fill = after_scale(color)), 
    shape = 21, alpha = .5, stroke = .7
  ) +
  scale_color_manual(
    values = pal_dark
  )

g +
  geom_point(
    aes(color = continent,
        fill = after_scale(
          clr_lighten(color, .6)
        )), 
    shape = 21, alpha = .5, stroke = .7
  ) +
  scale_color_manual(
    values = pal_dark
  )

grpd <- 
  ggplot(data = gm2007, 
         mapping = aes(x = gdpPercap, 
                       y = fct_reorder(continent, -gdpPercap))) +
  scale_x_continuous(labels = scales::dollar_format(),
                     breaks = 0:3*20000) +
  guides(color = "none") +
  labs(x = "GDP per capita", y = NULL) +
  theme(panel.grid.major.y = element_blank())

grpd +
  geom_boxplot(
    aes(fill = continent), 
    alpha = .7
  ) +
  scale_fill_manual(values = pal) 

grpd +
  geom_boxplot(
    aes(fill = continent, 
        color = after_scale(
          clr_darken(fill, .4)
        )), 
    alpha = .7
  ) +
  scale_fill_manual(values = pal)

corporate_colors <- function(...) {
  corporate_cols <- c(
    `jungle`   = "#28A87D",
    `ocean`    = "#3B27A8",
    `neutral`  = "#B6C4D2",
    `red`      = "#D75782",
    `green`    = "#C4D857"
  )

  cols <- c(...)

  if (is.null(cols)) return (cols)

  corporate_cols[cols]
}

corporate_pal_c <- function(palette = "default", reverse = FALSE, ...) {
  
  default_pal <- corporate_colors("ocean", "neutral", "jungle")
  
  palettes <- list(
    `default` = default_pal,
    `light`   = prismatic::clr_lighten(default_pal, .3),
    `dark`    = prismatic::clr_darken(default_pal, .3),
    `pale`    = prismatic::clr_desaturate(default_pal, .3),
    `inverse` = prismatic::clr_negate(default_pal)
  )

  pal <- palettes[[palette]]
  pal <- unname(pal)

  if (reverse) pal <- rev(pal)

  grDevices::colorRampPalette(pal, ...)
}

scale_color_corporate_c <- function(palette = "default", reverse = FALSE, ...) {
  
  if (!palette %in% c("default", "light", "dark", "pale", "inverse"))
    stop('Palette should be one of "default", "light", "dark", "pale" or "inverse".')

  pal <- corporate_pal_c(palette = palette, reverse = reverse)

  ggplot2::scale_color_gradientn(colours = pal(256), ...)
}

scale_fill_corporate_c <- function(palette = "default", reverse = FALSE, ...) {
  
  if (!palette %in% c("default", "light", "dark", "pale", "inverse"))
    stop('Palette should be one of "default", "light", "dark", "pale" or "inverse".')

  pal <- corporate_pal_c(palette = palette, reverse = reverse)

  ggplot2::scale_fill_gradientn(colours = pal(256), ...)
}

g + 
  geom_point( 
    aes(color = lifeExp), alpha = .5
  ) +
  scale_color_corporate_c()

g + 
  geom_point( 
    aes(color = lifeExp), alpha = .5
  ) +
  scale_color_corporate_c(
    palette = "dark",
    reverse = TRUE
  )

g + 
  geom_point( 
    aes(fill = lifeExp), alpha = .5,
    shape = 21, color = "white"
  ) +
  scale_fill_corporate_c(
    palette = "inverse"
  )

g + 
  geom_point( 
    aes(fill = lifeExp), alpha = .5,
    shape = 21, color = "white"
  ) +
  scale_fill_corporate_c(
    palette = "inverse"
  ) +
  guides(size = guide_legend(
    override.aes = list(fill = "#493B2D")
  ))


##------------------------------------------------------------------------------
## WORKING WITH ANNOTATIONS
##------------------------------------------------------------------------------

gp + 
  annotate(
    geom = "text",
    x = 1000,
    y = 80,
    label = "Some\nadditional\ntext"
  )

gp +
  annotate(
    geom = "text",
    x = 1000,
    y = 80,
    label = "Some\nadditional\ntext",
    size = 6,
    color = "firebrick",
    fontface = "bold",
    family = "Open Sans",
    lineheight = .9
  )

gp +
  annotate(
    geom = "text",
    x = c(1000, 16000),
    y = c(80, 45),
    label = c("Text A", "Text B"),
    color = c("black", "firebrick"),
    size = c(10, 5),
    family = c("Open Sans", "Hepta Slab"),
    fontface = c("plain", "bold")
  )

gp +
  annotate(
    geom = "text",
    x = 500,
    y = 80,
    label = "Note",
    size = 4,
    family = "Open Sans",
    vjust = -.3,
  ) +
  annotate(
    geom = "segment",
    x = 500, xend = 2100,
    y = 80, yend = 66.3
  )

gp +
  annotate(
    geom = "text",
    x = 500,
    y = 80,
    label = "Note",
    size = 4,
    family = "Open Sans",
    vjust = -.3,
  ) +
  annotate(
    geom = "curve",
    x = 500, xend = 2100,
    y = 80, yend = 66.3
  )

gp +
  annotate(
    geom = "text",
    x = 500,
    y = 80,
    label = "Note",
    size = 4,
    family = "Open Sans",
    hjust = 1.1
  ) +
  annotate(
    geom = "curve",
    x = 500, xend = 2300,
    y = 80, yend = 66.3,
    curvature = -.3,
    arrow = arrow()
  )

gp +
  annotate(
    geom = "text",
    x = 500,
    y = 80,
    label = "Note",
    size = 4,
    family = "Open Sans",
    hjust = 1.1
  ) +
  annotate(
    geom = "curve",
    x = 500, xend = 2300,
    y = 80, yend = 66.3,
    curvature = -.3,
    arrow = arrow(
      length = unit(10, "pt"),
      type = "closed",
      ends = "both"
    )
  )

gp +
  annotate(
    geom = "text",
    x = 500,
    y = 80,
    label = "Note",
    size = 4,
    family = "Open Sans",
    vjust = 1.2
  ) +
  annotate(
    geom = "curve",
    x = 500, xend = 2300,
    y = 80, yend = 66.3,
    curvature = -.8,
    angle = 140,
    arrow = arrow(
      length = unit(10, "pt"),
      type = "closed"
    )
  )

gp + 
  geom_text(
    aes(label = country),
    size = 4
  )

gp + 
  geom_text(
    data = filter(gm2007, pop > 10^8),
    aes(label = country),
    size = 4,
    family = "Roboto Condensed",
  )

gp + 
  geom_text(
    data = filter(gm2007, pop > 10^8),
    aes(label = country),
    size = 4,
    family = "Roboto Condensed",
    show.legend = FALSE
  )

gp + 
  geom_text_repel(
    data = filter(gm2007, pop > 10^8),
    aes(label = country),
    size = 4,
    family = "Roboto Condensed",
    show.legend = FALSE
  )

gp + 
  geom_text_repel(
    data = filter(gm2007, pop > 10^8),
    aes(label = country),
    size = 4,
    family = "Roboto Condensed",
    show.legend = FALSE,
    box.padding = 1,
    min.segment.length = 0
  )

url <- "https://libapps-au.s3-ap-southeast-2.amazonaws.com/accounts/211467/images/OReilly_logo_rgb.png"
img <- magick::image_read(url)

img

gp +
  annotation_custom(
    grid::rasterGrob(
      image = img
    )
  )

gp +
  annotation_custom(
    grid::rasterGrob(
      image = img,
      x = .5,
      y = .9,
      width = .9
    )
  ) +
  ylim(NA, 90)

gx <- gp + theme(
  plot.margin = margin(10, 10, 40, 10)
)

ggsave("gapminder2007.png", gx, width = 10, height = 12)

plot <- image_read("gapminder2007.png")

img450 <- image_resize(img, 450)

image_composite(
  plot, img450, 
  offset = "+2500+3450"
)


##------------------------------------------------------------------------------
## WORKING WITH LAYOUTS
##------------------------------------------------------------------------------

ga <- 
  g + 
  geom_point(aes(color = continent), alpha = .5) +
  scale_color_manual(values = continent_colors, name = "Region:")

gb <- 
  grpd + 
  geom_jitter(aes(color = continent), height = .2, alpha = .5) +
  scale_color_manual(values = continent_colors, name = "Region:") +
  guides(color = "legend") 

ga + gb

ga / gb

ga + plot_spacer() + gb

ga + plot_spacer() + gb + 
  plot_layout(widths = c(1, .2, 1))

ga + plot_spacer() + gb + 
  plot_layout(widths = c(1, .2, 1), guides = "collect")

ga + plot_spacer() + gb + 
  plot_layout(widths = c(1, .2, 1), guides = "collect") + 
  plot_annotation(tag_levels = "A", tag_suffix = ")")




##------------------------------------------------------------------------------
## EXCITING EXTENSIONS
##------------------------------------------------------------------------------

gp + ggthemes::theme_gdocs()

gp + ggthemes::theme_stata()

gp + hrbrthemes::theme_ipsum()

gp + hrbrthemes::theme_ipsum_rc()

gp + hrbrthemes::theme_ipsum_rc(grid = FALSE, ticks = TRUE)

gp + hrbrthemes::theme_ipsum_rc(plot_title_family = "Hepta Slab", plot_title_size = 30)

gp + 
  labs(
    title = "*Gapminder* 2007",
    x = "**GDP** ___per capita___",
    y = "<span style='font-family:times;color:red'>Life expectancy</span>"
  )

gp + 
  labs(
    title = "*Gapminder* 2007",
    x = "**GDP** ___per capita___",
    y = "<span style='font-family:times;color:red'>Life expectancy</span>"
  ) +
  theme(
    plot.title = element_markdown(),
    axis.title.x = element_markdown(),
    axis.title.y = element_markdown()
  )

gp + 
  labs(
    title = "*Gapminder* 2007",
    x = "**GDP** ___per capita___",
    y = "<span style='font-family:times;color:red'>Life expectancy</span>"
  ) +
  theme(
    plot.title = element_textbox_simple(
      fill = "grey80"
    ),
    axis.title.x = element_markdown(),
    axis.title.y = element_markdown()
  )

gp + 
  labs(
    title = "*Gapminder* 2007",
    x = "**GDP** ___per capita___",
    y = "<span style='font-family:times;color:red'>Life expectancy</span>"
  ) +
  theme(
    plot.title = element_textbox_simple(
      fill = "grey80",
      padding = margin(8, 5, 5, 8),
      margin = margin(5, 0, 10, 0),
      r = unit(.6, "lines"),
      linetype = "dotted",
      box.color = "red"
    ),
    axis.title.x = element_markdown(),
    axis.title.y = element_markdown()
  )

gp + 
  geom_richtext(
    aes(
      x = 700, y = 75, 
      label = "My <b style='color:#28a87d;'>fancy</b><br>annotation"
    )
  )

gp + 
  geom_richtext(
    aes(
      x = 700, y = 75, 
      label = "My <b style='color:#28a87d;'>fancy</b><br>annotation"
    ),
    inherit.aes = FALSE, 
    stat = "unique"
  )

gp + 
  geom_mark_ellipse(
    aes(label = continent, 
        color = continent),
    size = 1
  )

gp + 
  geom_mark_ellipse(
    aes(label = continent, 
        color = continent),
    size = 1
  ) +
  scale_x_log10(expand = c(.5, .5)) +
  scale_y_continuous(expand = c(1, 1))

gp + 
  geom_mark_ellipse(
    aes(label = continent, 
        color = continent),
    size = 1,
    con.cap = unit(0, "mm")
  ) +
  scale_x_log10(expand = c(.5, .5)) +
  scale_y_continuous(expand = c(1, 1))

gp + 
  geom_mark_ellipse(
    aes(label = continent, 
        color = continent,
        filter = continent == "Africa"),
    size = 1,
    con.cap = unit(0, "mm"),
    show.legend = FALSE
  )

gp + 
  geom_mark_hull(
    aes(label = continent, 
        color = continent,
        filter = continent == "Africa"),
    size = 1,
    show.legend = FALSE,
    expand = .03
  )

gp + 
  geom_mark_hull(
    aes(label = continent, 
        color = continent,
        filter = continent == "Africa"),
    size = 1,
    show.legend = FALSE,
    expand = .03,
    description = "Lorem ipsum dolor est."
  ) +
  scale_y_continuous(limits = c(NA, 90))

ggplot(gm2007, 
       aes(x = .panel_x, y = .panel_y,
           color = continent, size = pop)) +
  geom_point(alpha = .5) +
  scale_color_manual(
    values = continent_colors
  ) +
  ggforce::facet_matrix(
    vars("lifeExp", "gdpPercap", "pop")
  )

ggplot(gm2007, 
       aes(x = .panel_x, y = .panel_y)) +
  geom_point(
    aes(color = continent, size = pop),
    alpha = .5
  ) +
  geom_bin2d() +
  ggforce::geom_autodensity(alpha = .7) +
  scale_color_manual(
    values = continent_colors
  ) +
  scale_fill_viridis_c(direction = -1, end = .9) +
  ggforce::facet_matrix(
    vars("lifeExp", "gdpPercap", "pop"),
    layer.lower = 2, layer.diag = 3
  )

ggplot(gm2007, aes(x = gdpPercap, y = continent)) +
  ggdist::stat_dist_dots(color = "black")

ggplot(gm2007, aes(x = gdpPercap, y = continent)) +
  ggdist::stat_halfeye()

ggplot(gm2007, aes(x = gdpPercap, y = continent)) +
  ggdist::stat_interval() +
  scale_color_viridis_d(option = "rocket", direction = -1, end = .8)

ggplot(gm2007, aes(x = lifeExp, y = continent)) +
  ggridges::geom_density_ridges()

ggplot(gm2007, aes(x = lifeExp, y = continent, fill = stat(x))) +
  ggridges::geom_density_ridges_gradient() +
  scale_fill_viridis_c(option = "rocket", direction = -1, end = .8)

ggplotly(gp, autosize = FALSE, width = 1200, height = 550)

gg <- gp +
  geom_point_interactive(aes(tooltip = country, data_id = continent, color = continent))
girafe(ggobj = gg)

girafe(ggobj = gg,
       options = list(opts_hover_inv(css = "opacity:.1;"),
                      opts_hover(css = "fill:#28a87d;stroke:white;stroke-width:2px;"),
                      opts_tooltip(use_fill = TRUE)))

gm2007 %>%
  mutate(across(where(is.double), ~round(.x, 1))) %>%
  group_by(continent) %>%
  e_charts(gdpPercap) %>%
  e_scatter(lifeExp, pop, bind = country) %>%
  e_x_axis(type = 'log') -> e

e %>% e_tooltip()

e %>%
  e_tooltip(formatter = htmlwidgets::JS("
    function(params){
      return('<strong>' + params.name +
             '</strong><br /> GDP per capita: ' + params.value[0] +
             '<br />Life expectancy: ' + params.value[1] +
             '<br />Population (M): ' + parseFloat(params.value[2] / 1000000).toFixed(1))
    }
  "))

ggplot(gapminder, aes(x = gdpPercap, y = lifeExp, size = pop)) +
  geom_point(aes(color = continent), alpha = .5) +
  scale_color_manual(values = continent_colors, name = "Region:") +
  scale_x_log10(breaks = c(500, 2000, 8000, 32000),
                labels = scales::label_dollar(accuracy = 1)) +
  scale_size(range = c(1, 12), name = "Population:", breaks = c(10, 100, 1000)*1000000,
             labels = scales::label_comma(scale = 1 / 10^6, suffix = "M")) +
  labs(x = "GDP per capita", y = "Life expectancy", title = "Health & Income",
       subtitle = 'Year: {frame_time}') -> anim

animate(anim + transition_time(year),
        height = 6, width = 10, units = "in", res = 300)

anim +
  facet_wrap(~continent) +
  guides(color = "none") +
  theme(legend.position = c(.8, .2)) +
  transition_time(year) -> anim_facet

animate(anim_facet + transition_time(year),
        height = 6, width = 10, units = "in", res = 300)

mpg_count <- count(mpg, class, manufacturer)

gi <- ggplot(gm_g7, aes(x = year, y = lifeExp, group = country)) +
  geom_line_interactive(aes(tooltip = country, data_id = country), alpha = .5) +
  geom_line(data = filter(gm_g7, country == "Japan"),
            color = "red", size = 1) +
  geom_vline(xintercept = 1970, linetype = "13") +
  coord_cartesian(expand = FALSE) +
  annotate(geom = "text", x = 1990, y = 65, size = 4, vjust = 0,
           family = "Hepta Slab", lineheight = .9, color = "red",
           label = "In the 70s, Japan took\nthe lead and has since then\nthe highest life expectancy at\nbirth of the G7 countries.")

girafe(ggobj = gi, width_svg = 12, height_svg = 6)





##------------------------------------------------------------------------------
## APPENDIX
##------------------------------------------------------------------------------


corporate_pal_d <- function(palette = "default", reverse = FALSE) {
  function(n) {
    if(n > 5) stop('Palettes only contain 5 colors')

    if (palette == "default") { pal <- corporate_colors(c(1, 2, 4, 5, 3))[1:n] }
    if (palette == "jungle_hl") { pal <- c(corporate_colors("jungle"), "grey40", "grey55", "grey70", "grey85")[1:n] }
    if (palette == "ocean_hl") { pal <- c(corporate_colors("ocean"), "grey40", "grey55", "grey70", "grey85")[1:n] }
    if (palette == "red_hl") { pal <- c(corporate_colors("red"), "grey40", "grey55", "grey70", "grey85")[1:n] }
    if (palette == "green_hl") { pal <- c(corporate_colors("green"), "grey40", "grey55", "grey70", "grey85")[1:n] }
    
    pal <- unname(pal)

    if (reverse) rev(pal) else pal
  }
}

scale_color_corporate_d <- function(palette = "default", reverse = FALSE, ...) {
  if (!palette %in% c("default", "jungle_hl", "ocean_hl", "red_hl", "green_hl")) stop('Palette should be "default", "jungle_hl", "ocean_hl", "red_hl" or "green_hl".')

  pal <- corporate_pal_d(palette = palette, reverse = reverse)

  ggplot2::discrete_scale("colour", paste0("corporate_", palette), palette = pal, ...)
}

scale_fill_corporate_d <- function(palette = "default", reverse = FALSE, ...) {
  if (!palette %in% c("default", "jungle_hl", "ocean_hl", "red_hl", "green_hl")) stop('Palette should be "default", "jungle_hl", "ocean_hl", "red_hl" or "green_hl".')

  pal <- corporate_pal_d(palette = palette, reverse = reverse)

  ggplot2::discrete_scale("fill", paste0("corporate_", palette), palette = pal, ...)
}

g +
  geom_point(
    aes(color = continent), alpha = .5
  ) +
  scale_color_corporate_d()

g +
  geom_point(
    aes(color = continent), alpha = .5
  ) +
  scale_color_corporate_d(
    palette = "jungle_hl"
  )

g +
  geom_point(
    aes(color = fct_relevel(
      continent, "Asia", after = 0L)
    ), alpha = .5
  ) +
  scale_color_corporate_d(
    palette = "red_hl", name = NULL
  )


##------------------------------------------------------------------------------
## THAT'S IT FOLKS...
##------------------------------------------------------------------------------