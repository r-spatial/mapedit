## ----include = FALSE----------------------------------------------------------
google <- suppressWarnings(try(readLines("https://8.8.8.8", n = 1L), silent = TRUE))
is_online <- !inherits(google, "try-error")

knitr::opts_chunk$set(
  dev = "ragg_png",
  dpi = 144,
  collapse = TRUE,
  comment = "#>",
  fig.asp = NULL,
  fig.height = 4.326,
  fig.width = 7,
  eval = is_online
)

systemfonts::require_font("Spectral", fallback = "serif")

## -----------------------------------------------------------------------------
grid::grid.text(
  "Spectral 🎉",
  gp = grid::gpar(fontfamily = "Spectral", fontface = 2, fontsize = 30)
)

## -----------------------------------------------------------------------------
library(ggplot2)
ggplot(na.omit(penguins)) +
  geom_point(aes(x = bill_len, y = body_mass, colour = species)) +
  labs(x = "Bill Length", y = "Body Mass", colour = "Species") +
  theme_minimal(base_family = "Spectral")

## -----------------------------------------------------------------------------
systemfonts::match_fonts("Spectral", weight = "bold")
systemfonts::font_fallback("🎉", family = "Spectral", weight = "bold")

## -----------------------------------------------------------------------------
# systemfonts::system_fonts()

## -----------------------------------------------------------------------------
all_fonts <- systemfonts::system_fonts()
all_fonts <- all_fonts[!grepl("^/Users", all_fonts$path),]
rmarkdown::paged_table(all_fonts)

## -----------------------------------------------------------------------------
systemfonts::register_variant(
  name = "Spectral Light",
  family = "Spectral",
  weight = "light"
)

## -----------------------------------------------------------------------------
grid::grid.text(
  "Light weight is soo classy",
  gp = grid::gpar(fontfamily = "Spectral Light", fontsize = 30)
)

## -----------------------------------------------------------------------------
systemfonts::register_variant(
  name = "Spectral Small Caps",
  family = "Spectral",
  features = systemfonts::font_feature(
    letters = "small_caps"
  )
)
grid::grid.text(
  "All caps — Small caps",
  gp = grid::gpar(fontfamily = "Spectral Small Caps", fontsize = 30)
)

## -----------------------------------------------------------------------------
# systemfonts::get_from_google_fonts("Barrio")
# 
# grid::grid.text(
#   "A new font a day keeps Tufte away",
#   gp = grid::gpar(fontfamily = "Barrio", fontsize = 30)
# )

## -----------------------------------------------------------------------------
systemfonts::require_font("Barrio")
grid::grid.text(
  "A new font a day keeps Tufte away",
  gp = grid::gpar(fontfamily = "Barrio", fontsize = 30)
)

## -----------------------------------------------------------------------------
systemfonts::require_font("Rubik Distressed")

grid::grid.text(
  "There are no bad fonts\nonly bad text",
  gp = grid::gpar(fontfamily = "Rubik Distressed", fontsize = 30)
)

## -----------------------------------------------------------------------------
systemfonts::fonts_as_import("Barrio")
systemfonts::fonts_as_import("Rubik Distressed", type = "link")

## -----------------------------------------------------------------------------
substr(systemfonts::fonts_as_import("Arial", repositories = NULL), 1, 200)

## -----------------------------------------------------------------------------
svg <- svglite::svgstring(web_fonts = "Barrio")
grid::grid.text("Example", gp = grid::gpar(fontfamily = "Barrio"))
invisible(dev.off())
svg()

