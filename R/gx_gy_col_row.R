library(tibble)
library(ggplot2)

gridsize = 20
plotdim = c(500, 500)
by = 20
xyjust = -0.5
size = 4


tibble_gx_gy <- function(plotdim = c(1000, 500), by = 20) {
  gx <- seq(0, plotdim[[1]], by = by)
  gy <- seq(0, plotdim[[2]], by = by)
  
  if (length(gx) > length(gy)) {
    gy <- rep_len(gy, length.out = length(gx))
  } else {
    gx <- rep_len(gx, length.out = length(gy))
  }
  tibble::tibble(gx = gx, gy = gy)
}



col_row <- tibble_gx_gy(plotdim = plotdim, by = by) %>% 
  fgeo.tool::add_col_row(gridsize = gridsize, plotdim = plotdim) %>% 
  dplyr::filter(!is.na(col) & !is.na(row))


max_x <- max0(col_row$gx)
max_y <- max0(col_row$gy)

ggplot(col_row, aes(x = gx, y = gy)) +
  geom_hline(yintercept = seq(0, max_y + gridsize, gridsize)) +
  geom_vline(xintercept = seq(0, max_x + gridsize, gridsize)) +
  scale_x_continuous(limits = c(0, max_x + gridsize)) +
  scale_y_continuous(limits = c(0, max_y + gridsize)) +
  coord_fixed() +
  geom_text(
    aes(y = max_y + gridsize, x = gx + gridsize/2, label = col), vjust = xyjust, size = size
  ) +
  geom_text(
    aes(x = max_x + gridsize, y = gy + gridsize/2, label = row), hjust = xyjust, size = size
  ) +
  theme_bw()

