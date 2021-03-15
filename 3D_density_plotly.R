install.packages("plotly")
library(plotly)
library(MASS)

den3d <- kde2d(volas$log_garch, volas$log_pfrv)

plot_ly(x = den3d$x, 
        y = den3d$y, 
        z = den3d$z) %>% 
  add_surface()

# Source: https://stackoverflow.com/questions/13613157/plot-3d-density
