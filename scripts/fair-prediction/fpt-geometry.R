library(rgl)
library(plotly)
library(reshape2)

dt <- cbind(x = 1 + 0.5 * rnorm(100), y = 1 + 0.3 * rnorm(100),
            z = 1 + 0.3 * rnorm(100))
ellipse <- ellipse3d(cov(dt))

# plot <- plot_ly()%>%
#   add_trace(x=ellipse$vb [1,] - 2, y=ellipse$vb [2,], z=ellipse$vb [3,] - 1,
#             type='mesh3d', alphahull = 0, opacity = 0.4,
#             color = c(0, 0.33, 0.66, 1))%>%
#   layout(xaxis= list(showticklabels = FALSE)) %>%
#   layout(yaxis= list(showticklabels = FALSE))
#
# plot

plot <- plot_ly(x=ellipse$vb [1,] - 2, y=ellipse$vb [2,], z=ellipse$vb [3,] - 1,
        intensity = 2,
        type='mesh3d', alphahull = 0, opacity = 0.4,
        colors = colorRamp(c("darkgreen", "green"))) %>%
  layout(scene = list(xaxis=list(showticklabels = FALSE, title = ""),
                      yaxis=list(showticklabels = FALSE, title = ""),
                      zaxis=list(showticklabels = FALSE, title = "")))

graph_reso <- 0.05

#Setup Axis
axis_x <- seq(-1, 1, by = graph_reso)
axis_y <- seq(-1, 1, by = graph_reso)

set.seed(2024)
norm_vec <- runif(3)

surface <- expand.grid(x = axis_x, y = axis_y, KEEP.OUT.ATTRS = F)
surface$z <- -(surface$x * norm_vec[1] + surface$y * norm_vec[2]) / norm_vec[3]
surface <- acast(surface, y ~ x, value.var = "z") #y ~ x

plot %>% add_trace(z = surface,
                   x = axis_x,
                   y = axis_y,
                   type = "surface", opacity = 0.4)

