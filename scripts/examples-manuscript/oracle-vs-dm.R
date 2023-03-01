
library(ggbrace)
library(ggplot2)
library(data.table)
library(latex2exp)

set.seed(2023)

n <- 500

x <- rbinom(n, 1, 0.5)
w <- x + (-1)^x * runif(n)^(1/2)
uy <- runif(n)

alph <- 1 / 5
beta <- 1 / 3
delta <- beta * w

canon <- ifelse(uy > 0.5 + w * alph, "Safe",
                ifelse(uy > 0.5 + w * alph - w * beta, "Helped", "Doomed"))

df <- data.frame(x, w, uy, canon, delta)

dt.follow <-
  data.table(group = c(1,1,1),
             polygon.x = c(0.5, 0.5 + 1 * alph - 1  * beta,
                           0.5 + 1 * alph - 0 * beta),
             polygon.y = c(0, 1, 1))

dt.never <- data.table(group = c(1,1,1,1),
                       polygon.x = c(0, 0, 0.5 + 1 * alph - 1 * beta,
                                     0.5 + 0 * alph - 0 * beta),
                       polygon.y = c(0, 1, 1, 0))

dt.always <-
  data.table(group = c(1,1,1,1),
             polygon.x = c(0.5 + 1 * alph - 0 * beta, 1, 1, 0.5),
             polygon.y = c(1, 1, 0, 0))

p1 <- ggplot(df, aes(x = uy, y = w + x, color = canon)) +
  geom_polygon(data = dt.follow,
               aes(x = polygon.x, y = polygon.y, group = group, fill = "green"),
               color = "green", alpha = 0.3, linewidth = 0) +
  geom_polygon(data = dt.follow,
               aes(x = polygon.x, y = 1+polygon.y, group = group, fill = "green"),
               color = "green", alpha = 0.3, linewidth = 0) +
  geom_polygon(data = dt.never,
               aes(x = polygon.x, y = polygon.y, group = group, fill = "blue"),
               color = "blue", alpha = 0.3, linewidth = 0) +
  geom_polygon(data = dt.never,
               aes(x = polygon.x, y = 1+polygon.y, group = group, fill = "blue"),
               color = "blue", alpha = 0.3, linewidth = 0) +
  geom_polygon(data = dt.always,
               aes(x = polygon.x, y = polygon.y, group = group, fill = "orange"),
               color = "orange", alpha = 0.3, linewidth = 0) +
  geom_polygon(data = dt.always,
               aes(x = polygon.x, y = 1+polygon.y, group = group, fill = "orange"),
               color = "orange", alpha = 0.3, linewidth = 0) +
  geom_point() +
  scale_colour_manual(values = c("orange", rgb(27, 177, 0, maxColorValue = 255), "blue"),
                      name = "Canonical Type:") +
  theme_minimal() +
  geom_brace(aes(c(-0.1, 0), c(0, 1), label = "X = 0"), inherit.data=F, rotate = 270,
             labelsize = 5) +
  geom_brace(aes(c(-0.1, 0), c(1, 2), label = "X = 1"), inherit.data=F, rotate = 270,
             labelsize = 5) +
  coord_cartesian(y=1.1 * range(df$w + df$x), clip = "off") +
  xlab(latex2exp::TeX("$u_y$")) + ylab(latex2exp::TeX("$W = w$")) +
  theme(
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_text(color = "white"),
    legend.position = "bottom",
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.margin=margin(t = 0.31, b = 0.31, unit='cm'),
    axis.title = element_text(size = 16),
    axis.title.y.left = element_text()
  ) +
  annotate(
    "text", label = "U-space for Oracle",
    x = 0.5, y = 2.1, size = 6, colour = "black"
  ) + scale_fill_discrete(guide = "none") +
  geom_segment(aes(x = 0, y = 1, xend = 1, yend = 1), color = "black",
               linewidth = 0.5, linetype = "dashed")

p2 <- ggplot(df, aes(x = uy, y = w + x, color = delta)) +
  # geom_polygon(data = dt.follow,
  #              aes(x = polygon.x, y = polygon.y, group = group, fill = "green"),
  #              color = "green", alpha = 0.3, linewidth = 0) +
  # geom_polygon(data = dt.follow,
  #              aes(x = polygon.x, y = 1+polygon.y, group = group, fill = "green"),
  #              color = "green", alpha = 0.3, linewidth = 0) +
  # geom_polygon(data = dt.never,
  #              aes(x = polygon.x, y = polygon.y, group = group, fill = "blue"),
  #              color = "blue", alpha = 0.3, linewidth = 0) +
  # geom_polygon(data = dt.never,
  #              aes(x = polygon.x, y = 1+polygon.y, group = group, fill = "blue"),
  #              color = "blue", alpha = 0.3, linewidth = 0) +
  # geom_polygon(data = dt.always,
  #              aes(x = polygon.x, y = polygon.y, group = group, fill = "red"),
  #              color = "red", alpha = 0.3, linewidth = 0) +
  # geom_polygon(data = dt.always,
  #              aes(x = polygon.x, y = 1+polygon.y, group = group, fill = "red"),
  #              color = "red", alpha = 0.3, linewidth = 0) +
  geom_point() +
  scale_color_gradient(name = TeX("Benefit $\\Delta$"),
                       labels = NULL, low = rgb(1, 0, 0), high = rgb(0, 1, 0)) +
  theme_minimal() +
  geom_brace(aes(c(-0.1, 0), c(0, 1), label = "X = 0"), inherit.data=F, rotate = 270,
             labelsize = 5) +
  geom_brace(aes(c(-0.1, 0), c(1, 2), label = "X = 1"), inherit.data=F, rotate = 270,
             labelsize = 5) +
  coord_cartesian(y=1.1 * range(df$w + df$x), clip = "off") +
  xlab(latex2exp::TeX("$u_y$")) + ylab(latex2exp::TeX("$W = w$")) +
  theme(
    # axis.ticks.y = element_blank(),
    # axis.text.y = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(size = 16),
    axis.title.x = element_text(size = 16, color = "white"),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(color = "white")
  ) +
  annotate(
    "text", label = "U-space for Decision-Maker",
    x = 0.5, y = 2.1, size = 6, colour = "black"
  ) + geom_segment(aes(x = 0, y = 1, xend = 1, yend = 1), color = "black",
                   linewidth = 0.5, linetype = "dashed") +
  scale_y_continuous(position = "right",
                     labels = c("0.0", "0.5", "0.0\n1.0", "0.5", "1"),
                     breaks = c(0, 0.5, 1, 1.5, 2))


cowplot::plot_grid(p1, p2, ncol = 2L, labels = c("(a)", "(b)"))

ggsave("~/Desktop/oracle-vs-dm.png", width = 12, height = 6, bg = "white")


