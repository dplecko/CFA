
library(ggbrace)
library(ggplot2)
library(data.table)
library(latex2exp)
library(faircause)

set.seed(2023)
n <- 500

x <- rbinom(n, 1, 0.5)
w <- x + (-1)^x * runif(n)^(1/2)
uy <- runif(n)

medical <- TRUE
if (medical) {
  labels <- c("Doomed", "Helped", "Safe")
} else labels <- c("Unsafe", "Complier", "Safe")

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
                      name = "Canonical Type:",
                      labels = labels) +
  theme_minimal() +
  geom_brace(aes(c(-0.1, 0), c(0, 1), label = "X = 0"), inherit.data=F, rotate = 270,
             labelsize = 5, labelrotate = 90) +
  geom_brace(aes(c(-0.1, 0), c(1, 2), label = "X = 1"), inherit.data=F, rotate = 270,
             labelsize = 5, labelrotate = 90) +
  coord_cartesian(y=1.1 * range(df$w + df$x), clip = "off") +
  xlab(latex2exp::TeX("$u_y$ (resilience)")) + ylab(latex2exp::TeX("Sex")) +
  theme(
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    # axis.title.y = element_text(color = "white"),
    legend.position = "bottom",
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.margin=margin(t = 0.31, b = 0.31, unit='cm'),
    axis.title = element_text(size = 16)#,
    # axis.title.y.left = element_text()
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
             labelsize = 5, labelrotate = 90) +
  geom_brace(aes(c(-0.1, 0), c(1, 2), label = "X = 1"), inherit.data=F, rotate = 270,
             labelsize = 5, labelrotate = 90) +
  coord_cartesian(y=1.1 * range(df$w + df$x), clip = "off") +
  xlab(latex2exp::TeX("$u_y$ (resilience)")) + ylab(latex2exp::TeX("$W = w$")) +
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

# Module 5 additional plots

p1 <- ggplot(df, aes(x = w, y = delta, color = factor(x))) +
  geom_line() + theme_bw() +
  ylab(TeX("$P(y_{d_1} | w, x) - P(y_{d_0} | w, x)$")) +
  scale_color_discrete(labels = c("Male", "Female"), name = "Sex") +
  theme(legend.position = c(0.2, 0.8), legend.box.background = element_rect())

cowplot::plot_grid(p1, p2)

ggsave("~/Desktop/oracle-vs-dm-intuition.png",
       width = 10, height = 4)

# Section 5.3.3 additional plots

# decomposition of the disparity at threshold 1/2
df$d <- df$delta > 1 / 2
fcb <- fairness_cookbook(df[1:500,], X = "x", Z = NULL, W = "w", Y = "d",
                         x0 = 0, x1 = 1)

meas <- fcb$measures[fcb$measures$boot == 1 & fcb$measures$rep == 1, ]
meas <- meas[meas$measure %in% c("tv", "ctfde", "ctfie", "ctfse"), ]
meas$value <- c(-0.5, 0, 0, 0.5)

fcb$measures <- meas
autoplot(fcb) + ggtitle(NULL)
ggsave("~/Desktop/toolkitA.png", width = 6, height = 4)

# density of P(\Delta \mid x) for all x
data <- data.frame(x = c(seq(0, 1, 0.01), seq(0, 1, 0.01)) / 3,
                   y = 2 * c(seq(0, 1, 0.01), 1 - seq(0, 1, 0.01)),
                   sex = rep(c(0, 1), each = 101))
ggplot(data, aes(x = x, y = y, group = sex)) +
  geom_line() +
  geom_ribbon(data=data,aes(x=x,ymax=y, fill =factor(sex)),ymin=0,alpha=0.3) +
  theme_bw() + xlab(TeX("Benefit \\Delta")) + ylab("Distribution Density") +
  scale_fill_discrete(
    name = "Distribution",
    labels = c(TeX("$\\Delta | x_0$ (Male)"), TeX("$\\Delta | x_1$ (Female)"))) +
  theme(
    legend.position = "bottom",
    legend.box.background = element_rect(),
    legend.text = element_text(size = 18),
    legend.title = element_text(size = 16),
    axis.title = element_text(size = 16),
    legend.text.align = 0
  )
ggsave("~/Desktop/toolkitB.png", width = 6, height = 4.5)

# decomposition over all thresholds
decomp_delta <- function(delta) {

  c(delta = delta / 3, de = 0, ie = 1 - delta^2 - (1-delta)^2, se = 0)
}

dcmp_ot <- do.call(rbind, lapply(seq(0, 1, 0.01), function(x) decomp_delta(x)))
dcmp_ot <- as.data.frame(dcmp_ot)
dcmp_ot <- reshape2::melt(dcmp_ot, id.vars = "delta")
ggplot(dcmp_ot, aes(x = delta, y = value, color = variable)) +
  geom_line() + theme_bw() +
  scale_color_discrete(labels = c("DE", "IE", "SE"), name = "Effect") +
  xlab(TeX("Threshold \\delta")) + ylab("Value") +
  theme(legend.position = c(0.875, 0.775), axis.title = element_text(size = 14),
        legend.box.background = element_rect())
ggsave("~/Desktop/toolkitC.png", width = 6, height = 4)

# counterfactual density P(\Delta_C \mid x)
data <- data.frame(x = c(seq(0, 1, 0.01), seq(0, 1, 0.01)) / 3,
                   y = 2 * c(seq(0, 1, 0.01), seq(0, 1, 0.01)),
                   sex = rep(c(0, 1), each = 101))
ggplot(data, aes(x = x, y = y, group = sex)) +
  geom_line() +
  geom_ribbon(data=data,aes(x=x,ymax=y, fill =factor(sex)),ymin=0,alpha=0.3) +
  theme_bw() + xlab(TeX("Benefit \\Delta")) + ylab("Distribution Density") +
  scale_fill_discrete(name = "Distribution",
                      labels = c(TeX("$\\Delta | x_0$"),
                                 TeX("$\\Delta_{x_1, W_{x_0}} | x_1$"))) +
  theme(
    legend.position = "bottom",
    legend.box.background = element_rect(),
    legend.text = element_text(size = 18),
    legend.title = element_text(size = 16),
    axis.title = element_text(size = 16),
    legend.text.align = 0
  )
ggsave("~/Desktop/toolkitD.png", width = 6, height = 4.5)
