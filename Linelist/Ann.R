library(tidyverse)
library(gt)

res_log <- read_csv("https://raw.githubusercontent.com/kathoffman/steroids-trial-emulation/main/output/res_log.csv")
res <- read_csv("https://raw.githubusercontent.com/kathoffman/steroids-trial-emulation/main/output/res.csv")

res <- res_log |>
  rename_with(~str_c("log.", .), estimate:conf.high) |>
  select(-p.value) |>
  full_join(res)

## Ann data 
data <- readxl::read_xlsx("ann.xlsx")
glimpse(data)

data$OR <- as.double(data$OR)

res <- res_log |>
  rename_with(~str_c("log.", .), estimate:conf.high) |>
  select(-p.value) |>
  full_join(res)

p <- 
  res |>
  ggplot(aes(y = fct_rev(model))) + 
  theme_classic()
p

p <- p +
  geom_point(aes(x=log.estimate), shape=15, size=3) +
  geom_linerange(aes(xmin=log.conf.low, xmax=log.conf.high)) 
p

p <- p +
  geom_vline(xintercept = 0, linetype="dashed") +
  labs(x="Log Hazard Ratio", y="")
p

p <- p +
  coord_cartesian(ylim=c(1,11), xlim=c(-1, .5))
p


a <- data |> 
  ggplot(aes(y = fct_rev(Categories)))+
  theme_classic()


a <- a +
  geom_point(aes(x= OR), shape=15, size=2) +
  geom_linerange(aes(xmin=low_confhi, xmax=high_conf)) 
a


a <- a +
  geom_vline(xintercept = 1, linetype="dashed") +
  labs(x="Relative Risk of Death", y="")
a

a <- a +
  coord_cartesian(ylim=c(1,55), xlim=c(-20, 25))
a

a <- a +
  annotate("text", x = - 10, y = 2, label = "Decreased Risk") +
  annotate("text", x = 10, y = 2, label = "Increased Risk")
a

a_mid <- a + 
  theme(axis.line.y = element_blank(),
        axis.ticks.y= element_blank(),
        axis.text.y= element_blank(),
        axis.title.y= element_blank())
a_mid

##Other things 

# wrangle results into pre-plotting table form
data_plot <- data |>
  # round estimates and 95% CIs to 2 decimal places for journal specifications
  mutate(across(
    c(OR, low_confhi, high_conf),
    ~ str_pad(
      round(.x, 2),
      width = 4,
      pad = "0",
      side = "right"
    )
  ),
  # add an "-" between HR estimate confidence intervals
  estimate_lab = paste0(OR, " (", low_confhi, "-", high_conf, ")"))

p_left <-
  data_plot  |>
  ggplot(aes(y = fct_rev(Categories)))

p_left

p_left <- 
  p_left +
  geom_text(aes(x = 0, label = Categories), hjust = 0, fontface = "bold")

p_left

p_left <- 
  p_left +
  geom_text(
    aes(x = 1, label = estimate_lab),
    hjust = 0,
    fontface = ifelse(data_plot$estimate_lab == "Hazard Ratio (95% CI)", "bold", "plain")
  )

p_left


p_left <-
  p_left +
  theme_void() +
  coord_cartesian(xlim = c(0, 1.4))

p_left


# right side of plot - pvalues
p_right <-
  data_plot  |>
  ggplot() +
  geom_text(
    aes(x = 0, y = Categories, label = `p-value (Type 3)`),
    hjust = 0,
    fontface = ifelse(data_plot$`p-value (Type 3)` == "p-value", "bold", "plain")
  ) +
  theme_void() 

p_right

library(patchwork)
layout <- c(
  area(t = 0, l = 0, b = 30, r = 2), # left plot, starts at the top of the page (0) and goes 30 units down and 3 units to the right
  area(t = 5, l = 4, b = 30, r = 12), # middle plot starts a little lower (t=1) because there's no title. starts 1 unit right of the left plot (l=4, whereas left plot is r=3), goes to the bottom of the page (30 units), and 6 units further over from the left plot (r=9 whereas left plot is r=3)
  area(t = 2, l = 9, b = 30, r = 14) # right most plot starts at top of page, begins where middle plot ends (l=9, and middle plot is r=9), goes to bottom of page (b=30), and extends two units wide (r=11)
)
# final plot arrangement
p_left +  p_right +  a_mid  + plot_layout(design = layout)



##----- Attempt two 
library(grid)
library(forestploter)

dt <- read.csv(system.file("extdata", "example_data.csv", package = "forestploter"))

## Ann data 
data <- readxl::read_xlsx("ann.xlsx")
glimpse(data)

data <- data[,1:6]
dt <- dt[,1:6]


# Indent the subgroup if there is a number in the placebo column
# Indent the subgroup if there is a number in the placebo column
dt$Subgroup <- ifelse(is.na(dt$Placebo), 
                      dt$Subgroup,
                      paste0("   ", dt$Subgroup))

data$Categories <- ifelse(is.na(data$Categories ), 
                      data$Categories ,
                      paste0("   ", data$Categories ))

# NA to blank or NA will be transformed to carachter.
dt$Treatment <- ifelse(is.na(dt$Treatment), "", dt$Treatment)
dt$Placebo <- ifelse(is.na(dt$Placebo), "", dt$Placebo)
dt$se <- (log(dt$hi) - log(dt$est))/1.96

data$`HIV Positives n( weighted row %)` <- ifelse(is.na(data$`HIV Positives n( weighted row %)`), "", data$`HIV Positives n( weighted row %)`)
data$`HIV Negatives n(weighted row %)` <- ifelse(is.na(data$`HIV Negatives n(weighted row %)`), "", data$`HIV Negatives n(weighted row %)`)
data$se <- (log(data$high) - log(data$OR))/1.96

data$OR <- as.double(data$OR)

# Add a blank column for the forest plot to display CI.
# Adjust the column width with space, and increase the number of spaces below 
# to have a larger area to draw the CI. 
data$` ` <- paste(rep(" ", 20), collapse = " ")

# Create a confidence interval column to display
data$`HR (95% CI)` <- ifelse(is.na(data$se), "",
                           sprintf("%.2f (%.2f -  %.2f)",
                                   data$OR, data$low, data$high))
head(dt)

p <- forest(data[1:4],
            est = data$OR,
            lower = data$low, 
            upper = data$high,
            sizes = 0.4,
            ci_column = 4,
            ref_line = 1,
            arrow_lab = c("Decreased Risk", "Increased Risk"))

p

ggplot2::ggsave(filename = "rplot.png", plot = p,
                dpi = 300,
                width = 7.5, height = 7.5, units = "in")
# Print plot

# Get width and height
p_wh <- get_wh(plot = p, unit = "in")
png('rplot.png', res = 300, width = p_wh[1], height = p_wh[2], units = "in")
p
dev.off()

# Or get scale
get_scale <- function(plot,
                      width_wanted,
                      height_wanted,
                      unit = "in"){
  h <- convertHeight(sum(plot$heights), unit, TRUE)
  w <- convertWidth(sum(plot$widths), unit, TRUE)
  max(c(w/width_wanted,  h/height_wanted))
}
p_sc <- get_scale(plot = p, width_wanted = 6, height_wanted = 4, unit = "in")

ggplot2::ggsave(filename = "rplot.png", 
                plot = p,
                dpi = 300,
                width = 6, 
                height = 4,
                units = "in",
                scale = p_sc)
                
                

