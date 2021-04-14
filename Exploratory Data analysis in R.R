#Expoloratory Data Analysis (EDA)

rm(list = ls())
graphics.off()

# Libraries
library(dplyr)
library(tidyr)
library(ggplot2)

install.packages("cowplot")
library(cowplot)

# Diamonds dataset

# Check dataset
help("diamonds") # dataset description

str(diamonds) # dataset structure

nrow(diamonds) # rows - cases
ncol(diamonds) # columns - variables

View(diamonds) # view table
head(diamonds) # print first 6 rows

summary(diamonds)


# Save data tables for further usage
set.seed(123)

# original dataset
diamonds.big <- diamonds 

# smaller dataset
diamonds.small <- diamonds.big %>% dplyr::sample_n(size = 5000, replace = F)

# tiny dataset
diamonds.tiny <- diamonds.big %>% dplyr::sample_n(size = 500, replace = F)

# Save data
save(list = c("diamonds.big", "diamonds.small", "diamonds.tiny"), file = "./data/diamonds.RData")



# Questions to answer?
#   (Initial set of questions)
#   (Will modify questions throughout the course)

# What does the distribution of each variable look like?
# Can you see any interesting or strange patterns in given distributions?
# Are there any strange values?
# Are there any typical values or most common ones?
# What is the variation of values of each variables?
# Can you see any relationships among given variables?
# ... (to be continued)



# Dotplot
# (explore continuous variables)

# carat (weight of a diamond)
diamonds.small %>% 
  ggplot(aes(x = carat)) +
  geom_dotplot(binwidth = 0.007)

# check summary od carat
summary(diamonds.big$carat)

# Questions:
#   Maximum carat value around 3 carats?
#   Why some carat value more typical (around whole numbers)?
#   Why there are some extreme values (outliers)?

# different directions of stacks
diamonds.small %>% 
  ggplot(aes(x = carat)) +
  geom_dotplot(binwidth = 0.0075, stackdir = "up") +
  #  geom_dotplot(binwidth = 0.0075, stackdir = "down") +
  #  geom_dotplot(binwidth = 0.0075, stackdir = "center") +
  scale_x_continuous(breaks = seq(0,5,0.1))

# change other parameters of geom function
diamonds.small %>% 
  ggplot(aes(x = carat)) +
  geom_dotplot(binwidth = 0.0075, 
               stackdir = "up", 
               color = "red", 
               stackratio = 0.5,
               dotsize = 1) +
  scale_x_continuous(breaks = seq(0,5,0.2))

# color aesthetics VS color paramater inside geom
diamonds.small %>% 
  ggplot(aes(x = carat)) +
  geom_dotplot(binwidth = 0.0075, color = "blue")
diamonds.small %>% 
  ggplot(aes(x = carat, color = "blue")) +
  geom_dotplot(binwidth = 0.0075)

# final carat dotplot (for exporting)
diamonds.big %>% 
  ggplot(aes(x = carat)) +
  geom_dotplot(binwidth = 0.001, stackdir = "up", color = "gray20") +
  scale_x_continuous(breaks = seq(0,5,0.25)) +
  scale_y_continuous(breaks = seq(0,1,0.1)) +
  ggtitle("Diamond carat - dotplot") +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 25, face = "bold"))

ggsave(filename = "./figure/03_dotplot_carat_diamonds.png", plot = last_plot(),
       units = "cm", width = 29.7, height = 21, dpi = 600) 



# Histogram
# (explore continuous variables)

# diamond price (in USD)
diamonds.small %>% 
  ggplot(aes(x = price)) +
  geom_histogram()

# change parameters for controlling number of bins
diamonds.small %>% 
  ggplot(aes(x = price)) +
  geom_histogram(bins = 45)

diamonds.small %>% 
  ggplot(aes(x = price)) +
  geom_histogram(bins = 500)

diamonds.small %>% 
  ggplot(aes(x = price)) +
  geom_histogram(binwidth = 100) +
  scale_x_continuous(breaks = seq(0,20000,500))

# Questions:
# Why the peice peak at around 800 dollars?
# Why there is another bumb at around 4500 dollars?
# Why there are are almost no diamonds with price around 1500 dollars?

# Let's see what is happening on whole dataset:
diamonds.big %>% 
  ggplot(aes(x = price)) +
  geom_histogram(binwidth = 100) +
  scale_x_continuous(breaks = seq(0,20000,500))


# final diamond price histogram (for exporting)
diamonds.big %>% 
  ggplot(aes(x = price)) +
  geom_histogram(binwidth = 100, color = "black", fill = "deepskyblue3") +
  xlab("Price (in USD)") +
  ylab("Frequency") +
  scale_x_continuous(breaks = seq(0,20000,2500)) +
  scale_y_continuous(breaks = seq(0,3000,500)) +
  ggtitle("Diamond price - histogram") +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 25, face = "bold"))

ggsave(filename = "./figure/03_histogram_price_diamonds.png", plot = last_plot(),
       units = "cm", width = 29.7, height = 21, dpi = 600) 

# Density plot
# (explore continuous variables)

# diamond price (in USD)
diamonds.small %>% 
  ggplot(aes(x = price)) +
  geom_density()

# adjust parameters
diamonds.small %>% 
  ggplot(aes(x = price)) +
  geom_density(adjust = 1/5, linetype = "dashed", size = 1.2, color = "red", fill = "gray")

# final diamond price density plot (for exporting)
diamonds.big %>% 
  ggplot(aes(x = price)) +
  geom_density(adjust = 1/5, color = "black", fill = "deepskyblue3") +
  xlab("Price (in USD)") +
  ylab("Density") +
  scale_x_continuous(breaks = seq(0,20000,2500)) +
  ggtitle("Diamond price - density plot") +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 25, face = "bold"))

ggsave(filename = "./figure/03_density_price_diamonds.png", plot = last_plot(),
       units = "cm", width = 29.7, height = 21, dpi = 600) 



# Frequency plot
# (explore continuous variables)

# diamond dimensions x / y / z (in mm)

summary(select(diamonds.big, x, y, z))

# x - length
diamonds.small %>% 
  ggplot(aes(x = x)) +
  geom_freqpoly(binwidth = 0.05) +
  scale_x_continuous(breaks = seq(0,10,0.25))

# Questions:
#   Why there are some values around 0 than is a gap until around 3 (missing data values)?
#   Why there are some more densed values (peaks)?

# Lets check values around 0 (missing data)
diamonds.big %>% 
  filter(x < 3.5)

# redraw plot without missing values
diamonds.small %>% 
  filter(x > 3.5) %>% 
  ggplot(aes(x = x)) +
  geom_freqpoly(binwidth = 0.05) +
  scale_x_continuous(breaks = seq(0,10,0.25))

# y - width
diamonds.big %>% 
  ggplot(aes(x = y)) +
  geom_freqpoly(binwidth = 0.05)

# Questions:
#   Similar as with x regarding missing data.
#   Extreme value y over 50 (wrong data value)?

# Lets check wrong data
diamonds.big %>% 
  filter(y < 3.5 | y > 40)

# redraw plot without strange values
diamonds.small %>% 
  filter(y > 3.5 & y < 40) %>% 
  ggplot(aes(x = y)) +
  geom_freqpoly(binwidth = 0.05) +
  scale_x_continuous(breaks = seq(0,10,0.25))

# z - depth
diamonds.small %>% 
  ggplot(aes(x = z)) +
  geom_freqpoly(binwidth = 0.05)

diamonds.small %>% 
  filter(z > 2) %>% 
  ggplot(aes(x = z)) +
  geom_freqpoly(binwidth = 0.05)


# Lets try to make subplots (x,y,z on one plot)
#   install and load package cowplot

fig.x <- diamonds.small %>% 
  filter(x > 3.5) %>% 
  ggplot(aes(x = x)) +
  geom_freqpoly(binwidth = 0.05) +
  scale_x_continuous(breaks = seq(0,10,0.25))

fig.y <- diamonds.small %>% 
  filter(y > 3.5 & y < 40) %>% 
  ggplot(aes(x = y)) +
  geom_freqpoly(binwidth = 0.05) +
  scale_x_continuous(breaks = seq(0,10,0.25))

fig.z <- diamonds.small %>% 
  filter(z > 2) %>% 
  ggplot(aes(x = z)) +
  geom_freqpoly(binwidth = 0.05) +
  scale_x_continuous(breaks = seq(0,10,0.25))


fig.subplot <- cowplot::plot_grid(fig.x, fig.y, fig.z, ncol = 1, labels = c("X", "Y", "Z"))


# final diamond frequency plot - subplots (for exporting)
fig.x <- diamonds.big %>% 
  filter(x > 3.5) %>% 
  ggplot(aes(x = x)) +
  geom_freqpoly(binwidth = 0.05, size = 1.2, color = "black") +
  scale_x_continuous(breaks = seq(0,10,0.5), limits = c(0,10)) +
  xlab("") +
  ylab("Count") +
  ggtitle("Diamond dimensions (in mm) - frequency plots") +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 20, face = "bold"))

fig.y <- diamonds.big %>% 
  filter(y > 3.5 & y < 15) %>% 
  ggplot(aes(x = y)) +
  geom_freqpoly(binwidth = 0.05, size = 1.2, color = "gray40") +
  scale_x_continuous(breaks = seq(0,10,0.5), limits = c(0,10)) +
  xlab("") +
  ylab("Count") +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16))

fig.z <- diamonds.big %>% 
  filter(z > 2 & z < 15) %>% 
  ggplot(aes(x = z)) +
  geom_freqpoly(binwidth = 0.05, size = 1.2, color = "gray60") +
  scale_x_continuous(breaks = seq(0,10,0.5), limits = c(0,10)) +
  xlab("") +
  ylab("Count") +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16))

fig.subplot <- cowplot::plot_grid(fig.x, fig.y, fig.z, ncol = 1, labels = c("X", "Y", "Z"), label_size = 25)

ggsave(filename = "./figure/03_freqpoly_subplot_xyz_diamonds.png", plot = fig.subplot,
       units = "cm", width = 29.7, height = 21, dpi = 600) 


# Lets try to draw all dimensions without subplots
#   some data wrangling required
#   from wide format x,y,z three columns we need long format x,y,z in name-value column

diamonds.small.xyz.long <- diamonds.small %>% 
  select(x,y,z) %>% 
  tidyr::pivot_longer(c("x", "y", "z"), 
                      names_to = "dimension", values_to = "value") %>% 
  mutate(dimension = as.factor(dimension))

diamonds.big.xyz.long <- diamonds.big %>% 
  select(x,y,z) %>% 
  tidyr::pivot_longer(c("x", "y", "z"), 
                      names_to = "dimension", values_to = "value") %>% 
  mutate(dimension = as.factor(dimension))

# Frequency polygon for small dataset
diamonds.small.xyz.long %>% 
  ggplot(aes(x = value, color = dimension)) +
  geom_freqpoly(size = 1.2, binwidth = 0.05)


# final diamond frequency plot - all dimensions on one plot (for exporting)
diamonds.big.xyz.long %>% 
  filter(value >= 2 & value <= 15) %>% 
  ggplot(aes(x = value, color = dimension)) +
  geom_freqpoly(size = 1.2, binwidth = 0.05) +
  scale_x_continuous(breaks = seq(0,10,0.5), limits = c(0,10)) +
  scale_y_continuous(breaks = seq(0,5000,500)) +
  xlab("Value in mm") +
  ylab("Frequency") +
  ggtitle("Diamond dimensions (in mm) - frequency plots") +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20))

ggsave(filename = "./figure/03_freqpoly_xyz_diamonds.png", plot = last_plot(),
       units = "cm", width = 29.7, height = 21, dpi = 600) 



# Area plot
# (explore continuous variables)

# diamond depth
summary(select(diamonds.big, depth))

# basic plot
diamonds.small %>% 
  ggplot(aes(x = depth)) +
  geom_area(stat = "bin")
  
#  changing statistics to compute area plot

# compute bins
diamonds.small %>% 
  ggplot(aes(x = depth)) +
  geom_area(stat = "bin", binwidth = 0.02)

# compute density
diamonds.small %>% 
  ggplot(aes(x = depth)) +
  geom_area(stat = "density", kernel = "gaussian")

# possible kernels
help("density")


# Questions:
#   Why there are some gaps in the diamond depth value?
#   Why does depth distribution resemble Gaussian curve?
#   Does this stands for whole dataset?
#   Estimate mean value and standard deviation!
#   Generate Gaussian cutve from estimated values and draw it on the same graph!


# Lets check depth on the whole dataset
diamonds.big %>% 
  ggplot(aes(x = depth)) +
  geom_area(stat = "density", kernel = "gaussian")

# Compute Gaussian curve
depth.mu.sd <- diamonds.big %>% 
  summarise(d.mean = mean(depth),
            d.sd = sd(depth))

depth.gauss.curve <- data.frame(x = seq(40,80,0.02)) %>% 
  mutate(y = dnorm(x = x, mean = depth.mu.sd$d.mean, sd = depth.mu.sd$d.sd))

# Draw both curves on the same plot 
diamonds.big %>% 
  ggplot(aes(x = depth)) +
  geom_area(stat = "density", 
            kernel = "gaussian", 
            color = "deepskyblue1", 
            fill = "deepskyblue1",
            size = 1.1) +
  geom_area(data = depth.gauss.curve, 
            aes(x = x, y = y), 
            fill = "black",
            alpha = 1/2,
            size = 1.2)
  

# diamond table
summary(select(diamonds.big, table))

# basic plot
diamonds.small %>% 
  ggplot(aes(x = table)) +
  geom_area(stat = "bin", bins = 45)

# Questions:
#   Similar questions arise as with diamond depth.


# final diamond depth and table area plot - subplots (for exporting)
fig.depth <- diamonds.big %>% 
  ggplot(aes(x = depth)) +
  geom_area(stat = "bin", binwidth = 0.1, color = "black", fill = "gray") +
  scale_x_continuous(breaks = seq(20, 100, 5)) +
  scale_y_continuous(breaks = seq(0,3000,250)) +
  xlab("Depth") +
  ylab("Frequency") +
  ggtitle("Diamond depth - area plot") +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 20, face = "bold"))

fig.table <- diamonds.big %>% 
  ggplot(aes(x = table)) +
  geom_area(stat = "bin", binwidth = 1, color = "black", fill = "gray") +
  scale_x_continuous(breaks = seq(20, 100, 5)) +
  scale_y_continuous(breaks = seq(0,10000,1000)) +
  xlab("Table") +
  ylab("Frequency") +
  ggtitle("Diamond table - area plot") +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 20, face = "bold"))

fig.subplot <- cowplot::plot_grid(fig.depth, fig.table, ncol = 1)

ggsave(filename = "./figure/03_areaplot_subplot_depth_table_diamonds.png", plot = fig.subplot,
       units = "cm", width = 29.7, height = 21, dpi = 600) 



# Bar plot
# (explore discrete variables)

# cut (diamond cut)

# Questions:
#   How many different diamond cuts are in the dataset?
#   Which diamond cut is the most frequent?
#   Which diamond cut is the least frequent?

diamonds.small %>% 
  ggplot(aes(x = cut)) +
  geom_bar(stat = "count")

# Whole dataset
diamonds.big %>% 
  ggplot(aes(x = cut)) +
  geom_bar(stat = "count")


# color (diamond color)

# Questions:
#   How many different diamond colors are in the dataset?
#   Which diamond color is the most frequent?
#   Which diamond color is the least frequent?

# We will calculate our own count statistics table for diamond color:
diamonds.color.count <- diamonds.big %>% 
  group_by(color) %>% 
  count()

diamonds.color.count %>% 
  ggplot(aes(x = color, y = n)) +
  geom_bar(stat = "identity")


# clarity (diamond clarity)

# Questions:
#   How many different diamond clarity values are in the dataset?
#   Which diamond clarity value is the most frequent?
#   Which diamond clarity value is the least frequent?

diamonds.clarity.count <- diamonds.big %>% 
  group_by(clarity) %>% 
  count()

# Add annotations above columns
diamonds.clarity.count %>% 
  ggplot(aes(x = clarity, y = n)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n, y = n + 250))


# final diamond factor variables bar plot - subplots (for exporting)
fig.cut <- diamonds.big %>% 
  ggplot(aes(x = cut)) +
  geom_bar(stat = "count", fill = "gray60", color = "black") +
  scale_y_continuous(breaks = seq(0,25000,5000)) +
  xlab("Diamond cut") +
  ylab("Frequency") +
  ggtitle("Diamond cut / color / clarity - bar plot") +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 20, face = "bold"))

fig.color <- diamonds.big %>% 
  ggplot(aes(x = color)) +
  geom_bar(stat = "count", fill = "gray60", color = "black") +
  scale_y_continuous(breaks = seq(0,15000,2500)) +
  xlab("Diamond color") +
  ylab("Frequency") +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 20, face = "bold"))

fig.clarity <- diamonds.big %>% 
  ggplot(aes(x = clarity)) +
  geom_bar(stat = "count", fill = "gray60", color = "black") +
  scale_y_continuous(breaks = seq(0,15000,2500)) +
  xlab("Diamond clarity") +
  ylab("Frequency") +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 20, face = "bold"))

fig.subplot <- cowplot::plot_grid(fig.cut, fig.color, fig.clarity, ncol = 1)

ggsave(filename = "./figure/03_barplot_subplot_ccc_diamonds.png", plot = fig.subplot,
       units = "cm", width = 29.7, height = 21, dpi = 600) 

