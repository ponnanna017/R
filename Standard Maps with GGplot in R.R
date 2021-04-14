# ggplot2 for stamdard plots and beyond

rm(list = ls())
graphics.off()

# Libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)
library(ggwordcloud)
library(ggiraphExtra)
library(datasets)
library(ggmosaic)


# pie chart

# create some data
df <- data.frame(color = c("Red", "Green", "Blue"), value = c(30,55,15))
df <- df %>% mutate(color = factor(color, levels = c("Red", "Green", "Blue")))
print(df)

# first let's create barplot
df %>% 
  ggplot(aes(x = "", y = value, fill = color)) +
  geom_bar(width = 1, stat = "identity")

# for pie chart use barplot with polar coordinates
df %>% 
  ggplot(aes(x = "", y = value, fill = color)) +
  geom_bar(width = 1, stat = "identity", color = "black") +
  coord_polar(theta = "y", start = 0)

# create pie chart from factor variable
mpg %>% 
  ggplot(aes(x = "", fill = drv)) +
  geom_bar(width = 1) +
  coord_polar(theta = "y", start = 0)

# final pie chart for export
mpg %>% 
  ggplot(aes(x = "", fill = drv)) +
  geom_bar(width = 1, color = "black") +
  coord_polar(theta = "y", start = 0) +
  scale_fill_grey() +
  ggtitle("Type of drive - pie chart") +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 25, face = "bold", hjust = 0.5),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 20))

ggsave(filename = "./figure/07_piechart_drv_cars.png", 
       plot = last_plot(),
       units = "cm", width = 29.7, height = 21, dpi = 600) 


# donut chart

# data
data <- data.frame(class = c("A", "B", "C"),
                   size = c(20, 50, 30))
data
# calculate percentage and borders
data <- data %>% 
  mutate(fraction = size / sum(size),
         ymax = cumsum(fraction),
         ymin = ymax - fraction)

# create donut chart
data %>% 
  ggplot(aes(ymax = ymax, 
             ymin = ymin, 
             xmax = 3, 
             xmin = 2, 
             fill = class)) +
  geom_rect() +
  coord_polar(theta = "y") +
  xlim(c(1,3))


# a little bit of customization
data <- data %>% # compute labels and labels position
  mutate(label_position = (ymin + ymax) / 2,
         label = paste0(class, "\n size: ", size))
  
data %>% 
  ggplot(aes(ymax = ymax, 
             ymin = ymin, 
             xmax = 3, 
             xmin = 2, 
             fill = class)) +
  geom_rect(color = "black") +
  geom_label(x = 2.5, aes(y = label_position, label = label), size = 6) +
  coord_polar(theta = "y") +
  scale_fill_viridis_d(option = "viridis") +
  xlim(c(1,3)) +
  theme_void() +
  theme(legend.position = "none")
  

# final donut chart for export
data %>% 
  ggplot(aes(ymax = ymax, 
             ymin = ymin, 
             xmax = 3, 
             xmin = 2, 
             fill = class)) +
  geom_rect(color = "black") +
  geom_text(x = 1.2, aes(y = label_position, label = label, color = class), size = 6) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette = 3) +
  scale_color_brewer(palette = 3) +
  ggtitle("Classes count - donut chart") +
  xlim(c(-0.5,3)) +
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_text(size = 25, face = "bold", hjust = 0.5))

ggsave(filename = "./figure/07_donut_classes.png", 
       plot = last_plot(),
       units = "cm", width = 29.7, height = 21, dpi = 600) 


# time series visualization (line chart)

# data (ggplot2 dataset)
df <- ggplot2::economics

# visualize unemployment
df %>% 
  ggplot(aes(x = date, y = unemploy)) +
  geom_line()

# change labels for date
df %>% 
  ggplot(aes(x = date, y = unemploy)) +
  geom_line() +
  # scale_x_date(date_breaks = "year")
  # scale_x_date(date_breaks = "5 years")
  # scale_x_date(date_breaks = "10 years")
  # scale_x_date(date_breaks = "month")
  # scale_x_date(date_breaks = "10 years", date_labels = "%Y") # show only year
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") # show only year

# different line styles
df %>% 
  ggplot(aes(x = date, y = unemploy)) +
  # geom_line(linetype = "dashed") 
  # geom_line(linetype = "dotdash")
  # geom_line(linetype = "longdash")
  geom_line(size = 2, color = "red")


# multiple lines

# long format data
df.long <- df %>% 
  select(-pce, -pop) %>% 
  tidyr::pivot_longer(cols = colnames(df)[4:ncol(df)], 
                      names_to = "indicator", 
                      values_to = "value")  

df.long %>% 
  ggplot(aes(x = date, y = value, 
             group = indicator, color = indicator)) +
  geom_line() +
  scale_y_log10()

# final line plot for export
df.long %>% 
  ggplot(aes(x = date, y = value, 
             group = indicator, color = indicator)) +
  geom_line(size = 1.2) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  scale_y_log10() +
  xlab("Month") +
  ylab("Value") +
  ggtitle("Economic indicators - line plot") +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 25, face = "bold"),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 16),
        panel.border = element_rect(color = "black", fill = NA, size = 1.5))

ggsave(filename = "./figure/07_lineplot_economics.png", 
       plot = last_plot(),
       units = "cm", width = 29.7, height = 21, dpi = 600) 



# word cloud

# library: ggwordcloud  (have to install and load)

# create word cloud for car manufacturers from the cars dataset
data <- mpg %>% 
  group_by(manufacturer) %>% 
  count()

# create word cloud
set.seed(135) #randomness in positioning labels in the cloud

data %>% 
  ggplot(aes(label = manufacturer)) +
  geom_text_wordcloud() +
  theme_minimal()


# add text size - number of cars
data %>% 
  ggplot(aes(label = manufacturer, size = n)) +
  geom_text_wordcloud() +
  theme_minimal()

# determine max text size 
data %>% 
  ggplot(aes(label = manufacturer, size = n)) +
  geom_text_wordcloud() +
  scale_size_area(max_size = 20) +
  theme_minimal()

# text area
data %>% 
  ggplot(aes(label = manufacturer, size = n)) +
  #geom_text_wordcloud(area_corr = T) +
  geom_text_wordcloud(area_corr_power = 1) +
  scale_size_area(max_size = 25) +
  theme_minimal()

# rotate words
data <- data %>% 
  mutate(angle = 90 * sample(c(0,1), n(), replace = T, prob = c(0.7, 0.3))) %>% 
  mutate(angle1 = 45 * sample(c(-2:2), n(), replace = T, prob = c(1,1,4,1,1)))

data %>% 
  # ggplot(aes(label = manufacturer, size = n, angle = angle)) +
  ggplot(aes(label = manufacturer, size = n, angle = angle1)) +
  geom_text_wordcloud() +
  scale_size_area(max_size = 20) +
  theme_minimal()


# final word cloud for export
data %>% 
  ggplot(aes(label = manufacturer, 
             size = n, 
             angle = angle1,
             color = manufacturer)) +
  geom_text_wordcloud() +
  scale_size_area(max_size = 30) +
  scale_color_viridis_d(option = "magma") +
  theme_minimal()

ggsave(filename = "./figure/07_wordcloud_cars.png", 
       plot = last_plot(),
       units = "cm", width = 29.7, height = 21, dpi = 600) 


# waterfall chart

# data - economics: increase/decrease in unemployment
df <- ggplot2::economics %>% 
  select(date, unemploy)

# calculate differences between two successive months 
df <- df %>% 
  mutate(unemploy_lag = lag(unemploy, 1), # unemployment rate previous month
         `unemploy diff` = unemploy - unemploy_lag, # difference in employment between two months
  `diff type` = case_when(`unemploy diff` > 0 ~ "increase", # type of difference
                          `unemploy diff` < 0 ~ "decrease",
                          `unemploy diff` == 0 ~ "no change")) %>% 
  filter(!is.na(unemploy_lag)) # remove first month


# draw waterfall chart
df %>% 
  filter(date < "1970-01-01") %>% 
  ggplot(aes(x = date, fill = `diff type`)) +
  geom_rect(aes(x = date, 
                xmin = date - 10, xmax = date + 10, 
                ymin = unemploy_lag, ymax = unemploy))


# final waterfall chart for export
df %>% 
  filter(date >= "1980-01-01" & date <= "2000-01-01") %>% 
  ggplot(aes(x = date, fill = `diff type`)) +
  geom_rect(aes(x = date, 
                xmin = date - 15, xmax = date + 15, 
                ymin = unemploy_lag, ymax = unemploy)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_fill_manual(values = c("limegreen", "brown1", "gray")) +
  xlab("Month") +
  ylab("Number of unemployed in 1000") +
  ggtitle("Unemployement in the USA - waterfall chart") +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 25, face = "bold"),
        legend.position = "none",
        panel.border = element_rect(color = "black", fill = NA, size = 1))


ggsave(filename = "./figure/07_waterfall_chart_unemploymenet.png", 
       plot = last_plot(),
       units = "cm", width = 29.7, height = 21, dpi = 600) 



# radar chart / spider chart / 

# library: ggiraphExtra  (have to install and load)

# iris dataset
help(iris)
View(iris)

ggRadar(data=iris, aes(color=Species))
ggRadar(data=iris,aes(group=Species))

# cars dataset (interactive map)
help(mtcars)
View(mtcars)

df <- mtcars %>% 
  mutate(Transmission = case_when(am == "0" ~ "automatic",
                                  am == "1" ~ "manual"))

ggRadar(data = df, 
        aes(colour = Transmission, facet = cyl), 
        interactive=TRUE)

ggsave(filename = "./figure/07_radar_chart_mtcars.png", 
       plot = last_plot(),
       units = "cm", width = 29.7, height = 21, dpi = 600) 



# parallel coordinates plot

# data (iris)
df <- datasets::iris

# data wrangling (wide to long format, add flower id)
df.long <- df %>% 
  mutate(`Flower id` = row_number()) %>% # add unique flower id
  tidyr::pivot_longer(cols = colnames(df)[1:4], names_to = "Measure", values_to = "Value") %>% #to wide
  select(`Flower id`, Species, Measure, Value)

# create first parallel plot (used absolute measure values)
df.long %>% 
  ggplot(aes(x = Measure, 
             y = Value, 
             group = `Flower id`, 
             color = Species)) +
  geom_line(size = 0.65) +
  theme_minimal()

# data wrangling (create relative measure for each Value across given flowers)
df.long <- df.long %>% 
  group_by(Measure) %>% # calculate max value for each measure across all species
  mutate(`max value` = max(Value)) %>% # max value calculation
  mutate(`Relative value` = Value / `max value`) %>%  # relative value as value divided by max value (all flowers)
  ungroup()

# create second parallel plot (used relative measure values)
df.long %>% 
  ggplot(aes(x = Measure, 
             y = `Relative value`, 
             group = `Flower id`, 
             color = Species)) +
  geom_line(size = 0.65) +
  theme_minimal()

# reorder measure values
df.long <- df.long %>% 
  mutate(Measure = factor(Measure, levels = c("Sepal.Length", "Petal.Length", "Petal.Width", "Sepal.Width")))

# replot with relative measure values
df.long %>% 
  ggplot(aes(x = Measure, 
             y = `Relative value`, 
             group = `Flower id`, 
             color = Species)) +
  geom_line(size = 0.65) +
  theme_minimal()


# calculate modified relative measures (every measure value must start at zero - bottom of y axis)
df.long <- df.long %>% 
  group_by(Measure) %>% # for each measure
  mutate(`min value` = min(Value), # calculate min value for each measure across all species
         `Value modif` = Value - `min value`, # modified value (decresed value for minimal value)
         `max value modif` = max(`Value modif`), # calculate max modified value for each measure across all species
         `Relative value modif` = `Value modif` / `max value modif`) %>%  # modified relative value 
           ungroup()

# create third parallel plot (used modified relative measure values)
df.long %>% 
  ggplot(aes(x = Measure, 
             y = `Relative value modif`, 
             group = `Flower id`, 
             color = Species)) +
  geom_line(size = 0.65) +
  theme_minimal()


# highlighting one species of flowers

# highlight setosa
df.long %>% 
  ggplot(aes(x = Measure, 
             y = `Relative value modif`, 
             group = `Flower id`, 
             color = Species)) +
  geom_line(size = 0.7, 
            color = case_when(df.long$Species == "setosa" ~ "red",
                              T ~ "gray")) +
  theme_minimal()


# final parallel coordinates plot for export
df.long %>% 
  ggplot(aes(x = Measure, 
             y = `Relative value modif`, 
             group = `Flower id`, 
             color = Species)) +
  geom_line(size = 0.7) +
  xlab("") +
  ylab("") +
  ggtitle("Iris dataset - parallel coordinates plot") +
  theme_minimal() +
  theme(axis.text = element_text(size = 16),
        axis.text.y = element_blank(),
        plot.title = element_text(size = 20, face = "bold"),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18),
        panel.border = element_rect(color = "black", fill = NA, size = 1))

ggsave(filename = "./figure/07_parallelplot_iris.png", 
       plot = last_plot(),
       units = "cm", width = 29.7, height = 21, dpi = 600) 



# heat map

# data cars
df <- mpg

# add new variable for car manufacturer and car model
# randomly select 30 cars (just for demonstration)
set.seed(159)
df.wide <- df %>% 
  mutate(car = paste(manufacturer, "-", model)) %>% 
  sample_n(30)

# select columns for heat map and calculate scaled values
df.long <- df.wide %>% 
  select(car, displ, cyl, cty, hwy) %>% # selected columns 
  tidyr::pivot_longer(-car, names_to = "key", values_to = "val") %>% # to long format
  group_by(key) %>% # for every key - measure
  mutate(`max val` = max(val)) %>% # calculate max value for every measure
  ungroup() %>% 
  mutate(`val scaled` = val / `max val`) # scaled value - value / max value

# heat map
df.long %>% 
  ggplot(aes(x = key, 
             y = car, 
             fill = `val scaled`)) +
  geom_tile()

# final heat map for export
df.long %>% 
  rename(`Scaled value` = `val scaled`) %>% 
  mutate(key = factor(key, levels = c("cyl", "displ", "hwy", "cty"))) %>% 
  ggplot(aes(x = key, 
             y = car, 
             fill = `Scaled value`)) +
  geom_tile() +
  scale_fill_viridis_c(option = "magma") +
  xlab("Measure") +
  ylab("Car") +
  ggtitle("Cars - heat map") +
  theme_minimal() +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 18),
        plot.title = element_text(size = 20, face = "bold"),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18),
        panel.border = element_rect(color = "black", fill = NA, size = 1))

ggsave(filename = "./figure/07_heatmap_cars.png", 
       plot = last_plot(),
       units = "cm", width = 29.7, height = 21, dpi = 600) 



# Mosaic plot / Marimekko plot

# library: ggmosaic  (have to install and load)

# Titanic dataset
df <- datasets::Titanic
str(df)
df

# convert to data frame and add factor variable
df <- as.data.frame(df) %>% 
  mutate(Survived = factor(Survived, levels = c("Yes", "No")))

# first mosaic plot - break down by Class
df %>% 
ggplot() +
  geom_mosaic(aes(weight = Freq, 
                  x = product(Class), 
                  fill = Survived))

# mosaic plot - break down by Class, Age
df %>% 
  ggplot() +
  geom_mosaic(aes(weight = Freq, 
                  x = product(Class, Age), 
                  fill = Survived))

# mosaic plot - break down by Class, Age, Sex
df %>% 
  ggplot() +
  geom_mosaic(aes(weight = Freq, 
                  x = product(Class, Age, Sex), 
                  fill = Survived))

# rotate x labels
df %>% 
  ggplot() +
  geom_mosaic(aes(weight = Freq, 
                  x = product(Class, Age, Sex), 
                  fill = Survived)) +
  theme(axis.text.x = element_text(angle = 90))

# final mosaic plot for export
df %>% 
  ggplot() +
  geom_mosaic(aes(weight = Freq, 
                  x = product(Class, Age, Sex), 
                  fill = Survived)) +
  scale_fill_manual(values = c("forestgreen", "brown1")) +
  xlab("") +
  ylab("") +
  ggtitle("Titanic survived - mosaic plot") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, size = 14),
        axis.text.y = element_text(size = 14),
        title = element_text(size = 20, face = "bold"),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16))

ggsave(filename = "./figure/07_mosaic_plot_titanic.png", 
       plot = last_plot(),
       units = "cm", width = 29.7, height = 21, dpi = 600) 



# The Coronavirus dataset
#   data about virus spread over each country
#   reported daily count:
#     - Confirmed (infected people)
#     - Deaths (killed people)
#     - Recovered (survived & healed people)

# source url: https://data.humdata.org/dataset/novel-coronavirus-2019-ncov-cases
# download dataset from course sources or from the given url!!!

# data import
infected <- read.csv(file = "./data/time_series-ncov-Confirmed.csv", 
                     colClasses = "character")
killed <- read.csv(file = "./data/time_series-ncov-Deaths.csv", 
                   colClasses = "character")
recovered <- read.csv(file = "./data/time_series-ncov-Recovered.csv", 
                      colClasses = "character")

# data cleaning

# remove first row (some meta data)
infected <- infected[-1,]
killed <- killed[-1,]
recovered <- recovered[-1,]

# convert & rename columns
infected <- infected %>% 
  mutate(Lat = as.numeric(Lat),
         Long = as.numeric(Long),
         Date = as.Date(Date),
         Value = as.numeric(Value)) %>% 
  rename(Infected = Value) %>% 
  arrange(Country.Region, Date)

killed <- killed %>% 
  mutate(Lat = as.numeric(Lat),
         Long = as.numeric(Long),
         Date = as.Date(Date),
         Value = as.numeric(Value)) %>% 
  rename(Killed = Value) %>% 
  arrange(Country.Region, Date)

recovered <- recovered %>% 
  mutate(Lat = as.numeric(Lat),
         Long = as.numeric(Long),
         Date = as.Date(Date),
         Value = as.numeric(Value)) %>% 
  rename(Recovered = Value) %>% 
  arrange(Country.Region, Date)

# create one df
infected_ <- infected %>% 
  rename(Count = Infected) %>% 
  mutate(Type = "infected")
killed_ <- killed %>% 
  rename(Count = Killed) %>% 
  mutate(Type = "killed")
recovered_ <- recovered %>% 
  rename(Count = Recovered) %>% 
  mutate(Type = "recovered")

corona.all <- rbind(infected_, killed_, recovered_)
rm("infected_", "killed_", "recovered_")


# Create maps with ggplot2

# library: maps  (have to install and load)
install.packages("maps")
library(maps)

# Map of Coronavirus spread
last.date <- infected %>% summarise(last.date = max(Date)) %>% pull() # get last date

# Map infected for each country on the last date (absolute count)
infected %>% 
  filter(Date == last.date & Infected > 0) %>%  # interested only for the last date and countries with at least one infected person
  ggplot(aes(x = Long,
             y = Lat,
             size = Infected)) +
  geom_point() +
  borders() +
  coord_quickmap()

# add point size based on count
infected %>% 
  filter(Date == last.date & Infected > 0) %>%  
  ggplot(aes(x = Long,
             y = Lat,
             size = Infected)) +
  geom_point(color = "red", alpha = 1/3) +
  borders() +
  scale_size_area(max_size = 40) +
  xlab("") +
  ylab("") +
  coord_quickmap() +
  theme_bw()

# create map of infected, killed and recovered as subplot

# for proportions size
infected.max <- max(infected$Infected)
recovered.max <- max(recovered$Recovered)
killed.max <- max(killed$Killed)

infected.ratio <- infected.max / max(infected.max, recovered.max, killed.max) 
recovered.ratio <- recovered.max / max(infected.max, recovered.max, killed.max)
killed.ratio <- killed.max / max(infected.max, recovered.max, killed.max)

subplot.infected <- infected %>% 
  filter(Date == last.date & Infected > 0) %>% 
  ggplot(aes(x = Long, y = Lat, size = Infected)) +
  borders() +
  geom_point(color = "red", alpha = 1/3) +
  scale_size_area(max_size = 40*infected.ratio) +
  xlab("") +
  ylab("") +
  ggtitle("Coronavirus - Infected") +
  coord_quickmap() +
  theme_bw() +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = "none",
        title = element_text(size = 25))

subplot.recovered <- recovered %>% 
  filter(Date == last.date & Recovered > 0) %>% 
  ggplot(aes(x = Long, y = Lat, size = Recovered)) +
  borders() +
  geom_point(color = "green", alpha = 1/3) +
  scale_size_area(max_size = 40*recovered.ratio) +
  xlab("") +
  ylab("") +
  ggtitle("Coronavirus - Recovered") +
  coord_quickmap() +
  theme_bw() +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = "none",
        title = element_text(size = 25))

subplot.killed <- killed %>% 
  filter(Date == last.date & Killed > 0) %>% 
  ggplot(aes(x = Long, y = Lat, size = Killed)) +
  borders() +
  geom_point(color = "black", alpha = 1/3) +
  scale_size_area(max_size = 40*killed.ratio) +
  xlab("") +
  ylab("") +
  ggtitle("Coronavirus - Killed") +
  coord_quickmap() +
  theme_bw() +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = "none",
        title = element_text(size = 25))

cowplot::plot_grid(subplot.infected, subplot.recovered, subplot.killed)

ggsave(filename = "./figure/07_map_coronavirus1.png", 
       plot = last_plot(),
       units = "cm", width = 29.7, height = 21, dpi = 600) 


# create map of infected, killed and recovered with facets
corona.all %>% 
  filter(Date == last.date & Count > 0) %>% 
  ggplot(aes(x = Long, y = Lat, size = Count, color = Type)) +
  borders() +
  geom_point(alpha = 1/3) +
  facet_grid(rows = vars(Type)) +
  scale_size_area(max_size = 30) +
  xlab("") +
  ylab("") +
  ggtitle("Coronavirus spread") +
  coord_quickmap() +
  theme_bw() +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        title = element_text(size = 25),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(face = "bold", size = 20),
        legend.position = "none")

ggsave(filename = "./figure/07_map_coronavirus2.png", 
       plot = last_plot(),
       units = "cm", width = 29.7, height = 21, dpi = 600) 

# export dataset
save(list = c("corona.all", "infected", "killed", "recovered"), 
     file = "./data/corona.RData")

