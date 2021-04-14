# data Layer

rm(list = ls())
graphics.off()

install.packages("ggplot2")
install.packages("dplyr")

library(ggplot2)
library(dplyr)


#session info

sessionInfo()

#data frame for the plot

df=ggplot2::mpg
help(mpg)
View(df)

#create new variable transmission from trans

df<- df %>% 
  mutate(transmission = substr(trans, 1,1)) %>% 
  mutate(transmission= case_when(transmission == "a" ~ "automatic trans.",
                                 transmission=="m" ~ "manual trans."))

#create a new variable type_of_drive from drv

df<- df %>% 
  mutate(type_of_drive = case_when(drv == "f" ~ "front-wheel",
                                   drv == "r" ~ "rear-wheel",
                                   drv == "4" ~ "4-wheel drive"))
# Convert to factor variable
df<- df %>% 
  mutate(type_of_drive = factor(type_of_drive, levels = c("front-wheel","rear-wheel", "4-wheel drive")),
         transmission = factor(transmission, levels = c("automatic trans.", "manual trans.")))
View(df)
# start building a plot with data layer 
ggplot(data = df)
ggplot(df)
df %>% ggplot()

# add aesthetic mapping
# we map 
# - Var disp -> x axis
# - var hwy -> y axis
ggplot(data= df, mapping = aes(x= displ, y= hwy))
ggplot(df, aes(x= displ, y= hwy))

#Add Geometry
# Scatter plot 
# - Rendering object as points 
# - Point size, Point transparency
ggplot(df, aes(x= displ, y= hwy))+
  geom_point(size=5)
ggplot(df, aes(x= displ, y= hwy))+
  geom_jitter(size=5, alpha= 1/3)

ggplot(df, aes(x= displ, y= hwy))+
  geom_jitter(size=5, alpha= 1/4)

ggplot(df, aes(x= displ, y= hwy))+
  geom_point(size=5, alpha= 1/3)

# Add Facet 
# Splitting Original plots into facets
#- Facet Grid
# - Row Split : Transmission
# - Column Split : Type of drive
ggplot(df, aes(x= displ, y= hwy))+
  geom_point(size=5, alpha= 1/3)+
  facet_grid(transmission ~ type_of_drive, scales = "free")

ggplot(df, aes(x= displ, y= hwy))+
  geom_point(size=5, alpha= 1/3)+
  facet_grid(transmission ~ type_of_drive, scales = "free")

# add statistics
# fit a linear regression model (regression line) for each facet (subplot)
ggplot(df, aes(x= displ, y= hwy))+
  geom_point(size=5, alpha= 1/3)+
  facet_grid(transmission ~ type_of_drive, scales = "free")+
  geom_smooth(method = "lm")

# Add Cordinate Layer and scales

ggplot(df, aes(x= displ, y= hwy))+
  geom_point(size=5, alpha= 1/3)+
  facet_grid(transmission ~ type_of_drive, scales = "free")+
  geom_smooth(method = 'lm')+
  coord_cartesian()+
  scale_x_continuous(breaks = seq(0,10,.5))+
  scale_y_continuous(breaks = seq(0,50,5))+
  xlab("Engine displacement(Volume in litre)")+
  ylab("Highway miles per Gallon(MPG)")+
  ggtitle("Car Fuel Consumption")

# Add default theme layer 
ggplot(df, aes(x= displ, y= hwy))+
  geom_point(size=5, alpha= 1/3)+
  facet_grid(transmission ~ type_of_drive, scales = "free")+
  geom_smooth(method = 'lm')+
  coord_cartesian()+
  scale_x_continuous(breaks = seq(0,10,.5))+
  scale_y_continuous(breaks = seq(0,50,5))+
  xlab("Engine displacement(Volume in litre)")+
  ylab("Highway miles per Gallon(MPG)")+
  ggtitle("Car Fuel Consumption")+
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        axis.title.y = element_text(margin = margin(t=0, r=20, b=0, l= 0)),
        axis.title.x = element_text(margin = margin(t=20, r=0, b=0, l= 0)),
        plot.title = element_text(size = 30, face = "bold", hjust = 0.5,
                                  margin = margin(t=0, r=0, b=20, l= 0)),
        strip.background = element_rect(colour = "black", fill = "white"),
        strip.text = element_text(face = "bold", size = 16))


# Export the plot
figure<- ggplot(df, aes(x= displ, y= hwy))+
  geom_point(size=5, alpha= 1/3)+
  facet_grid(transmission ~ type_of_drive, scales = "free")+
  geom_smooth(method = 'lm')+
  coord_cartesian()+
  scale_x_continuous(breaks = seq(0,10,.5))+
  scale_y_continuous(breaks = seq(0,50,5))+
  xlab("Engine displacement(Volume in litre)")+
  ylab("Highway miles per Gallon(MPG)")+
  ggtitle("Car Fuel Consumption")+
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        axis.title.y = element_text(margin = margin(t=0, r=20, b=0, l= 0)),
        axis.title.x = element_text(margin = margin(t=20, r=0, b=0, l= 0)),
        plot.title = element_text(size = 30, face = "bold", hjust = 0.5,
                                  margin = margin(t=0, r=0, b=20, l= 0)),
        strip.background = element_rect(colour = "black", fill = "white"),
        strip.text = element_text(face = "bold", size = 16))

class(figure)
typeof(figure)
graphics.off()
print(figure)

#save plot to a figure
ggsave(filename = "C:/Users/Rohan Bopanna/Desktop.png", plot = figure,
       units = "cm", width = 29.7, height = 21, dpi = 600)

