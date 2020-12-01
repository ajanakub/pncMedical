# October 27, 2020
# "My Fancy Graphs" - Bukola Ajanaku
#  My project for Ellyn Butler.

library("ggplot2")
library("MASS")
library(extrafont)
library(wesanderson)
library("viridis")

plot1 <- ggplot(mtcars, aes(mpg, disp, color=as.factor(cyl))) + geom_point() +
scale_color_manual(values = wes_palette(n=3, name="GrandBudapest1")) +
ggtitle("MTCars Against # of Forward Gears and Carburetors") + xlab("Miles Per Gallon") + ylab("Displacement") +
  theme(
    plot.title = element_text(color="hotpink4", size=20, face = "bold", hjust = 0.5, family="Georgia"),
    axis.title.x = element_text(color="hotpink3", size= 16, face = "bold.italic", family="Georgia"),
    axis.title.y = element_text(color="hotpink3", size= 16, face = "bold.italic", family="Georgia"),
    panel.background = element_rect(fill = 'gray96', colour = 'black'),
legend.position = "bottom"
) +
labs(col="Cylinder") +
facet_grid(vars(gear), vars(carb))
# Add level labels to facets: "Gear" + gear

plot2 <- ggplot(mtcars, aes(mpg, wt, color=as.factor(cyl))) + geom_point() +
scale_color_viridis(option="viridis", discrete=TRUE) +
ggtitle("MTCars Against Engine Shape and Transmission") + xlab("Miles Per Gallon") + ylab("Weight (lbs)") +
  theme(
    plot.title = element_text(color="deepskyblue4", size=20, face = "bold", hjust = 0.5, family="Georgia"),
    axis.title.x = element_text(color="deepskyblue1", size= 16, face = "bold.italic", family="Georgia"),
    axis.title.y = element_text(color="deepskyblue1", size= 16, face = "bold.italic", family="Georgia"),
    panel.background = element_rect(fill = 'gray96', colour = 'black'),
    legend.position = "bottom"
    ) +
labs(col="Cylinder") +
facet_grid(vs + am ~ gear, margins = TRUE)
# theme_linedraw(); ...minimal; will get a list from Ellyn

pdf(file= "/Users/bukola/Documents/PennBBL/FancyGraphs.pdf",
    width = 8,
    height = 6)
plot1
plot2
dev.off()
