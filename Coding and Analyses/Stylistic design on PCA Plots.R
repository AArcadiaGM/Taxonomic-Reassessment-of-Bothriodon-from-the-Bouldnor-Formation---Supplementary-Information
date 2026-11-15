#Before running this code, run the code in "Triplicate Bouldnor Test" up to this line:
#ids <- dimnames(gpa$coords)[[3]]

#Assigning Colours to Each Dataset (only use one line of code, depending on if its the anthracothere or artiodactyl analysis)
group_cols <- c(
  "Bouldnor" = "#af8dc3",
  "International" = "#7fbf7b",
  "Artiodactyl" = "#fdb863"
)

group_cols <- c(
  "Bouldnor" = "#af8dc3",
  "International" = "#7fbf7b"
)

#Ordering the groups (only use one line of code, depending on if its the anthracothere or artiodactyl analysis)
group <- factor(
  group,
  levels = c("Bouldnor", "International", "Artiodactyl")
)

group <- factor(
  group,
  levels = c("Bouldnor", "International")
)

#Creating Blank PCA
plot(
  pca$x[,1], pca$x[,2],
  type = "n",
  xlab = "PC1 (43.25%)",
  ylab = "PC2 (14.24%)",
  main = "PC1 vs PC2",
  font.lab = 2
)

#Creating the group boundaries
for (g in levels(group)) {
  
  group_indices <- which(group == g)
  
  group_x <- pca$x[group_indices, 1]
  group_y <- pca$x[group_indices, 2]
  
  hull <- chull(group_x, group_y)
  
  polygon(
    group_x[hull],
    group_y[hull],
    col = adjustcolor(group_cols[g], alpha.f = 0.35),
    border = group_cols[g],
    lwd = 1.5
  )
}

#Add PCA Specimens
points(
  pca$x[,1], pca$x[,2],
  col = group_cols[as.character(group)],
  pch = 19,
  cex = 1
)

#Labelling the Speciemns
text(
  pca$x[,1], pca$x[,2],
  labels = ids,
  pos = 3,
  cex = 0.8
)

#Adding the background grid
abline(v = 0, lty = 2, lwd = 1)
abline(h = 0, lty = 2, lwd = 1)

grid(
  nx = NULL,
  ny = NULL,
  col = adjustcolor("grey20", alpha.f = 0.3),
  lty = 3,
  lwd = 0.7
)

#Increasing graph border size
box(
  which = "plot",
  lwd = 2
)

#Legend (For the analysis using all datasets)
legend(
  "bottomright",
  legend = c(
    "Bouldnor Dataset",
    "Global Anthracothere Dataset",
    "Global Artiodactyl Dataset"
  ),
  col = c("#af8dc3", "#7fbf7b", "#fdb863"),
  pch = 19,
  pt.cex = 1.2,
  cex = 1.3,
  y.intersp = 0.5,
  x.intersp = 0.4,
  inset = 0.01,
  bty = "o",
  box.lwd = 1,
  bg = "white",
  text.width = strwidth("Bouldnor Dataset", cex = 1.15)
)


#Legend (For Anthracothere Datasets Only)
legend(
  "topleft",
  legend = c(
    "Bouldnor Dataset",
    "Global Anthracothere Dataset"
  ),
  col = c("#af8dc3", "#7fbf7b"),
  pch = 19,
  pt.cex = 1.2,
  cex = 1.3,
  y.intersp = 0.5,
  x.intersp = 0.4,
  inset = 0.01,
  bty = "o",
  box.lwd = 1,
  bg = "white",
  text.width = strwidth("Bouldnor Dataset", cex = 1.15)
)
