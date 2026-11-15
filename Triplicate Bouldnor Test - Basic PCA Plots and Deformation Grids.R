setwd("C:/Users/alfie/OneDrive/Documents/University/Masters/Semester 4/Dinosaur Isle Bothriodon Specimens/Astragali/Dorsal Views/Dorsal Views Analysis")

library("geomorph")

##########################
#Dat Points Averaged

dat1 <- readland.tps("Dorsal View 2.tps", specID = "ID")
dat2 <- readland.tps("Dorsal View 3.tps", specID = "ID")
dat3 <- readland.tps("Dorsal View 4.tps", specID = "ID")
dat4 <- readland.tps("DVIA 2.tps", specID = "ID")
dat5 <- readland.tps("DVIA 3.tps", specID = "ID")
dat6 <- readland.tps("DVIA 4.tps", specID = "ID")
dat7 <- readland.tps("IA2.1.tps", specID = "ID")
dat8 <- readland.tps("IA2.2.tps", specID = "ID")
dat9 <- readland.tps("IA2.3.tps", specID = "ID")
dat10 <- readland.tps("Artiodactyl Astragalus Dataset.tps", specID = "ID")
dat11 <- readland.tps("Pecora.tps", specID = "ID")

#dat12 <- readland.tps("Artiodactyl Astragalus Dataset EW.tps", specID = "ID")

# Aerage of Bouldnor Datasets
avg123 <- (dat1 + dat2 + dat3) / 3

# Average of International Anthracothere Dataset P1
avg456 <- (dat4 + dat5 + dat6) / 3

# Average of International Anthracothere Dataset P2
avg789 <- (dat7 + dat8 + dat9) / 3

# Combine the averaged datasets
combined <- abind::abind(avg123, avg456, avg789, dat10, dat11, along = 3)
combined <- abind::abind(avg123, avg456, avg789, along = 3)


gpa <- gpagen(combined)

pca <- gm.prcomp(gpa$coords)

group <- c(
  rep("Bouldnor", dim(avg123)[3]),
  rep("International", dim(avg456)[3] + dim(avg789)[3]))

group <- c(
  rep("Bouldnor", dim(avg123)[3]),
  rep("International", dim(avg456)[3] + dim(avg789)[3]),
  rep("Artiodactyl", dim(dat10)[3] + dim(dat11)[3]))


ids <- dimnames(gpa$coords)[[3]]

# Plotting the PCA
plot(
  pca$x[,1], pca$x[,2],
  col = c("#fdb863", "#af8dc3", "#7fbf7b")[factor(group)],
  pch = 19,
  xlab = "PC1",
  ylab = "PC2",
  main = "PC1 vs PC1"
)



text(
  pca$x[,1], pca$x[,2],
  labels = ids,
  pos = 3,
  cex = 0.8
)

abline(v = 0, lty = 2, lwd = 1)
abline(h = 0, lty = 2, lwd = 1)


legend(
  "bottomright",
  legend = c("Bouldnor Dataset", "Global Dataset", "Artiodactyl Dataset"),
  col = c("#af8dc3", "#7fbf7b", "#fdb863"),
  pch = 19
)


summary(pca)










#################
#Deformation Grids
# Mean shape
mean_shape <- mshape(gpa$coords)


links <- matrix(
  c(
    1, 2,
    2, 3,
    3, 4,
    4, 5,
    5, 6,
    6, 7,
    7, 8,
    8, 9,
    9, 10
  ),
  byrow = TRUE,
  ncol = 2
)


#Rotation of Astragali

rotate_90_ccw <- function(shape) {
  cbind(
    -shape[, 2],
    shape[, 1]
  )
}

#Plotting

plot_deformation <- function(
    reference,
    target,
    title,
    links = NULL,
    rotate = FALSE
) {
  

  if (rotate) {
    reference <- rotate_90_ccw(reference)
    target <- rotate_90_ccw(target)
  }
  

  plotRefToTarget(
    reference,
    target,
    method = "TPS",
    mag = 1,
    main = title
  )
  
 
  if (!is.null(links)) {
    segments(
      target[links[, 1], 1],
      target[links[, 1], 2],
      target[links[, 2], 1],
      target[links[, 2], 2],
      lwd = 1.5
    )
  }

  text(
    target[, 1],
    target[, 2],
    labels = 1:nrow(target),
    pos = 3,
    cex = 0.8,
    font = 2
  )
}


###PC1
PC1_min <- pca$shapes$shapes.comp1$min
PC1_max <- pca$shapes$shapes.comp1$max

#PLot the graphs side-by-side
par(mfrow = c(1, 2),
    oma = c(0, 0, 3, 0), 
    mar = c(2, 2, 3, 2))  

#PC1 Min
plot_deformation(
  mean_shape,
  PC1_min,
  "",
  links = links,
  rotate = TRUE
)
mtext("PC1 - Low", side = 3, line = 1, cex = 1.2, font = 2)

#PC1 Max
plot_deformation(
  mean_shape,
  PC1_max,
  "",
  links = links,
  rotate = TRUE
)
mtext("PC1 - High", side = 3, line = 1, cex = 1.2, font = 2)


#Title
mtext("PC1 Shape Deformation", 
      outer = TRUE, 
      cex = 1.5, 
      font = 2)




#############
###PC2
PC2_min <- pca$shapes$shapes.comp2$min
PC2_max <- pca$shapes$shapes.comp2$max

#Aligning the plots
par(mfrow = c(1, 2),
    oma = c(0, 0, 3, 0),  
    mar = c(2, 2, 3, 2))  

#PC2 Min
plot_deformation(
  mean_shape,
  PC2_min,
  "",
  links = links,
  rotate = TRUE
)
mtext("PC2 - Low", side = 3, line = 1, cex = 1.2, font = 2)

#PC2 Max
plot_deformation(
  mean_shape,
  PC2_max,
  "",
  links = links,
  rotate = TRUE
)
mtext("PC2 - High", side = 3, line = 1, cex = 1.2, font = 2)


#Title
mtext("PC2 Shape Deformation", 
      outer = TRUE, 
      cex = 1.5, 
      font = 2)



############
#PC3
PC3_min <- pca$shapes$shapes.comp3$min
PC3_max <- pca$shapes$shapes.comp3$max

#Align plots next to one another
par(mfrow = c(1, 2),
    oma = c(0, 0, 3, 0),  
    mar = c(2, 2, 3, 2))  

#PC1 Min
plot_deformation(
  mean_shape,
  PC3_min,
  "",
  links = links,
  rotate = TRUE
)
mtext("PC3 - Low", side = 3, line = 1, cex = 1.2, font = 2)

#PC1 Max
plot_deformation(
  mean_shape,
  PC3_max,
  "",
  links = links,
  rotate = TRUE
)
mtext("PC3 - High", side = 3, line = 1, cex = 1.2, font = 2)


#Title
mtext("PC3 Shape Deformation", 
      outer = TRUE, 
      cex = 1.5, 
      font = 2)

#For Resetting Plot layout
par(mfrow = c(1, 1))
