

##############
#Hierarchical clustering Using PCA Coordinates

pcs <- pca$x[,1:18]


dist.pca <- dist(pcs)


hc.pca <- hclust(dist.pca, method = "ward.D2")

plot(
  hc.pca,
  labels = dimnames(gpa$coords)[[3]],
  hang = -1,
  main = "Hierarchical Clustering of Artiodactyl Astragali (PC1-PC18)",
  xlab = "",
  sub = "",
  ylim = c(-0.12, max(hc.pca$height) * 1.05)
)


#Clustering the optimal number of groups
rect.hclust(hc.pca, k = 4, border = c("#009E73", "#0072B2", "#CC79A7", "#000000"))

#Adding a border for the dendrogram (note: in this study, this border was added with Adobe Photoshop, this line was not used)
box(
  which = "plot",
  lwd = 2
)


###############
#Visualising the Hierarchical Clustering into a PCA Plot (Note: This was not visualised in the final work)

# Desginating the Number of Clusters
clusters <- cutree(hc.pca, k = 4)

cols <- c("#000000", "#CC79A7", "#0072B2", "#009E73")

#Plotting the PCA
plot(
  pca$x[,1],
  pca$x[,2],
  pch = 21,
  bg = cols[clusters],
  xlab = paste0("PC1 (", round(100*pca$d[1]/sum(pca$d),2), "%)"),
  ylab = paste0("PC2 (", round(100*pca$d[2]/sum(pca$d),2), "%)"),
  main = "Hierarchical Clustering of Astragali (PC1 vs PC2)"
)

text(
  pca$x[,1],
  pca$x[,2],
  labels = dimnames(gpa$coords)[[3]],
  pos = 3,
  cex = 0.7
)

#Adding the PCA Grid
abline(v = 0, lty = 2, lwd = 1)
abline(h = 0, lty = 2, lwd = 1)

grid(
  nx = NULL,
  ny = NULL,
  col = adjustcolor("grey20", alpha.f = 0.3),
  lty = 3,
  lwd = 0.7
)

#Adding a bolder border to the PCA Plot
box(
  which = "plot",
  lwd = 2
)

#Legend (Not Updated to fit rest of graph)
legend(
  "bottomright",
  legend = c(
    "Cluster 1",
    "Cluster 2",
    "Cluster 3"
  ),
  col = c("#7570b3", "#d95f02", "#1b9e77"),
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



##########
#Determining the Optimal Number of Clusters
library(cluster)

#Silhouette Widths
sil_width <- numeric(9)

for (k in 2:10) {
  clusters <- cutree(hc.pca, k = k)
  
  sil <- silhouette(clusters, dist.pca)
  
  sil_width[k - 1] <- mean(sil[, 3])
}

#Average Silhouette Width Plot
plot(
  2:10,
  sil_width,
  type = "b",
  pch = 19,
  xlab = "Number of Clusters",
  ylab = "Average Silhouette Width",
  main = "Optimal Number of Clusters"
)

summary(sil_width)

sil_width


