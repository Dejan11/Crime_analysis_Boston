library(sp)
library(class)
library(kohonen)
library(dummies)
library(ggplot2)
library(maptools)
library(reshape2)
library(rgeos)  

boston_map = readShapePoly("C:/GEOTECH/NOVA/GPS/GPS Project/Boston/Final/Boston_final.shp")

color_palette = colors()[c(26,36,254,552,176,394,261)]

results = data.frame(boston_map)
results1 = results
#Explaratory data analysis
boxplot(results$Bugl2012, results$Bugl2013,results$Bugl2014, las=1, main = "Boxplots for Residential Burglaries", ylab = "Number of burglaries", names = c("2012", "2013", "2014"))
boxplot(results$Hom2012, results$Hom2013, results$Hom2014, las = 1, main = "Boxplots for homicide", ylab = "Number of homicides", names = c("2012", "2013", "2014"))
boxplot(results$AutoThf12, results$AutoThf13, results$AutoThf14, las = 1, main = "Boxplots for Auto Theft", ylab = "Number of auto thefts", names = c("2012", "2013", "2014"))
boxplot(results$CC2012, results$CC2013, results$CC2014, las = 1, main = "Boxplots for Crime agains children", ylab = "Number of crimes", names = c("2012", "2013", "2014"))
boxplot(results1$White/results$TotalPopul*100, results1$Afro_Amer/results$TotalPopul*100, results1$Asian/results$TotalPopul*100, las = 1, main = "Boxplots for Race", ylab = "Percentage", names = c("White", "Afro-American", "Asian"))
boxplot(results$PCI_2010,las = 1, main = "Boxplots for Income $", xlab = "Data from Census 2010")
#Scatterplots

plot(results$Bugl2012, results$AutoThf12, xlim = c(0,20), ylim = c(0,20), xlab = "Recidence Burglary", ylab = "Auto Theft", main = "Recidence Burglary vs Auto Theft 2012") 
+ abline(lm(results$AutoThf12 ~ results$Bugl2012), col = 'red') 
+ legend("topleft", bty="n", legend=paste("R2 is", format(summary(lm(results$AutoThf12 ~ results$Bugl2012))$adj.r.squared, digits=4)))

plot(results$Households, results$Bugl2014, xlab = "Households", ylab = "Recidence Burglary", main = "Households vs Recidence Burglary 2014") 
+ abline(lm(results$Bugl2014 ~ results$Households), col = 'red') 
+ legend("topleft", bty="n", legend=paste("R2 is", format(summary(lm(results$Bugl2014 ~ results$Households))$adj.r.squared, digits=4)))

plot(results$White, results$Afro_Amer, xlab = "White", ylab = "Afro_Amer", xlim = c(0,3500), ylim = c(0,3500), main = "White with Afro-American", las = 2) 
+ abline(lm(results$Afro_Amer ~ results$White), col = 'red') 
+ legend("topright", bty="n", legend=paste("R2 is", format(summary(lm(results$White ~ results$Afro_Amer))$adj.r.squared, digits=4)))

plot(results$PCI_2010, results$POVERTY, xlim = c(0,50000), ylim = c(300,1500), xlab = "Per Capita Income ", ylab = "Poverty", main = "Poverty with Pe Capita Income") 
+ abline(lm(results$POVERTY ~ results$PCI_2010), col = 'red') 
+ legend("topright", bty="n", legend=paste("R2 is", format(summary(lm(results$PCI_2010 ~ results$POVERTY))$adj.r.squared, digits=4)))

#data normalization

#results[,c(4,5,8,11,14,17,19,20,24,25,26)]
#results[,c(4,6,9,12,15,17,19,20,24,25,26)]
#results[,c(4,7,10,13,16,17,19,20,24,25,26)]
data_som = results[,c(4,7,10,13,16,17,19,20,24,25,26)]
data_som_matrix = scale(data_som,center = FALSE, scale = apply(data_som,2,sd, na.rm=TRUE))
#data_som_matrix[,9] = results1$Households/sum(results1$Households)*100
data_som_matrix[,9] = results1$White/results$TotalPopul*100
data_som_matrix[,10] = results1$Afro_Amer/results$TotalPopul*100
data_som_matrix[,11] =  results1$Asian/results$TotalPopul*100
data_som_matrix[,8] = results1$POVERTY/results$TotalPopul*100
data_som_matrix = scale(data_som,center = FALSE, scale = apply(data_som,2,sd, na.rm=TRUE))

names(data_som_matrix) <- names(data_som)
require(kohonen)
# Execute SOM
set.seed(7)
som_model <- som(data_som_matrix, 
                 grid=somgrid( xdim = 10, ydim = 8, topo = "hexagonal" ), 
                 rlen=100, 
                 alpha=c(0.05,0.01), 
                 n.hood = "circular",
                 keep.data = TRUE )
plot(som_model, main = "Crime distribution 2013")

plot(som_model, type = "changes")
#counts within nodes
plot(som_model, type = "counts", main="Node Counts", palette.name=coolBlueHotRed)
#map quality
plot(som_model, type = "quality", main="Node Quality/Distance", palette.name=coolBlueHotRed)
#neighbour distances
plot(som_model, type="dist.neighbours", main = "SOM neighbour distances", palette.name=grey.colors)
#code spread

# Plot the heatmap for a variable at scaled / normalised values
par(mfrow=c(3,4))
  
for (i in 1:11) 
  plot(som_model, type = "property", property = som_model$codes[,i], main=names(som_model$data)[i], palette.name=grey.colors)

#Clustering SOM
mydata_clust <- som_model$codes
wss <- (nrow(mydata_clust)-1)*sum(apply(mydata_clust,2,var))
for (i in 2:8) wss[i] <- sum(kmeans(mydata_clust, centers=i)$withinss)
# adjusting plot location
#par(mar=c(5.1,4.1,4.1,2.1))
par(mfrow=c(1,1))
plot(wss, xlab="Number of Clusters", ylab="Within groups sum of squares", main="Within cluster sum of squares (WCSS)")
# Form clusters on grid
## use hierarchical clustering to cluster the codebook vectors
som_cluster <- cutree(hclust(dist(som_model$codes)), 5)
# Show the map with different colours for every cluster
color_palette = colors()[c(176,254,26,552,36,176,394,261)]
plot(som_model, type="mapping", bgcol = color_palette[som_cluster], main = "Clusters")
add.cluster.boundaries(som_model, som_cluster)
#show the same plot with the codes instead of just colours
plot(som_model, type="codes", bgcol = color_palette[som_cluster], main = "Clusters")
add.cluster.boundaries(som_model, som_cluster)
# Plot the map of Boston, coloured by the clusters the map to show locations
cluster_details1 = data.frame(id=results$GEOID10, cluster = som_cluster[som_model$unit.classif])
# converting boston shapefile into data.frame for better print
boston = fortify(boston_map, region="GEOID10")
# merging clusters with boston data.frame by id's 
boston = merge(boston, cluster_details1, by="id")
#printing geographic map of Boston
g = ggplot(boston) + aes(long, lat, group=group, fill=factor(cluster)) + geom_polygon() + geom_path( color = "white") + coord_equal() + scale_fill_manual(values = color_palette, breaks=c(4,3,2,5,1), labels=c("Safety", "Harmless","Unpredicted zone", "Risky", "I wouldn't go")) + ggtitle("The map of Boston 2014 - Clusters") + theme(legend.key = element_rect(colour = "black"), legend.title = element_text(face = "italic")) + guides(fill = guide_legend(title = "CLUSTERS", title.position = "top", label.position = "bottom")) 
plot(g)

#set.seed(42)
#p1 <- hist(rnorm(500,4))
#p2 <- hist(rnorm(500,6))                    
#plot( p1, col=rgb(0,0,1,1/4), xlim=c(0,10))  
#plot( p2, col=rgb(1,0,0,1/4), xlim=c(0,10), add=T)

#p1 = hist(results$ResBugl14)
#p2 = hist(results$PCI_2010)
#plot( p1, col=rgb(0,0,1,1/4))  
#plot( p2, col=rgb(1,0,0,1/4), add=T)
