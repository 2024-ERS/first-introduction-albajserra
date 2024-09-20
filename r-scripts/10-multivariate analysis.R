# multivariate analysis of vegetation composition in relation to environmental factors

# clear everything in memory (of R)
remove(list=ls())

library(vegan) # multivariate analysis of ecological community data 
library(psych) # usefull for panel plots of multivariate datasets
library(tidyverse)

# read the vegetation data
vegdat0<-readr::read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSJFU7gDlXuBM6hgWwcYJ-c_ofNdeqBe50_lDCEWO5Du3K7kPUDRh_esyKuHpoF_GbmBAoT4ZygrGWq/pub?gid=2036214007&single=true&output=csv") |>
  filter(year==2023 & TransectPoint_ID<=1150) |>     # filter for only the only 2023 data and plots with vegetation
  dplyr::select(-c(year,bare,litter,mosses,SalicSpp)) |>  # select  only distance_m and the species names as variable to use
  tibble::column_to_rownames(var="TransectPoint_ID") # convert distance_m to the row names of the tibble. Use this in my work to make it multivariate, so each column is a species

vegdat<-vegdat0 |> dplyr::select(which(colSums(vegdat0) != 0)) # remove species that did not occur in this year in any plot


# read the macrotransect elevation data
elevdat<-readr::read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vT4C7olgh28MHskOjCIQGYlY8v5z1sxza9XaCccwITnjoafF_1Ntfyl1g7ngQt4slnQlseWT6Fk3naB/pub?gid=1550309563&single=true&output=csv") %>%
  dplyr::filter(year==2023 & !is.na(TransectPoint_ID) & TransectPoint_ID<=1150) %>%
  dplyr::select(TransectPoint_ID,elevation_m)   # select  only distance_m and elevation 
elevdat

# read the macrotransect clay thickness from the soil profile dataset
claydat<-readr::read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQyEg6KzIt6SdtSKLKbbL3AtPbVffq-Du-3RY9Xq0T9TwPRFcgvKAYKQx89CKWhpTKczPG9hKVGUfTw/pub?gid=943188085&single=true&output=csv") |>
  dplyr::filter(Year==2024 & SoilType_ID %in% c("clay","clay-organic") & TransectPoint_ID<=1150) |>
  dplyr::select(TransectPoint_ID,corrected_depth) |>     
  group_by(TransectPoint_ID) |> 
  dplyr::summarize(clay_cm=mean(corrected_depth,na.rm=T)) #calculate average clay layer thickness  for each pole
claydat


##### read the flooding proportion (proportion of the time of  the growing season flooded)
# from 1 april - 30 aug
flooddat<-readr::read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTiJsz0xcUAhOjlTcHpT7NXhSysGMdzI6NfYm_kzpFtV99Eyea2up7dB-5a_P-jAKiwMBAXYURBTpRU/pub?gid=600890443&single=true&output=csv") |> 
  dplyr::filter(year==2023) |>
  dplyr::select(-year,-flood_ID)
flooddat

# read distance to gulley of each plot
gulleydist<-readr::read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vT4C7olgh28MHskOjCIQGYlY8v5z1sxza9XaCccwITnjoafF_1Ntfyl1g7ngQt4slnQlseWT6Fk3naB/pub?gid=2102201825&single=true&output=csv") |>
  dplyr::filter(Year==2023,TransectPoint_ID<=1150) |>
  dplyr::select(-Year,-DistanceToGullley_ID)
gulleydist
  
# also add redox
redox<-readr::read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQyEg6KzIt6SdtSKLKbbL3AtPbVffq-Du-3RY9Xq0T9TwPRFcgvKAYKQx89CKWhpTKczPG9hKVGUfTw/pub?gid=1911552509&single=true&output=csv") |>
  dplyr::filter(Year==2023,TransectPoint_ID<=1150) %>%
  dplyr::group_by(TransectPoint_ID,ProbeDepth) %>%
  dplyr::summarize(redox_mV=mean(redox_raw2_mV,na.rm=T)) %>%
  tidyr::pivot_wider(id_cols=TransectPoint_ID,
                     names_from=ProbeDepth,
                     names_prefix = "redox",
                     values_from = redox_mV)
redox

##### merge elevation, clay thickness, flooding proportion and distance to gulley data in
# a sequential join operation in a pipe
# start from elevdat and use left_join, calling the tibble envdat
# set distance_m as the row names of the  tibble
# set the NAs for floodprop equal to zero
# . (dot) means " outcome of the pipe so far
envdat <-
  dplyr::left_join(elevdat,claydat,by="TransectPoint_ID") |>
  dplyr::left_join(flooddat,by="elevation_m") |>
  dplyr::left_join(gulleydist,by="TransectPoint_ID") |>
  dplyr::left_join(redox,by="TransectPoint_ID") |>
  dplyr::mutate(floodprob=ifelse(is.na(floodprob),0,floodprob),
                floodprob=round(floodprob,4),  # round the floodprob to 4 decimals
                clay_cm=ifelse(is.na(clay_cm),0,clay_cm)) |> # set the NAs for clay_cm equal to zero
  tibble::column_to_rownames(var="TransectPoint_ID")

# for ordination with functions from the vegan library you need an separate environmental factors dataset
# and a species composition dataset with the same rownames, indicating  the same sites / samples
# the species data (community composition) need to be in wide format
view(vegdat)
view(envdat)

##### explore the correlations among the environmental factors in a panel pairs plot
psych::pairs.panels(envdat,smooth=F,ci=T,ellipses=F,stars=T,method="pearson")
psych::pairs.panels(envdat,smooth=F,ci=T,ellipses=F,stars=T,method="spearman")
# note that the units are very different! 

##### Ordination: run a Principal Component Analysis (PCA) on the environmental data
# .scale=T means: use correlations instead of covariances
# use .scale=T for datasets where the variables are measured in different use

# do a principal component analysis (pca) 
pca_env<-prcomp(envdat,center =T,scale=T)
pca_env
summary(pca_env) #pc1 and pc2 explain 80% of the variation -> most important values

#show the site scores for axis 1
pca_env$x

# the PCs are reduced dimensions of the dataset
# you reduce 6 variables to 2 dimensions
# make a biplot (variable scores plus sample score) the pca ordination
# and label the axis with the explained variation
biplot(pca_env,xlab="PC1 (49%)",ylab="PC2 (21%)")

##### ordination: calculate and plot a Non-metric Multidimensional Scaling (NMDS) ordination
# explore the distance (dissimilarity) in species composition between plots
d1<-vegan::vegdist(vegdat,method="euclidean") # calculate the Euclidean distance matrix
d2<-vegan::vegdist(vegdat,method="bray") # calculate the Bray-Curtis dissimilarity matrix
d1
d2

##### improve the NMDS ordination plot by only showing the dominant species
# non-metric multidimension scaling / indirect gradient analysis (only species composition)
nmds_veg<-vegan::metaMDS(vegdat,k=2,trace=F,trymax=1000,distance="bray") # k=2 means 2 dimensions
nmds_veg
vegan::ordiplot(nmds_veg,type="t") # plot the NMDS ordination without the species scores. If numbers are close together -> have species in common hence are in the same plot
#NMDS1 corresponds to elevation, flooding, gullies... -> what ordinance does: ordered the gradient
#NMDS2 corresponds to clay thickness gradient

# and show the ordination with the most abundance species with priority
# only see most abundant species on the graph
SpecTotCover<-colSums(vegdat) # calculate the total cover of each species
vegan::ordiplot(nmds_veg,display="sites",cex=1,type="t") # plot the NMDS ordination without the species scores
vegan::orditorp(nmds_veg,display="species",priority=SpecTotCover,
                col="red",pcol="red",pch="+",cex=1.1) # add the species scores to the plot with color red

#### ordination: compare to a DCA -> decide what ordination we should do, linear or unimodal? 
# how long are the gradients? Should I use linear (PCA) or unimodal method (NMDS, DCA)
dca<-vegan::decorana(vegdat) # decorana is a detrended correspondence analysis
dca

# first axis is 8.1 standard deviations of species responses
# result: length of first ordination axis is >8 standard deviations
# only when <1.5 you can use a PCA or RDA
# plot the dca results as a biplot
vegan::ordiplot(dca,display="sites",cex=0.7,type="text",xlim=c(-6,6)) # plot the DCA ordination without the species scores
vegan::orditorp(dca,display="species",priority=SpecTotCover,
                col="red",pcol="red",pch="+",cex=0.8) # add the species scores to the plot with color red

##### fit the environmental factors to the dca ordination surface
names(envdat)
ef_dca<-vegan::envfit(dca~elevation_m+clay_cm+floodprob+DistGulley_m+redox5+redox10,data=envdat,na.rm=T)

#add the result to the ordination plot as vectors for each variable
plot(ef_dca,add=T) # add the environmental factors to the DCA ordination plot


##### add contour surfaces to the dca ordination for the relevant abiotic variables
vegan::ordis
urf(dca,envdat$elevation_m,add=T,col="brown") # add a contour surface for elevation
vegan::ordisurf(dca,envdat$clay_cm,add=T,col="darkgreen") # add a contour surface for clay thickness

##### make the same plot but using a nmds
##### fit the environmental factors to the nmds ordination surface
vegan::ordisurf(dca,vegdat$FestuRub,add=T,col="darkgreen") # add a contour surface for clay thickness

##### fit the environmental factors to the dca ordination surface (same process as before)

#add the result to the ordination plot as vectors for each variable

##### add contour surfaces to the nmds ordination for the relevant abiotic variables


##### compare an unconstrainted (DCA) and constrained (CCA) ordination
# did you miss important environmental factors?
# show the results of the detrended correspondence analysis
dca

# the eigenvalues represent the variation explained by each axis
names(envdat)
cca1<-vegan::cca(vegdat~elevation_m+clay_cm+floodprob+DistGulley_m+redox5+redox10,data=envdat,na.rm=T)
summary(cca)

# kick out variables that are least significant - simplify the model
anova(cca1,by="axis") # test the significance of the axes
anova(cca1,by="margin") # test the significance of the margins (variables)

# redo onlt with important uncorrelated vaiables
cca2<-vegan::cca(vegdat~floodprob+DistGulley_m,data=envdat,na.rm=T)
summary(cca2)
anova(cca2,by="axis") # test the significance of the axes
anova(cca2,by="margin") # test the significance of the margins (variables)

# add the environmental factors to the cca ordination plot
vegan::ordiplot(cca2,display="sites",cex=1,type="text",xlab="CCA1 (21%)",ylab="CCA2 (14%)") # plot the CCA ordination without the species scores
vegan::orditorp(cca2,display="species",priority=SpecTotCover,
                col="red",pcol="red",pch="+",cex=1.1) # add the species scores to the plot with color red
vegan::ordisurf(cca2,envdat$floodprob,add=T,col="blue") # add a contour surface for flooding proportion. add=T means adding to existing graph
vegan::ordisurf(cca2,envdat$DistGulley_m,add=T,col="green") # add a contour surface for distance to gulley

# test if the variables and axes (margins) are significant
# You have measured the right things!
# for example - test this if you would have only measured clay thickness
# yes, clay thickness significantly affects vegetation composition

##### cluster analysis (classification) of  communities --> USE THIS FOR MY DATASET
# first calculate a dissimilarity matrix, using Bray-Curtis dissimilarity
d<-vegan::vegdist(vegdat,method="bray")

 # show the dissimilarity matrix (1= completely different, 0= exactly the same)
d

# now cluster the sites based on similarity in species composition 
# using average linkage as the sorting algorithm
cavg<-hclust(d,method="average") # average linkage clustering
plot(cavg) # plot the dendrogram

# back to clustering based on species composition - show the dendrogram and cut in in 4 communities
rect.hclust(cavg,4) # cut the dendrogram in 4 clusters
c4<-cutree(cavg,4) # assign the plots to the 4 clusters
c4

##### add the clustering of plots to your cca ordination
vegan::ordiplot(cca1,display="sites",cex=1,type="text",xlab="CCA1 (21%)",ylab="CCA2 (14%)") # plot the CCA ordination without the species scores
vegan::orditorp(cca1,display="species",priority=SpecTotCover,
                col="red",pcol="red",pch="+",cex=1.1) # add the species scores to the plot with color red
vegan::ordihull(cca1,c4,lty=2,col = "darkgreen",lwd = 2) # add the cluster polygons to the plot

#add the vegetation type to the environmental data
envdat2<-envdat |>
  dplyr::mutate(vegtype=factor(c4)) # add the vegetation type to the environmental data
levels(envdat2$vegtype)<-c("Dune","High Saltmarsh","Low Saltmash","Pioneer Zone") # rename the levels of the factor

# install package patchwork
install.packages("patchwork")
library(patchwork)

# test if DistGulley_m is different between the vegetation types
p1<-envdat2 |>
  ggplot(aes(x=vegtype,y=floodprob))+
  geom_boxplot()+
  xlab(NULL)
p2<-envdat2 |>
  ggplot(aes(x=vegtype,y=clay_cm))+
  geom_boxplot()
p1+p2+patchwork::plot_layout(ncol=1)

# what do you write: 
# the vegetation types were significantly different in distance to gulley (F3,18=21.36, P<0.001)
# * P<0.05, ** P<0.01, *** P<0.001
# means with the same letter are not significantly different