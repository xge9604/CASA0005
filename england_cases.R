library(spatstat)
library(here)
library(sp)
library(rgeos)
library(maptools)
library(GISTools)
library(tmap)
library(sf)
library(geojson)
library(geojsonio)
library(tmaptools)
library(tidyverse)
library(sf)
library(stringr)
library(tidymodels)
library(highcharter)
library(tidyverse)
library(downloader)
library(rgdal)
library(sf)
library(ggplot2)
library(reshape2)
library(plotly)
library(raster)
library(downloader)
library(rgdal)
library(raster)
library(fpc)
library(dbscan)
library(spdep)
#####england by local authority 
e_w = st_read(here::here("Population_Density_in_England_and_Wales_per_District_mid_year_2018-shp",
                         "Population_Density_in_England_and_Wales_per_District_mid_year_2018.shp"))%>%
  st_transform(.,4326)


#OSM <- st_read(here::here("england-latest-free.shp (1)",
#                          "gis_osm_landuse_a_free_1.shp"))%>%
# filter(fclass =='house')
###region in England
regionmap <- st_read(here::here("Region__December_2015__Boundaries-shp",
                             "Region__December_2015__Boundaries.shp"))%>%
  st_transform(.,4326)

region <- read_csv("region_cases.csv")
#################################################
regionmap <- regionmap %>%
  merge(.,
        region,
        by.x="rgn15cd",
        by.y="areaCode")
tmap_mode("plot")
tm_shape(regionmap) +
  tm_polygons("cum_cases",
              style="jenks",
              palette="PuBu",
              midpoint=NA,
              popup.vars=c("areaName", "cum_cases"),
              title="Cumulative cases")+
  tm_compass(position = c("right", "bottom"),type = "arrow") + 
  tm_scale_bar(position = c("right", "bottom")) +
  tm_layout(legend.position = c("left", "top"))


###############################
cases = read_csv("englandcases.csv")
auto = read_csv ("auto.csv")#####without isles
eng <- e_w %>%
dplyr::filter(str_detect(lad11cd, "^E0"))%>%
            left_join(.,
            cases,
            by=c("lad11cd" = "area_code"))


####qtm(eng)

############################

cor.test(cum_cases,engpop_density,method="pearson")

cor.test(cum_cases,engpop_density,method="spearman")

shapiro.test(logcase)


###########################
eng%>%
  head(.,n=10)

tmap_mode("plot")
#eng%>%
# qtm(eng,fill="cum_cases")
#########
tm_shape(eng) +
  tm_polygons("cum_cases",
              style="jenks",
              palette="BuPu",
              midpoint=NA,
              popup.vars=c("area_name", "cum_cases"),
              title="Cumulative cases")+
  tm_compass(position = c("right", "bottom"),type = "arrow") + 
  tm_scale_bar(position = c("right", "bottom")) +
  tm_layout(legend.position = c("left", "top"))
  
#########

tm1 <- tm_shape(eng)+
  tm_polygons("cum_cases", 
              palette="PuBu",
              midpoint=NA,
              title="Cumulative Cases",
              alpha = 1) + 
  tm_compass(position = c("right", "bottom"),type = "arrow") + 
  tm_scale_bar(position = c("right", "bottom")) +
  tm_layout(legend.position = c("left", "top"))
tm1




############################################################## 


summary(cases)

#########################################################

##########task 1 Descripivate Statistics
#1 check data type
Datatypelist <- enguse %>% 
  st_drop_geometry()%>%
  summarise_all(class) %>%
  pivot_longer(everything(), 
               names_to="All_variables", 
               values_to="Variable_class")


#make groups based on types of variables
Groups <- enguse %>% 
  st_drop_geometry()%>%
  dplyr::select(is.numeric)%>%
  pivot_longer(everything(),
               names_to="All_variables", 
               values_to="val")%>%
  mutate(All_variables = tolower(All_variables))%>%
  mutate(group = case_when(str_detect(All_variables, "cases") ~ "caseeng"))
                           
                         
                    


casehist <- Groups%>%
  filter(group=="caseeng")%>%
  ggplot(., aes(x=log(val))) + 
  geom_histogram(aes(x = log(val), y = ..density..))+
  geom_density(colour="red", size=1, adjust=1)+
  facet_wrap(~All_variables, scales = 'free')+
  ggtitle("Cumulative cases (After normalization)")+
  theme(plot.title=element_text(hjust=0.5))
casehist

caseshist <- Groups%>%
  filter(group=="caseeng")%>%
  ggplot(., aes(x=log(val))) + 
  geom_histogram(aes(x = log(val), y = ..density..))+
  geom_density(colour="red", size=1, adjust=1)+
  facet_wrap(~All_variables, scales = 'free')+
  ggtitle("Cumulative cases in England (Before normalization)")+
  xlab("Lacal authorities")+
  ylab("Cumulative cases")
  theme(plot.title=element_text(hjust=0.5))
  
denhist

attach(eng)
summary(cum_cases)


###############################################################################
mydata <- eng %>%
  st_drop_geometry()%>%
  dplyr::select(c(logcase, logpopd))


#######spatial autocorrelation 
#######################################################################
auto = read_csv ("auto.csv")

remove_isle <-e_w%>%
  merge(.,
        auto,
        by.x="lad11cd",
        by.y="area_code")
########################
enguse <- remove_isle %>%
  dplyr::select(lad11cd,cum_cases,engpop_density,area_name,logcase)
########################
mydata1<- enguse %>%
  st_drop_geometry()%>%
  dplyr::select(c(logcase, cum_cases))

########before transformation 
histplot <- ggplot(data=mydata1, aes(x=cum_cases))+
  ggtitle("Cumulative cases (before normalization)")+
  theme(plot.title=element_text(hjust=0.5))

histplot +geom_histogram()

histplot <- ggplot(data=mydata1, aes(x=logcase))+
  ggtitle("Cumulative cases (after normalization)")+
  theme(plot.title=element_text(hjust=0.5))

histplot +geom_histogram()


########################
tm_shape(enguse) +
  tm_polygons("engpop_density",
              style="jenks",
              palette="PuOr",
              midpoint=NA,
              popup.vars=c("area_name", "engpop_density"),
              title="Population Density")

coordsW <- enguse%>%
  st_centroid()%>%
  st_geometry()

plot(coordsW,axes=TRUE)

#create a neighbours list

england_nb <- enguse %>%
  poly2nb(., queen=T)

#plot them
plot(england_nb, st_geometry(coordsW), col="red")
#add a map underneath
plot(enguse$geometry, add=T)

#create spatial weights from weight 
england.lw<-england_nb %>%
  nb2listw(.,style="C")

head(england.lw$neighbours)

#########spatial auto-correlation
casesglobal <- enguse %>%
  pull(logcase) %>%
  as.vector()%>%
  moran.test(., england.lw)

casesglobal
######results > o, exist clustering 
################got global moran I
#################Geary's C
case_c<- 
  enguse %>%
  pull(logcase) %>%
  as.vector()%>%
  geary.test(., england.lw)

case_c
######result<1, clustering 
##############global Geary C
#####weather high ...
G_case <- 
  enguse %>%
  pull(logcase) %>%
  as.vector()%>%
  globalG.test(., england.lw)

G_case
########result < expectation low value cluster
###################################local data for hot-spots
I_england_local <-enguse %>%
  pull(logcase)%>%
  as.vector%>%
  localmoran(.,england.lw)%>%
  as_tibble()

slice_head(I_england_local,n=6)
###########################################
enguse <-enguse%>%
  mutate(I_cases = as.numeric(I_england_local$Ii))%>%
  mutate(Iz_cases = as.numeric(I_england_local$Z.Ii))

###################################
#####finally we plot it#########################
Gi_eng <- enguse %>%
  pull(logcase) %>%
  as.vector()%>%
  localG(., england.lw)

head(Gi_eng)

enguse <- enguse %>%
  mutate(case_G = as.numeric(Gi_eng))
##############
breaks1<-c(-1000,-2.58,-1.96,-1.65,1.65,1.96,2.58,1000)

MoranColours<- rev(brewer.pal(8, "RdGy"))
tm_shape(enguse) +
  tm_polygons("Iz_cases",
              style="fixed",
              breaks=breaks1,
              palette=MoranColours,
              midpoint=NA,
              title="Local Moran's I")+
  tm_layout(legend.position = c("left", "top"))+
  tm_compass(position = c("right", "bottom"),type = "arrow") + 
  tm_scale_bar(position = c("right", "bottom"))
##########
GIColours<- rev(brewer.pal(8, "RdBu"))
#now plot on an interactive map
tm_shape(enguse) +
  tm_polygons("case_G",
              style="fixed",
              breaks=breaks1,
              palette=GIColours,
              midpoint=NA,
              title="Gi*")+
  tm_layout(legend.position = c("left", "top"))+
  tm_compass(position = c("right", "bottom"),type = "arrow") + 
  tm_scale_bar(position = c("right", "bottom"))









#######K-mean##########################################################

library(tidymodels)
fit <- mydata %>%
  kmeans(.,3,nstart = 30)
centroid <- tidy(fit)%>%
  #print the results of the cluster groupings
  print()%>%
  dplyr::select(logcase, logpopd)

##################################
# as we only have variable two dimensions we can plot the clusters on a graph
p <- ggplot(mydata,aes(logcase, logpopd))+
  geom_point(aes(colour=factor(fit$cluster)))+
  geom_point(data=centroid,aes(logcase, logpopd), size=7, shape=18)+ theme(legend.position="left top")
p
eng<-fit %>%
  augment(., eng)%>%
  dplyr::select(lad11cd, .cluster)%>%
  #make sure the .cluster column is numeric
  mutate(across(.cluster, as.numeric))%>%
  # join the .cluster to our sf layer
  left_join(eng, 
            .,
            by = c("lad11cd" = "lad11cd"))


#now map our geodeomographic classification
map <- ggplot(eng) + 
  geom_sf(mapping = aes(fill=.cluster))+
   scale_fill_continuous(breaks=c(1,2,3,4))+
  ggtitle("Cluster result for Cumulative cases and population density")
map
#############################################
####not significant also not fit the assumptions normal, less cor 
######### did not use k-mean 
#####################further research?
