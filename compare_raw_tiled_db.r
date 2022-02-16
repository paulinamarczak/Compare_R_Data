#Compare outputs graphically of spatial fire, harvest, and salvage for raw, tiled, and database
#Not sure where indicators come into play- so compare with my script outputs?
#Then for my area calculations for one layer for each draw, run bens script. of create_area_grid and check_Layer_area_batch

library(ggplot2)
library(tidyverse)
library(grid)
library(gridExtra)
library(scales)
library(lessR)

setwd("C://Users//pmarczak//Documents//compare_raw_tiled_db")
dir<-getwd()

##Query database files
database_path<- file.path(dir, "database")
database_fire<- read.csv(file.path(database_path, "fire.csv"))
head(database_fire)
colnames(database_fire)<- c("year", "area_ha", "area_ha")

database_fire_base<-database_fire[c(1,2)]
database_fire_base$class<-"database_fire_base"
head(database_fire_base)

database_fire_miti<-database_fire[c(1,3)]
head(database_fire_miti)
database_fire_miti$class<-"database_fire_miti"


database_harvest<- read.csv(file.path(database_path, "Clearcut_Harvest.csv"))
colnames(database_harvest)<- c("year", "area_ha", "area_ha")

head(database_harvest)
tail(database_harvest)
database_harvest_base<-database_harvest[c(1,2)]
database_harvest_base$class<-"database_harvest_base"
head(database_harvest_base)

database_harvest_miti<-database_harvest[c(1,3)]
database_harvest_miti$class<-"database_harvest_miti"
head(database_harvest_miti)

database_salvage<- read.csv(file.path(database_path, "Salvage_Harvest.csv"))
head(database_salvage)
tail(database_salvage)
colnames(database_salvage)<- c("year", "area_ha", "area_ha")
database_salvage_base<-database_salvage[c(1,2)]
database_salvage_base$class<-"database_salvage_base"
head(database_salvage_base)

database_salvage_miti<-database_salvage[c(1,3)]
database_salvage_miti$class<-"database_salvage_miti"
head(database_salvage_miti)

##Query raw files
raw<- read.csv("raw.csv")
head(raw)
tail(raw)

#remember raw doesnt have salvage because its starting point so no fire has occured.??
raw_fire<-filter(raw, raw$dist_type=="fire")
raw_fire<-raw_fire[c(4,5)]
head(raw_fire)
raw_fire$class<-"raw_fire"


raw_harvest<-filter(raw, raw$dist_type=="harvest")
head(raw_harvest)
raw_harvest<-raw_harvest[c(4,5)]
raw_harvest$class<-"raw_harvest"

##Query tiled files
#no harvest here
tiled<-read.csv("tiled.csv")
head(tiled)
tiled<-filter(tiled, tiled$folder=="draw001")
tiled_fire<-filter(tiled, tiled$dist_type=="burn")
tiled_fire<-tiled_fire[c(4,5)]
tiled_fire$class<-"tiled_fire"


head(tiled_fire)
tiled_salvage<-filter(tiled, dist_type=="salvage")
head(tiled_salvage)
tiled_salvage<-tiled_salvage[c(4,5)]
tiled_salvage$class<-"tiled_salvage"


scenario_palette <- c("#009E73", "#00e6de", "#E69F00", "#0072B2", "#003b00") 

#Fire merge
Fire<-Reduce(function(x,y) mrg (x,y), list(
database_fire_base,
database_fire_miti,
raw_fire,
tiled_fire))
head(Fire)
Fire$class<-factor(Fire$class)

#harvest merge
Harvest<-Reduce(function(x,y) mrg (x,y), list(
	database_harvest_base,
	database_harvest_miti,
raw_harvest
))

#salvage merge
Salvage<-Reduce(function(x,y) mrg (x,y), list(
	database_salvage_base,
	database_salvage_miti,
	tiled_salvage
))

#scenario_palette <- c("#0072B2", "#E69F00","#00e6de", "#009E73") 

#fire
plot1<- ggplot(data= Fire, mapping=aes(year, area_ha)) +
		geom_line(aes(color= factor(class))) +
		geom_point(aes(color= (class), shape= (class)),size = .5, show.legend = FALSE) +
		scale_shape_manual(values= c(16, 4, 8, 3)) +
		scale_colour_manual(values=scenario_palette, name ="") +
		scale_size_manual(values=0.1) +
		theme(legend.position = "bottom")+
		labs(y="", x="", title= "Fire") +
		scale_y_continuous(labels= comma)

#harvest
plot2<- ggplot(data= Harvest, mapping=aes(year, area_ha)) +
		geom_line(aes(color= factor(class))) +
		geom_point(aes(color= (class), shape= (class)),size = .5, show.legend = FALSE) +
		scale_shape_manual(values= c(16, 4, 8, 3)) +
		scale_colour_manual(values=scenario_palette, name ="") +
		scale_size_manual(values=0.1) +
		theme(legend.position = "bottom")+
		labs(y="", x="", title= "Harvest") +
		scale_y_continuous(labels= comma)


#salvage
plot3<- ggplot(data= Salvage, mapping=aes(year, area_ha)) +
		geom_line(aes(color= factor(class))) +
		geom_point(aes(color= (class), shape= (class)),size = .5, show.legend = FALSE) +
		scale_shape_manual(values= c(16, 4, 8, 3)) +
		scale_colour_manual(values=scenario_palette, name ="") +
		scale_size_manual(values=0.1) +
		theme(legend.position = "bottom")+
		labs(y="", x="", title= "Salvage") +
		scale_y_continuous(labels= comma)

# blankPlot <- ggplot()+geom_blank(aes(1,1)) + 
#   cowplot::theme_nothing()

# get_legend<-function(myggplot){
#   tmp <- ggplot_gtable(ggplot_build(plot1))
#   leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
#   legend <- tmp$grobs[[leg]]
#   return(legend)
# }

# legend<-get_legend(plot1)

# plot1<-plot1 +theme(legend.position="none")
{
png(filename= "area_qa_graph.png",
    units="in", 
    width=20, 
    height=5, 
    pointsize=12, 
    res=500)}

grid.arrange(plot1,plot2,plot3, ncol=3, nrow=1,heights=c(2.3), 
	top = textGrob("Area QA Comparisons between Raw, Tiled, and Database (DB) for Draw001 1990-2070", 
	x=.2770, gp=gpar(fontsize=14,font=2)),
	left=textGrob(label="Area burned (ha)", rot=90, gp=gpar(fontsize=15)),
	bottom=(textGrob(label = expression("Year"), gp=gpar(fontsize=15))))

dev.off()
