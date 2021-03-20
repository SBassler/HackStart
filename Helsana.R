
require(gtools)
require(Hmisc)
require(gplots)
require(LSD)
require(corrplot)
library(igraph)
require(plotrix)
library(RColorBrewer)
library(scales)
library(grid)
library(ggridges)
library(padr)
library(extrafont)
font_import()
loadfonts(device = "win")
library(ggplot2)
library(viridis)
library(hrbrthemes)
library(reshape2)
require(drc)
library(ggsci)
library(Hmisc)
library(RColorBrewer)
require(drm)
library(growthcurver)
library(pheatmap)
library(plotly)
library(data.table)
library(gifski)
library(gridExtra)
library(gganimate)
library(dplyr)
library(ggpubr)
library(tibble)
library(data.table)
library(magick)
library(tidyverse)
library(RCircos)
require(gtools)
require(graphics)
require(stats)
require(zoo)
require(corrplot)
library(igraph)
require(plotrix)
library(matrixStats)
require(drc)
library(ggrepel)
library(ggplot2)
library(ggridges)
library(gsubfn) 
library(lubridate)

ID <- c()
activity_time <- c()
recognized_activity <- c()
basic_activity <- c()
activity_details <- c()
activity_day <- c()

test <- data.frame(read_csv("/Users/bassler/Desktop/Combined_data.csv"))
for (e in 1:nrow(test)){
  text <- strsplit(test[e,], split = "_") [[1]]
  ID <- c(ID, text[1])
  activity_time <- c(activity_time, as.character(ymd_hms(gsub("[T]", " ",text [2]), tz = "CET")))
  recognized_activity <- c(recognized_activity, text[3])
  basic_activity <- c(basic_activity, text[4])
  activity_details <- c(activity_details, text[5])
  time <- as.character(ymd_hms(gsub("[T]", " ",text [2]), tz = "CET"))
  activity_day <- c(activity_day, strsplit(time, split = " ")[[1]][1])
}



basic_activity <- gsub("[^A-Za-z0-9: ,]", "", basic_activity)
full_data <- data.frame(ID, activity_time, activity_day, recognized_activity, activity_details, basic_activity)
full_data <- full_data %>% mutate(month = as.yearmon(activity_day))

full_data_saved <- full_data
tibble(full_data)
user_list <- unique(full_data$ID)
move <- c("Personal exercise","Discover Trails App", "Daily session","Trails App Run", "Sports and fitness membership",
              "Movement programme Coach","Daily Session Movement Coach", "Keep fit with Helsana Coach",
              "Discover Helsana-Trails", "Hiking challenge","Gym membership","Keep fit with Helsana Coach","Helsana Coach Live Session",
              "Step Challenge - Helsana Coach App" ,"Challenge dancefloor moves","Spring activity challenge",
              "Dancing challenge", "Movement programme", "Connection to Fitnesstracker","February’s recipe cooking challenge 2021",
          "Link to health tracker app", "Discover Helsana-Trails", "Ball balancing challenge", "24h World Cup challenge" ,
          "Winter sports+ challenge", "Heart challenge part 2" , "Tartan Track","Step challenge Helsana Coach", 
          "Movement course", "Clean up your trail challenge", "Ergonomic sitting"
              )
recharge <- c("Relaxation courses","Session mindfulness Coach","Stretching exercises for your back","Happiness Week",
              "Mindfulness programme", "Family challenge", "Declaration of love", "Social memberships", "Recovery position",
              "Prevention", "Relaxation courses", "Make a child laugh", "Hurdles", "Regeneration","Pumpkin carving",
              "Tennis ball massage", "Fun challenge", "Your sanctuary", "Volunteer work" , "Hunt for the Easter surprise boxes",
              "Medical early detection","Mindfulness programme Coach","Local area challenge","Spring activity", "Easter egg challenge",
              "Daily Session mindfulness Coach" 
              )
eat <- c("Nutrition programme","Session nutrition Coach","Nutrition programme Coach","Vitamin-rich snacking","December’s recipe cooking challenge",
          "Bonus recipe June","April’s recipe cooking challenge","July’s recipe cooking challenge" , "Bonus recipe July" ,"July’s recipe cooking challenge",
          "August’s recipe cooking challenge", "December’s recipe cooking challenge 2020", "September’s recipe cooking challenge 2020",
          "March’s recipe cooking challenge", "February’s recipe cooking challenge", "June’s recipe cooking challenge",
          "January’s recipe cooking challenge", "October’s recipe cooking challenge", "September’s recipe cooking challenge", 
          "Bonus recipe August", "November’s recipe cooking challenge 2020", "Bonus recipe September 2020","Bonus recipe October 2020", "Bonus recipe August", "November’s recipe cooking challenge 2020", 
         "Bonus recipe September 2020", "Bonus recipe November 2020","May’s recipe cooking challenge", "Nutrition course",
         "Bonus recipe January 2021", "Bonus recipe December 2020", "January’s recipe cooking challenge 2021", "Helsana health bus roadshow",
         "Bonus recipe February 2021","Bonus recipe April", "Bonus recipe March 2021", "Advent’s recipe cooking", "Bonus recipe May", 
         "Daily Session nutrition Coach", "Challenge recipe cooking" , "Christmas menu cooking" , "October’s recipe cooking challenge 2020",
         "November’s recipe cooking challenge"
          )

full_data$final_activity <- rep("None", nrow(full_data))
full_data[full_data$recognized_activity %in% move,]$final_activity <- "move"
full_data[full_data$recognized_activity %in% recharge,]$final_activity <- "recharge"
full_data[full_data$recognized_activity %in% eat,]$final_activity <- "eat"
write.table(full_data, paste0("/Users/bassler/Desktop/Summary.txt"), sep="\t", quote = F, row.names=FALSE)

#full_data <- read.table("/Users/bassler/Desktop/Summary.txt", header = T)

good_users <- c()
for (n in 1:length(user_list)){
  test_data <- full_data [full_data$ID %in% user_list[n],]
  acts <- test_data[test_data$activity_day > "2020.04.01",]$final_activity
  if (c("move") %in% acts & ("recharge") %in% acts & ("eat") %in% acts){
    good_users <- c(good_users, n)
  }
}

colors <- RColorBrewer::brewer.pal(7, "PuBu")

for (n in good_users){
  test_data <- full_data [full_data$ID %in% user_list[n] & full_data$final_activity == "move",]
  
}







#####Move#####
for (n in good_users){
  test_data <- full_data [full_data$ID %in% user_list[n] & full_data$final_activity == "move",]
  ggplot(test_data, aes(x=as.Date(activity_day), y=..density..)) + 
    geom_density(aes(fill=recognized_activity),color=NA, position="stack")+
    scale_x_date(date_labels = '%b-%Y', date_breaks  ="3 month", limits = c(as.Date("2020-02-01"), as.Date("2021-03-01")))+
    theme(axis.text.x = element_text(angle=75, vjust=0.6, size=12, family="SF Pro Display"),
          panel.background = element_blank(),#, axis.line = element_line(colour = "black"),
          plot.title = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          legend.position = "right",
          legend.title = element_blank())+
    labs(title=paste0("User ",user, " profile"),
         #subtitle="City Mileage grouped by Class of vehicle",
         legend="Recognized activity") -> p
  ggsave(paste0("/Users/bassler/Dropbox/Hackathon/Move_plot/Plots_User_",n,"_move.png"), plot=p)
}

#####Eat#####
for (n in good_users){
  test_data <- full_data [full_data$ID %in% user_list[n] & full_data$final_activity == "eat",]
  ggplot(test_data, aes(x=as.Date(activity_day), y=..density..)) + 
    geom_density(aes(fill=recognized_activity),color=NA, position="stack")+
    scale_x_date(date_labels = '%b-%Y', date_breaks  ="3 month", limits = c(as.Date("2020-02-01"), as.Date("2021-03-01")))+
    theme(axis.text.x = element_text(angle=75, vjust=0.6, size=12, family="SF Pro Display"),
          panel.background = element_blank(),#, axis.line = element_line(colour = "black"),
          plot.title = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          legend.position = "right",
          legend.title = element_blank())+
    labs(title=paste0("User ",user, " profile"),
         #subtitle="City Mileage grouped by Class of vehicle",
         legend="Recognized activity") -> p
  ggsave(paste0("/Users/bassler/Dropbox/Hackathon/Eat_plot/Plots_User_",n,"_eat.png"), plot=p)
}


#####Recharge#####
for (n in good_users){
  test_data <- full_data [full_data$ID %in% user_list[n] & full_data$final_activity == "recharge",]
  ggplot(test_data, aes(x=as.Date(activity_day), y=..density..)) + 
    geom_density(aes(fill=recognized_activity),color=NA, position="stack")+
    scale_x_date(date_labels = '%b-%Y', date_breaks  ="3 month", limits = c(as.Date("2020-02-01"), as.Date("2021-03-01")))+
    theme(axis.text.x = element_text(angle=75, vjust=0.6, size=12, family="SF Pro Display"),
          panel.background = element_blank(),#, axis.line = element_line(colour = "black"),
          plot.title = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          legend.position = "right",
          legend.title = element_blank())+
    labs(title=paste0("User ",user, " profile"),
         #subtitle="City Mileage grouped by Class of vehicle",
         legend="Recognized activity") -> p
  ggsave(paste0("/Users/bassler/Dropbox/Hackathon/Recharge_plot/Plots_User_",n,"_recharge.png"), plot=p)
}


#####Summary#####
for (n in good_users){
  test_data <- full_data [full_data$ID %in% user_list[n],]
  filter(test_data, final_activity != "None") %>%
    ggplot(aes(x=as.Date(activity_day), y=final_activity, fill=..y..))+
    #geom_density_ridges()+
    geom_density_ridges_gradient(scale = 2, rel_min_height = 0.0001, color = NA) +
#    geom_density_ridges(stat = "binline", binwidth=1, draw_baseline = F, color = NA)+
  #  scale_fill_viridis(name = "Counts") +
    theme(axis.text.x = element_text(angle=75, vjust=0.6, size=12, family="SF Pro Display"),
          panel.background = element_blank(),#, axis.line = element_line(colour = "black"),
          plot.title = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.position = "none",
          legend.title = element_blank(),
          axis.text.y = element_text(size=12, family="SF Pro Display"))+
    scale_x_date(date_labels = '%b-%Y',date_breaks  ="3 month", limits = c(as.Date("2020-02-01"), as.Date("2021-03-01"))) +
    labs(title=paste0("User ",user, " profile"),
         #subtitle="City Mileage grouped by Class of vehicle",
         x="Month",
         y="Categories") -> p
  ggsave(paste0("/Users/bassler/Dropbox/Hackathon/Plots_User_",n,".png"), plot=p)#, width=5, height=2.83, dpi=500, limitsize = FALSE)
}




