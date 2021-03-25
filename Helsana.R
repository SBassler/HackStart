
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
library(ggiraphExtra)
library(ggradar)
library(extrafont)
font_import()
loadfonts(device = "win")
library(ggplot2)
library(viridis)
library(hrbrthemes)
library(reshape2)
library(Rtsne)
require(drc)
library(ggsci)
library(Hmisc)
library(RColorBrewer)
require(drm)
library(growthcurver)
library(pheatmap)
library(plotly)
library(M3C)
library(data.table)
library(gifski)
library(Rtsne)
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
library(umap)
library(magrittr)
set.seed(12345)
par(mar=c(1,1,1,1))
create_beautiful_radarchart <- function(data, color = "#00AFBB", 
                                        vlabels = colnames(data), vlcex = 0.7,
                                        caxislabels = NULL, title = NULL, ...){
  radarchart(
    data, axistype = 1,
    # Customize the polygon
    pcol = color, pfcol = scales::alpha(color, 0.5), plwd = 2, plty = 1,
    # Customize the grid
    cglcol = "grey", cglty = 1, cglwd = 0.8,
    # Customize the axis
    axislabcol = "grey", 
    # Variable labels
    vlcex = vlcex, vlabels = vlabels,
    caxislabels = caxislabels, title = title, ...
  )
}


#####Preparation#####
ID <- c()
activity_time <- c()
recognized_activity <- c()
basic_activity <- c()
activity_details <- c()
activity_day <- c()
test <- data.frame(read_csv("/Volumes/GoogleDrive/My Drive/Hackathon_Delage/Combined_data.csv"))
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
#####Users#####

full_data <- read.table("/Volumes/GoogleDrive/My Drive/Hackathon_Delage/Summary.txt", header = T, sep="\t")

good_users <- c()
for (n in 1:length(user_list)){
  test_data <- full_data [full_data$ID %in% user_list[n],]
  acts <- test_data[test_data$activity_day > "2020.04.01",]$final_activity
  if (c("move") %in% acts & ("recharge") %in% acts & ("eat") %in% acts){
    good_users <- c(good_users, n)
  }
}

colors <- RColorBrewer::brewer.pal(7, "PuBu")

recharge_score <- c()
move_score <- c()
eat_score <- c()
for (n in 1:length(user_list)){
  test_data <- full_data [full_data$ID %in% user_list[n] & full_data$activity_day > "2020.04.01",]
  recharge_score <- c(recharge_score, nrow(test_data [test_data$final_activity == "recharge",]))
  move_score <- c(move_score, nrow(test_data [test_data$final_activity == "move",]))
  eat_score <- c(eat_score, nrow(test_data [test_data$final_activity == "eat",]))
}

score_frame <- data_frame(user_list, recharge_score, move_score, eat_score)
max(score_frame$recharge_score)
max(score_frame$move_score)
max(score_frame$eat_score)

max_recharge <- 49
max_move <- 142
max_eat <- 50

#####Total score#####
recharge_score <- c()
move_score <- c()
eat_score <- c()
total_score <- c()
for (n in 1:length(user_list)){
  test_data <- full_data [full_data$ID %in% user_list[n] & full_data$activity_day > "2020.04.01",]
  recharge_score <- c(recharge_score, (nrow(test_data [test_data$final_activity == "recharge",])/max_recharge)*100)
  move_score <- c(move_score, (nrow(test_data [test_data$final_activity == "move",])/max_move)*100)
  eat_score <- c(eat_score, (nrow(test_data [test_data$final_activity == "eat",])/max_eat)*100)
  total_score <- c(total_score, mean( 
                  (nrow(test_data [test_data$final_activity == "recharge",])/max_recharge)*100,
                  (nrow(test_data [test_data$final_activity == "move",])/max_move)*100, 
                  (nrow(test_data [test_data$final_activity == "eat",])/max_eat)*100))
}

user_scores <- data.frame (user_list, recharge_score, move_score, eat_score, total_score)

write.table(user_scores, paste0("/Users/bassler/Desktop/User_scores.txt"), sep="\t", quote = F, row.names=FALSE)

#####Spider plot radarchart#####
for (n in good_users){
  png(paste0("/Volumes/GoogleDrive/My Drive/Hackathon_Delage/Spider_plot2/Plots_User_",n,"_spider.png"))
  data <-user_scores[n, ]
  rownames(data) <- data$user_list
  data<- data[,2:5]
  max_min <- data.frame(
    Recharge = c(100, 0), Move = c(100, 0), Eat = c(100, 0),
    Total = c(100, 0))
  df <- rbind(max_min, data)
  rownames(df) <- c("Max", "Min", "User")
  
  op <- par(mar = c(1, 1, 1, 1))
  create_beautiful_radarchart(df, caxislabels = c(0, 25, 50, 75, 100))
  par(op)
  dev.off()
}


#####Spider ggradar#####
colnames(user_scores) <- c("user_list", "Recharge", "Move","Eat","Total")
for (n in good_users){
  ggradar(
    user_scores[n, ], 
    values.radar = c("0", "50", "100"),
    grid.min = 0, grid.mid = 50, grid.max = 100,
    # Polygons
    group.line.width = 1, 
    group.point.size = 3,
    group.colours = "#00AFBB",
    pcol = color, pfcol = scales::alpha(color, 0.5), plwd = 2, plty = 1,
    # Customize the grid
    cglcol = "lightblue", cglty = 1, cglwd = 0.8,
    # Background and grid lines
    background.circle.colour = "lightblue",
    gridline.mid.colour = "grey"
  ) -> p
  ggsave(paste0("/Volumes/GoogleDrive/My Drive/Hackathon_Delage/Spider_plot/Plots_User_",n,"_spider.png"), plot=p)
}


#####Spider ggplot#####
scores <- user_scores[n, ]
colnames(scores) <- c("user_list", "Recharge", "Move", "Eat", "Total" )
data <- pivot_longer(scores, cols = c(recharge_score, move_score, eat_score, total_score))

ggplot(data, aes(name, value, group = 1)) +
  geom_polygon(fill = "blue", colour = "blue", alpha = 0.4) +
  geom_point(colour = "blue") +
  coord_radar() +
  ylim(0, 100) +
  theme(axis.text.x = element_text(size=12, family="SF Pro Display"),
        plot.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
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
          legend.title = element_blank()) -> p
  ggsave(paste0("/Volumes/GoogleDrive/My Drive/Hackathon_Delage/Move_plot/Plots_User_",n,"_move.png"), plot=p)
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
  ggsave(paste0("/Volumes/GoogleDrive/My Drive/Hackathon_Delage/Eat_plot/Plots_User_",n,"_eat.png"), plot=p)
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
  ggsave(paste0("/Volumes/GoogleDrive/My Drive/Hackathon_Delage/Recharge_plot/Plots_User_",n,"_recharge.png"), plot=p)
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
  ggsave(paste0("/Volumes/GoogleDrive/My Drive/Hackathon_Delage/Plots_User_",n,".png"), plot=p)#, width=5, height=2.83, dpi=500, limitsize = FALSE)
}


#####Processing#####
full_data <- read.table("/Volumes/GoogleDrive/My Drive/Hackathon_Delage/Summary.txt", header = T, sep="\t")
user_scores <- read.table("/Volumes/GoogleDrive/My Drive/Hackathon_Delage/User_scores.txt", header = T, sep="\t")
user_list <- unique(full_data$ID)
good_users <- c()
for (n in 1:length(user_list)){
  test_data <- full_data [full_data$ID %in% user_list[n],]
  acts <- test_data[test_data$activity_day > "2020.04.01",]$final_activity
  if (c("move") %in% acts & ("recharge") %in% acts & ("eat") %in% acts){
    good_users <- c(good_users, user_list[n])
  }
}


summary_recognized_month <- 
  filter(full_data, final_activity != "None") %>%# & ID %in% good_users) %>% 
  group_by(ID, month, final_activity,recognized_activity) %>% 
  summarise(n = as.double(n()))#, mean = mean(value), ymin = min(value), ymax = max(value))

summary_recognized_year <- 
  filter(full_data, final_activity != "None") %>%# & ID %in% good_users) %>% 
  group_by(ID, month, final_activity,recognized_activity) %>% 
  separate(month, remove = F, sep= " ", into=c("mon", "year")) %>%
  group_by(ID, year, final_activity,recognized_activity) %>%
  summarise(n = as.double(n()))#, mean = mean(value), ymin = min(value), ymax = max(value))

summary_final_month <- 
  filter(full_data, final_activity != "None") %>%# & ID %in% good_users) %>% 
  group_by(ID, month, final_activity) %>% 
  summarise(n = as.double(n()))#, mean = mean(value), ymin = min(value), ymax = max(value))

summary_final_year <- 
  filter(full_data, final_activity != "None" & ID %in% good_users) %>% 
  group_by(ID, month, final_activity) %>% 
  separate(month, remove = F, sep= " ", into=c("mon", "year")) %>%
  group_by(ID, year, final_activity) %>% 
  summarise(n = as.double(n()))#, mean = mean(value), ymin = min(value), ymax = max(value))



summary_recognized_year_prep <-
  filter(full_data, final_activity != "None") %>%# & ID %in% good_users) %>% 
  group_by(ID, month, recognized_activity) %>% 
  separate(month, remove = F, sep= " ", into=c("mon", "year")) %>%
  group_by(ID, year, recognized_activity) %>%
  summarise(n = as.double(n()))#, mean = mean(value), ymin = min(value), ymax = max(value))




#####Merge Years#####
summary_final <-summary_recognized_year_prep %>%
  #group_by(ID, recognized_activity) %>% 
  #filter(year == 2021) %>%
  unite(year, recognized_activity, col="activity_year", sep=" ") %>%
  pivot_wider(names_from = activity_year, values_from = n)



ID <- summary_final$ID
summary_final$ID <- NULL
summary_final_end <- setnafill(summary_final, fill=0)
summary_final_end <- summary_final_end[,1:148]

#summary_final_end <- summary_final_end %>% add_column(ID)
#summary_final_end_df <- data.frame(summary_final_end)
#colnames(summary_final_end_df) <- colnames(summary_final_end)
#write.table(data.frame(summary_final_end_df), paste0("/Volumes/GoogleDrive/My Drive/Hackathon_Delage/Summary_final_end_ID.txt"), sep="\t", quote = F, row.names=FALSE)


umap_results <- umap::umap(summary_final_end)
umap_plot_df <- data.frame(umap_results$layout, ID)
colnames(user_scores) <- c("ID", "recharge_score", "move_score", "eat_score", "total_score")
final_umap <- left_join(umap_plot_df, user_scores, by="ID")

p1 <- ggplot(final_umap, aes(x = X1, y = X2, color=recharge_score))+
  geom_point()+
  scale_color_gradient2(low = muted("red"), mid = "grey65", high = muted("blue"), midpoint = 50)

p2 <- ggplot(final_umap, aes(x = X1, y = X2, color=move_score))+
  geom_point()+
  scale_color_gradient2(low = muted("red"), mid = "grey65", high = muted("blue"), midpoint = 50)

p3 <- ggplot(final_umap, aes(x = X1, y = X2, color=eat_score))+
  geom_point()+
  scale_color_gradient2(low = muted("red"), mid = "grey65", high = muted("blue"), midpoint = 50)

p4 <- ggplot(final_umap, aes(x = X1, y = X2, color=total_score))+
  geom_point()+
  scale_color_gradient2(low = muted("red"), mid = "grey65", high = muted("blue"), midpoint = 50)

p<-ggarrange(p1, p2, p3, p4, ncol=4, legend="right")# common.legend = TRUE)
p2<-annotate_figure(p,
                    top = text_grob("Distribution Health Insurance users_all", color = "black", face = "bold", size = 20))#,
#bottom = text_grob("Data source: Pilot 4", color = "black",
#                   hjust = 1, x = 1, face = "italic", size = 10))#,
ggsave(paste0("/Volumes/GoogleDrive/My Drive/Hackathon_Delage/Final_data_all.pdf"), plot=p2, width=15, height=10, dpi=200, limitsize = FALSE)



tsne_results <-Rtsne(as.matrix(summary_final_end), check_duplicates = FALSE, pca = FALSE, perplexity=30, theta=0.5, dims=2)
tsne_plot_df <- data.frame(tsne_results$Y, ID)
colnames(user_scores) <- c("ID", "recharge_score", "move_score", "eat_score", "total_score")
final_tsne <- left_join(tsne_plot_df, user_scores, by="ID")

p1 <- ggplot(final_tsne, aes(x = X1, y = X2, color=recharge_score))+
  geom_point()+
  scale_color_gradient2(low = muted("red"), mid = "grey65", high = muted("blue"), midpoint = 50)

p2 <- ggplot(final_tsne, aes(x = X1, y = X2, color=move_score))+
  geom_point()+
  scale_color_gradient2(low = muted("red"), mid = "grey65", high = muted("blue"), midpoint = 50)

p3 <- ggplot(final_tsne, aes(x = X1, y = X2, color=eat_score))+
  geom_point()+
  scale_color_gradient2(low = muted("red"), mid = "grey65", high = muted("blue"), midpoint = 50)

p4 <- ggplot(final_tsne, aes(x = X1, y = X2, color=total_score))+
  geom_point()+
  scale_color_gradient2(low = muted("red"), mid = "grey65", high = muted("blue"), midpoint = 50)

p<-ggarrange(p1, p2, p3, p4, ncol=4, legend="right")# common.legend = TRUE)
p2<-annotate_figure(p,
                    top = text_grob("Distribution Health Insurance users", color = "black", face = "bold", size = 20))#,
#bottom = text_grob("Data source: Pilot 4", color = "black",
#                   hjust = 1, x = 1, face = "italic", size = 10))#,
ggsave(paste0("/Volumes/GoogleDrive/My Drive/Hackathon_Delage/Final_data_all_TSNE.pdf"), plot=p2, width=15, height=10, dpi=200, limitsize = FALSE)


#####Individual Years#####

colors <- RColorBrewer::brewer.pal(5, "PuBu")

summary_final_mutli_year <-summary_recognized_year_prep %>%
  #group_by(ID, recognized_activity) %>% 
  #filter(year == 2021) %>%
  #unite(year, recognized_activity, col="activity_year", sep=" ") %>%
  pivot_wider(names_from = recognized_activity, values_from = n)

ID <- summary_final_mutli_year$ID
ID_year <- summary_final_mutli_year$year

summary_final_mutli_year$ID <- NULL
summary_final_mutli_year$year <- NULL
summary_final_mutli_year_end<- setnafill(summary_final_mutli_year, fill=0)
summary_final_mutli_year_end<- summary_final_mutli_year_end[,1:92]


umap_results <- umap::umap(summary_final_mutli_year_end)
umap_plot_df <- data.frame(umap_results$layout, ID, ID_year)
colnames(user_scores) <- c("ID", "recharge_score", "move_score", "eat_score", "total_score")
final_umap <- left_join(umap_plot_df, user_scores, by="ID")

p1 <- ggplot(final_umap, aes(x = X1, y = X2, color=recharge_score))+
  geom_point()+
  scale_color_gradient2(low = muted("red"), mid = "grey65", high = muted("blue"), midpoint = 50)

p2 <- ggplot(final_umap, aes(x = X1, y = X2, color=move_score))+
  geom_point()+
  scale_color_gradient2(low = muted("red"), mid = "grey65", high = muted("blue"), midpoint = 50)

p3 <- ggplot(final_umap, aes(x = X1, y = X2, color=eat_score))+
  geom_point()+
  scale_color_gradient2(low = muted("red"), mid = "grey65", high = muted("blue"), midpoint = 50)

p4 <- ggplot(final_umap, aes(x = X1, y = X2, color=total_score))+
  geom_point()+
  scale_color_gradient2(low = muted("red"), mid = "grey65", high = muted("blue"), midpoint = 50)+
  geom_line(data = ~ head(.x, 20))

test <- final_umap [1:50,]
p5 <- filter(test, ID_year %in% c(2017,2018,2019,2020,2021)) %>%
  ggplot(aes(x = X1, y = X2, color=ID_year, group=ID))+
  geom_point()+
  geom_line()+
  theme(legend.title = element_blank())+
  scale_color_brewer(colors)





p<-ggarrange(p1, p2, p3, p4, p5, legend="right")# common.legend = TRUE)
p2<-annotate_figure(p,
                    top = text_grob("Distribution Health Insurance users_all", color = "black", face = "bold", size = 20))#,
#bottom = text_grob("Data source: Pilot 4", color = "black",
#                   hjust = 1, x = 1, face = "italic", size = 10))#,
ggsave(paste0("/Volumes/GoogleDrive/My Drive/Hackathon_Delage/Final_data_all.pdf"), plot=p2, width=15, height=10, dpi=200, limitsize = FALSE)



tsne_results <-Rtsne(as.matrix(summary_final_mutli_year_end), check_duplicates = FALSE, pca = FALSE, perplexity=30, theta=0.5, dims=2)
tsne_plot_df <- data.frame(tsne_results$Y, ID, ID_year)
colnames(user_scores) <- c("ID", "recharge_score", "move_score", "eat_score", "total_score")
final_tsne <- left_join(tsne_plot_df, user_scores, by="ID")

p1 <- ggplot(final_tsne, aes(x = X1, y = X2, color=recharge_score))+
  geom_point()+
  scale_color_gradient2(low = muted("red"), mid = "grey65", high = muted("blue"), midpoint = 50)

p2 <- ggplot(final_tsne, aes(x = X1, y = X2, color=move_score))+
  geom_point()+
  scale_color_gradient2(low = muted("red"), mid = "grey65", high = muted("blue"), midpoint = 50)

p3 <- ggplot(final_tsne, aes(x = X1, y = X2, color=eat_score))+
  geom_point()+
  scale_color_gradient2(low = muted("red"), mid = "grey65", high = muted("blue"), midpoint = 50)

p4 <- ggplot(final_tsne, aes(x = X1, y = X2, color=total_score))+
  geom_point()+
  scale_color_gradient2(low = muted("red"), mid = "grey65", high = muted("blue"), midpoint = 50)+
  geom_line(data = ~ head(.x, 20))

test <- final_tsne [1:50,]
p5 <- filter(test, ID_year %in% c(2017,2018,2019,2020,2021)) %>%
  ggplot(aes(x = X1, y = X2, color=ID_year, group=ID))+
  geom_point()+
  geom_line()+
  theme(legend.title = element_blank())+
  scale_color_brewer(colors)




p<-ggarrange(p1, p2, p3, p4,p5, legend="right")# common.legend = TRUE)
p2<-annotate_figure(p,
                    top = text_grob("Distribution Health Insurance users", color = "black", face = "bold", size = 20))#,
#bottom = text_grob("Data source: Pilot 4", color = "black",
#                   hjust = 1, x = 1, face = "italic", size = 10))#,
ggsave(paste0("/Volumes/GoogleDrive/My Drive/Hackathon_Delage/Final_data_all_TSNE.pdf"), plot=p2, width=15, height=10, dpi=200, limitsize = FALSE)




