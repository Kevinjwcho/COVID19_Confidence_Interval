library(data.table)
library(dplyr)
library(ggplot2)

setwd("C:/Users/kevin/Dropbox/SNU_medical/corona_virus_est/mortality/data")

plot_dat <- fread("covid_20200630.csv")
str(plot_dat)
plot_dat <- plot_dat[-nrow(plot_dat), ]
plot_dat <- plot_dat %>% mutate(dx_date = as.Date(dx_date,"%Y-%m-%d"))


plot_dat_melt <- plot_dat %>% reshape2::melt(id = 1) %>% mutate(loc_n = variable %>% tstrsplit("_", keep=1) %>% unlist,
                                                                cate = variable %>% tstrsplit("_", keep=2) %>% unlist,
                                                                mil = variable %>% tstrsplit("_", keep=3) %>% unlist)



plot_dat_dx <- plot_dat_melt %>% filter(cate == "dx", is.na(mil))
plot_dat_death <- plot_dat_melt %>% filter(cate == "dth", is.na(mil))
# plot_dat <- plot_dat_melt %>% filter(cum == "no")



# install.packages("patchwork")
library(patchwork)

# p1 <- ggplot(plot_dat_dx, aes(x= dx_date, y=value))+geom_line() + facet_wrap(vars(loc_n))
# p2 <- ggplot(plot_dat_death, aes(x= dx_date, y=value))+geom_line(col = "blue") + facet_wrap(vars(loc_n))
# p1+p2
plot_dat_melt <- plot_dat_melt %>% mutate(value = value %>% as.numeric)

plot_dat_cast <- plot_dat_melt  %>% reshape2::dcast(dx_date + loc_n + mil~ cate, fun.aggregate = sum, na.rm = T)
plot_dat_cast <- plot_dat_cast %>% filter(loc_n != "intl") %>% filter(loc_n != "int")

plot_dat_cast<- plot_dat_cast %>% mutate(loc_n = case_when(loc_n == "busan" ~ "Busan",
                                                           loc_n == "cb" ~ "Chungbuk",
                                                           loc_n == "cn" ~ "Chungnam",
                                                           loc_n == "daegu" ~ "Daegu",
                                                           loc_n == "daejeon" ~ "Daejeon",
                                                           loc_n == "gg" ~ "Gyunggi",
                                                           loc_n == "gw" ~ "Gangwon",
                                                           loc_n == "gwangju" ~ "Gwangju",
                                                           loc_n == "incheon" ~ "Incheon",
                                                           loc_n == "jb" ~ "Jeonbuk",
                                                           loc_n == "jj" ~ "Jeju",
                                                           loc_n == "jn" ~ "Jeonnam",
                                                           loc_n == "kb" ~ "Kyungbuk",
                                                           loc_n == "kn" ~ "Kyungnam",
                                                           loc_n == "sejong" ~ "Sejong",
                                                           loc_n == "seoul" ~ "Seoul",
                                                           loc_n == "ulsan" ~ "Ulsan"
)
)

plot_dat_cast1 <- plot_dat_cast %>% filter(mil %>% is.na)
colors <- c("Death" = "red", "Cases" = "blue")
p<- ggplot(plot_dat_cast1, aes(x = dx_date, color = "Cases"))+geom_line(aes(y=dth*40, color = "Death")) + geom_line(aes(y=dx))+
  scale_y_continuous(
    name = "Cases",
    sec.axis = sec_axis(~.*1/40, name = "Death")
  )+ xlab("Month") + labs(color = "") + scale_color_manual(values = colors)+
  facet_wrap(~loc_n) +
  scale_x_continuous(labels=c("2" = "Feb", "3" = "Mar",
                            "4" = "Apr", "5" = "May",
                            "6" = "June")) +
  theme(axis.text.y.left = element_text(color = "blue"),
        axis.ticks.y.left = element_line(color = "blue"),
        axis.text.y.right = element_text(color = "red"),
        axis.ticks.y.right = element_line(color = "red"),
        axis.title = element_blank(),
        axis.text.x = element_text(angle = -45, vjust = 0.5, hjust = 0.5))

pdf("C:/Users/kevin/Dropbox/SNU_medical/corona_virus_est/mortality/plot_final/covid_loc_death_detect.pdf",
    width = 8, height = 8)
print(p)
dev.off()

### Map contouring -------------------- 10.08

.cran_packages <- c("dplyr", "data.table", "ggplot2", "ggmap", "raster", "rgeos",
                    "maptools", "rgdal", "lubridate", "scales")
.bioc_packages <- c()
.inst <- .cran_packages %in% installed.packages()
if(any(!.inst)) {
  install.packages(.cran_packages[!.inst])
}
.inst <- .bioc_packages %in% installed.packages()
if(any(!.inst)) {
  BiocManager::install(.bioc_packages[!.inst])
}

sapply(c(.cran_packages, .bioc_packages), require, character.only = TRUE)


wd_path <- "C:/Users/kevin/Dropbox/SNU_medical/corona_virus_est/mortality/data"
setwd(wd_path)
korea = shapefile("CTPRVN_202005/CTPRVN.shp")
korea

korea<-spTransform(korea, CRS("+proj=longlat"))
korea_map<-fortify(korea)


# plot_dat_cast %>% mutate(month = month(dx_date)) %>% filter(month == 3, mil == "mil") ## I think of March, but it seems not suitable. there is not many cases.

mon <- c(2:6)
mon_n <- c("FEB", "MAR", "APR", "MAY", "JUN")

for(i in 1: length(mon)){
  map_dat <- plot_dat_cast %>% mutate(month = month(dx_date)) %>% filter(mil == "mil", month == mon[i] ) %>%
    group_by(loc_n) %>% 
    summarise(dth_mean = mean(dth, na.rm = T), dx_mean = mean(dx, na.rm = T))
  
  map_dat <- map_dat %>% mutate(id = case_when(loc_n == "Seoul" ~ 0,
                                               loc_n == "Busan" ~ 1,
                                               loc_n == "Daegu" ~ 2,
                                               loc_n == "Incheon" ~ 3,
                                               loc_n == "Gwangju" ~ 4,
                                               loc_n == "Daejeon" ~ 5,
                                               loc_n == "Ulsan" ~ 6,
                                               loc_n == "Sejong" ~ 7,
                                               loc_n == "Gyunggi" ~ 8,
                                               loc_n == "Gangwon" ~ 9,
                                               loc_n == "Chungbuk" ~ 10,
                                               loc_n == "Chungnam" ~ 11,
                                               loc_n == "Jeonbuk" ~ 12,
                                               loc_n == "Jeonnam" ~ 13,
                                               loc_n == "Kyungbuk" ~ 14,
                                               loc_n == "Kyungnam" ~ 15,
                                               loc_n == "Jeju" ~ 16) %>% as.character())
  
  merge_map <- korea_map %>% left_join(map_dat, by = "id")
  
  
  p <- ggplot() + geom_map(data=merge_map, map = korea_map, aes(x=long, y=lat, fill=dth_mean, map_id = id), color = "black") +
    scale_fill_gradient2(na.value = "transparent", high = muted("red"))+
    # scale_fill_gradient(na.value = "transparent", low = "white", high = "red")+
    theme(legend.position = "right",
          panel.background = element_rect(fill = "transparent"), # bg of the panel
          plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
          panel.grid.major = element_blank(), # get rid of major grid
          panel.grid.minor = element_blank(), # get rid of minor grid
          # legend.background = element_rect(fill = "transparent"), # get rid of legend bg
          # legend.box.background = element_rect(fill = "transparent"), # get rid of legend panel bg)
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.x=element_blank(),
          axis.ticks.y=element_blank()
    )+
    labs(fill = "")+xlab("")+ylab("") + ggtitle(paste0("Deaths per a million (", mon_n[i], ")"))
  
  
  annote <- merge_map %>% group_by(loc_n) %>% summarise(long_m = mean(long),
                                                        lat_m = mean(lat),
                                                        dth_mean = mean(dth_mean),
                                                        dx_mean = mean(dx_mean))
  
  p1 <- p + geom_label(data = annote, aes(x = long_m, y= lat_m,
                                          label = round(dth_mean,3) %>% as.character()),
                       size = 3)
  pdf(paste0("C:/Users/kevin/Dropbox/SNU_medical/corona_virus_est/mortality/plot_final/deaths/", mon_n[i], "_deaths.pdf"),
      width = 8, height = 8)
  print(p1)
  dev.off()
  
  
  p <- ggplot() + geom_map(data=merge_map, map = korea_map, aes(x=long, y=lat, fill=dx_mean, map_id = id), color = "black") +
    scale_fill_gradient2(na.value = "transparent", high = muted("red"), low = muted("orange"))+
    # scale_fill_gradient(na.value = "transparent", low = "white", high = "red")+
    theme(legend.position = "right",
          panel.background = element_rect(fill = "transparent"), # bg of the panel
          plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
          panel.grid.major = element_blank(), # get rid of major grid
          panel.grid.minor = element_blank(), # get rid of minor grid
          # legend.background = element_rect(fill = "transparent"), # get rid of legend bg
          # legend.box.background = element_rect(fill = "transparent"), # get rid of legend panel bg)
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.x=element_blank(),
          axis.ticks.y=element_blank()
    )+
    labs(fill = "")+xlab("")+ylab("") + ggtitle(paste0("Cases per a million (", mon_n[i], ")"))
  
  p1 <- p+geom_label(data = annote, aes(x = long_m, y= lat_m,
                                        label = round(dx_mean,3) %>% as.character()),
                     size = 3)
  pdf(paste0("C:/Users/kevin/Dropbox/SNU_medical/corona_virus_est/mortality/plot_final/cases/", mon_n[i], "_cases.pdf"),
      width = 8, height = 8)
  print(p1)
  dev.off()
  
}


### Total -----------

map_dat <- plot_dat_cast %>% mutate(month = month(dx_date)) %>% filter(mil == "mil") %>%
  group_by(loc_n) %>% summarise(dth_mean = sum(dth, na.rm = T),
                                dx_mean = sum(dx, na.rm = T)) 

map_dat <- map_dat %>% mutate(id = case_when(loc_n == "Seoul" ~ 0,
                                             loc_n == "Busan" ~ 1,
                                             loc_n == "Daegu" ~ 2,
                                             loc_n == "Incheon" ~ 3,
                                             loc_n == "Gwangju" ~ 4,
                                             loc_n == "Daejeon" ~ 5,
                                             loc_n == "Ulsan" ~ 6,
                                             loc_n == "Sejong" ~ 7,
                                             loc_n == "Gyunggi" ~ 8,
                                             loc_n == "Gangwon" ~ 9,
                                             loc_n == "Chungbuk" ~ 10,
                                             loc_n == "Chungnam" ~ 11,
                                             loc_n == "Jeonbuk" ~ 12,
                                             loc_n == "Jeonnam" ~ 13,
                                             loc_n == "Kyungbuk" ~ 14,
                                             loc_n == "Kyungnam" ~ 15,
                                             loc_n == "Jeju" ~ 16) %>% as.character())

merge_map <- korea_map %>% left_join(map_dat, by = "id")


p <- ggplot() + geom_map(data=merge_map, map = korea_map, aes(x=long, y=lat, fill=dth_mean, map_id = id), color = "black") +
  scale_fill_gradient2(na.value = "transparent", high = muted("red"))+
  # scale_fill_gradient(na.value = "transparent", low = "white", high = "red")+
  theme(legend.position = "right",
        panel.background = element_rect(fill = "transparent"), # bg of the panel
        plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
        panel.grid.major = element_blank(), # get rid of major grid
        panel.grid.minor = element_blank(), # get rid of minor grid
        # legend.background = element_rect(fill = "transparent"), # get rid of legend bg
        # legend.box.background = element_rect(fill = "transparent"), # get rid of legend panel bg)
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank()
  )+
  labs(fill = "")+xlab("")+ylab("") + ggtitle("Deaths per a million")


annote <- merge_map %>% group_by(loc_n) %>% summarise(long_m = mean(long),
                                                      lat_m = mean(lat),
                                                      dth_mean = mean(dth_mean),
                                                      dx_mean = mean(dx_mean))

p1 <- p + geom_label(data = annote, aes(x = long_m, y= lat_m,
                                        label = round(dth_mean,3) %>% as.character()),
                     size = 3)
pdf(paste0("C:/Users/kevin/Dropbox/SNU_medical/corona_virus_est/mortality/plot_final/deaths/", "Total", "_deaths.pdf"),
    width = 8, height = 8)
print(p1)
dev.off()


p <- ggplot() + geom_map(data=merge_map, map = korea_map, aes(x=long, y=lat, fill=dx_mean, map_id = id), color = "black") +
  scale_fill_gradient2(na.value = "transparent", high = muted("red"), low = muted("orange"))+
  # scale_fill_gradient(na.value = "transparent", low = "white", high = "red")+
  theme(legend.position = "right",
        panel.background = element_rect(fill = "transparent"), # bg of the panel
        plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
        panel.grid.major = element_blank(), # get rid of major grid
        panel.grid.minor = element_blank(), # get rid of minor grid
        # legend.background = element_rect(fill = "transparent"), # get rid of legend bg
        # legend.box.background = element_rect(fill = "transparent"), # get rid of legend panel bg)
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank()
  )+
  labs(fill = "")+xlab("")+ylab("") + ggtitle(paste0("Cases per a million"))

p1 <- p+geom_label(data = annote, aes(x = long_m, y= lat_m,
                                      label = round(dx_mean,3) %>% as.character()),
                   size = 3)
pdf(paste0("C:/Users/kevin/Dropbox/SNU_medical/corona_virus_est/mortality/plot_final/cases/", "Total", "_cases.pdf"),
    width = 8, height = 8)
print(p1)
dev.off()
