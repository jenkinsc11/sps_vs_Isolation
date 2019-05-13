library(ggplot2)
library(ggpubr)
library(dplyr)
sps_iso_data <- read.csv('Lumos_a0239_0_SPS_PSMs.csv')
sps_iso_data <- na.omit(sps_iso_data)
sps_vec <- vector()

for (row in 1:nrow(sps_iso_data)){
  if (sps_iso_data[row,"SPS.Mass.Matches...."] %in% 0:10) {
    sps_vec <- c(sps_vec, "0:10")
  } else if (sps_iso_data[row,"SPS.Mass.Matches...."] %in% 10:20) {
    sps_vec <- c(sps_vec, "10:20")
  } else if (sps_iso_data[row,"SPS.Mass.Matches...."] %in% 20:30) {
    sps_vec <- c(sps_vec, "20:30")
  } else if (sps_iso_data[row,"SPS.Mass.Matches...."] %in% 30:40) {
    sps_vec <- c(sps_vec, "30:40")
  } else if (sps_iso_data[row,"SPS.Mass.Matches...."] %in% 40:50) {
    sps_vec <- c(sps_vec, "40:50")
  } else if (sps_iso_data[row,"SPS.Mass.Matches...."] %in% 50:60) {
    sps_vec <- c(sps_vec, "50:60")
  } else if (sps_iso_data[row,"SPS.Mass.Matches...."] %in% 60:70) {
    sps_vec <- c(sps_vec, "60:70")
  } else if (sps_iso_data[row,"SPS.Mass.Matches...."] %in% 70:80) {
    sps_vec <- c(sps_vec, "70:80")
  } else if (sps_iso_data[row,"SPS.Mass.Matches...."] %in% 80:90) {
    sps_vec <- c(sps_vec, "80:90")
  } else {
    sps_vec <- c(sps_vec, "90:100")
  }
}

iso_vec <- vector()

for (row in 1:nrow(sps_iso_data)){
  x <- sps_iso_data[row,"Isolation.Interference...."]
  if (between(x,0,10) ) {
    iso_vec <- c(iso_vec, "0:10")
  } else if (between(x,10,20)) {
    iso_vec <- c(iso_vec, "10:20")
  } else if (between(x,20,30)) {
    iso_vec <- c(iso_vec, "20:30")
  } else if (between(x,30,40)) {
    iso_vec <- c(iso_vec, "30:40")
  } else if (between(x,40,50)){
    iso_vec <- c(iso_vec, "40:50")
  } else if (between(x,50,60)) {
      iso_vec <- c(iso_vec, "50:60")
  } else if (between(x,60,70)) {
      iso_vec <- c(iso_vec, "60:70")
  } else if (between(x,70,80)) {
      iso_vec <- c(iso_vec, "70:80")
  } else if (between(x,80,90)){
    iso_vec <- c(iso_vec, "80:90")
  } else if (between(x,90,100)){
    iso_vec <- c(iso_vec, "90:100")
  }
}


final_data <- cbind(iso_vec,sps_vec)

fields <- c("Master.Protein.Accessions","Annotated.Sequence", "Isolation.Interference....","SPS.Mass.Matches....")

minimized_data <- sps_iso_data[fields]

final_data <- cbind(final_data,minimized_data)

ggplot(final_data, aes(sps_vec,iso_vec)) + geom_jitter(aes(color = sps_vec), size = 0.3) + ggpubr::color_palette("jco")+ggpubr::theme_pubclean()+ theme(legend.title = element_blank()) + labs(x = "SPS %",y="Isolation Interference %")
                                                                                              
                                 
bottom_right <- final_data[which (sps_vec == "81:100" & iso_vec == "0:20"),]                                                                                                                               


#write.csv(bottom_right, file = "high_sps_low_isolation_Fusion.csv")


ggplot(final_data, aes(SPS.Mass.Matches...., fill = iso_vec)) + geom_histogram(binwidth = 10) + xlab("SPS Mass Matches (%)") + ylab("Number of PSMs") + guides(fill=guide_legend(title="Isolation interference (%)")) + ggtitle(label = "Histogram of the PSMs")

