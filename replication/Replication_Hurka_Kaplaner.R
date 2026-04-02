#Replication
#Are popular and powerful committees more representative? 
#Evidence from the Ninth European Parliament (Special Issue 2020: EP Elections)
#Steffen Hurka, Constantin Kaplaner

#### Needed packages ####
library(tidyverse)
library(overlapping)
library(truncnorm)
library(dotwhisker)
library(broom)
library(stargazer)
library(gridExtra)

#########################################
############ I - Main Analysis ##########
#########################################

#### 1. - Simulated overlap (Figure 2) ####
#overlap sim
nn <- 1e4
set.seed(1)

#Create bimodal distribution
sims <- c(rtruncnorm(nn/2, a=1, b=5, mean=2, sd=.5),
          rtruncnorm(nn/2, a=1, b=5, mean=4, sd=.5))

set.seed(50)

#Create unimodal distribution
sims2 <- rtruncnorm(nn,a=1, b=5, mean=3, sd=1)
sims2 <- rnorm(10000, mean = 3, sd = 1)

#Calculate overlap
simsover <- overlap(list(sims,sims2))

#Create df for plot
sims2 <- as.data.frame(sims2)
sims2$shape <- "normal"
sims <- as.data.frame(sims)
sims$shape <- "bimodal"

colnames(sims2) <- c("Value","Shape")
colnames(sims) <- c("Value","Shape")

simulation <- bind_rows(sims2,sims)

#Plot Figure 2
ggplot(simulation, aes(x=Value, color=Shape, fill=Shape)) + geom_density(alpha=0.7) + 
  scale_fill_manual(values=c("#1E1E24","#C2C1C2"))+
  scale_color_manual(values=c("black","white"))+
  theme_classic()+
  geom_vline(xintercept=3, linetype="dashed")+
  theme(legend.position = "none")+
  ylab("Density")+
  ggtitle("Overlap = 60%")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text=element_text(family="Times New Roman", size=12)) 
ggsave("Figure2.jpg",width=15,height=10,dpi=800,units="cm")

#### 2. - Load Dataset ####

#Load Dataset
load("ep9.Rdata")

#Create plenum
plenum_ep9 <- ep9 %>% distinct(id, .keep_all = TRUE)

#Filter only members
ep9_member <- filter(ep9, type == "Member")

#Create dataset for plots
ep9_clean <- ep9_member[,c(9,10,17,18)]
ep9_wide <- reshape(ep9_clean, idvar = "id", timevar = "committee_short", direction = "wide")

#Reduce dataset to releveant variables
ep9_sort <- ep9_member[c("lrgen","position","committee_short")]

#Split by committee
ep9_sort <- split(ep9_sort, f=ep9_sort$committee_short)

#### 3. - Representativeness of AFET and PECH (Figure 3) ####
options(warn=-1)
AFET_plot <- ggplot(ep9_wide, aes(x=position.AFET))+ 
  geom_density(data =plenum_ep9, aes(x=position), fill = "black", alpha =0.5) + 
  geom_density(fill="white", alpha = 0.5)+
  theme_classic() +
  xlab("pro/anti EU")+
  ylab("Density")+
  ggtitle("AFET")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text=element_text(family="Times New Roman", size=12)) #Times New Roman, 12pt, Bold

PECH_plot <- ggplot(ep9_wide, aes(x=position.PECH))+ 
  geom_density(data =plenum_ep9, aes(x=position), fill = "black", alpha =0.5) + 
  geom_density(fill="white", alpha = 0.5)+
  theme_classic() +
  xlab("pro/anti EU")+
  ylab("Density")+
  ggtitle("PECH")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text=element_text(family="Times New Roman", size=12)) #Times New Roman, 12pt, Bold

jpeg("Figure3.jpg",width=4200,height = 1700, res=300)
grid.arrange(AFET_plot, PECH_plot, nrow=1)
dev.off()

#### 4. -  Calculate Overlaps ####

#Set boundries for overlap calculation
boundaries_lrgen <- list( from = 0, to = 10) #lrgen is coded from 0 to 10
boundaries_position <- list( from = 1, to = 7) #position is coded from 1 to 7

#### 4.1 - Lrgen Overlap ####
#AFCO
AFCO <- list(na.omit(plenum_ep9$lrgen), na.omit(ep9_sort$AFCO$lrgen))
AFCO_overlap <- overlap(AFCO, plot =TRUE, boundaries = boundaries_lrgen)
AFCO_overlap$OV

AFCO_ov_df <- as.data.frame(AFCO_overlap$OV)
AFCO_ov_df$committee_short <- "AFCO"
AFCO_ov_df$type <- "lrgen"
colnames(AFCO_ov_df) <- c("overlap","committee_short","type")

#AFET
AFET <- list(na.omit(plenum_ep9$lrgen), na.omit(ep9_sort$AFET$lrgen))
AFET_overlap <- overlap(AFET, plot =TRUE, boundaries = boundaries_lrgen)
AFET_overlap$OV

AFET_ov_df <- as.data.frame(AFET_overlap$OV)
AFET_ov_df$committee_short <- "AFET"
AFET_ov_df$type <- "lrgen"
colnames(AFET_ov_df) <- c("overlap","committee_short","type")

#AGRI
AGRI <- list(na.omit(plenum_ep9$lrgen), na.omit(ep9_sort$AGRI$lrgen))
AGRI_overlap <- overlap(AGRI, plot =TRUE, boundaries = boundaries_lrgen)
AGRI_overlap$OV

AGRI_ov_df <- as.data.frame(AGRI_overlap$OV)
AGRI_ov_df$committee_short <- "AGRI"
AGRI_ov_df$type <- "lrgen"
colnames(AGRI_ov_df) <- c("overlap","committee_short","type")

#BUDG
BUDG <- list(na.omit(plenum_ep9$lrgen), na.omit(ep9_sort$BUDG$lrgen))
BUDG_overlap <- overlap(BUDG, plot =TRUE, boundaries = boundaries_lrgen)
BUDG_overlap$OV

BUDG_ov_df <- as.data.frame(BUDG_overlap$OV)
BUDG_ov_df$committee_short <- "BUDG"
BUDG_ov_df$type <- "lrgen"
colnames(BUDG_ov_df) <- c("overlap","committee_short","type")

#CONT
CONT <- list(na.omit(plenum_ep9$lrgen), na.omit(ep9_sort$CONT$lrgen))
CONT_overlap <- overlap(CONT, plot =TRUE, boundaries = boundaries_lrgen)
CONT_overlap$OV

CONT_ov_df <- as.data.frame(CONT_overlap$OV)
CONT_ov_df$committee_short <- "CONT"
CONT_ov_df$type <- "lrgen"
colnames(CONT_ov_df) <- c("overlap","committee_short","type")

#CULT
CULT <- list(na.omit(plenum_ep9$lrgen), na.omit(ep9_sort$CULT$lrgen))
CULT_overlap <- overlap(CULT, plot =TRUE, boundaries = boundaries_lrgen)
CULT_overlap$OV

CULT_ov_df <- as.data.frame(CULT_overlap$OV)
CULT_ov_df$committee_short <- "CULT"
CULT_ov_df$type <- "lrgen"
colnames(CULT_ov_df) <- c("overlap","committee_short","type")

#DEVE
DEVE <- list(na.omit(plenum_ep9$lrgen), na.omit(ep9_sort$DEVE$lrgen))
DEVE_overlap <- overlap(DEVE, plot =TRUE, boundaries = boundaries_lrgen)
DEVE_overlap$OV

DEVE_ov_df <- as.data.frame(DEVE_overlap$OV)
DEVE_ov_df$committee_short <- "DEVE"
DEVE_ov_df$type <- "lrgen"
colnames(DEVE_ov_df) <- c("overlap","committee_short","type")

#ECON
ECON <- list(na.omit(plenum_ep9$lrgen), na.omit(ep9_sort$ECON$lrgen))
ECON_overlap <- overlap(ECON, plot =TRUE, boundaries = boundaries_lrgen)
ECON_overlap$OV

ECON_ov_df <- as.data.frame(ECON_overlap$OV)
ECON_ov_df$committee_short <- "ECON"
ECON_ov_df$type <- "lrgen"
colnames(ECON_ov_df) <- c("overlap","committee_short","type")

#EMPL
EMPL <- list(na.omit(plenum_ep9$lrgen), na.omit(ep9_sort$EMPL$lrgen))
EMPL_overlap <- overlap(EMPL, plot =TRUE, boundaries = boundaries_lrgen)
EMPL_overlap$OV

EMPL_ov_df <- as.data.frame(EMPL_overlap$OV)
EMPL_ov_df$committee_short <- "EMPL"
EMPL_ov_df$type <- "lrgen"
colnames(EMPL_ov_df) <- c("overlap","committee_short","type")

#ENVI
ENVI <- list(na.omit(plenum_ep9$lrgen), na.omit(ep9_sort$ENVI$lrgen))
ENVI_overlap <- overlap(ENVI, plot =TRUE, boundaries = boundaries_lrgen)
ENVI_overlap$OV

ENVI_ov_df <- as.data.frame(ENVI_overlap$OV)
ENVI_ov_df$committee_short <- "ENVI"
ENVI_ov_df$type <- "lrgen"
colnames(ENVI_ov_df) <- c("overlap","committee_short","type")

#FEMM
FEMM <- list(na.omit(plenum_ep9$lrgen), na.omit(ep9_sort$FEMM$lrgen))
FEMM_overlap <- overlap(FEMM, plot =TRUE, boundaries = boundaries_lrgen)
FEMM_overlap$OV

FEMM_ov_df <- as.data.frame(FEMM_overlap$OV)
FEMM_ov_df$committee_short <- "FEMM"
FEMM_ov_df$type <- "lrgen"
colnames(FEMM_ov_df) <- c("overlap","committee_short","type")


#IMCO
IMCO <- list(na.omit(plenum_ep9$lrgen), na.omit(ep9_sort$IMCO$lrgen))
IMCO_overlap <- overlap(IMCO, plot =TRUE, boundaries = boundaries_lrgen)
IMCO_overlap$OV

IMCO_ov_df <- as.data.frame(IMCO_overlap$OV)
IMCO_ov_df$committee_short <- "IMCO"
IMCO_ov_df$type <- "lrgen"
colnames(IMCO_ov_df) <- c("overlap","committee_short","type")

#INTA
INTA <- list(na.omit(plenum_ep9$lrgen), na.omit(ep9_sort$INTA$lrgen))
INTA_overlap <- overlap(INTA, plot =TRUE, boundaries = boundaries_lrgen)
INTA_overlap$OV

INTA_ov_df <- as.data.frame(INTA_overlap$OV)
INTA_ov_df$committee_short <- "INTA"
INTA_ov_df$type <- "lrgen"
colnames(INTA_ov_df) <- c("overlap","committee_short","type")

#ITRE
ITRE <- list(na.omit(plenum_ep9$lrgen), na.omit(ep9_sort$ITRE$lrgen))
ITRE_overlap <- overlap(ITRE, plot =TRUE, boundaries = boundaries_lrgen)
ITRE_overlap$OV

ITRE_ov_df <- as.data.frame(ITRE_overlap$OV)
ITRE_ov_df$committee_short <- "ITRE"
ITRE_ov_df$type <- "lrgen"
colnames(ITRE_ov_df) <- c("overlap","committee_short","type")

#JURI
JURI <- list(na.omit(plenum_ep9$lrgen), na.omit(ep9_sort$JURI$lrgen))
JURI_overlap <- overlap(JURI, plot =TRUE, boundaries = boundaries_lrgen)
JURI_overlap$OV

JURI_ov_df <- as.data.frame(JURI_overlap$OV)
JURI_ov_df$committee_short <- "JURI"
JURI_ov_df$type <- "lrgen"
colnames(JURI_ov_df) <- c("overlap","committee_short","type")

#LIBE
LIBE <- list(na.omit(plenum_ep9$lrgen), na.omit(ep9_sort$LIBE$lrgen))
LIBE_overlap <- overlap(LIBE, plot =TRUE, boundaries = boundaries_lrgen)
LIBE_overlap$OV

LIBE_ov_df <- as.data.frame(LIBE_overlap$OV)
LIBE_ov_df$committee_short <- "LIBE"
LIBE_ov_df$type <- "lrgen"
colnames(LIBE_ov_df) <- c("overlap","committee_short","type")

#PECH
PECH <- list(na.omit(plenum_ep9$lrgen), na.omit(ep9_sort$PECH$lrgen))
PECH_overlap <- overlap(PECH, plot =TRUE, boundaries = boundaries_lrgen)
PECH_overlap$OV

PECH_ov_df <- as.data.frame(PECH_overlap$OV)
PECH_ov_df$committee_short <- "PECH"
PECH_ov_df$type <- "lrgen"
colnames(PECH_ov_df) <- c("overlap","committee_short","type")

#PETI
PETI <- list(na.omit(plenum_ep9$lrgen), na.omit(ep9_sort$PETI$lrgen))
PETI_overlap <- overlap(PETI, plot =TRUE, boundaries = boundaries_lrgen)
PETI_overlap$OV

PETI_ov_df <- as.data.frame(PETI_overlap$OV)
PETI_ov_df$committee_short <- "PETI"
PETI_ov_df$type <- "lrgen"
colnames(PETI_ov_df) <- c("overlap","committee_short","type")

#REGI
REGI <- list(na.omit(plenum_ep9$lrgen), na.omit(ep9_sort$REGI$lrgen))
REGI_overlap <- overlap(REGI, plot =TRUE, boundaries = boundaries_lrgen)
REGI_overlap$OV

REGI_ov_df <- as.data.frame(REGI_overlap$OV)
REGI_ov_df$committee_short <- "REGI"
REGI_ov_df$type <- "lrgen"
colnames(REGI_ov_df) <- c("overlap","committee_short","type")

#TRAN
TRAN <- list(na.omit(plenum_ep9$lrgen), na.omit(ep9_sort$TRAN$lrgen))
TRAN_overlap <- overlap(TRAN, plot =TRUE, boundaries = boundaries_lrgen)
TRAN_overlap$OV

TRAN_ov_df <- as.data.frame(TRAN_overlap$OV)
TRAN_ov_df$committee_short <- "TRAN"
TRAN_ov_df$type <- "lrgen"
colnames(TRAN_ov_df) <- c("overlap","committee_short","type")



#### 4.2 - Position Overlap ####

#AFCO
AFCO_position <- list(na.omit(plenum_ep9$position), na.omit(ep9_sort$AFCO$position))
AFCO_overlap_position <- overlap(AFCO_position , plot =TRUE, boundaries = boundaries_position)
AFCO_overlap_position$OV

AFCO_ov_df_position <- as.data.frame(AFCO_overlap_position$OV)
AFCO_ov_df_position$committee_short <- "AFCO"
AFCO_ov_df_position$type <- "position"
colnames(AFCO_ov_df_position) <- c("overlap_position","committee_short","type")

#AFET
AFET_position  <- list(na.omit(plenum_ep9$position), na.omit(ep9_sort$AFET$position))
AFET_overlap_position <- overlap(AFET_position , plot =TRUE, boundaries = boundaries_position)
AFET_overlap_position$OV

AFET_ov_df_position <- as.data.frame(AFET_overlap_position$OV)
AFET_ov_df_position$committee_short <- "AFET"
AFET_ov_df_position$type <- "position"
colnames(AFET_ov_df_position) <- c("overlap_position","committee_short","type")

#AGRI
AGRI_position  <- list(na.omit(plenum_ep9$position), na.omit(ep9_sort$AGRI$position))
AGRI_overlap_position <- overlap(AGRI_position , plot =TRUE, boundaries = boundaries_position)
AGRI_overlap_position$OV

AGRI_ov_df_position <- as.data.frame(AGRI_overlap_position$OV)
AGRI_ov_df_position$committee_short <- "AGRI"
AGRI_ov_df_position$type <- "position"
colnames(AGRI_ov_df_position) <- c("overlap_position","committee_short","type")

#BUDG
BUDG_position  <- list(na.omit(plenum_ep9$position), na.omit(ep9_sort$BUDG$position))
BUDG_overlap_position <- overlap(BUDG_position , plot =TRUE, boundaries = boundaries_position)
BUDG_overlap_position$OV

BUDG_ov_df_position <- as.data.frame(BUDG_overlap_position$OV)
BUDG_ov_df_position$committee_short <- "BUDG"
BUDG_ov_df_position$type <- "position"
colnames(BUDG_ov_df_position) <- c("overlap_position","committee_short","type")

#CONT
CONT_position  <- list(na.omit(plenum_ep9$position), na.omit(ep9_sort$CONT$position))
CONT_overlap_position <- overlap(CONT_position , plot =TRUE, boundaries = boundaries_position)
CONT_overlap_position$OV

CONT_ov_df_position <- as.data.frame(CONT_overlap_position$OV)
CONT_ov_df_position$committee_short <- "CONT"
CONT_ov_df_position$type <- "position"
colnames(CONT_ov_df_position) <- c("overlap_position","committee_short","type")

#CULT
CULT_position  <- list(na.omit(plenum_ep9$position), na.omit(ep9_sort$CULT$position))
CULT_overlap_position <- overlap(CULT_position , plot =TRUE, boundaries = boundaries_position)
CULT_overlap_position$OV

CULT_ov_df_position <- as.data.frame(CULT_overlap_position$OV)
CULT_ov_df_position$committee_short <- "CULT"
CULT_ov_df_position$type <- "position"
colnames(CULT_ov_df_position) <- c("overlap_position","committee_short","type")

#DEVE
DEVE_position  <- list(na.omit(plenum_ep9$position), na.omit(ep9_sort$DEVE$position))
DEVE_overlap_position <- overlap(DEVE_position , plot =TRUE, boundaries = boundaries_position)
DEVE_overlap_position$OV

DEVE_ov_df_position <- as.data.frame(DEVE_overlap_position$OV)
DEVE_ov_df_position$committee_short <- "DEVE"
DEVE_ov_df_position$type <- "position"
colnames(DEVE_ov_df_position) <- c("overlap_position","committee_short","type")

#ECON
ECON_position  <- list(na.omit(plenum_ep9$position), na.omit(ep9_sort$ECON$position))
ECON_overlap_position <- overlap(ECON_position, plot =TRUE, boundaries = boundaries_position)
ECON_overlap_position$OV

ECON_ov_df_position <- as.data.frame(ECON_overlap_position$OV)
ECON_ov_df_position$committee_short <- "ECON"
ECON_ov_df_position$type <- "position"
colnames(ECON_ov_df_position) <- c("overlap_position","committee_short","type")

#EMPL
EMPL_position  <- list(na.omit(plenum_ep9$position), na.omit(ep9_sort$EMPL$position))
EMPL_overlap_position <- overlap(EMPL_position , plot =TRUE, boundaries = boundaries_position)
EMPL_overlap_position$OV

EMPL_ov_df_position <- as.data.frame(EMPL_overlap_position$OV)
EMPL_ov_df_position$committee_short <- "EMPL"
EMPL_ov_df_position$type <- "position"
colnames(EMPL_ov_df_position) <- c("overlap_position","committee_short","type")

#ENVI
ENVI_position  <- list(na.omit(plenum_ep9$position), na.omit(ep9_sort$ENVI$position))
ENVI_overlap_position <- overlap(ENVI_position , plot =TRUE, boundaries = boundaries_position)
ENVI_overlap_position$OV

ENVI_ov_df_position <- as.data.frame(ENVI_overlap_position$OV)
ENVI_ov_df_position$committee_short <- "ENVI"
ENVI_ov_df_position$type <- "position"
colnames(ENVI_ov_df_position) <- c("overlap_position","committee_short","type")

#FEMM
FEMM_position  <- list(na.omit(plenum_ep9$position), na.omit(ep9_sort$FEMM$position))
FEMM_overlap_position <- overlap(FEMM_position , plot =TRUE, boundaries = boundaries_position)
FEMM_overlap_position$OV

FEMM_ov_df_position <- as.data.frame(FEMM_overlap_position$OV)
FEMM_ov_df_position$committee_short <- "FEMM"
FEMM_ov_df_position$type <- "position"
colnames(FEMM_ov_df_position) <- c("overlap_position","committee_short","type")


#IMCO
IMCO_position  <- list(na.omit(plenum_ep9$position), na.omit(ep9_sort$IMCO$position))
IMCO_overlap_position <- overlap(IMCO_position , plot =TRUE, boundaries = boundaries_position)
IMCO_overlap_position$OV

IMCO_ov_df_position <- as.data.frame(IMCO_overlap_position$OV)
IMCO_ov_df_position$committee_short <- "IMCO"
IMCO_ov_df_position$type <- "position"
colnames(IMCO_ov_df_position) <- c("overlap_position","committee_short","type")

#INTA
INTA_position  <- list(na.omit(plenum_ep9$position), na.omit(ep9_sort$INTA$position))
INTA_overlap_position <- overlap(INTA_position , plot =TRUE, boundaries = boundaries_position)
INTA_overlap_position$OV

INTA_ov_df_position <- as.data.frame(INTA_overlap_position$OV)
INTA_ov_df_position$committee_short <- "INTA"
INTA_ov_df_position$type <- "position"
colnames(INTA_ov_df_position) <- c("overlap_position","committee_short","type")

#ITRE
ITRE_position  <- list(na.omit(plenum_ep9$position), na.omit(ep9_sort$ITRE$position))
ITRE_overlap_position <- overlap(ITRE_position , plot =TRUE, boundaries = boundaries_position)
ITRE_overlap_position$OV

ITRE_ov_df_position <- as.data.frame(ITRE_overlap_position$OV)
ITRE_ov_df_position$committee_short <- "ITRE"
ITRE_ov_df_position$type <- "position"
colnames(ITRE_ov_df_position) <- c("overlap_position","committee_short","type")

#JURI
JURI_position  <- list(na.omit(plenum_ep9$position), na.omit(ep9_sort$JURI$position))
JURI_overlap_position <- overlap(JURI_position , plot =TRUE, boundaries = boundaries_position)
JURI_overlap_position$OV

JURI_ov_df_position <- as.data.frame(JURI_overlap_position$OV)
JURI_ov_df_position$committee_short <- "JURI"
JURI_ov_df_position$type <- "position"
colnames(JURI_ov_df_position) <- c("overlap_position","committee_short","type")

#LIBE
LIBE_position  <- list(na.omit(plenum_ep9$position), na.omit(ep9_sort$LIBE$position))
LIBE_overlap_position <- overlap(LIBE_position , plot =TRUE, boundaries = boundaries_position)
LIBE_overlap_position$OV

LIBE_ov_df_position <- as.data.frame(LIBE_overlap_position$OV)
LIBE_ov_df_position$committee_short <- "LIBE"
LIBE_ov_df_position$type <- "position"
colnames(LIBE_ov_df_position) <- c("overlap_position","committee_short","type")

#PECH
PECH_position  <- list(na.omit(plenum_ep9$position), na.omit(ep9_sort$PECH$position))
PECH_overlap_position <- overlap(PECH_position , plot =TRUE, boundaries = boundaries_position)
PECH_overlap_position$OV

PECH_ov_df_position <- as.data.frame(PECH_overlap_position$OV)
PECH_ov_df_position$committee_short <- "PECH"
PECH_ov_df_position$type <- "position"
colnames(PECH_ov_df_position) <- c("overlap_position","committee_short","type")

#PETI
PETI_position  <- list(na.omit(plenum_ep9$position), na.omit(ep9_sort$PETI$position))
PETI_overlap_position <- overlap(PETI_position , plot =TRUE, boundaries = boundaries_position)
PETI_overlap_position$OV

PETI_ov_df_position <- as.data.frame(PETI_overlap_position$OV)
PETI_ov_df_position$committee_short <- "PETI"
PETI_ov_df_position$type <- "position"
colnames(PETI_ov_df_position) <- c("overlap_position","committee_short","type")

#REGI
REGI_position  <- list(na.omit(plenum_ep9$position), na.omit(ep9_sort$REGI$position))
REGI_overlap_position <- overlap(REGI_position , plot =TRUE, boundaries = boundaries_position)
REGI_overlap_position$OV

REGI_ov_df_position <- as.data.frame(REGI_overlap_position$OV)
REGI_ov_df_position$committee_short <- "REGI"
REGI_ov_df_position$type <- "position"
colnames(REGI_ov_df_position) <- c("overlap_position","committee_short","type")

#TRAN
TRAN_position  <- list(na.omit(plenum_ep9$position), na.omit(ep9_sort$TRAN$position))
TRAN_overlap_position <- overlap(TRAN_position , plot =TRUE, boundaries = boundaries_position)
TRAN_overlap_position$OV

TRAN_ov_df_position <- as.data.frame(TRAN_overlap_position$OV)
TRAN_ov_df_position$committee_short <- "TRAN"
TRAN_ov_df_position$type <- "position"
colnames(TRAN_ov_df_position) <- c("overlap_position","committee_short","type")



#### 4.3 - Create complete Dataset ####

#Combine all estimates
complete_ov_lrgen <- bind_rows(AFCO_ov_df,AFET_ov_df, AGRI_ov_df, BUDG_ov_df, CONT_ov_df, CULT_ov_df, DEVE_ov_df, ECON_ov_df,EMPL_ov_df, ENVI_ov_df, FEMM_ov_df,
                               IMCO_ov_df, INTA_ov_df, ITRE_ov_df, JURI_ov_df, LIBE_ov_df, PECH_ov_df, PETI_ov_df, REGI_ov_df, TRAN_ov_df)

complete_ov_position <- bind_rows(AFCO_ov_df_position,AFET_ov_df_position, AGRI_ov_df_position, BUDG_ov_df_position, CONT_ov_df_position, CULT_ov_df_position, DEVE_ov_df_position, ECON_ov_df_position,EMPL_ov_df_position, ENVI_ov_df_position, FEMM_ov_df_position,
                                  IMCO_ov_df_position, INTA_ov_df_position, ITRE_ov_df_position, JURI_ov_df_position, LIBE_ov_df_position, PECH_ov_df_position, PETI_ov_df_position, REGI_ov_df_position, TRAN_ov_df_position)

#Create df
complete_ov <- merge(complete_ov_lrgen, complete_ov_position, by="committee_short", all.x=TRUE)

#Code popularity
#popularity Whitaker 2019
complete_ov$popularity <- ifelse(complete_ov$committee_short== "AFET",14,NA)
complete_ov$popularity <- ifelse(complete_ov$committee_short== "ITRE",11,complete_ov$popularity)
complete_ov$popularity <- ifelse(complete_ov$committee_short== "ECON",10,complete_ov$popularity)
complete_ov$popularity <- ifelse(complete_ov$committee_short== "ENVI",9,complete_ov$popularity)
complete_ov$popularity <- ifelse(complete_ov$committee_short== "LIBE",9,complete_ov$popularity)
complete_ov$popularity <- ifelse(complete_ov$committee_short== "EMPL",7,complete_ov$popularity)
complete_ov$popularity <- ifelse(complete_ov$committee_short== "TRAN",6,complete_ov$popularity)
complete_ov$popularity <- ifelse(complete_ov$committee_short== "AGRI",6,complete_ov$popularity)
complete_ov$popularity <- ifelse(complete_ov$committee_short== "INTA",6,complete_ov$popularity)
complete_ov$popularity <- ifelse(complete_ov$committee_short== "IMCO",5,complete_ov$popularity)
complete_ov$popularity <- ifelse(complete_ov$committee_short== "REGI",3,complete_ov$popularity)
complete_ov$popularity <- ifelse(complete_ov$committee_short== "CULT",3,complete_ov$popularity)
complete_ov$popularity <- ifelse(complete_ov$committee_short== "BUDG",3,complete_ov$popularity)
complete_ov$popularity <- ifelse(complete_ov$committee_short== "JURI",3,complete_ov$popularity)
complete_ov$popularity <- ifelse(complete_ov$committee_short== "PECH",2,complete_ov$popularity)
complete_ov$popularity <- ifelse(complete_ov$committee_short== "CONT",2,complete_ov$popularity)
complete_ov$popularity <- ifelse(complete_ov$committee_short== "DEVE",1,complete_ov$popularity)
complete_ov$popularity <- ifelse(complete_ov$committee_short== "PETI",1,complete_ov$popularity)
complete_ov$popularity <- ifelse(is.na(complete_ov$popularity), 0,complete_ov$popularity)

#Powerful Yordanova 2009
complete_ov$powerful <- ifelse(complete_ov$committee_short == "BUDG" |
                                 complete_ov$committee_short == "CULT"|
                                 complete_ov$committee_short == "ECON"| 
                                 complete_ov$committee_short == "EMPL"| 
                                 complete_ov$committee_short == "ENVI"| 
                                 complete_ov$committee_short == "IMCO"|
                                 complete_ov$committee_short == "ITRE"|
                                 complete_ov$committee_short == "LIBE"|
                                 complete_ov$committee_short == "TRAN"|
                                 complete_ov$committee_short == "JURI", "powerful","not-powerful")


#Keep only relvant objects
rm(list=setdiff(ls(), c("complete_ov","plenum_ep9","ep9_member","ep9_wide")))

#### 5. - Descriptives (Figure 4 & Figure 5) ####

#Figure 4: Commmittee power and representativeness
ggplot(complete_ov, aes(x=overlap, y=overlap_position, color=powerful)) + 
  geom_text(aes(label=committee_short), show.legend = FALSE)+
  geom_point(size=NA) +
  xlab("Overlap left/right")+
  ylab("Overlap pro/anti EU")+
  theme_classic()+
  scale_color_manual(values=c("#032B43","#F42C04"), name="Power")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom") +
  theme(text=element_text(family="Times New Roman", size=12))+
  guides(colour=guide_legend(override.aes=list(size=4)))
ggsave("Figure4.jpg",width=20,height=25,dpi=800,units="cm")


#Figure 5: Committe popularity and representativeness
ggplot(complete_ov, aes(x=overlap, y=overlap_position, color=popularity)) + geom_text(aes(label=committee_short))+
  xlab("Overlap left/right")+
  ylab("Overlap pro/anti EU")+
  theme_classic()+
  scale_colour_gradientn(colors=c("#032B43","#F42C04"), name="Popularity")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom") +
  theme(text=element_text(family="Times New Roman", size=12))
ggsave("Figure5.jpg",width=20,height=25,dpi=800,units="cm")


#Table EXTRA
overlap_percent <- complete_ov[,c("committee_short","overlap","overlap_position")]
colnames(overlap_percent) <- c("Committee","Overlap left/right","Overlap pro/anti EU")
overlap_percent$`Overlap left/right` <- overlap_percent$`Overlap left/right`*100
overlap_percent$`Overlap pro/anti EU` <- overlap_percent$`Overlap pro/anti EU`*100
stargazer(overlap_percent, summary = F, type = "html", out="TableEXTRA.html", digits = 2)

#### 6. - Regression (Table 1) ####

#Rescale to percentages
reg_base <- complete_ov
reg_base$overlap_position <- reg_base$overlap_position * 100
reg_base$overlap <- reg_base$overlap * 100

#Create rank variable for popularity
reg_base <- reg_base %>% mutate(popularity_rank = rank(desc(popularity), ties.method = "min"))
              
#Model 1
model1 <- lm(overlap ~ popularity, data = reg_base)
summary(model1)

#Model 2
model2 <- lm(overlap ~ popularity_rank, data = reg_base)
summary(model2)

#Model 3
model3 <- lm(overlap ~ powerful, data=reg_base)
summary(model3)

#Model 4
model4 <- lm(overlap_position ~ popularity, data = reg_base)
summary(model4)

#Model 5
model5 <- lm(overlap_position ~ popularity_rank, data = reg_base)
summary(model5)

#Model 6
model6 <- lm(overlap_position ~ powerful, data=reg_base)
summary(model6)

# Table 1
stargazer(model1,model2,model3,model4,model5,model6, 
          type = "html",
          out="Table1.html",
          covariate.labels = c("Committee popularity (Whitaker, 2019)",
                               "Committee popularity, rank",
                               "Committee power (Yordanova 2009)"),
          dep.var.labels=c("left-right","pro-anti EU"),
          keep.stat = c("n","rsq"))

#### 7. - Jackknife (Figure 6 & 7) ####

#Function for jackknife (removes one observation for each regression)
results_popularity <- lapply(1:20, function(i) lm(overlap_position[-i] ~ popularity[-i],data=reg_base))
results_power <- lapply(1:20, function(i) lm(overlap_position[-i] ~ powerful[-i],data=reg_base))

#Add model with all committees
results_popularity <- append(list(model4), results_popularity)
results_power <-  append(list(model6), results_power)

#Add names
names <- c("All committees",
           "Without AFCO", 
           "Without AFET",
           "Without AGRI",
           "Without BUDG",
           "Without CONT",
           "Without CULT",
           "Without DEVE",
           "Without ECON",
           "Without EMPL",
           "Without ENVI",
           "Without FEMM",
           "Without IMCO",
           "Without INTA",
           "Without ITRE",
           "Without JURI",
           "Without LIBE",
           "Without PECH",
           "Without PETI",
           "Without REGI",
           "Without TRAN")

names(results_popularity) <- names
names(results_power) <- names

#Create tidy models
tidy_results_popularity <- lapply(results_popularity, tidy, conf.int = TRUE, conf.level = 0.95)
tidy_results_popularity <-bind_rows(tidy_results_popularity, .id = "model") %>% arrange(desc(model))

tidy_results_power <- lapply(results_power, tidy, conf.int = TRUE, conf.level = 0.95)
tidy_results_power <-bind_rows(tidy_results_power, .id = "model") %>% arrange(desc(model))




#Figure 6
dwplot(tidy_results_popularity, dodge_size = 0.84, by_2sd = FALSE, conf.level=.95) %>% relabel_predictors(`popularity[-i]`="", popularity="") + 
  theme_minimal(base_size = 14) + 
  geom_vline(xintercept=0, linetype = "dashed")+
  guides(colour = guide_legend(ncol=1,reverse = T)) +
  theme(text=element_text(family="Times New Roman", size=12))+
  scale_x_continuous(limits = c(-1.1, 1.1),breaks = scales::pretty_breaks(n = 10)) +
  theme(legend.title = element_blank())
ggsave("Figure6.jpg",width=30,height=20,dpi=800,units="cm")

#Figure 7
dwplot(tidy_results_power, dodge_size = 0.84, conf.level=.95) %>%relabel_predictors(`powerful[-i]powerful`="",powerfulpowerful="") + 
  theme_minimal(base_size = 14) + 
  geom_vline(xintercept=0, linetype = "dashed")+
  guides(colour = guide_legend(reverse=T,ncol=1)) +
  theme(text=element_text(family="Times New Roman", size=12))+
  scale_x_continuous(limits = c(-8, 8),breaks = scales::pretty_breaks(n = 8)) +
  theme(legend.title = element_blank())
ggsave("Figure7.jpg",width=30,height=20,dpi=800,units="cm")

#########################################
############# II. APENDIX ###############
#########################################

#### Party size (Figure A) ####

#Count national party size
party_size <- plenum_ep9 %>% 
  group_by(national_party) %>%
  mutate(count = n())

#Count european party size
party_size <- party_size %>% 
  group_by(european_party) %>%
  mutate(count_eu = n())

#Calculate relative party size on committee level
party_size$relative <- party_size$count / party_size$count_eu
party_size$relative <- ifelse(party_size$relative == 1, 1/742,party_size$relative)
committee_party_size <- party_size %>% group_by(committee_short) %>% summarize(mean_party_size = mean(relative, na.rm = TRUE))
committee_party_size <- merge(committee_party_size, complete_ov, by = "committee_short", all.x = TRUE)

#Figure A
p1 <- ggplot(committee_party_size, aes(x=mean_party_size, y=overlap)) + geom_smooth(method="lm", color ="black")+ 
  geom_point()  + 
  theme_classic()+
  xlab("Mean relative party size in committee")+
  ylab("Overlap left/right")+
  theme(text=element_text(family="Times New Roman", size=12))
  

p2 <- ggplot(committee_party_size, aes(x=mean_party_size, y=overlap_position)) + geom_smooth(method="lm", color ="black")+ 
  geom_point()  + 
  theme_classic()+
  xlab("Mean relative party size in committee")+
  ylab("Overlap pro/anti EU")+
  theme(text=element_text(family="Times New Roman", size=12))

jpeg("FigureA.jpg", width = 4000, height=1600, res=300)
grid.arrange(p2,p1, nrow=1)
dev.off()

#### 1. - Missings ####

#Identify missings NOTE: The initial dataset alraedy includes the estimated positions! 
ep9_member$missing <- ifelse(is.na(ep9_member$lrgen2014) & is.na(ep9_member$lrgen2017),1,0)
ep9_member$missing <- ifelse(is.na(ep9_member$lrgen), 0,ep9_member$missing)

#Dataset for overlap calculation
ep9_member_MIS <-  ep9_member

#Set missings back to NA
ep9_member_MIS$lrgen <- ifelse(is.na(ep9_member_MIS$lrgen2014) & is.na(ep9_member_MIS$lrgen2017), NA, ep9_member_MIS$lrgen )
ep9_member_MIS$position <- ifelse(is.na(ep9_member_MIS$position2014) & is.na(ep9_member_MIS$position2014), NA, ep9_member_MIS$position )

#Set missings back to NA for plenum dataset
plenum_ep9_MIS <- plenum_ep9
plenum_ep9_MIS$position <- ifelse(is.na(plenum_ep9_MIS$position2014) & is.na(plenum_ep9_MIS$position2014), NA, plenum_ep9_MIS$position )
plenum_ep9_MIS$lrgen <- ifelse(is.na(plenum_ep9_MIS$lrgen2014) & is.na(plenum_ep9_MIS$lrgen2017), NA, ep9_member_MIS$lrgen )

#Split for overlap calculation
ep9_sort_MIS <- ep9_member_MIS[c("lrgen","position","committee_short")]
ep9_sort_MIS <- split(ep9_sort_MIS, f=ep9_sort_MIS$committee_short)

#### 1.1 - Descriptives (Table A) ####
#Percentage missing per committee
t1_miss <- ep9_member %>% group_by(committee_short) %>% summarize(mean_missing = mean(missing))

#Total missing per committee
t2_miss <- ep9_member %>% group_by(committee_short) %>% summarize(sum_missing = sum(missing))

missings_committee <- merge(t1_miss, t2_miss, by = "committee_short")

#In percent
missings_committee$mean_missing <-missings_committee$mean_missing*100

#Output Table A
colnames(missings_committee) <- c("Committee","Relative number of MEPs","Total number of MEPs")
stargazer(missings_committee, type="html",out="TableA.html", summary = F)

#### 2. - Regression with missings####

#Set boundries for overlap calculation
boundaries_lrgen <- list( from = 0, to = 10) #lrgen is coded from 0 to 10
boundaries_position <- list( from = 1, to = 7) #position is coded from 1 to 7

#### 2.1 - Missings overlap lrgen ####
#AFCO
AFCO <- list(na.omit(plenum_ep9_MIS$lrgen), na.omit(ep9_sort_MIS$AFCO$lrgen))
AFCO_overlap <- overlap(AFCO, plot =F, boundaries = boundaries_lrgen)
AFCO_overlap$OV

AFCO_ov_df <- as.data.frame(AFCO_overlap$OV)
AFCO_ov_df$committee_short <- "AFCO"
AFCO_ov_df$type <- "lrgen"
colnames(AFCO_ov_df) <- c("overlap","committee_short","type")

#AFET
AFET <- list(na.omit(plenum_ep9_MIS$lrgen), na.omit(ep9_sort_MIS$AFET$lrgen))
AFET_overlap <- overlap(AFET, plot =F, boundaries = boundaries_lrgen)
AFET_overlap$OV

AFET_ov_df <- as.data.frame(AFET_overlap$OV)
AFET_ov_df$committee_short <- "AFET"
AFET_ov_df$type <- "lrgen"
colnames(AFET_ov_df) <- c("overlap","committee_short","type")

#AGRI
AGRI <- list(na.omit(plenum_ep9_MIS$lrgen), na.omit(ep9_sort_MIS$AGRI$lrgen))
AGRI_overlap <- overlap(AGRI, plot =F, boundaries = boundaries_lrgen)
AGRI_overlap$OV

AGRI_ov_df <- as.data.frame(AGRI_overlap$OV)
AGRI_ov_df$committee_short <- "AGRI"
AGRI_ov_df$type <- "lrgen"
colnames(AGRI_ov_df) <- c("overlap","committee_short","type")

#BUDG
BUDG <- list(na.omit(plenum_ep9_MIS$lrgen), na.omit(ep9_sort_MIS$BUDG$lrgen))
BUDG_overlap <- overlap(BUDG, plot =F, boundaries = boundaries_lrgen)
BUDG_overlap$OV

BUDG_ov_df <- as.data.frame(BUDG_overlap$OV)
BUDG_ov_df$committee_short <- "BUDG"
BUDG_ov_df$type <- "lrgen"
colnames(BUDG_ov_df) <- c("overlap","committee_short","type")

#CONT
CONT <- list(na.omit(plenum_ep9_MIS$lrgen), na.omit(ep9_sort_MIS$CONT$lrgen))
CONT_overlap <- overlap(CONT, plot =F, boundaries = boundaries_lrgen)
CONT_overlap$OV

CONT_ov_df <- as.data.frame(CONT_overlap$OV)
CONT_ov_df$committee_short <- "CONT"
CONT_ov_df$type <- "lrgen"
colnames(CONT_ov_df) <- c("overlap","committee_short","type")

#CULT
CULT <- list(na.omit(plenum_ep9_MIS$lrgen), na.omit(ep9_sort_MIS$CULT$lrgen))
CULT_overlap <- overlap(CULT, plot =F, boundaries = boundaries_lrgen)
CULT_overlap$OV

CULT_ov_df <- as.data.frame(CULT_overlap$OV)
CULT_ov_df$committee_short <- "CULT"
CULT_ov_df$type <- "lrgen"
colnames(CULT_ov_df) <- c("overlap","committee_short","type")

#DEVE
DEVE <- list(na.omit(plenum_ep9_MIS$lrgen), na.omit(ep9_sort_MIS$DEVE$lrgen))
DEVE_overlap <- overlap(DEVE, plot =F, boundaries = boundaries_lrgen)
DEVE_overlap$OV

DEVE_ov_df <- as.data.frame(DEVE_overlap$OV)
DEVE_ov_df$committee_short <- "DEVE"
DEVE_ov_df$type <- "lrgen"
colnames(DEVE_ov_df) <- c("overlap","committee_short","type")

#ECON
ECON <- list(na.omit(plenum_ep9_MIS$lrgen), na.omit(ep9_sort_MIS$ECON$lrgen))
ECON_overlap <- overlap(ECON, plot =F, boundaries = boundaries_lrgen)
ECON_overlap$OV

ECON_ov_df <- as.data.frame(ECON_overlap$OV)
ECON_ov_df$committee_short <- "ECON"
ECON_ov_df$type <- "lrgen"
colnames(ECON_ov_df) <- c("overlap","committee_short","type")

#EMPL
EMPL <- list(na.omit(plenum_ep9_MIS$lrgen), na.omit(ep9_sort_MIS$EMPL$lrgen))
EMPL_overlap <- overlap(EMPL, plot =F, boundaries = boundaries_lrgen)
EMPL_overlap$OV

EMPL_ov_df <- as.data.frame(EMPL_overlap$OV)
EMPL_ov_df$committee_short <- "EMPL"
EMPL_ov_df$type <- "lrgen"
colnames(EMPL_ov_df) <- c("overlap","committee_short","type")

#ENVI
ENVI <- list(na.omit(plenum_ep9_MIS$lrgen), na.omit(ep9_sort_MIS$ENVI$lrgen))
ENVI_overlap <- overlap(ENVI, plot =F, boundaries = boundaries_lrgen)
ENVI_overlap$OV

ENVI_ov_df <- as.data.frame(ENVI_overlap$OV)
ENVI_ov_df$committee_short <- "ENVI"
ENVI_ov_df$type <- "lrgen"
colnames(ENVI_ov_df) <- c("overlap","committee_short","type")

#FEMM
FEMM <- list(na.omit(plenum_ep9_MIS$lrgen), na.omit(ep9_sort_MIS$FEMM$lrgen))
FEMM_overlap <- overlap(FEMM, plot =F, boundaries = boundaries_lrgen)
FEMM_overlap$OV

FEMM_ov_df <- as.data.frame(FEMM_overlap$OV)
FEMM_ov_df$committee_short <- "FEMM"
FEMM_ov_df$type <- "lrgen"
colnames(FEMM_ov_df) <- c("overlap","committee_short","type")


#IMCO
IMCO <- list(na.omit(plenum_ep9_MIS$lrgen), na.omit(ep9_sort_MIS$IMCO$lrgen))
IMCO_overlap <- overlap(IMCO, plot =F, boundaries = boundaries_lrgen)
IMCO_overlap$OV

IMCO_ov_df <- as.data.frame(IMCO_overlap$OV)
IMCO_ov_df$committee_short <- "IMCO"
IMCO_ov_df$type <- "lrgen"
colnames(IMCO_ov_df) <- c("overlap","committee_short","type")

#INTA
INTA <- list(na.omit(plenum_ep9_MIS$lrgen), na.omit(ep9_sort_MIS$INTA$lrgen))
INTA_overlap <- overlap(INTA, plot =F, boundaries = boundaries_lrgen)
INTA_overlap$OV

INTA_ov_df <- as.data.frame(INTA_overlap$OV)
INTA_ov_df$committee_short <- "INTA"
INTA_ov_df$type <- "lrgen"
colnames(INTA_ov_df) <- c("overlap","committee_short","type")

#ITRE
ITRE <- list(na.omit(plenum_ep9_MIS$lrgen), na.omit(ep9_sort_MIS$ITRE$lrgen))
ITRE_overlap <- overlap(ITRE, plot =F, boundaries = boundaries_lrgen)
ITRE_overlap$OV

ITRE_ov_df <- as.data.frame(ITRE_overlap$OV)
ITRE_ov_df$committee_short <- "ITRE"
ITRE_ov_df$type <- "lrgen"
colnames(ITRE_ov_df) <- c("overlap","committee_short","type")

#JURI
JURI <- list(na.omit(plenum_ep9_MIS$lrgen), na.omit(ep9_sort_MIS$JURI$lrgen))
JURI_overlap <- overlap(JURI, plot =F, boundaries = boundaries_lrgen)
JURI_overlap$OV

JURI_ov_df <- as.data.frame(JURI_overlap$OV)
JURI_ov_df$committee_short <- "JURI"
JURI_ov_df$type <- "lrgen"
colnames(JURI_ov_df) <- c("overlap","committee_short","type")

#LIBE
LIBE <- list(na.omit(plenum_ep9_MIS$lrgen), na.omit(ep9_sort_MIS$LIBE$lrgen))
LIBE_overlap <- overlap(LIBE, plot =F, boundaries = boundaries_lrgen)
LIBE_overlap$OV

LIBE_ov_df <- as.data.frame(LIBE_overlap$OV)
LIBE_ov_df$committee_short <- "LIBE"
LIBE_ov_df$type <- "lrgen"
colnames(LIBE_ov_df) <- c("overlap","committee_short","type")

#PECH
PECH <- list(na.omit(plenum_ep9_MIS$lrgen), na.omit(ep9_sort_MIS$PECH$lrgen))
PECH_overlap <- overlap(PECH, plot =F, boundaries = boundaries_lrgen)
PECH_overlap$OV

PECH_ov_df <- as.data.frame(PECH_overlap$OV)
PECH_ov_df$committee_short <- "PECH"
PECH_ov_df$type <- "lrgen"
colnames(PECH_ov_df) <- c("overlap","committee_short","type")

#PETI
PETI <- list(na.omit(plenum_ep9_MIS$lrgen), na.omit(ep9_sort_MIS$PETI$lrgen))
PETI_overlap <- overlap(PETI, plot =F, boundaries = boundaries_lrgen)
PETI_overlap$OV

PETI_ov_df <- as.data.frame(PETI_overlap$OV)
PETI_ov_df$committee_short <- "PETI"
PETI_ov_df$type <- "lrgen"
colnames(PETI_ov_df) <- c("overlap","committee_short","type")

#REGI
REGI <- list(na.omit(plenum_ep9_MIS$lrgen), na.omit(ep9_sort_MIS$REGI$lrgen))
REGI_overlap <- overlap(REGI, plot =F, boundaries = boundaries_lrgen)
REGI_overlap$OV

REGI_ov_df <- as.data.frame(REGI_overlap$OV)
REGI_ov_df$committee_short <- "REGI"
REGI_ov_df$type <- "lrgen"
colnames(REGI_ov_df) <- c("overlap","committee_short","type")

#TRAN
TRAN <- list(na.omit(plenum_ep9_MIS$lrgen), na.omit(ep9_sort_MIS$TRAN$lrgen))
TRAN_overlap <- overlap(TRAN, plot =F, boundaries = boundaries_lrgen)
TRAN_overlap$OV

TRAN_ov_df <- as.data.frame(TRAN_overlap$OV)
TRAN_ov_df$committee_short <- "TRAN"
TRAN_ov_df$type <- "lrgen"
colnames(TRAN_ov_df) <- c("overlap","committee_short","type")



#### 2.2 - Missings position overlap ####

#AFCO
AFCO_position <- list(na.omit(plenum_ep9_MIS$position), na.omit(ep9_sort_MIS$AFCO$position))
AFCO_overlap_position <- overlap(AFCO_position , plot =F, boundaries = boundaries_position)
AFCO_overlap_position$OV

AFCO_ov_df_position <- as.data.frame(AFCO_overlap_position$OV)
AFCO_ov_df_position$committee_short <- "AFCO"
AFCO_ov_df_position$type <- "position"
colnames(AFCO_ov_df_position) <- c("overlap_position","committee_short","type")

#AFET
AFET_position  <- list(na.omit(plenum_ep9_MIS$position), na.omit(ep9_sort_MIS$AFET$position))
AFET_overlap_position <- overlap(AFET_position , plot =F, boundaries = boundaries_position)
AFET_overlap_position$OV

AFET_ov_df_position <- as.data.frame(AFET_overlap_position$OV)
AFET_ov_df_position$committee_short <- "AFET"
AFET_ov_df_position$type <- "position"
colnames(AFET_ov_df_position) <- c("overlap_position","committee_short","type")

#AGRI
AGRI_position  <- list(na.omit(plenum_ep9_MIS$position), na.omit(ep9_sort_MIS$AGRI$position))
AGRI_overlap_position <- overlap(AGRI_position , plot =F, boundaries = boundaries_position)
AGRI_overlap_position$OV

AGRI_ov_df_position <- as.data.frame(AGRI_overlap_position$OV)
AGRI_ov_df_position$committee_short <- "AGRI"
AGRI_ov_df_position$type <- "position"
colnames(AGRI_ov_df_position) <- c("overlap_position","committee_short","type")

#BUDG
BUDG_position  <- list(na.omit(plenum_ep9_MIS$position), na.omit(ep9_sort_MIS$BUDG$position))
BUDG_overlap_position <- overlap(BUDG_position , plot =F, boundaries = boundaries_position)
BUDG_overlap_position$OV

BUDG_ov_df_position <- as.data.frame(BUDG_overlap_position$OV)
BUDG_ov_df_position$committee_short <- "BUDG"
BUDG_ov_df_position$type <- "position"
colnames(BUDG_ov_df_position) <- c("overlap_position","committee_short","type")

#CONT
CONT_position  <- list(na.omit(plenum_ep9_MIS$position), na.omit(ep9_sort_MIS$CONT$position))
CONT_overlap_position <- overlap(CONT_position , plot =F, boundaries = boundaries_position)
CONT_overlap_position$OV

CONT_ov_df_position <- as.data.frame(CONT_overlap_position$OV)
CONT_ov_df_position$committee_short <- "CONT"
CONT_ov_df_position$type <- "position"
colnames(CONT_ov_df_position) <- c("overlap_position","committee_short","type")

#CULT
CULT_position  <- list(na.omit(plenum_ep9_MIS$position), na.omit(ep9_sort_MIS$CULT$position))
CULT_overlap_position <- overlap(CULT_position , plot =F, boundaries = boundaries_position)
CULT_overlap_position$OV

CULT_ov_df_position <- as.data.frame(CULT_overlap_position$OV)
CULT_ov_df_position$committee_short <- "CULT"
CULT_ov_df_position$type <- "position"
colnames(CULT_ov_df_position) <- c("overlap_position","committee_short","type")

#DEVE
DEVE_position  <- list(na.omit(plenum_ep9_MIS$position), na.omit(ep9_sort_MIS$DEVE$position))
DEVE_overlap_position <- overlap(DEVE_position , plot =F, boundaries = boundaries_position)
DEVE_overlap_position$OV

DEVE_ov_df_position <- as.data.frame(DEVE_overlap_position$OV)
DEVE_ov_df_position$committee_short <- "DEVE"
DEVE_ov_df_position$type <- "position"
colnames(DEVE_ov_df_position) <- c("overlap_position","committee_short","type")

#ECON
ECON_position  <- list(na.omit(plenum_ep9_MIS$position), na.omit(ep9_sort_MIS$ECON$position))
ECON_overlap_position <- overlap(ECON_position, plot =F, boundaries = boundaries_position)
ECON_overlap_position$OV

ECON_ov_df_position <- as.data.frame(ECON_overlap_position$OV)
ECON_ov_df_position$committee_short <- "ECON"
ECON_ov_df_position$type <- "position"
colnames(ECON_ov_df_position) <- c("overlap_position","committee_short","type")

#EMPL
EMPL_position  <- list(na.omit(plenum_ep9_MIS$position), na.omit(ep9_sort_MIS$EMPL$position))
EMPL_overlap_position <- overlap(EMPL_position , plot =F, boundaries = boundaries_position)
EMPL_overlap_position$OV

EMPL_ov_df_position <- as.data.frame(EMPL_overlap_position$OV)
EMPL_ov_df_position$committee_short <- "EMPL"
EMPL_ov_df_position$type <- "position"
colnames(EMPL_ov_df_position) <- c("overlap_position","committee_short","type")

#ENVI
ENVI_position  <- list(na.omit(plenum_ep9_MIS$position), na.omit(ep9_sort_MIS$ENVI$position))
ENVI_overlap_position <- overlap(ENVI_position , plot =F, boundaries = boundaries_position)
ENVI_overlap_position$OV

ENVI_ov_df_position <- as.data.frame(ENVI_overlap_position$OV)
ENVI_ov_df_position$committee_short <- "ENVI"
ENVI_ov_df_position$type <- "position"
colnames(ENVI_ov_df_position) <- c("overlap_position","committee_short","type")

#FEMM
FEMM_position  <- list(na.omit(plenum_ep9_MIS$position), na.omit(ep9_sort_MIS$FEMM$position))
FEMM_overlap_position <- overlap(FEMM_position , plot =F, boundaries = boundaries_position)
FEMM_overlap_position$OV

FEMM_ov_df_position <- as.data.frame(FEMM_overlap_position$OV)
FEMM_ov_df_position$committee_short <- "FEMM"
FEMM_ov_df_position$type <- "position"
colnames(FEMM_ov_df_position) <- c("overlap_position","committee_short","type")


#IMCO
IMCO_position  <- list(na.omit(plenum_ep9_MIS$position), na.omit(ep9_sort_MIS$IMCO$position))
IMCO_overlap_position <- overlap(IMCO_position , plot =F, boundaries = boundaries_position)
IMCO_overlap_position$OV

IMCO_ov_df_position <- as.data.frame(IMCO_overlap_position$OV)
IMCO_ov_df_position$committee_short <- "IMCO"
IMCO_ov_df_position$type <- "position"
colnames(IMCO_ov_df_position) <- c("overlap_position","committee_short","type")

#INTA
INTA_position  <- list(na.omit(plenum_ep9_MIS$position), na.omit(ep9_sort_MIS$INTA$position))
INTA_overlap_position <- overlap(INTA_position , plot =F, boundaries = boundaries_position)
INTA_overlap_position$OV

INTA_ov_df_position <- as.data.frame(INTA_overlap_position$OV)
INTA_ov_df_position$committee_short <- "INTA"
INTA_ov_df_position$type <- "position"
colnames(INTA_ov_df_position) <- c("overlap_position","committee_short","type")

#ITRE
ITRE_position  <- list(na.omit(plenum_ep9_MIS$position), na.omit(ep9_sort_MIS$ITRE$position))
ITRE_overlap_position <- overlap(ITRE_position , plot =F, boundaries = boundaries_position)
ITRE_overlap_position$OV

ITRE_ov_df_position <- as.data.frame(ITRE_overlap_position$OV)
ITRE_ov_df_position$committee_short <- "ITRE"
ITRE_ov_df_position$type <- "position"
colnames(ITRE_ov_df_position) <- c("overlap_position","committee_short","type")

#JURI
JURI_position  <- list(na.omit(plenum_ep9_MIS$position), na.omit(ep9_sort_MIS$JURI$position))
JURI_overlap_position <- overlap(JURI_position , plot =F, boundaries = boundaries_position)
JURI_overlap_position$OV

JURI_ov_df_position <- as.data.frame(JURI_overlap_position$OV)
JURI_ov_df_position$committee_short <- "JURI"
JURI_ov_df_position$type <- "position"
colnames(JURI_ov_df_position) <- c("overlap_position","committee_short","type")

#LIBE
LIBE_position  <- list(na.omit(plenum_ep9_MIS$position), na.omit(ep9_sort_MIS$LIBE$position))
LIBE_overlap_position <- overlap(LIBE_position , plot =F, boundaries = boundaries_position)
LIBE_overlap_position$OV

LIBE_ov_df_position <- as.data.frame(LIBE_overlap_position$OV)
LIBE_ov_df_position$committee_short <- "LIBE"
LIBE_ov_df_position$type <- "position"
colnames(LIBE_ov_df_position) <- c("overlap_position","committee_short","type")

#PECH
PECH_position  <- list(na.omit(plenum_ep9_MIS$position), na.omit(ep9_sort_MIS$PECH$position))
PECH_overlap_position <- overlap(PECH_position , plot =F, boundaries = boundaries_position)
PECH_overlap_position$OV

PECH_ov_df_position <- as.data.frame(PECH_overlap_position$OV)
PECH_ov_df_position$committee_short <- "PECH"
PECH_ov_df_position$type <- "position"
colnames(PECH_ov_df_position) <- c("overlap_position","committee_short","type")

#PETI
PETI_position  <- list(na.omit(plenum_ep9_MIS$position), na.omit(ep9_sort_MIS$PETI$position))
PETI_overlap_position <- overlap(PETI_position , plot =F, boundaries = boundaries_position)
PETI_overlap_position$OV

PETI_ov_df_position <- as.data.frame(PETI_overlap_position$OV)
PETI_ov_df_position$committee_short <- "PETI"
PETI_ov_df_position$type <- "position"
colnames(PETI_ov_df_position) <- c("overlap_position","committee_short","type")

#REGI
REGI_position  <- list(na.omit(plenum_ep9_MIS$position), na.omit(ep9_sort_MIS$REGI$position))
REGI_overlap_position <- overlap(REGI_position , plot =F, boundaries = boundaries_position)
REGI_overlap_position$OV

REGI_ov_df_position <- as.data.frame(REGI_overlap_position$OV)
REGI_ov_df_position$committee_short <- "REGI"
REGI_ov_df_position$type <- "position"
colnames(REGI_ov_df_position) <- c("overlap_position","committee_short","type")

#TRAN
TRAN_position  <- list(na.omit(plenum_ep9_MIS$position), na.omit(ep9_sort_MIS$TRAN$position))
TRAN_overlap_position <- overlap(TRAN_position , plot =F, boundaries = boundaries_position)
TRAN_overlap_position$OV

TRAN_ov_df_position <- as.data.frame(TRAN_overlap_position$OV)
TRAN_ov_df_position$committee_short <- "TRAN"
TRAN_ov_df_position$type <- "position"
colnames(TRAN_ov_df_position) <- c("overlap_position","committee_short","type")


#### 2.3 - Missings Dataset #####
complete_ov_lrgen_MIS <- bind_rows(AFCO_ov_df,AFET_ov_df, AGRI_ov_df, BUDG_ov_df, CONT_ov_df, CULT_ov_df, DEVE_ov_df, ECON_ov_df,EMPL_ov_df, ENVI_ov_df, FEMM_ov_df,
                               IMCO_ov_df, INTA_ov_df, ITRE_ov_df, JURI_ov_df, LIBE_ov_df, PECH_ov_df, PETI_ov_df, REGI_ov_df, TRAN_ov_df)

complete_ov_position_MIS <- bind_rows(AFCO_ov_df_position,AFET_ov_df_position, AGRI_ov_df_position, BUDG_ov_df_position, CONT_ov_df_position, CULT_ov_df_position, DEVE_ov_df_position, ECON_ov_df_position,EMPL_ov_df_position, ENVI_ov_df_position, FEMM_ov_df_position,
                                  IMCO_ov_df_position, INTA_ov_df_position, ITRE_ov_df_position, JURI_ov_df_position, LIBE_ov_df_position, PECH_ov_df_position, PETI_ov_df_position, REGI_ov_df_position, TRAN_ov_df_position)


complete_ov_MIS <- merge(complete_ov_lrgen_MIS, complete_ov_position_MIS, by="committee_short", all.x=TRUE)

rm(list=setdiff(ls(), c("complete_ov","complete_ov_MIS", "ep9_wide","plenum_ep9","ep9_member","ep9_member_MIS",
                        "model1","model2","model3","model4","model5","model6","reg_base",
                        "results_popularity","results_power")))


#Prepare regression data
reg_base_MIS <- complete_ov_MIS
reg_base_MIS$overlap <- reg_base_MIS$overlap * 100
reg_base_MIS$overlap_position <- reg_base_MIS$overlap_position * 100
reg_base_MIS$powerful <- reg_base$powerful
reg_base_MIS$popularity <- reg_base$popularity
reg_base_MIS$popularity_rank <- reg_base$popularity_rank

#####2.4 - Regression models (Table B) ###

#Model 1
model1_MIS <- lm(overlap ~ popularity, data = reg_base_MIS)
summary(model1_MIS)

#Model 2
model2_MIS <- lm(overlap ~ popularity_rank, data = reg_base_MIS)
summary(model2_MIS)

#Model 3
model3_MIS <- lm(overlap ~ powerful, data=reg_base_MIS)
summary(model3_MIS)

#Model 4
model4_MIS <- lm(overlap_position ~ popularity, data = reg_base_MIS)
summary(model4_MIS)

#Model 5
model5_MIS <- lm(overlap_position ~ popularity_rank, data = reg_base_MIS)
summary(model5_MIS)

#Model 6
model6_MIS <- lm(overlap_position ~ powerful, data=reg_base_MIS)
summary(model6_MIS)

stargazer(model1_MIS,model2_MIS,model3_MIS,model4_MIS,model5_MIS,model6_MIS, 
          type = "html",
          out="TableB.html",
          covariate.labels = c("Committee popularity (Whitaker, 2019)",
                               "Committee popularity, rank",
                               "Committee power (Yordanova 2009)"),
          dep.var.labels=c("left-right","pro-anti EU"),
          keep.stat = c("n","rsq"))

#### 3 - Densities for all committees (Figure C & D) ####


##### 3.1 - Figure C includes the following plots: ######
jpeg("eng_plenum_lrgen_AFCO.jpg",width=2100,height = 1700, res=300)
ggplot(ep9_wide, aes(x=lrgen.AFCO))+ 
  geom_density(data =plenum_ep9, aes(x=lrgen), fill = "black", alpha =0.5) + 
  geom_density(fill="white", alpha = 0.5)+
  theme_classic() +
  xlab("left/right")+
  ylab("Density")+
  ggtitle("AFCO")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text=element_text(family="Times New Roman", size=12)) #Times New Roman, 12pt, Bold
dev.off()

jpeg("eng_plenum_lrgen_AFET.jpg",width=2100,height = 1700, res=300)
ggplot(ep9_wide, aes(x=lrgen.AFET))+ 
  geom_density(data =plenum_ep9, aes(x=lrgen), fill = "black", alpha =0.5) + 
  geom_density(fill="white", alpha = 0.5)+
  theme_classic() +
  xlab("left/right")+
  ylab("Density")+
  ggtitle("AFET")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text=element_text(family="Times New Roman", size=12)) #Times New Roman, 12pt, Bold
dev.off()

jpeg("eng_plenum_lrgen_AGRI.jpg",width=2100,height = 1700, res=300)
ggplot(ep9_wide, aes(x=lrgen.AGRI))+ 
  geom_density(data =plenum_ep9, aes(x=lrgen), fill = "black", alpha =0.5) + 
  geom_density(fill="white", alpha = 0.5)+
  theme_classic() +
  xlab("left/right")+
  ylab("Density")+
  ggtitle("AGRI")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text=element_text(family="Times New Roman", size=12)) #Times New Roman, 12pt, Bold
dev.off()

jpeg("eng_plenum_lrgen_BUDG.jpg",width=2100,height = 1700, res=300)
ggplot(ep9_wide, aes(x=lrgen.BUDG))+ 
  geom_density(data =plenum_ep9, aes(x=lrgen), fill = "black", alpha =0.5) + 
  geom_density(fill="white", alpha = 0.5)+
  theme_classic() +
  xlab("left/right")+
  ylab("Density")+
  ggtitle("BUDG")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text=element_text(family="Times New Roman", size=12)) #Times New Roman, 12pt, Bold
dev.off()

jpeg("eng_plenum_lrgen_CONT.jpg",width=2100,height = 1700, res=300)
ggplot(ep9_wide, aes(x=lrgen.CONT))+ 
  geom_density(data =plenum_ep9, aes(x=lrgen), fill = "black", alpha =0.5) + 
  geom_density(fill="white", alpha = 0.5)+
  theme_classic() +
  xlab("left/right")+
  ylab("Density")+
  ggtitle("CONT")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text=element_text(family="Times New Roman", size=12)) #Times New Roman, 12pt, Bold
dev.off()

jpeg("eng_plenum_lrgen_CULT.jpg",width=2100,height = 1700, res=300)
ggplot(ep9_wide, aes(x=lrgen.CULT))+ 
  geom_density(data =plenum_ep9, aes(x=lrgen), fill = "black", alpha =0.5) + 
  geom_density(fill="white", alpha = 0.5)+
  theme_classic() +
  xlab("left/right")+
  ylab("Density")+
  ggtitle("CULT")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text=element_text(family="Times New Roman", size=12)) #Times New Roman, 12pt, Bold
dev.off()

jpeg("eng_plenum_lrgen_DEVE.jpg",width=2100,height = 1700, res=300)
ggplot(ep9_wide, aes(x=lrgen.DEVE))+ 
  geom_density(data =plenum_ep9, aes(x=lrgen), fill = "black", alpha =0.5) + 
  geom_density(fill="white", alpha = 0.5)+
  theme_classic() +
  xlab("left/right")+
  ylab("Density")+
  ggtitle("DEVE")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text=element_text(family="Times New Roman", size=12)) #Times New Roman, 12pt, Bold
dev.off()

jpeg("eng_plenum_lrgen_ECON.jpg",width=2100,height = 1700, res=300)
ggplot(ep9_wide, aes(x=lrgen.ECON))+ 
  geom_density(data =plenum_ep9, aes(x=lrgen), fill = "black", alpha =0.5) + 
  geom_density(fill="white", alpha = 0.5)+
  theme_classic() +
  xlab("left/right")+
  ylab("Density")+
  ggtitle("ECON")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text=element_text(family="Times New Roman", size=12)) #Times New Roman, 12pt, Bold
dev.off()

jpeg("eng_plenum_lrgen_EMPL.jpg",width=2100,height = 1700, res=300)
ggplot(ep9_wide, aes(x=lrgen.EMPL))+ 
  geom_density(data =plenum_ep9, aes(x=lrgen), fill = "black", alpha =0.5) + 
  geom_density(fill="white", alpha = 0.5)+
  theme_classic() +
  xlab("left/right")+
  ylab("Density")+
  ggtitle("EMPL")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text=element_text(family="Times New Roman", size=12)) #Times New Roman, 12pt, Bold
dev.off()

jpeg("eng_plenum_lrgen_ENVI.jpg",width=2100,height = 1700, res=300)
ggplot(ep9_wide, aes(x=lrgen.ENVI))+ 
  geom_density(data =plenum_ep9, aes(x=lrgen), fill = "black", alpha =0.5) + 
  geom_density(fill="white", alpha = 0.5)+
  theme_classic() +
  xlab("left/right")+
  ylab("Density")+
  ggtitle("ENVI")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text=element_text(family="Times New Roman", size=12)) #Times New Roman, 12pt, Bold
dev.off()

jpeg("eng_plenum_lrgen_FEMM.jpg",width=2100,height = 1700, res=300)
ggplot(ep9_wide, aes(x=lrgen.FEMM))+ 
  geom_density(data =plenum_ep9, aes(x=lrgen), fill = "black", alpha =0.5) + 
  geom_density(fill="white", alpha = 0.5)+
  theme_classic() +
  xlab("left/right")+
  ylab("Density")+
  ggtitle("FEMM")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text=element_text(family="Times New Roman", size=12)) #Times New Roman, 12pt, Bold
dev.off()

jpeg("eng_plenum_lrgen_IMCO.jpg",width=2100,height = 1700, res=300)
ggplot(ep9_wide, aes(x=lrgen.IMCO))+ 
  geom_density(data =plenum_ep9, aes(x=lrgen), fill = "black", alpha =0.5) + 
  geom_density(fill="white", alpha = 0.5)+
  theme_classic() +
  xlab("left/right")+
  ylab("Density")+
  ggtitle("IMCO")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text=element_text(family="Times New Roman", size=12)) #Times New Roman, 12pt, Bold
dev.off()

jpeg("eng_plenum_lrgen_INTA.jpg",width=2100,height = 1700, res=300)
ggplot(ep9_wide, aes(x=lrgen.INTA))+ 
  geom_density(data =plenum_ep9, aes(x=lrgen), fill = "black", alpha =0.5) + 
  geom_density(fill="white", alpha = 0.5)+
  theme_classic() +
  xlab("left/right")+
  ylab("Density")+
  ggtitle("INTA")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text=element_text(family="Times New Roman", size=12)) #Times New Roman, 12pt, Bold
dev.off()

jpeg("eng_plenum_lrgen_ITRE.jpg",width=2100,height = 1700, res=300)
ggplot(ep9_wide, aes(x=lrgen.ITRE))+ 
  geom_density(data =plenum_ep9, aes(x=lrgen), fill = "black", alpha =0.5) + 
  geom_density(fill="white", alpha = 0.5)+
  theme_classic() +
  xlab("left/right")+
  ylab("Density")+
  ggtitle("ITRE")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text=element_text(family="Times New Roman", size=12)) #Times New Roman, 12pt, Bold
dev.off()

jpeg("eng_plenum_lrgen_JURI.jpg",width=2100,height = 1700, res=300)
ggplot(ep9_wide, aes(x=lrgen.JURI))+ 
  geom_density(data =plenum_ep9, aes(x=lrgen), fill = "black", alpha =0.5) + 
  geom_density(fill="white", alpha = 0.5)+
  theme_classic() +
  xlab("left/right")+
  ylab("Density")+
  ggtitle("JURI")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text=element_text(family="Times New Roman", size=12)) #Times New Roman, 12pt, Bold
dev.off()

jpeg("eng_plenum_lrgen_LIBE.jpg",width=2100,height = 1700, res=300)
ggplot(ep9_wide, aes(x=lrgen.LIBE))+ 
  geom_density(data =plenum_ep9, aes(x=lrgen), fill = "black", alpha =0.5) + 
  geom_density(fill="white", alpha = 0.5)+
  theme_classic() +
  xlab("left/right")+
  ylab("Density")+
  ggtitle("LIBE")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text=element_text(family="Times New Roman", size=12)) #Times New Roman, 12pt, Bold
dev.off()

jpeg("eng_plenum_lrgen_PECH.jpg",width=2100,height = 1700, res=300)
ggplot(ep9_wide, aes(x=lrgen.PECH))+ 
  geom_density(data =plenum_ep9, aes(x=lrgen), fill = "black", alpha =0.5) + 
  geom_density(fill="white", alpha = 0.5)+
  theme_classic() +
  xlab("left/right")+
  ylab("Density")+
  ggtitle("PECH")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text=element_text(family="Times New Roman", size=12)) #Times New Roman, 12pt, Bold
dev.off()

jpeg("eng_plenum_lrgen_PETI.jpg",width=2100,height = 1700, res=300)
ggplot(ep9_wide, aes(x=lrgen.PETI))+ 
  geom_density(data =plenum_ep9, aes(x=lrgen), fill = "black", alpha =0.5) + 
  geom_density(fill="white", alpha = 0.5)+
  theme_classic() +
  xlab("left/right")+
  ylab("Density")+
  ggtitle("PETI")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text=element_text(family="Times New Roman", size=12)) #Times New Roman, 12pt, Bold
dev.off()

jpeg("eng_plenum_lrgen_REGI.jpg",width=2100,height = 1700, res=300)
ggplot(ep9_wide, aes(x=lrgen.REGI))+ 
  geom_density(data =plenum_ep9, aes(x=lrgen), fill = "black", alpha =0.5) + 
  geom_density(fill="white", alpha = 0.5)+
  theme_classic() +
  xlab("left/right")+
  ylab("Density")+
  ggtitle("REGI")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text=element_text(family="Times New Roman", size=12)) #Times New Roman, 12pt, Bold
dev.off()

jpeg("eng_plenum_lrgen_TRAN.jpg",width=2100,height = 1700, res=300)
ggplot(ep9_wide, aes(x=lrgen.TRAN))+ 
  geom_density(data =plenum_ep9, aes(x=lrgen), fill = "black", alpha =0.5) + 
  geom_density(fill="white", alpha = 0.5)+
  theme_classic() +
  xlab("left/right")+
  ylab("Density")+
  ggtitle("TRAN")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text=element_text(family="Times New Roman", size=12)) #Times New Roman, 12pt, Bold
dev.off()


#### 3.2 - Figure D includes the following plots #####

jpeg("eng_plenum_position_AFCO.jpg",width=2100,height = 1700, res=300)
ggplot(ep9_wide, aes(x=position.AFCO))+ 
  geom_density(data =plenum_ep9, aes(x=position), fill = "black", alpha =0.5) + 
  geom_density(fill="white", alpha = 0.5)+
  theme_classic() +
  xlab("pro/anti EU")+
  ylab("Density")+
  ggtitle("AFCO")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text=element_text(family="Times New Roman", size=12)) #Times New Roman, 12pt, Bold
dev.off()

jpeg("eng_plenum_position_AFET.jpg",width=2100,height = 1700, res=300)
ggplot(ep9_wide, aes(x=position.AFET))+ 
  geom_density(data =plenum_ep9, aes(x=position), fill = "black", alpha =0.5) + 
  geom_density(fill="white", alpha = 0.5)+
  theme_classic() +
  xlab("pro/anti EU")+
  ylab("Density")+
  ggtitle("AFET")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text=element_text(family="Times New Roman", size=12)) #Times New Roman, 12pt, Bold
dev.off()

jpeg("eng_plenum_position_AGRI.jpg",width=2100,height = 1700, res=300)
ggplot(ep9_wide, aes(x=position.AGRI))+ 
  geom_density(data =plenum_ep9, aes(x=position), fill = "black", alpha =0.5) + 
  geom_density(fill="white", alpha = 0.5)+
  theme_classic() +
  xlab("pro/anti EU")+
  ylab("Density")+
  ggtitle("AGRI")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text=element_text(family="Times New Roman", size=12)) #Times New Roman, 12pt, Bold
dev.off()

jpeg("eng_plenum_position_BUDG.jpg",width=2100,height = 1700, res=300)
ggplot(ep9_wide, aes(x=position.BUDG))+ 
  geom_density(data =plenum_ep9, aes(x=position), fill = "black", alpha =0.5) + 
  geom_density(fill="white", alpha = 0.5)+
  theme_classic() +
  xlab("pro/anti EU")+
  ylab("Density")+
  ggtitle("BUDG")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text=element_text(family="Times New Roman", size=12)) #Times New Roman, 12pt, Bold
dev.off()

jpeg("eng_plenum_position_CONT.jpg",width=2100,height = 1700, res=300)
ggplot(ep9_wide, aes(x=position.CONT))+ 
  geom_density(data =plenum_ep9, aes(x=position), fill = "black", alpha =0.5) + 
  geom_density(fill="white", alpha = 0.5)+
  theme_classic() +
  xlab("pro/anti EU")+
  ylab("Density")+
  ggtitle("CONT")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text=element_text(family="Times New Roman", size=12)) #Times New Roman, 12pt, Bold
dev.off()

jpeg("eng_plenum_position_CULT.jpg",width=2100,height = 1700, res=300)
ggplot(ep9_wide, aes(x=position.CULT))+ 
  geom_density(data =plenum_ep9, aes(x=position), fill = "black", alpha =0.5) + 
  geom_density(fill="white", alpha = 0.5)+
  theme_classic() +
  xlab("pro/anti EU")+
  ylab("Density")+
  ggtitle("CULT")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text=element_text(family="Times New Roman", size=12)) #Times New Roman, 12pt, Bold
dev.off()

jpeg("eng_plenum_position_DEVE.jpg",width=2100,height = 1700, res=300)
ggplot(ep9_wide, aes(x=position.DEVE))+ 
  geom_density(data =plenum_ep9, aes(x=position), fill = "black", alpha =0.5) + 
  geom_density(fill="white", alpha = 0.5)+
  theme_classic() +
  xlab("pro/anti EU")+
  ylab("Density")+
  ggtitle("DEVE")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text=element_text(family="Times New Roman", size=12)) #Times New Roman, 12pt, Bold
dev.off()

jpeg("eng_plenum_position_ECON.jpg",width=2100,height = 1700, res=300)
ggplot(ep9_wide, aes(x=position.ECON))+ 
  geom_density(data =plenum_ep9, aes(x=position), fill = "black", alpha =0.5) + 
  geom_density(fill="white", alpha = 0.5)+
  theme_classic() +
  xlab("pro/anti EU")+
  ylab("Density")+
  ggtitle("ECON")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text=element_text(family="Times New Roman", size=12)) #Times New Roman, 12pt, Bold
dev.off()

jpeg("eng_plenum_position_EMPL.jpg",width=2100,height = 1700, res=300)
ggplot(ep9_wide, aes(x=position.EMPL))+ 
  geom_density(data =plenum_ep9, aes(x=position), fill = "black", alpha =0.5) + 
  geom_density(fill="white", alpha = 0.5)+
  theme_classic() +
  xlab("pro/anti EU")+
  ylab("Density")+
  ggtitle("EMPL")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text=element_text(family="Times New Roman", size=12)) #Times New Roman, 12pt, Bold
dev.off()


jpeg("eng_plenum_position_ENVI.jpg",width=2100,height = 1700, res=300)
ggplot(ep9_wide, aes(x=position.ENVI))+ 
  geom_density(data =plenum_ep9, aes(x=position), fill = "black", alpha =0.5) + 
  geom_density(fill="white", alpha = 0.5)+
  theme_classic() +
  xlab("pro/anti EU")+
  ylab("Density")+
  ggtitle("ENVI")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text=element_text(family="Times New Roman", size=12)) #Times New Roman, 12pt, Bold
dev.off()

jpeg("eng_plenum_position_FEMM.jpg",width=2100,height = 1700, res=300)
ggplot(ep9_wide, aes(x=position.FEMM))+ 
  geom_density(data =plenum_ep9, aes(x=position), fill = "black", alpha =0.5) + 
  geom_density(fill="white", alpha = 0.5)+
  theme_classic() +
  xlab("pro/anti EU")+
  ylab("Density")+
  ggtitle("FEMM")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text=element_text(family="Times New Roman", size=12)) #Times New Roman, 12pt, Bold
dev.off()

jpeg("eng_plenum_position_IMCO.jpg",width=2100,height = 1700, res=300)
ggplot(ep9_wide, aes(x=position.IMCO))+ 
  geom_density(data =plenum_ep9, aes(x=position), fill = "black", alpha =0.5) + 
  geom_density(fill="white", alpha = 0.5)+
  theme_classic() +
  xlab("pro/anti EU")+
  ylab("Density")+
  ggtitle("IMCO")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text=element_text(family="Times New Roman", size=12)) #Times New Roman, 12pt, Bold
dev.off()

jpeg("eng_plenum_position_INTA.jpg",width=2100,height = 1700, res=300)
ggplot(ep9_wide, aes(x=position.INTA))+ 
  geom_density(data =plenum_ep9, aes(x=position), fill = "black", alpha =0.5) + 
  geom_density(fill="white", alpha = 0.5)+
  theme_classic() +
  xlab("pro/anti EU")+
  ylab("Density")+
  ggtitle("INTA")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text=element_text(family="Times New Roman", size=12)) #Times New Roman, 12pt, Bold
dev.off()

jpeg("eng_plenum_position_ITRE.jpg",width=2100,height = 1700, res=300)
ggplot(ep9_wide, aes(x=position.ITRE))+ 
  geom_density(data =plenum_ep9, aes(x=position), fill = "black", alpha =0.5) + 
  geom_density(fill="white", alpha = 0.5)+
  theme_classic() +
  xlab("pro/anti EU")+
  ylab("Density")+
  ggtitle("ITRE")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text=element_text(family="Times New Roman", size=12)) #Times New Roman, 12pt, Bold
dev.off()

jpeg("eng_plenum_position_JURI.jpg",width=2100,height = 1700, res=300)
ggplot(ep9_wide, aes(x=position.JURI))+ 
  geom_density(data =plenum_ep9, aes(x=position), fill = "black", alpha =0.5) + 
  geom_density(fill="white", alpha = 0.5)+
  theme_classic() +
  xlab("pro/anti EU")+
  ylab("Density")+
  ggtitle("JURI")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text=element_text(family="Times New Roman", size=12)) #Times New Roman, 12pt, Bold
dev.off()

jpeg("eng_plenum_position_LIBE.jpg",width=2100,height = 1700, res=300)
ggplot(ep9_wide, aes(x=position.LIBE))+ 
  geom_density(data =plenum_ep9, aes(x=position), fill = "black", alpha =0.5) + 
  geom_density(fill="white", alpha = 0.5)+
  theme_classic() +
  xlab("pro/anti EU")+
  ylab("Density")+
  ggtitle("LIBE")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text=element_text(family="Times New Roman", size=12)) #Times New Roman, 12pt, Bold
dev.off()

jpeg("eng_plenum_position_PECH.jpg",width=2100,height = 1700, res=300)
ggplot(ep9_wide, aes(x=position.PECH))+ 
  geom_density(data =plenum_ep9, aes(x=position), fill = "black", alpha =0.5) + 
  geom_density(fill="white", alpha = 0.5)+
  theme_classic() +
  xlab("pro/anti EU")+
  ylab("Density")+
  ggtitle("PECH")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text=element_text(family="Times New Roman", size=12)) #Times New Roman, 12pt, Bold
dev.off()

jpeg("eng_plenum_position_PETI.jpg",width=2100,height = 1700, res=300)
ggplot(ep9_wide, aes(x=position.PETI))+ 
  geom_density(data =plenum_ep9, aes(x=position), fill = "black", alpha =0.5) + 
  geom_density(fill="white", alpha = 0.5)+
  theme_classic() +
  xlab("pro/anti EU")+
  ylab("Density")+
  ggtitle("PETI")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text=element_text(family="Times New Roman", size=12)) #Times New Roman, 12pt, Bold
dev.off()

jpeg("eng_plenum_position_REGI.jpg",width=2100,height = 1700, res=300)
ggplot(ep9_wide, aes(x=position.REGI))+ 
  geom_density(data =plenum_ep9, aes(x=position), fill = "black", alpha =0.5) + 
  geom_density(fill="white", alpha = 0.5)+
  theme_classic() +
  xlab("pro/anti EU")+
  ylab("Density")+
  ggtitle("REGI")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text=element_text(family="Times New Roman", size=12)) #Times New Roman, 12pt, Bold
dev.off()

jpeg("eng_plenum_position_TRAN.jpg",width=2100,height = 1700, res=300)
ggplot(ep9_wide, aes(x=position.TRAN))+ 
  geom_density(data =plenum_ep9, aes(x=position), fill = "black", alpha =0.5) + 
  geom_density(fill="white", alpha = 0.5)+
  theme_classic() +
  xlab("pro/anti EU")+
  ylab("Density")+
  ggtitle("TRAN")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text=element_text(family="Times New Roman", size=12)) #Times New Roman, 12pt, Bold
dev.off()
