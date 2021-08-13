rm(list = ls())

library(microxanox)
library(tidyverse)
library(patchwork)
library(here)


#### Calculate realized growth rates at regime shifts ####

var_expt <- readRDS("./experiments/experiment 1/data/ss_data_1000000_20factorial.RDS")

var_expt_levels <- var_expt[,1:6]

# (1) get trait values for the treatments with maximum trait variation 

# these are the nine CB strains when there is no variation, i.e. all have the mean trait values
#CBtraits <- var_expt$pars[[1]]$CB
#CBtraits

# find the row with maximum variation in CB
which(max(rowSums(abs(var_expt_levels[,1:2]))) ==
        rowSums(abs(var_expt_levels[,1:2])) &
        rowSums(abs(var_expt_levels[,3:6]))==0)

# these are the nine CB strains when there is maximum variation 
CBtraits <- var_expt$pars[[381]]$CB
CBtraits


# find the row with maximum variation in SBPB
which(max(rowSums(abs(var_expt_levels[,3:6]))) ==
        rowSums(abs(var_expt_levels[,3:6])) &
        rowSums(abs(var_expt_levels[,1:2]))==0)

# these are the nine SB and PB strains when there is maximum variation 
SBtraits <- var_expt$pars[[20]]$SB
SBtraits

PBtraits<- var_expt$pars[[20]]$PB
PBtraits


# (2) get abiotic data for no and maximum diversity

# find various combinations of diversity
no_diversity <- which(rowSums(abs(var_expt_levels))==0)

max_diversity_all <- which(max(rowSums(abs(var_expt_levels))) ==
                            rowSums(abs(var_expt_levels)))

max_only_CB_diversity <- which(max(rowSums(abs(var_expt_levels[,1:2]))) ==
                                 rowSums(abs(var_expt_levels[,1:2])) &
                                 rowSums(abs(var_expt_levels[,3:6]))==0)


max_only_SBPB_diversity <- which(max(rowSums(abs(var_expt_levels[,3:6]))) ==
                                   rowSums(abs(var_expt_levels[,3:6])) &
                                   rowSums(abs(var_expt_levels[,1:2]))==0)


# get the densities and abiotic conditions

no_div <- var_expt[no_diversity,]$ss_res[[1]]
CB_div_only <- var_expt[max_only_CB_diversity,]$ss_res[[1]]
SBPB_div_only <- var_expt[max_only_SBPB_diversity,]$ss_res[[1]]
all_div <- var_expt[max_diversity_all,]$ss_res[[1]]

no_div$diversity <- "no diversity"
CB_div_only$diversity <- "CB only diversity"
SBPB_div_only$diversity <- "SBPB only diversity"
all_div$diversity <- "all diversity"

all_data <- rbind(no_div, CB_div_only, SBPB_div_only, all_div)

# only keep rows if all columns that have "B_" in the column-name (i.e. the population densities) have values larger than -1 (there are some very large negative values; these are the rows that produce negative SO values, by the way)
all_data <- filter(all_data, if_all(contains("B_"), ~ (.x > - 1)))



# (3) calculate realized growth rates of the extreme and average strains (i.e. strains #1, 5, 9)

# mortality rates
CB_m <- 0.02
PB_m <- 0.028
SB_m <- 0.04


all_data$CB1_r <- growth1(all_data$P, CBtraits$g_max_CB[1], CBtraits$k_CB_P[1]) * inhibition(all_data$SR, CBtraits$h_SR_CB[1]) - CB_m
all_data$CB5_r <- growth1(all_data$P, CBtraits$g_max_CB[5], CBtraits$k_CB_P[5]) * inhibition(all_data$SR, CBtraits$h_SR_CB[5]) - CB_m
all_data$CB9_r <- growth1(all_data$P, CBtraits$g_max_CB[9], CBtraits$k_CB_P[9]) * inhibition(all_data$SR, CBtraits$h_SR_CB[9]) - CB_m

all_data$SB1_r <- growth2(all_data$P, all_data$SO, SBtraits$g_max_SB[1], SBtraits$k_SB_P[1], SBtraits$k_SB_SO[1]) * inhibition(all_data$O, SBtraits$h_O_SB[1]) - SB_m
all_data$SB5_r <- growth2(all_data$P, all_data$SO, SBtraits$g_max_SB[5], SBtraits$k_SB_P[5], SBtraits$k_SB_SO[5]) * inhibition(all_data$O, SBtraits$h_O_SB[5]) - SB_m
all_data$SB9_r <- growth2(all_data$P, all_data$SO, SBtraits$g_max_SB[9], SBtraits$k_SB_P[9], SBtraits$k_SB_SO[9]) * inhibition(all_data$O, SBtraits$h_O_SB[9]) - SB_m

all_data$PB1_r <- growth2(all_data$P, all_data$SR, PBtraits$g_max_PB[1], PBtraits$k_PB_P[1], PBtraits$k_PB_SR[1]) * inhibition(all_data$O, PBtraits$h_O_PB[1]) - PB_m
all_data$PB5_r <- growth2(all_data$P, all_data$SR, PBtraits$g_max_PB[5], PBtraits$k_PB_P[5], PBtraits$k_PB_SR[5]) * inhibition(all_data$O, PBtraits$h_O_PB[5]) - PB_m
all_data$PB9_r <- growth2(all_data$P, all_data$SR, PBtraits$g_max_PB[9], PBtraits$k_PB_P[9], PBtraits$k_PB_SR[9]) * inhibition(all_data$O, PBtraits$h_O_PB[9]) - PB_m


# zoom in on regime shifts

initial_CB_low <- all_data[all_data$initial_N_CB==1,]
initial_CB_high <- all_data[all_data$initial_N_CB > 1,]

initial_CB_low$ID <- 1:nrow(initial_CB_low)
initial_CB_high$ID <- 1:nrow(initial_CB_high)

# label with "1" when SR is at least 10 units smaller than at the previous ox diff level
initial_CB_low <- initial_CB_low %>% mutate(shift = ifelse(SR + 10 < lag(SR), 1, 0))
initial_CB_high <- initial_CB_high %>% mutate(shift = ifelse(SR + 10 < lag(SR), 1, 0))

# get the IDs (i.e. row-numbers) of regime shifts
shift_IDs_initial_CB_low <- na.omit(initial_CB_low[initial_CB_low$shift==1,]$ID)
shifts_IDs_initial_CB_high <- na.omit(initial_CB_high[initial_CB_high$shift==1,]$ID)

# start 10 steps before the regime shift
before_shift_initial_CB_low <- shift_IDs_initial_CB_low - 10
before_shift_initial_CB_high <- shifts_IDs_initial_CB_high - 10

# get the IDs 10 steps before and after the shift

shift_range_initial_CB_low <- matrix(nrow = length(before_shift_initial_CB_low), ncol=20)

for(i in 1:length(before_shift_initial_CB_low)){
  shift_range_initial_CB_low[i,] <- seq(before_shift_initial_CB_low[i], before_shift_initial_CB_low[i]+19, by=1)
}

# change to vector
shift_range_initial_CB_low <- c(shift_range_initial_CB_low)

# same for initial_CB_high
shift_range_initial_CB_high <- matrix(nrow = length(before_shift_initial_CB_high), ncol=20)

for(i in 1:length(before_shift_initial_CB_high)){
  shift_range_initial_CB_high[i,] <- seq(before_shift_initial_CB_high[i], before_shift_initial_CB_high[i]+19, by=1)
}

shift_range_initial_CB_high <- c(shift_range_initial_CB_high)

# get data before and after regime shift

shifts_initial_CB_low <- initial_CB_low[initial_CB_low$ID %in% shift_range_initial_CB_low,]
shifts_initial_CB_high <- initial_CB_high[initial_CB_high$ID %in% shift_range_initial_CB_high,]

all_shifts <- rbind(shifts_initial_CB_low, shifts_initial_CB_high)

# change to long format
all_shifts_long <- subset(all_shifts, select=c(initial_N_CB, ID, a_O, diversity, CB1_r, CB5_r, CB9_r, PB1_r, PB5_r, PB9_r, SB1_r, SB5_r, SB9_r))
all_shifts_long <- gather(all_shifts_long, Strain, realized_growth_rate, -(1:4))

all_shifts_long$Group <- substr(all_shifts_long$Strain, 1,2)

# get colours
colfunc_CB <- colorRampPalette(c("#024F17", "#B5FFC9"))
colfunc_SB <- colorRampPalette(c("#7D1402", "#FCBEB3"))
colfunc_PB <- colorRampPalette(c("#6E0172", "#F9AEFC"))

strain_colours <- c(colfunc_CB(3)[1],colfunc_CB(3)[2],colfunc_CB(3)[3],colfunc_PB(3)[1],colfunc_PB(3)[2],
                colfunc_PB(3)[3],colfunc_SB(3)[1],colfunc_SB(3)[2],colfunc_SB(3)[3])

# change order
all_shifts_long$diversity <- factor(all_shifts_long$diversity, levels = c("no diversity", "CB only diversity", "SBPB only diversity", "all diversity"))

all_shifts_long$initial_N_CB_factor <- all_shifts_long$initial_N_CB
all_shifts_long$initial_N_CB_factor[all_shifts_long$initial_N_CB_factor==1] <- "low initial_N_CB"
all_shifts_long$initial_N_CB_factor[all_shifts_long$initial_N_CB_factor==1e+10] <- "high initial_N_CB"

all_shifts_long$initial_N_CB_factor <- factor(all_shifts_long$initial_N_CB_factor, levels = c("low initial_N_CB", "high initial_N_CB"))

# remove strains that do not occur
all_shifts_long2 <- all_shifts_long[!(all_shifts_long$diversity=="no diversity" & all_shifts_long$Strain %in% c("CB1_r", "CB9_r", "PB1_r", "PB9_r", "SB1_r", "SB9_r")),]
all_shifts_long2 <- all_shifts_long2[!(all_shifts_long2$diversity=="CB only diversity" & all_shifts_long2$Strain %in% c("PB1_r", "PB9_r", "SB1_r", "SB9_r")),]
all_shifts_long2 <- all_shifts_long2[!(all_shifts_long2$diversity=="SBPB only diversity" & all_shifts_long2$Strain %in% c("CB1_r", "CB9_r")),]


pdf("./experiments/experiment 1/realized_growth_rates.pdf", width = 12,  height = 6)

x.plot <- qplot(data=all_shifts_long2,
                x=log10(a_O),
                y=realized_growth_rate,
                colour=Strain)+
  facet_wrap(initial_N_CB_factor ~ diversity, scales = "free_x", nrow = 2)+
  scale_colour_manual(values = strain_colours)

print(x.plot)


for(i in unique(all_shifts_long2$Group)){
  
  temp.data <- all_shifts_long2[all_shifts_long2$Group==i,]
  
  x.plot <- qplot(data=temp.data,
                  x=log10(a_O),
                  y=realized_growth_rate,
                  colour=Strain,
                  main=i)+
    facet_wrap(initial_N_CB_factor ~ diversity, scales = "free_x", nrow = 2)+
    scale_colour_manual(values=c("magenta", "navyblue", "orange2"))
  
  print(x.plot)
  
  
}


graphics.off()


write.csv(all_shifts, "./experiments/experiment 1/shifts_1e6_20factorial.csv", row.names = F)

