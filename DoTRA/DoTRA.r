#### Calculate Topographic Relief Alteration
#### Christopoulos Konstantinos 
### Load Required Packages
 ## Ipak Function by Steven Worthington
  # https://gist.github.com/stevenworthington
ipak <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg)) 
        install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
}

ipak(c("raster","SDMTools","maptools","scatterplot3d"))

#########################################################
#########################################################
### DATA
 ## CHANGE THESE:
  # FROM HERE
setwd("C:/") # Path with files here
Initial_State <- raster("state_0") # Initial Raster
Final_State <- raster("state_1") # Final Raster
Boundaries <-  readShapePoly("boundary.shp") # Boundary Raster
  # TO HERE
#########################################################
#########################################################

### Collect required spatial data
state_0 <- Initial_State
state_1 <- Final_State
slope_state_0 <- terrain(state_0, opt='slope', unit='degrees', neighbors=8)
slope_state_1 <- terrain(state_1, opt='slope', unit='degrees', neighbors=8)
aspect_state_0 <- terrain(state_0, opt='aspect', unit='degrees', neighbors=8)
aspect_state_1 <- terrain(state_1, opt='aspect', unit='degrees', neighbors=8)

elevation_points_0 <- rasterToPoints(state_0)
elevation_points_1 <- rasterToPoints(state_1)
slope_points_0 <- rasterToPoints(slope_state_0)
slope_points_1 <- rasterToPoints(slope_state_1)
aspect_points_0 <- rasterToPoints(aspect_state_0)
aspect_points_1 <- rasterToPoints(aspect_state_1)

data_elev <- as.data.frame(cbind(elevation_points_0, elevation_points_1[,3]))
colnames(data_elev) <-c("x","y","state_0","state_1")
data_slope <- as.data.frame(cbind(slope_points_0, slope_points_1[,3]))
colnames(data_slope) <-c("x","y","state_0","state_1")
data_aspect <- as.data.frame(cbind(aspect_points_0, aspect_points_1[,3]))
colnames(data_aspect) <-c("x","y","state_0","state_1")

### Plots
 ## Raster Elevation
  # Fig.1
par(mfrow = c(1,2))
plot(state_0,
     main = "Initial State",
     xlab = "", ylab = "")
lines(Boundaries, col = "red", lwd = 2)
contour(state_0, add = T)

plot(state_1,
     main = "Final State")
lines(Boundaries, col = "red", lwd = 2)
contour(state_1, add = T)

 ## Raster Slope
  # Fig.2
par(mfrow = c(1,2))
plot(slope_state_0,
     main = "Initial State",
     xlab = "", ylab = "")
lines(Boundaries, col = "red", lwd = 2)

plot(slope_state_1,
     main = "Final State")
lines(Boundaries, col = "red", lwd = 2)

 ## Raster Aspect
  # Fig.3
par(mfrow = c(1,2))
plot(aspect_state_0,
     main = "Initial State",
     xlab = "", ylab = "")
lines(Boundaries, col = "red", lwd = 2)

plot(aspect_state_1,
     main = "Final State")
lines(Boundaries, col = "red", lwd = 2)

 ## Elevation Comparison
  # Fig.4
par(mfrow = c(1,2))
scatterplot3d(
  data_elev$x, data_elev$y, data_elev$state_0,
  color = "darkgreen", pch = ".",
  col.axis = "blue", col.grid = "lightblue",
  xlab = "X", ylab = "Y", zlab = "Z",
  main = "Initial State"
)
scatterplot3d(
  data_elev$x, data_elev$y, data_elev$state_1,
  color = "red", pch = ".",
  col.axis = "blue", col.grid = "lightblue",
  xlab = "X", ylab = "Y", zlab = "Z",
  main = "Final State"
)

  # Fig.5
par(mfrow = c(1,1))
plot(
  data_elev$state_0,data_elev$state_1,
  pch = 0, cex = 0.2, col = "red2",
  main = "Elevation Comparison #2",
  xlab = "Initial State Elevation (m)",
  ylab = "Final State Elevation (m)",
  xlim = c(60,180),ylim = c(60,180)
)
abline(0, 1, lwd = 1, col = "blue")

  # Fig.6
plot(
  data_elev$state_0,
  pch = ".", cex = 1.2, col = "darkgreen",
  main = "Elevation Comparison #3",
  xlab = "Index",
  ylab = "Elevation (m)",
  ylim = c(50,185)
)
points(data_elev$state_1, pch = ".", cex = 1.2, col = "red2")
mdl_1 = lm(data_elev$state_0 ~ poly(seq(1,nrow(data_elev)), 3, raw = TRUE))
mdl_2 = lm(data_elev$state_1 ~ poly(seq(1,nrow(data_elev)), 3, raw = TRUE))
lines(predict(mdl_1), lwd = 2, col = "darkgreen")
lines(predict(mdl_2), lwd = 2, col = "red2")

  # Fig.7
plot(
  data_elev$state_1 - data_elev$state_0,
  pch = "x", cex = 0.8, col = "gray25",
  main = "Elevation Comparison #4",
  xlab = "Index",
  ylab = "Δ Elevation = Final - Initial (m)"
)

   # Volume Difference
volume <-
  sum((data_elev$state_1 - data_elev$state_0) * (xres(state_0) * yres(state_0)))
add_text <- sprintf("The Volume Difference is: %s", volume, "m^3")
mtext(paste(
  "The Volume Difference is: ",round(volume, digits = 0), "cubic meters"
),
col = "red")



 ## Slope Comparison
  # Fig.8
plot(
  data_slope$state_0,
  pch = ".", cex = 1.2, col = "darkgreen",
  main = "Slope Comparison",
  xlab = "Index",
  ylab = "Slope (Deg)",
  ylim = c(0,50)
)
points(data_slope$state_1, pch = ".", cex = 1.2, col = "red2")
mdl_1 = lm(data_slope$state_0 ~ poly(seq(1,nrow(data_slope)), 3, raw = TRUE))
mdl_2 = lm(data_slope$state_1 ~ poly(seq(1,nrow(data_slope)), 3, raw = TRUE))
lines(predict(mdl_1), lwd = 2, col = "darkgreen")
lines(predict(mdl_2), lwd = 2, col = "red2")

 ## Aspect Comparison
  # Fig.9
plot(
  data_aspect$state_0,
  pch = ".", cex = 1.2, col = "darkgreen",
  main = "Aspect Comparison",
  xlab = "Index",
  ylab = "Aspect (Deg)",
  ylim = c(0,360)
)
points(data_aspect$state_1, pch = ".", cex = 1.2, col = "red2")
mdl_1 = lm(data_aspect$state_0 ~ poly(seq(1,nrow(data_aspect)), 3, raw =
                                        TRUE))
mdl_2 = lm(data_aspect$state_1 ~ poly(seq(1,nrow(data_aspect)), 3, raw =
                                        TRUE))
lines(predict(mdl_1), lwd = 2, col = "darkgreen")
lines(predict(mdl_2), lwd = 2, col = "red2")

### Remove unwanted variables from memory
rm(list=ls()[! ls() %in% c("data_elev", "data_slope", "data_aspect")])

### Calculate Indexes for DoTRA
 ## Landform Index (LI)
var_elev_0 <- var(data_elev$state_0)
var_elev_1 <- var(data_elev$state_1)
cov_elev <- cov(data_elev$state_0,data_elev$state_1)
	  
LI <- (var_elev_0 + var_elev_1 - 2 * cov_elev) /
      (var_elev_0 + var_elev_1)
	  
LI <- round(LI, digits=2)

 ## Altitude Index (AI)
Δ_mean_elev <- mean(data_elev$state_0) - mean(data_elev$state_1)

Δ_elev_0_mean_elev_2 <- (data_elev$state_0 - 
						 mean(data_elev$state_0))**2
Δ_elev_1_mean_elev_2 <- (data_elev$state_0 - 
						 Δ_mean_elev -
						 mean(data_elev$state_0))**2

AI <- (sqrt(sum(Δ_elev_1_mean_elev_2)) /
       sqrt(sum(Δ_elev_0_mean_elev_2)) - 1) * 100
	   
AI <- round(AI, digits=0)

 ## Slope Index (SI)
Δ_slope <- data_slope$state_0 - data_slope$state_1
Δ_slope_abs_90 <- abs(Δ_slope)/90

SI <- mean(Δ_slope_abs_90)

SI <- round(SI, digits=2)

 ## Aspect Index (AsI)
reduct360to180 <- function(x) {
    ifelse(x < 180, x, x-360)
}

reduct360to180_aspect_0 <- reduct360to180(data_aspect$state_0)
reduct360to180_aspect_1 <- reduct360to180(data_aspect$state_1)

Δ_red360to180_aspect <- abs(reduct360to180_aspect_0 - reduct360to180_aspect_1)

reduct360to180_2 <- function(x) {
    ifelse(x < 180, x, 360-x)
}

red360to180_Δ_red360to180_aspect <- reduct360to180_2(Δ_red360to180_aspect)

AsI <- mean(red360to180_Δ_red360to180_aspect/180)

AsI <- round(AsI, digits=2)

 ## Category of Landform Index (LI_c)
if (0<=LI & LI<=0.15){
    LI_c <- "Class 1"
	} else if (0.16<=LI & LI<=0.25){
	LI_c <- "Class 2"
	} else if (0.26<=LI & LI<=0.35){
	LI_c <- "Class 3"
	} else if (0.36<=LI & LI<=0.45){
	LI_c <- "Class 4"
	} else if (0.46<=LI & LI<=0.55){
	LI_c <- "Class 5"
	} else if (0.56<=LI & LI<=0.65){
	LI_c <- "Class 6"
	} else if (0.66<=LI & LI<=0.75){
	LI_c <- "Class 7"
	} else if (0.76<=LI & LI<=0.85){
	LI_c <- "Class 8"
	} else if (0.86<=LI & LI<=0.95){
	LI_c <- "Class 9"
	} else {
	LI_c <- "Class 10"
}

 ## Category of Altitude Index (AI_c)
if (0<=AI & AI<=10){
    AI_c <- "Class 1"
	} else if (11<=AI & AI<=20){
	AI_c <- "Class 2"
	} else if (21<=AI & AI<=30){
	AI_c <- "Class 3"
	} else if (31<=AI & AI<=40){
	AI_c <- "Class 4"
	} else if (41<=AI & AI<=50){
	AI_c <- "Class 5"
	} else if (51<=AI & AI<=60){
	AI_c <- "Class 6"
	} else if (61<=AI & AI<=70){
	AI_c <- "Class 7"
	} else if (71<=AI & AI<=80){
	AI_c <- "Class 8"
	} else if (81<=AI & AI<=90){
	AI_c <- "Class 9"
	} else {
	AI_c <- "Class 10"
}

 ## Adjusted Landform Index(ALI)
ALI <- as.numeric(gsub("[^0-9]", "", LI_c, ""))/10 * 0.8 +
	   as.numeric(gsub("[^0-9]", "", AI_c, ""))/10 * 0.2

 ## Category of Adjusted Landform Index (ALI_c)
ALI_c <- paste("Class",round(ALI * 10, digits=0))

 ## Category of Slope Index (SI_c)
if (0<=SI & SI<=0.05){
    SI_c <- "Class A"
	} else if (0.06<=SI & SI<=0.15){
	SI_c <- "Class B"
	} else if (0.16<=SI & SI<=0.30){
	SI_c <- "Class C"
	} else if (0.31<=SI & SI<=0.60){
	SI_c <- "Class D"
	} else {
	SI_c <- "Class E"
}

 ## Category of Aspect Index (AsI_c)
if (0<=AsI & AsI<=0.25){
    AsI_c <- "Class A"
	} else if (0.26<=AsI & AsI<=0.50){
	AsI_c <- "Class B"
	} else if (0.51<=AsI & AsI<=0.75){
	AsI_c <- "Class C"
	} else {
	AsI_c <- "Class D"
}

### Calculate Degree of Topographic Relief Alteration (DoTRA)
 ## DoTRA
Parameters <- c(
"Class A#Class A#Class 1","Class A#Class A#Class 2","Class A#Class A#Class 3",
"Class A#Class A#Class 4","Class A#Class A#Class 5","Class A#Class A#Class 6",
"Class A#Class A#Class 7","Class A#Class A#Class 8","Class A#Class A#Class 9",
"Class A#Class A#Class 10","Class A#Class B#Class 1","Class A#Class B#Class 2",
"Class A#Class B#Class 3","Class A#Class B#Class 4","Class A#Class B#Class 5",
"Class A#Class B#Class 6","Class A#Class B#Class 7","Class A#Class B#Class 8",
"Class A#Class B#Class 9","Class A#Class B#Class 10","Class A#Class C#Class 1",
"Class A#Class C#Class 2","Class A#Class C#Class 3","Class A#Class C#Class 4",
"Class A#Class C#Class 5","Class A#Class C#Class 6","Class A#Class C#Class 7",
"Class A#Class C#Class 8","Class A#Class C#Class 9","Class A#Class C#Class 10",
"Class A#Class D#Class 1","Class A#Class D#Class 2","Class A#Class D#Class 3",
"Class A#Class D#Class 4","Class A#Class D#Class 5","Class A#Class D#Class 6",
"Class A#Class D#Class 7","Class A#Class D#Class 8","Class A#Class D#Class 9",
"Class A#Class D#Class 10","Class B#Class A#Class 1","Class B#Class A#Class 2",
"Class B#Class A#Class 3","Class B#Class A#Class 4","Class B#Class A#Class 5",
"Class B#Class A#Class 6","Class B#Class A#Class 7","Class B#Class A#Class 8",
"Class B#Class A#Class 9","Class B#Class A#Class 10","Class B#Class B#Class 1",
"Class B#Class B#Class 2","Class B#Class B#Class 3","Class B#Class B#Class 4",
"Class B#Class B#Class 5","Class B#Class B#Class 6","Class B#Class B#Class 7",
"Class B#Class B#Class 8","Class B#Class B#Class 9","Class B#Class B#Class 10",
"Class B#Class C#Class 1","Class B#Class C#Class 2","Class B#Class C#Class 3",
"Class B#Class C#Class 4","Class B#Class C#Class 5","Class B#Class C#Class 6",
"Class B#Class C#Class 7","Class B#Class C#Class 8","Class B#Class C#Class 9",
"Class B#Class C#Class 10","Class B#Class D#Class 1","Class B#Class D#Class 2",
"Class B#Class D#Class 3","Class B#Class D#Class 4","Class B#Class D#Class 5",
"Class B#Class D#Class 6","Class B#Class D#Class 7","Class B#Class D#Class 8",
"Class B#Class D#Class 9","Class B#Class D#Class 10","Class C#Class A#Class 1",
"Class C#Class A#Class 2","Class C#Class A#Class 3","Class C#Class A#Class 4",
"Class C#Class A#Class 5","Class C#Class A#Class 6","Class C#Class A#Class 7",
"Class C#Class A#Class 8","Class C#Class A#Class 9","Class C#Class A#Class 10",
"Class C#Class B#Class 1","Class C#Class B#Class 2","Class C#Class B#Class 3",
"Class C#Class B#Class 4","Class C#Class B#Class 5","Class C#Class B#Class 6",
"Class C#Class B#Class 7","Class C#Class B#Class 8","Class C#Class B#Class 9",
"Class C#Class B#Class 10","Class C#Class C#Class 1","Class C#Class C#Class 2",
"Class C#Class C#Class 3","Class C#Class C#Class 4","Class C#Class C#Class 5",
"Class C#Class C#Class 6","Class C#Class C#Class 7","Class C#Class C#Class 8",
"Class C#Class C#Class 9","Class C#Class C#Class 10","Class C#Class D#Class 1",
"Class C#Class D#Class 2","Class C#Class D#Class 3","Class C#Class D#Class 4",
"Class C#Class D#Class 5","Class C#Class D#Class 6","Class C#Class D#Class 7",
"Class C#Class D#Class 8","Class C#Class D#Class 9","Class C#Class D#Class 10",
"Class D#Class A#Class 1","Class D#Class A#Class 2","Class D#Class A#Class 3",
"Class D#Class A#Class 4","Class D#Class A#Class 5","Class D#Class A#Class 6",
"Class D#Class A#Class 7","Class D#Class A#Class 8","Class D#Class A#Class 9",
"Class D#Class A#Class 10","Class D#Class B#Class 1","Class D#Class B#Class 2",
"Class D#Class B#Class 3","Class D#Class B#Class 4","Class D#Class B#Class 5",
"Class D#Class B#Class 6","Class D#Class B#Class 7","Class D#Class B#Class 8",
"Class D#Class B#Class 9","Class D#Class B#Class 10","Class D#Class C#Class 1",
"Class D#Class C#Class 2","Class D#Class C#Class 3","Class D#Class C#Class 4",
"Class D#Class C#Class 5","Class D#Class C#Class 6","Class D#Class C#Class 7",
"Class D#Class C#Class 8","Class D#Class C#Class 9","Class D#Class C#Class 10",
"Class D#Class D#Class 1","Class D#Class D#Class 2","Class D#Class D#Class 3",
"Class D#Class D#Class 4","Class D#Class D#Class 5","Class D#Class D#Class 6",
"Class D#Class D#Class 7","Class D#Class D#Class 8","Class D#Class D#Class 9",
"Class D#Class D#Class 10","Class E#Class A#Class 1","Class E#Class A#Class 2",
"Class E#Class A#Class 3","Class E#Class A#Class 4","Class E#Class A#Class 5",
"Class E#Class A#Class 6","Class E#Class A#Class 7","Class E#Class A#Class 8",
"Class E#Class A#Class 9","Class E#Class A#Class 10","Class E#Class B#Class 1",
"Class E#Class B#Class 2","Class E#Class B#Class 3","Class E#Class B#Class 4",
"Class E#Class B#Class 5","Class E#Class B#Class 6","Class E#Class B#Class 7",
"Class E#Class B#Class 8","Class E#Class B#Class 9","Class E#Class B#Class 10",
"Class E#Class C#Class 1","Class E#Class C#Class 2","Class E#Class C#Class 3",
"Class E#Class C#Class 4","Class E#Class C#Class 5","Class E#Class C#Class 6",
"Class E#Class C#Class 7","Class E#Class C#Class 8","Class E#Class C#Class 9",
"Class E#Class C#Class 10","Class E#Class D#Class 1","Class E#Class D#Class 2",
"Class E#Class D#Class 3","Class E#Class D#Class 4","Class E#Class D#Class 5",
"Class E#Class D#Class 6","Class E#Class D#Class 7","Class E#Class D#Class 8",
"Class E#Class D#Class 9","Class E#Class D#Class 10")

Parameters_Values <- c(
  0.08,0.12,0.15,0.18,0.22,0.25,0.28,0.32,0.35,0.38,0.17,0.20,0.23,0.27,0.30,
  0.33,0.37,0.40,0.43,0.47,0.25,0.28,0.32,0.35,0.38,0.42,0.45,0.48,0.52,0.55,
  0.33,0.37,0.40,0.43,0.47,0.50,0.53,0.57,0.60,0.63,0.11,0.14,0.18,0.21,0.24,
  0.28,0.31,0.34,0.38,0.41,0.19,0.23,0.26,0.29,0.33,0.36,0.39,0.43,0.46,0.49,
  0.28,0.31,0.34,0.38,0.41,0.44,0.48,0.51,0.54,0.58,0.36,0.39,0.43,0.46,0.49,
  0.53,0.56,0.59,0.63,0.66,0.15,0.18,0.22,0.25,0.28,0.32,0.35,0.38,0.42,0.45,
  0.23,0.27,0.30,0.33,0.37,0.40,0.43,0.47,0.50,0.53,0.32,0.35,0.38,0.42,0.45,
  0.48,0.52,0.55,0.58,0.62,0.40,0.43,0.47,0.50,0.53,0.57,0.60,0.63,0.67,0.70,
  0.23,0.26,0.29,0.33,0.36,0.39,0.43,0.46,0.49,0.53,0.31,0.34,0.38,0.41,0.44,
  0.48,0.51,0.54,0.58,0.61,0.39,0.43,0.46,0.49,0.53,0.56,0.59,0.63,0.66,0.69,
  0.48,0.51,0.54,0.58,0.61,0.64,0.68,0.71,0.74,0.78,0.34,0.38,0.41,0.44,0.48,
  0.51,0.54,0.58,0.61,0.64,0.43,0.46,0.49,0.53,0.56,0.59,0.63,0.66,0.69,0.73,
  0.51,0.54,0.58,0.61,0.64,0.68,0.71,0.74,0.78,0.81,0.59,0.63,0.66,0.69,0.73,
  0.76,0.79,0.83,0.86,0.89
)

DoTRA_table <- cbind(Parameters,Parameters_Values)
DoTRA_key <- paste (SI_c,AsI_c,ALI_c, sep = "#")
DoTRA <- as.numeric(DoTRA_table[grep(DoTRA_key, DoTRA_table),2])

 ## Category of DoTRA (DoTRA_c)
if (0<=DoTRA & DoTRA<=0.15){
    DoTRA_c <- "Very Low"
	} else if (0.17<=DoTRA & DoTRA<=0.25){
	DoTRA_c <- "Low"
	} else if (0.26<=DoTRA & DoTRA<=0.40){
	DoTRA_c <- "Intermediate"
	} else if (0.41<=DoTRA & DoTRA<=0.58){
	DoTRA_c <- "High"
	} else {
	DoTRA_c <- "Very High"
}

### Remove unwanted variables from memory
rm(list=ls()[! ls() %in% c("LI","AI","SI","AsI","LI_c","AI_c","ALI",
                           "ALI_c","SI_c","AsI_c","DoTRA","DoTRA_c")])
						   

### Print DoTRA and it's indexes
msg_00 <- sprintf("LI:  %s %s",LI, LI_c)
msg_01 <- sprintf("\nAI:    %s %s",AI, AI_c)
msg_02 <- sprintf("\nSI:  %s %s",SI, SI_c)
msg_03 <- sprintf("\nAsI: %s %s",AsI, AsI_c)
msg_04 <- sprintf("\nALI: %s %s\n",ALI, ALI_c)

msg_1 <- sprintf(" The Degree of Topographic Relief Alteration is: %s", DoTRA)
msg_2 <- sprintf("\n  It is characterized as: %s\n", DoTRA_c)
cat(msg_00,msg_01,msg_02,msg_03,msg_04,msg_1,msg_2)
