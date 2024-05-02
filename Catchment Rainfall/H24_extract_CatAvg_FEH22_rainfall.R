library("rgdal")
library("raster")
library("exactextractr")

R22 <- list.files(path = "//nercwlsmb01/data/feh22_sepa/programs/6.Make_ASCII/Sample-point_dur_&_rp", full = TRUE)

Locations <- readOGR("P:/08458 CWI-EA 2019-21 Flood Review/Data/Locations/Shapefiles", "ALLSHAPES")

R22_1 <- gtools::mixedsort(G22[which(substr(R22, 75, 79) == "GB_1_")])
R22_6 <- gtools::mixedsort(G22[which(substr(R22, 75, 79) == "GB_6_")])
R22_24 <- gtools::mixedsort(G22[which(substr(R22, 75, 80) == "GB_24_")])
R22_96 <- gtools::mixedsort(G22[which(substr(R22, 75, 80) == "GB_96_")])

Table_1h <- array(NA, dim = c(200, 24))
Table_6h <- array(NA, dim = c(200, 24))
Table_1d <- array(NA, dim = c(200, 24))
Table_4d <- array(NA, dim = c(200, 24))

for (i in 1:24) {
  
  R_1 <- raster(R22_1[i])
  R_6 <- raster(R22_6[i])
  R_24 <- raster(R22_24[i])
  R_96 <- raster(R22_96[i])
  
  E_1 <- exact_extract(R_1, Locations)
  E_6 <- exact_extract(R_6, Locations)
  E_24 <- exact_extract(R_24, Locations)
  E_96 <- exact_extract(R_96, Locations)
  
  for (j in 1:200) {
    
    C_1 <- E_1[j]
    C_6 <- E_6[j]
    C_24 <- E_24[j]
    C_96 <- E_96[j]
    
    Table_1h[j, i] <- sum(C_1[[1]][1] * C_1[[1]][2]) / sum(C_1[[1]][2])
    Table_6h[j, i] <- sum(C_6[[1]][1] * C_6[[1]][2]) / sum(C_6[[1]][2])
    Table_1d[j, i] <- sum(C_24[[1]][1] * C_24[[1]][2]) / sum(C_24[[1]][2])
    Table_4d[j, i] <- sum(C_96[[1]][1] * C_96[[1]][2]) / sum(C_96[[1]][2])
    
  }
  
}

Meta <- Locations@data[ , 1:5]

RPs <- c("X1.3", "X1.58", "X2", "X3", "X6", "X10", "X18", "X31", "X56",
                        "X100", "X180", "X310", "X560",
                        "X1000", "X1800", "X3100", "X5600",
                        "X10000", "X18000", "X31000", "X56000",
                        "X100000", "X250000", "X500000")

colnames(Table_1h) <- RPs
colnames(Table_6h) <- RPs
colnames(Table_1d) <- RPs
colnames(Table_4d) <- RPs

setwd("P:/08458 CWI-EA 2019-21 Flood Review")

write.csv(data.frame(Meta, round(Table_1h/10, 1)), "Data/H24 results/Station_FEH22_depths_1h.csv", row.names = FALSE)
write.csv(data.frame(Meta, round(Table_6h/10, 1)), "Data/H24 results/Station_FEH22_depths_6h.csv", row.names = FALSE)
write.csv(data.frame(Meta, round(Table_1d/10, 1)), "Data/H24 results/Station_FEH22_depths_1d.csv", row.names = FALSE)
write.csv(data.frame(Meta, round(Table_4d/10, 1)), "Data/H24 results/Station_FEH22_depths_4d.csv", row.names = FALSE)
