library(tidyverse)
library(foreign)
library(sf)
library(emdi)
library(patchwork)
library(caret)
library(corrplot)
library(regclass)


# Import data
raw_dhs <- read.dta(file = "Jordan_HR_DT/JOHR81FL.DTA")
district_borders <- st_read("gadm41_JOR_shp/gadm41_JOR_2.shp")
cluster_coords <- st_read("JOGE81FL/JOGE81FL.shp")
raw_geodata <- read_csv(file = "Jordan_geospatialCovariates/JOGC81FL.csv")

# Process DHS data
## Select relevant cols from raw_dhs
dhs_data <- select(raw_dhs, c(hhid:hv003, hv005, hv012, hv024:hv025, hv270:hv271))

colnames(dhs_data)[which(colnames(dhs_data) == "hv000")] <- "country_code"
colnames(dhs_data)[which(colnames(dhs_data) == "hv001")] <- "cluster_nr"
colnames(dhs_data)[which(colnames(dhs_data) == "hv002")] <- "hh_nr"
colnames(dhs_data)[which(colnames(dhs_data) == "hv003")] <- "respondent_nr"
colnames(dhs_data)[which(colnames(dhs_data) == "hv005")] <- "weights"
colnames(dhs_data)[which(colnames(dhs_data) == "hv012")] <- "dejure_hh_members"
colnames(dhs_data)[which(colnames(dhs_data) == "hv024")] <- "region"
colnames(dhs_data)[which(colnames(dhs_data) == "hv025")] <- "urb_rur"
colnames(dhs_data)[which(colnames(dhs_data) == "hv270")] <- "wi_quintiles"
colnames(dhs_data)[which(colnames(dhs_data) == "hv271")] <- "wealth_index"

## Adjust weights and wealth index
dhs_data$weights <- dhs_data$weights / 1000000
dhs_data$wealth_index <- dhs_data$wealth_index / 100000

# Map cluster coordinates (cluster_coords) to the 52 districts (district_borders)
## Match clusters to district borders, select relevant cols
cluster_join <- st_join(x = cluster_coords, 
                        y = district_borders, 
                        join = st_within)
length(table(cluster_districts$district, useNA = "always"))
cluster_districts <- select(cluster_join, c(DHSID, DHSCLUST, NAME_1, NAME_2))
colnames(cluster_districts)[2:4] <- c("cluster_nr", "region", "district")
View(cluster_districts)
## Check matching of clusters to districts
setdiff(district_borders$NAME_2, unique(cluster_districts$district)) 
# --> 2 districts without clusters

bani_kenanah <- district_borders[district_borders$NAME_2 == "Bani Kenanah", ]

ghour_essafi <- district_borders[district_borders$NAME_2 == "Ghour Essafi", ]

cluster_plot <- ggplot() +
  geom_sf(data = district_borders) +
  geom_sf(data = bani_kenanah, fill = "red", alpha = 0.5) +
  geom_sf(data = ghour_essafi, fill = "red", alpha = 0.5) +
  geom_sf(data = cluster_coords, color = "blue", size = 0.5)

cluster_plot
# -> no clusters inside both districts

table(cluster_districts$district, useNA = "ifany")
cluster_districts[is.na(cluster_districts$district), ]
# -> cluster 451 is not matched to any district's borders

# Match cluster_districts with geospatial covariates
## Select only necessary cols from raw_geodata
## Match geospatial data to clusters
geodata_2020 <- raw_geodata |>
  select(DHSCLUST, Elevation, Global_Human_Footprint,Growing_Season_Length, Irrigation, Travel_Times,Nightlights_Composite,
         c(colnames(raw_geodata)[grepl(pattern = "_2020$", x = colnames(raw_geodata))])
         )

geo_cluster <- merge(x = cluster_districts, 
                     y = geodata_2020,
                     by.x = "cluster_nr",
                     by.y = "DHSCLUST") 

# Match cluster geospatial data to DHS household data
dhs_geo <- merge(x = dhs_data, 
                 y = geo_cluster,
                 by.x = "cluster_nr",
                 by.y = "cluster_nr")


## Check households per district
table(dhs_geo$district, useNA = "ifany") 
dhs_geo[is.na(dhs_geo$district), ]
# -> 20 households (cluster 451) not mapped to a district, remove
dhs_geo <- dhs_geo[!is.na(dhs_geo$district), ]

# Calculate direct estimates for wealth index by districts using survey weights

direct_estimates <- direct(
  y = "wealth_index",
  smp_data = dhs_geo,
  smp_domains = "district",
  weights = "weights",
  var = TRUE,
  B = 500,
  na.rm = TRUE)


summary(direct_estimates)

## Check results
direct_results <- data.frame(estimators(direct_estimates, MSE = TRUE, CV = TRUE, indicator = c("Mean")))

direct_results$Mean_CV <- abs(direct_results$Mean_CV) # take the absolute CV

summary(direct_results$Mean)
summary(direct_results$Mean_MSE)
summary(direct_results$Mean_CV)

# Plotting direct estimation results
## Match direct estimation results to district borders
direct_map <- merge(x = district_borders, 
                    y = direct_results,
                    by.x = "NAME_2",
                    by.y = "Domain", 
                    all.x = TRUE)

## Plot direct estimation means
direct_means_map <- ggplot(data = direct_map, aes(fill = Mean)) +
  geom_sf() +
  labs(title = "Direct Estimate Mean") +
  scale_fill_continuous(na.value = "orange", name = "Means") +
  theme_bw()

direct_means_map

## Plot direct estimation CVs
direct_cv_map <- ggplot(data = direct_map, aes(fill = Mean_CV)) +
  geom_sf() + 
  theme_bw() +
  labs(title = "CVs of Direct Estimates") +
  scale_fill_continuous(na.value = "orange", 
                        name = "CV", 
                        limits = c(0, 1), 
                        breaks = c(0.25, 0.5, 0.75, 1),
                        labels = c("0.25", "0.5", "0.75", "1")
                        )
direct_cv_map

## Aggregate geospatial covariates (dhs_geo) on district level
covnames_to_aggregate <- colnames(dhs_geo)[15:38]
covnames_to_aggregate

aggregated_covs <- dhs_geo |>
  group_by(district) |>
  summarise(across(all_of(covnames_to_aggregate), mean))



## Combine direct estimation results and aggregated covariates
direct_covariates <- merge(x = direct_results,
                           y = aggregated_covs,
                           by.x = "Domain",
                           by.y = "district")
head(direct_covariates, 3)

# Fay-Herriot model
## Variable selection
## Checking for highly correlated variables 
cor(direct_covariates[5:length(direct_covariates)], use = "complete.obs")

possible_predictors <- direct_covariates[, covnames_to_aggregate] 
cor_matrix <- cor(possible_predictors, use = "complete.obs")
corrplot(cor_matrix)

high_cor <- findCorrelation(x = cor_matrix, cutoff = 0.85, verbose = TRUE, names = TRUE)
high_cor

## Remove high_cor cols
intersect(x = covnames_to_aggregate, y = high_cor)
idx <- covnames_to_aggregate %in% high_cor
covariates <- covnames_to_aggregate[!idx]
covariates


## Check variation inflation factor (VIF) for the covariates, remove > 10
first_formula <- as.formula(paste("Mean ~", paste(covariates, collapse = "+")))
VIF(lm(first_formula, data = direct_covariates))

## Remove redundant temperature variables, keep Mean_Temperature_2020
temp_covs <-  c("Frost_Days_2020", "Land_Surface_Temperature_2020", "Maximum_Temperature_2020", "Night_Land_Surface_Temp_2020")

final_covariates <- covariates[!covariates %in% temp_covs] 
final_covariates

final_formula <- as.formula(paste("Mean ~", paste(final_covariates, collapse = "+")))
final_lmodel <- lm(final_formula, data = direct_covariates)
VIF(final_lmodel)

first_fh <- fh(fixed = final_formula, 
               vardir = "Mean_MSE", 
               combined_data = direct_covariates, 
               method = "ml",
               domains = "Domain", 
               MSE = TRUE
               )

summary(first_fh)
fh_stepped <- step(object = fh_model)
summary(fh_stepped)

## Fit the model after step() selection, OOS-areas not included
first_fh_stepped <- fh(fixed = fh_stepped$call$fixed, 
                       vardir = "Mean_MSE", 
                       combined_data = direct_covariates, 
                       method = "reml",
                       domains = "Domain", 
                       MSE = TRUE
                       )


summary(first_fh_stepped)
plot(first_fh_stepped)
compare_plot(first_fh_stepped, MSE = TRUE)

# -> first model: normality assumptions clearly violated

# FH model including out-of-sample districts (Bani Kenanah, Ghour Essafi)
## Calc aggregated covariates for both districts using the covariates' mean of their respective region
## Region of Bani Kenanah, Ghour Essafi
district_borders[district_borders$NAME_2 == "Bani Kenanah", "NAME_1"] # Irbid
district_borders[district_borders$NAME_2 == "Ghour Essafi", "NAME_1"] # Karak

## Aggregate covariates using all clusters in region of district  
irbid <- as.data.frame(district_borders[district_borders$NAME_1 == "Irbid", "NAME_2"])
irbid_districts <- setdiff(irbid$NAME_2, "Bani Kenanah") # remove Bani Kenanah
irbid_districts

karak <- as.data.frame(district_borders[district_borders$NAME_1 == "Karak", "NAME_2"])
karak_districts <- setdiff(karak$NAME_2, "Ghour Essafi") # remove Ghour Essafi
karak_districts

## Calc mean for all clusters in these districts
### Bani Kenanah (Irbid) 
irbid_clusters <- geo_cluster[geo_cluster$district %in% irbid_districts, "cluster_nr"]$cluster_nr # 116 clusters in Irbid

irbid_mean <- colMeans(st_drop_geometry(geo_cluster[irbid_clusters, 5:28])) # irbid region means for all covariates

bani_kenanah_vector <- c(c("Bani Kenanah", NA, NA, NA), irbid_mean)
names(bani_kenanah_vector) <- names(direct_covariates)

complete_covariates <- rbind(direct_covariates, as.data.frame(t(bani_kenanah_vector)))

### Ghour Essafi (Karak)
karak_clusters <- geo_cluster[geo_cluster$district %in% karak_districts, "cluster_nr"] # 71 clusters in Karak

karak_mean <- colMeans(st_drop_geometry(geo_cluster[karak_clusters, 5:28])) # karak region means for all covariates

ghour_essafi_vector <-c(c("Ghour Essafi", NA, NA, NA), karak_mean)
names(ghour_essafi_vector) <- names(direct_covariates)

complete_covariates <- rbind(complete_covariates, as.data.frame(t(ghour_essafi_vector)))
str(complete_covariates) 

## Convert character cols to numeric, except Domain
complete_covariates[ ,2:28] <- apply(complete_covariates[,2:28], MARGIN = 2, FUN = as.numeric)
str(complete_covariates) 

## FH with out-of-sample districts
final_fh <- fh(fixed = fh_stepped$call$fixed, 
               vardir = "Mean_MSE", 
               combined_data = complete_covariates, 
               method = "reml",
               domains = "Domain", 
               MSE = TRUE 
)

summary(final_fh)
plot(final_fh)
compare_plot(final_fh)

# -> final FH model with out-of-sample districts also not normally distributed

# Analysis of results
complete_results <- estimators(final_fh, MSE = T, CV = T)$ind
## Descriptive statistics about distribution of mean Wealth Index estimators for table 1
summary(complete_results$Direct)
summary(complete_results$FH)

## Compiling results
complete_results$Direct_CV <- abs(complete_results$Direct_CV)
complete_results$FH_CV <- abs(complete_results$FH_CV)
complete_results$better_cv <- complete_results$Direct_CV > complete_results$FH_CV

sample_sizes <- as.data.frame(table(dhs_geo$district))

complete_results <- merge(x = complete_results, 
                          y = sample_sizes,
                          by.x = "Domain",
                          by.y = "Var1", 
                          all.x = TRUE)

## Did the CV improve with FH?
sum(complete_results$better_cv, na.rm = TRUE) # 33 districts have better CVs in FH

# How big is the mean difference?
idx_cv_diff <- complete_results$better_cv
summary(complete_results$Direct_CV[idx_cv_diff] - complete_results$FH_CV[idx_cv_diff], na.rm = TRUE)

# -> Median difference of only 0,1%!
# -> Mean difference of 15%, two regions clearly better with 19% and one with 422% difference in CV between Direct-FH

## Combine data for plotting
only_borders <- select(district_borders, c("NAME_2", "geometry")) 
names(only_borders) <- c("district", "geometry")
complete_borders <- merge(x = only_borders,
                          y = complete_results, 
                          by.x = "district",
                          by.y = "Domain")




# Exploratory: Try FH Model with Log-Shift Transformation
min_mean <- min(complete_covariates$Mean, na.rm = T)

shift <- abs(min_mean) + 1
shift

complete_covariates$Mean_log <- log(complete_covariates$Mean + shift)
complete_covariates$MSE_log <- complete_covariates$Mean_MSE / (complete_covariates$Mean + shift)^2

summary(complete_covariates$Mean_log)
summary(complete_covariates$MSE_log)
summary(complete_covariates$Mean_MSE)

log_formula <- as.formula(paste("Mean_log ~", paste(final_covariates, collapse = "+")))

fh_log_shift <- fh(fixed = log_formula, 
                   vardir = "MSE_log", 
                   combined_data = complete_covariates,
                   domains = "Domain", 
                   method = "ml",
                   MSE = TRUE)

summary(fh_log_shift)
fh_log_stepped <- step(fh_log_shift)

## Use step selection model for fh_log
fh_log_shift_stepped <- fh(fixed = fh_log_stepped$call$fixed, 
                          vardir = "MSE_log", 
                          combined_data = complete_covariates,
                          domains = "Domain", 
                          method = "reml",
                          MSE = TRUE)

summary(fh_log_shift_stepped)
plot(fh_log_stepped)

# -> with log-shift transformation, the random effects don't deviate significantly from normality
# -> residuals still deviate from normality
