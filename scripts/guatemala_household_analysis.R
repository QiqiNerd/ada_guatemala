rm(list=ls())
# install.packages(c("tidyverse", "naniar", "GGally"))

# 1. Read data ---------------------------------------------------------------

library(tidyverse)
library(naniar)
library(GGally)

data <- read.csv("cleaned_data/guatemala_household_data.csv")
metadata <- read.csv("metadata/guatemala_metadata.csv")
glimpse(data)
dim(data)
dim(metadata)

# Count the number of variables in each category
category_counts <- metadata %>%
  group_by(category) %>%
  summarise(count = n()) %>%
  arrange(desc(count))
print(category_counts)

ggplot(category_counts, aes(x = reorder(category, -count), y = count)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = count), vjust = -0.5) +
  labs(title = "Number of Variables per Category",
       x = "Category", y = "Count of Variables") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 2. Descriptive statistics + distribution exploration --------------------

library(forcats)

data <- data %>%
  mutate(child_enrollment_rate = ifelse(num_children == 0, NA, num_children_enrolled / num_children))

# select some representative numerical variables for plotting (example)
num_vars <- c("hh_head_age", "thogar", "child_enrollment_rate",
              "p01a06", # Number of rooms
              "p15b02", # Number of businesses
              "num_gas_stove", "num_tv", "num_phone", "num_refrigerator",
              "land_area_sqm", "num_chickens", "num_cattle", "num_pigs",
              "distance_public_hospital","distance_primary_school","distance_market")

cat_vars <- c("region", "area",
              "p01a01", "p01a02", # type of house, wall material
              "p01a05a", "p01a05c", "p01a05d", # water, electricity, phone connection
              "idenho_1", "getnicoh", 
              "p10d01",
              "hh_head_gender", "hh_head_literacy", "max_education_level","female_max_education_level",
              "has_paved_or_gravel_road", "has_train"
)

label_mapping <- c(
  region = "Region", area = "Area", p01a01 = "Type of Housing", p01a02 = "Wall Material",
  p01a05a = "Water Connection", p01a05c = "Electricity Connection", p01a05d = "Phone Connection",
  idenho_1 = "Self-identity", getnicoh = "Indigenous Identity",
  p10d01 = "Worked in the last 12 months - Yes/No",
  hh_head_gender = "Head Gender", hh_head_literacy = "Head Literacy",
  max_education_level = "Max Education", female_max_education_level = "Female Max Education",
  has_paved_or_gravel_road = "Paved Road", has_train = "Train Access"
)

data %>%
  pivot_longer(cols = all_of(cat_vars), names_to = "variable", values_to = "value") %>%
  filter(!is.na(value)) %>%
  mutate(variable = factor(variable, levels = cat_vars)) %>%
  ggplot(aes(x = fct_infreq(value), weight = hh_wgt)) +
  geom_bar(fill = "coral") +
  facet_wrap(~ variable, scales = "free_x", labeller = labeller(variable = label_mapping)) +
  labs(title = "Weighted Distribution of Categorical Variables",
       x = "Category (in English)", y = "Weighted Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

num_label_mapping <- c(
  hh_head_age = "Age of Household Head",
  thogar = "Household Size (num_people)",
  child_enrollment_rate = "Child Enrollment Rate",
  p01a06 = "Number of Rooms",
  p15b02 = "Number of Businesses",
  num_gas_stove = "Gas Stoves", num_tv = "TVs", num_phone = "Phones",
  num_refrigerator = "Refrigerators", land_area_sqm = "Land Area (sqm)",
  num_chickens = "Chickens", num_cattle = "Cattle", num_pigs = "Pigs",
  distance_public_hospital = "Distance to Hospital",
  distance_primary_school = "Distance to School",
  distance_market = "Distance to Market"
)

data %>%
  select(all_of(num_vars), hh_wgt) %>%
  mutate(across(c("land_area_sqm", "num_chickens", "num_cattle", "num_pigs",
                  "distance_public_hospital", "distance_primary_school", "distance_market"), log1p)) %>%
  pivot_longer(cols = -hh_wgt, names_to = "variable", values_to = "value") %>%
  filter(!is.na(value)) %>%
  mutate(variable = factor(variable, levels = num_vars)) %>%
  ggplot(aes(x = value, weight = hh_wgt)) +
  geom_density(fill = "skyblue", alpha = 0.6) +
  facet_wrap(~ variable, scales = "free", labeller = labeller(variable = num_label_mapping)) +
  labs(title = "Weighted Density (Log Scale for Skewed Variables)",
       x = "Value (or log1p)", y = "Density")

# target var
ggplot(data, aes(x = consumption_per_capita_per_day, weight = hh_wgt)) +
  geom_density(fill = "darkgreen", alpha = 0.5) +
  labs(title = "Weighted Density of Daily Per Capita Consumption (USD PPP, 2017)",
       x = "Consumption per Capita per Day",
       y = "Weighted Density") +
  theme_minimal()

library(survey)

svy_design <- svydesign(ids = ~1, weights = ~hh_wgt, data = data)
svymean(~consumption_per_capita_per_day, design = svy_design)
sqrt(svyvar(~consumption_per_capita_per_day, design = svy_design))
svyquantile(~consumption_per_capita_per_day, design = svy_design, 
            quantiles = c(0, 0.25, 0.5, 0.75, 1), ci = TRUE)
svytotal(~I(!is.na(consumption_per_capita_per_day)), design = svy_design)


# 3. Explore Household Assets Data------------------------------------------------------------------

# 3.1 PCA for whole household assets  ----------------------

library(readr)
library(fastDummies)
library(factoextra)

# household assets data
asset_vars <- metadata %>%
  filter(category == "household assets") %>%
  pull(variable_name) %>%
  intersect(colnames(data))

# Extract the asset data and convert it into dummy variables
asset_data <- data %>%
  select(all_of(asset_vars), hh_wgt)  # add survey weight

asset_cat_vars <- asset_data %>%
  select(where(~is.character(.x) | is.factor(.x))) %>%
  colnames()

asset_data_dummy <- fastDummies::dummy_cols(asset_data, select_columns = asset_cat_vars,
                                            remove_first_dummy = TRUE, remove_selected_columns = TRUE)

# Convert to pure numerical type and remove the missing values
asset_data_numeric <- asset_data_dummy %>%
  select(where(is.numeric)) %>%
  drop_na()

# Remove the columns with a variance of 0 (constant sequence)
asset_data_clean <- asset_data_numeric %>%
  select(where(~ var(.x, na.rm = TRUE) != 0))

# Separate the variables and weights
weights <- asset_data_clean$hh_wgt
X <- asset_data_clean %>% select(-hh_wgt) %>% as.matrix()

# Standardization (using weighted mean and weighted standard deviation)
weighted_mean <- colSums(X * weights) / sum(weights)
centered_X <- sweep(X, 2, weighted_mean, "-")

weighted_sd <- sqrt(colSums(weights * centered_X^2) / sum(weights))
standardized_X <- sweep(centered_X, 2, weighted_sd, "/")

# Construct the weighted covariance matrix
weighted_cov <- t(standardized_X) %*% (standardized_X * weights) / sum(weights)
pca_result <- eigen(weighted_cov)

top_n <- 20
explained_var <- pca_result$values / sum(pca_result$values)
scree_data <- data.frame(
  PC = 1:length(explained_var),
  Variance = explained_var * 100
) %>%
  slice(1:top_n)

ggplot(scree_data, aes(x = PC, y = Variance)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = round(Variance, 1)), vjust = -0.5) +
  labs(title = "Weighted PCA Scree Plot (Top 20 Components)",
       x = "Principal Component",
       y = "Variance Explained (%)") +
  theme_minimal()


# 3.1. CA, rowSums for equips   ---------------------------------------------

# Reselect the variables (eliminate abnormal/rare/reverse variables)
equip_vars_cleaned <- c("num_tv", "num_radio", "num_computer", "num_phone",
                        "num_refrigerator", "num_washer", "num_blender", "num_camera",
                        "num_sewing_machine", "num_motorcycle", "num_bicycle",
                        "num_car", "num_cd_player", "num_fan", "num_vacuum",
                        "num_typewriter", "num_tape_recorder", "num_dryer", 
                        "num_printer", "num_gas_stove", "num_microwave", "num_coffee_maker")

equip_df <- data %>%
  select(all_of(equip_vars_cleaned), hh_wgt) %>%
  drop_na()

weights <- equip_df$hh_wgt
equip_only <- equip_df %>% select(-hh_wgt)

# check Cronbach's Alpha
library(psych)
psych::alpha(equip_only)

# Manual weighted standardization (mean=0, sd=1)
weighted_mean <- colSums(equip_only * weights) / sum(weights)
centered <- sweep(equip_only, 2, weighted_mean, "-")

weighted_sd <- sqrt(colSums(weights * centered^2) / sum(weights))
standardized <- sweep(centered, 2, weighted_sd, "/")

# Calculate the weighted rowSums and write the original data (note the order!)
equip_index_final <- rowSums(standardized)

# Write the index to the original data frame
data$equip_index_final <- NA
data$equip_index_final[complete.cases(equip_df)] <- equip_index_final

# Visualization (Weighted density map)
ggplot(data %>% filter(!is.na(equip_index_final)), aes(x = equip_index_final, weight = hh_wgt)) +
  geom_density(fill = "skyblue", alpha = 0.6) +
  labs(title = "Weighted Distribution of Standardized Equip Index",
       x = "Standardized Equip Score", y = "Density")



# 3.2. MCA for housing conditions ----------------------------------------------
library(FactoMineR)
library(factoextra)

housing_cats <- data %>%
  select(p01a01, p01a02, p01a03, p01a04, 
         p01a05a, p01a05b, p01a05c, p01a05d, p01a05e, p01a05f,
         p01a10, p01a11, p01a12, p01a13, p01a14,
         p01a23, p01a25, p01a26,
         p01a34, p01a35, p01a39, p01a44, p01a45, p01c01,
         hh_wgt) %>%
  drop_na()

housing_vars <- housing_cats %>% select(-hh_wgt)
weights <- housing_cats$hh_wgt

housing_mca <- MCA(housing_vars, row.w = weights, graph = FALSE)

# Extract the score of household in the first dimension as the index
# Nhousing_vars and data must be in the same order to be assigned correctly!
data$housing_index_mca1 <- NA
data_complete <- complete.cases(housing_cats)
data$housing_index_mca1[data_complete] <- housing_mca$ind$coord[, 1]

ggplot(data %>% filter(!is.na(housing_index_mca1)), 
       aes(x = housing_index_mca1, weight = hh_wgt)) +
  geom_density(fill = "darkgreen", alpha = 0.6) +
  labs(title = "Housing Quality Index (MCA with Survey Weights)", 
       x = "Housing Score (Dimension 1)", 
       y = "Density")

# eigenvalues
housing_eig_vals <- get_eigenvalue(housing_mca)
print(housing_eig_vals)

fviz_screeplot(housing_mca, addlabels = TRUE, barfill = "darkgreen",
               main = "Variance Explained by Dimensions (Weighted MCA)")

# Check the cos2 of the variable (indicating quality)
var_cos2 <- get_mca_var(housing_mca)$cos2
head(sort(var_cos2[,1], decreasing = TRUE), 10)


# 3.3. K means for livestock and land ------------------------------------------------------

library(tidyverse)
library(reshape2)

# agri vars and weights
agri_vars <- c("num_cattle", "num_goats", "num_sheep", "num_pigs",
               "num_rabbits", "num_chickens", "num_turkeys", "num_ducks",
               "num_horses_donkeys_mules", "num_beehives", "num_other_animals",
               "land_area_sqm")

agri_data <- data %>%
  select(all_of(agri_vars), hh_wgt) %>%
  drop_na()

# Remove the variables with a variance of 0
nonconstant_vars <- agri_data %>%
  select(-hh_wgt) %>%
  select(where(~ var(.) != 0)) %>%
  colnames()
agri_data <- agri_data %>% select(all_of(nonconstant_vars), hh_wgt)

# log(x + 1) transformation
log_agri <- agri_data %>%
  mutate(across(-hh_wgt, ~ log(. + 1)))

# Weighted standardization
X <- log_agri %>% select(-hh_wgt) %>% as.matrix()
weights <- log_agri$hh_wgt
weighted_mean <- colSums(X * weights) / sum(weights)
centered <- sweep(X, 2, weighted_mean, "-")
weighted_sd <- sqrt(colSums(weights * centered^2) / sum(weights))
standardized_X <- sweep(centered, 2, weighted_sd, "/")

# clustering
set.seed(123)
agri_clusters <- kmeans(standardized_X, centers = 3)
data$agri_cluster <- NA
data$agri_cluster[as.numeric(rownames(log_agri))] <- agri_clusters$cluster
data$agri_cluster <- factor(data$agri_cluster)

# Consumption difference boxplot (Optional weighted visualization)
ggplot(data %>% filter(!is.na(agri_cluster)), 
       aes(x = agri_cluster, y = consumption_per_capita_per_day)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "Consumption by Agricultural Cluster",
       x = "Agricultural Engagement Group", y = "Daily Consumption (USD PPP)")

# Cluster center heatmap
centers_df <- as.data.frame(agri_clusters$centers)
centers_df$cluster <- factor(1:nrow(centers_df))
centers_long <- melt(centers_df, id.vars = "cluster")

ggplot(centers_long, aes(x = variable, y = cluster, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "darkgreen") +
  labs(title = "Agricultural Cluster Centers (log-scaled + weighted standardized)", 
       x = "Variables", y = "Cluster") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# 3.4. MCA for Energy usage --------------------------------------------------------

# library(FactoMineR)
# library(factoextra)
# library(tidyverse)

# Extract the energy variable column
energy_data <- data %>% 
  select(108:135, hh_wgt) %>%
  drop_na()

# Split the data and weights
energy_vars <- energy_data %>% select(-hh_wgt)
weights <- energy_data$hh_wgt

# weighted MCA
energy_mca <- MCA(energy_vars, row.w = weights, graph = FALSE)

# Extract the household score of the first dimension
data$energy_index_mca1 <- NA
complete_idx <- complete.cases(data[, 108:135])
data$energy_index_mca1[complete_idx] <- energy_mca$ind$coord[, 1]

# Visual score distribution (using survey weight)
ggplot(data %>% filter(!is.na(energy_index_mca1)), 
       aes(x = energy_index_mca1, weight = hh_wgt)) +
  geom_density(fill = "darkorange", alpha = 0.6) +
  labs(title = "Household Energy Use Index (MCA, Weighted)",
       x = "Energy Score (Dim 1)", y = "Density")

# Scree plot of explained variance
fviz_screeplot(energy_mca, addlabels = TRUE, barfill = "darkorange",
               main = "MCA Scree Plot: Energy Usage (Weighted)")

# Extract the cos² ranking of the variable in the first dimension (to measure the contribution)
energy_var_cos2 <- get_mca_var(energy_mca)$cos2
head(sort(energy_var_cos2[, 1], decreasing = TRUE), 10)


# 4. PCA for community characteristic ----------------------------------------------------------------------

library(tidyverse)
library(fastDummies)
library(ggplot2)

# Obtain the community characteristic variables (ensure the intersection exists)
community_vars <- metadata %>%
  filter(category == "community characteristics") %>%
  pull(variable_name) %>%
  intersect(colnames(data))

# Extract the data and add the survey weight
community_data <- data %>%
  select(all_of(community_vars), hh_wgt) %>%
  drop_na()

# Identify and process the categorical variable as dummy
community_cat_vars <- community_data %>%
  select(where(~is.character(.x) | is.factor(.x))) %>%
  colnames()

community_data_dummy <- fastDummies::dummy_cols(community_data, 
                                                select_columns = community_cat_vars,
                                                remove_first_dummy = TRUE,
                                                remove_selected_columns = TRUE)

# Filter the numerical variables and remove the column with variance of 0
community_data_numeric <- community_data_dummy %>%
  select(where(is.numeric))

community_data_clean <- community_data_numeric %>%
  select(where(~ var(.x, na.rm = TRUE) != 0))

# Split the data matrix and the weights
weights <- as.numeric(community_data_clean$hh_wgt)  # 🔧 强制为 numeric
X <- community_data_clean %>% select(-hh_wgt) %>% as.matrix()

# Weighted standardization (dealing with overflow.
weighted_mean <- colSums(X * weights, na.rm = TRUE) / sum(weights)
centered <- sweep(X, 2, weighted_mean, "-")

weighted_sd <- sqrt(colSums((centered^2) * weights, na.rm = TRUE) / sum(weights))

# Filter out the columns with sd of 0 or NA
nonzero_sd_idx <- which(weighted_sd > 0 & !is.na(weighted_sd))
X_valid <- X[, nonzero_sd_idx]
mean_valid <- weighted_mean[nonzero_sd_idx]
sd_valid <- weighted_sd[nonzero_sd_idx]

# standardization
centered_valid <- sweep(X_valid, 2, mean_valid, "-")
standardized_X <- sweep(centered_valid, 2, sd_valid, "/")

# Check for NA or Inf and remove the problematic columns
standardized_X <- standardized_X[, colSums(is.finite(standardized_X)) == nrow(standardized_X)]

# Construct the covariance matrix and perform eigen decomposition
weighted_cov <- t(standardized_X) %*% (standardized_X * weights) / sum(weights)
community_pca <- eigen(weighted_cov)

# Scree plot
explained_var <- community_pca$values / sum(community_pca$values)
scree_data <- data.frame(PC = 1:length(explained_var),
                         Variance = explained_var * 100)

ggplot(scree_data[1:10,], aes(x = PC, y = Variance)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = round(Variance, 1)), vjust = -0.5) +
  labs(title = "Scree Plot of Community Characteristics (Weighted PCA)",
       x = "Principal Component", y = "Variance Explained (%)") +
  theme_minimal()

# loading
loadings <- as.data.frame(community_pca$vectors)
rownames(loadings) <- colnames(X)

top_pc1 <- loadings %>%
  rownames_to_column(var = "variable") %>%
  arrange(desc(abs(V1))) %>%
  slice(1:10)
print(top_pc1)

# household PC1 
community_index_pca1 <- standardized_X %*% community_pca$vectors[, 1]

# write to data
data$community_index_pca1 <- NA
data$community_index_pca1[as.numeric(rownames(community_data_clean))] <- community_index_pca1

# distribution diagram
ggplot(data %>% filter(!is.na(community_index_pca1)), 
       aes(x = community_index_pca1, weight = hh_wgt)) +
  geom_density(fill = "skyblue", alpha = 0.6) +
  labs(title = "Community Infrastructure Index (Weighted PCA)",
       x = "PC1 Score", y = "Density")

# 5. Household demographics -----------------------------------------------

# Household Dependency Index
data <- data %>%
  mutate(dependency_index = (num_children + num_elderly) / (num_adults + 1))  # Prevent 1 from being divided by 0
# Gender Power Proxy
data <- data %>%
  mutate(
    female_ratio = num_female / (num_male + num_female + 0.01), 
    hh_head_female = if_else(hh_head_gender == "femenino", 1, 0, missing = NA_real_)  # 1 = female head
  )
data <- data %>%
  mutate(gender_power_index = 0.5 * female_ratio + 0.5 * hh_head_female)

data <- data %>%
  mutate(
    is_indigenous = if_else(getnicoh == "Indigenas", 1, 0, missing = NA_real_),
    is_minor_ethnicity = if_else(idenho_1 != "No indigena", 1, 0, missing = NA_real_),
    is_nonmarried_head = if_else(hh_head_marital_status %in% c("soltero (a)", "viudo (a)", "divorciado (a)", "separado (a)"), 1, 0, missing = NA_real_),
    social_margin_index = rowSums(across(c(is_indigenous, is_minor_ethnicity, is_nonmarried_head)), na.rm = TRUE)
  )

# Weighted Density Plot for Dependency Index
ggplot(data %>% filter(!is.na(dependency_index)), 
       aes(x = dependency_index, weight = hh_wgt)) +
  geom_density(fill = "steelblue", alpha = 0.6) +
  labs(title = "Dependency Index (Weighted)", x = "Dependency Index", y = "Density")

# Weighted Density Plot for Gender Power Index
ggplot(data %>% filter(!is.na(gender_power_index)), 
       aes(x = gender_power_index, weight = hh_wgt)) +
  geom_density(fill = "darkorange", alpha = 0.6) +
  labs(title = "Gender Power Index (Weighted)", x = "Gender Power Score", y = "Density")

# Weighted Bar Plot for Social Margin Index
ggplot(data %>% filter(!is.na(social_margin_index)), 
       aes(x = factor(social_margin_index), weight = hh_wgt)) +
  geom_bar(fill = "purple", alpha = 0.7) +
  labs(title = "Social Margin Index (Weighted)", x = "Index Score", y = "Weighted Count")


# 6. FAMD for Human Capital -----------------------------------------------

data <- data %>%
  mutate(across(c(hh_head_literacy,
                  hh_head_education_level,
                  max_education_level,
                  female_max_education_level), as.factor))
library(FactoMineR)
library(factoextra)

edu_vars <- data %>%
  select(hh_head_literacy,
         hh_head_education_level,
         max_education_level,
         female_max_education_level,
         child_enrollment_rate,
         hh_wgt) %>%
  drop_na()

# row weight
famd_result <- FAMD(edu_vars %>% select(-hh_wgt), 
                    row.w = edu_vars$hh_wgt, 
                    graph = FALSE)

# The first dimension is the level of family education
data$education_index <- NA
data$education_index[as.numeric(rownames(edu_vars))] <- famd_result$ind$coord[,1]

fviz_screeplot(famd_result, addlabels = TRUE, barfill = "coral") +
  labs(title = "FAMD Scree Plot: Household Education Index")


# distribution diagram
ggplot(data %>% filter(!is.na(education_index)), 
       aes(x = education_index, weight = hh_wgt)) +
  geom_density(fill = "coral", alpha = 0.6) +
  labs(title = "Human Capital Index (Weighted PCA)",
       x = "Factor 1 Score", y = "Density")


# 7. Regression -----------------------------------------------------------

library(survey)

# survey design object
design <- svydesign(ids = ~1, weights = ~hh_wgt, data = data)

# weighted regression
model_svy <- svyglm(consumption_per_capita_per_day ~ 
                      equip_index_final + housing_index_mca1 + agri_cluster + energy_index_mca1 +
                      community_index_pca1 + 
                      dependency_index + gender_power_index + social_margin_index +
                      education_index +
                      area +
                      p15b01 + p15b02 + p10d01 + p10d03 + p10d04 + p10d06 + p10e05a,
                    design = design)

summary(model_svy)

# continuous consumption forecast
# lm_model <- lm(consumption_per_capita_per_day ~ 
#                  equip_index_final + housing_index_mca1 + agri_cluster + energy_index_mca1 +
#                  community_index_pca1 + 
#                  dependency_index + gender_power_index + social_margin_index + 
#                  hh_head_literacy + hh_head_education_level + max_education_level + female_max_education_level + num_children_enrolled +
#                  area + 
#                  p15b01 + p15b02 + p10d01 + p10d03 + p10d04 + p10d06 + p10e05a, 
#                data = data)
# summary(lm_model)

# Calculate Variance Inflation Factors (VIF)
# install.packages("olsrr")
library(olsrr)
ols_vif_tol(model_svy)

# Load libraries
library(car)

# The working status changes to whether to work or not
data <- data %>%
  mutate(worked_12mo = if_else(p10d01 == "si", 1, 0, missing = 0))

# Binary variables of government subsidies
data <- data %>%
  mutate(government_aid = if_else(p10e05a == "si", 1, 0, missing = 0))

# Establish a simplified version of the model
design <- svydesign(ids = ~1, weights = ~hh_wgt, data = data)

model_svy_2 <- svyglm(consumption_per_capita_per_day ~ 
                      equip_index_final + housing_index_mca1 + agri_cluster + energy_index_mca1 +
                      community_index_pca1 + 
                      dependency_index + gender_power_index + social_margin_index + 
                      education_index +
                      area + 
                      p15b01 + p15b02 + worked_12mo + government_aid,
                    design = design)
summary(model_svy_2)

# VIF
library(olsrr)
ols_vif_tol(model_svy_2)

# install.packages("sjPlot")
# Load additional libraries
library(sjPlot)
library(pROC)

# Create meaningful axis labels for variables
pretty_labels <- c(
  "(Intercept)" = "Intercept",
  "equip_index_final" = "Equipment Index",
  "housing_index_mca1" = "Housing Quality Index",
  "agri_cluster2" = "Agri Cluster: Low Involvement",
  "agri_cluster3" = "Agri Cluster: High Involvement",
  "energy_index_mca1" = "Energy Use Index",
  "community_index_pca1" = "Community Infrastructure Index",
  "dependency_index" = "Dependency Ratio",
  "gender_power_index" = "Gender Power Index",
  "social_margin_index" = "Social Marginalization Score",
  "education_index" = "Human Capital Index",
  "areaurbana" = "Urban Area",
  "p15b01si" = "Household Has Business",
  "p15b02" = "Number of Businesses",
  "worked_12mo" = "Worked in Last 12 Months",
  "government_aid" = "Received Government Aid"
)

# Draw the coefficient plot with custom axis labels
plot_model(model_svy_2, 
           type = "est", 
           show.values = TRUE, 
           value.offset = 0.3, 
           title = "Regression Coefficient Plot (with Labels)", 
           axis.labels = rev(pretty_labels[names(coef(model_svy_2))]))  # reverse to match ggplot order


# Coefficient Plot
plot_model(model_svy_2, type = "est", show.values = TRUE, value.offset = 0.3, 
           title = "Regression Coefficient Plot", axis.labels = NULL)


rss <- sum(residuals(model_svy_2)^2)
tss <- sum((data$consumption_per_capita_per_day - mean(data$consumption_per_capita_per_day, na.rm = TRUE))^2)
pseudo_r2 <- 1 - rss / tss
pseudo_r2

regTermTest(model_svy_2, ~ equip_index_final + housing_index_mca1 + agri_cluster +
              energy_index_mca1 + community_index_pca1 + dependency_index + 
              gender_power_index + social_margin_index + education_index + 
              areaurbana + worked_12mo + government_aid)


# 8. logistic regression ---------------------------------------------------------------------

model_vars <- c("is_poor", 
                "equip_index_final", "housing_index_mca1", "agri_cluster", "energy_index_mca1",
                "community_index_pca1", "dependency_index", "gender_power_index", 
                "social_margin_index", "education_index", "area", "p15b01", "p15b02", "worked_12mo", "government_aid",
                "hh_wgt")

# Delete the lines containing NA
data_model <- data %>% select(all_of(model_vars)) %>% drop_na()

# new survey design
svy_design_clean <- svydesign(ids = ~1, weights = ~hh_wgt, data = data_model)

# fit
svy_logit_model <- svyglm(is_poor ~ 
                            equip_index_final + housing_index_mca1 + agri_cluster + energy_index_mca1 +
                            community_index_pca1 + dependency_index + gender_power_index + 
                            social_margin_index + education_index + area + p15b01 + p15b02 + worked_12mo + government_aid,
                          design = svy_design_clean,
                          family = quasibinomial())

# predict
data_model$prob_poor <- predict(svy_logit_model, type = "response")

# ROC
library(pROC)
roc_result <- roc(response = data_model$is_poor, predictor = data_model$prob_poor)
plot(roc_result, col = "darkgreen", main = "Survey-weighted Logistic Model: ROC Curve")
auc(roc_result)

data_model$predicted_class <- ifelse(data_model$prob_poor >= 0.5, 1, 0)
confusion_matrix <- table(Predicted = data_model$predicted_class,
                          Actual = data_model$is_poor)
print(confusion_matrix)


# 9. Random forest -----------------------------------------------------------
library(randomForest)
library(pROC)
library(caret)
library(dplyr)

all_data <- data[, c(1:2, 4:82, 89:190,208)]
all_data$is_poor <- as.factor(all_data$is_poor)

#install.packages("ranger")
library(ranger)
rf_data <- data %>%
  filter(!is.na(is_poor), !is.na(hh_wgt)) %>%
  select(all_of(colnames(all_data))) %>%
  drop_na()
rf_data$is_poor <- as.factor(rf_data$is_poor)

set.seed(123)
n <- nrow(rf_data)
train_index <- sample(1:n, size = 0.7 * n)

train_data <- rf_data[train_index, ]
test_data <- rf_data[-train_index, ]

# train model
rf_model <- ranger(
  formula = is_poor ~ .,
  data = train_data %>% select(-hh_wgt),
  case.weights = train_data$hh_wgt,
  probability = TRUE,
  num.trees = 500,
  importance = "impurity"
)

# predict test
rf_test_probs <- predict(rf_model, data = test_data %>% select(-is_poor, -hh_wgt))$predictions[,2]
test_data$rf_pred_prob <- rf_test_probs
test_data$rf_pred_class <- ifelse(rf_test_probs > 0.5, 1, 0)

# AUC and confusion matrix
roc_test <- roc(test_data$is_poor, test_data$rf_pred_prob)
plot(roc_test, col = "firebrick", main = "Random Forest ROC (Test Set)")
auc(roc_test)
table(Predicted = test_data$rf_pred_class, Actual = test_data$is_poor)

coords(roc_test, x = "best", best.method = "youden", ret = c("threshold", "sensitivity", "specificity"))

library(caret)
confusionMatrix(as.factor(test_data$rf_pred_class), test_data$is_poor, positive = "1")

importance_df <- data.frame(
  variable = names(rf_model$variable.importance),
  importance = rf_model$variable.importance
) %>%
  arrange(desc(importance))

head(importance_df, 15)
