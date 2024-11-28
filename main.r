

## Checking for packages installation
my_packages <- c("tidyverse", "mice", "VIM", "readr",
                 "dplyr", "stringr", "ggplot2",
                 "psych", "knitr", "nortest", "car", "zoo", "agricolae")
missing_packages <- my_packages[!(my_packages %in%
                                    installed.packages()[, "Package"])]
if (length(missing_packages)) install.packages(missing_packages)
remove("my_packages")
remove("missing_packages")

## Library initialization
library("tidyverse")
library("mice")
library("VIM")
library("readr")
library("dplyr")
library("stringr")
library("ggplot2")
library("psych")
library("knitr")
library("nortest")
library("car")
library("zoo")

## Reading the dataset
cpu_data <- read.csv("Dataset\\Intel_CPUs.csv", header = TRUE,
                     stringsAsFactors = FALSE, na.strings = c("", "N/A"))

cpu_data <- cpu_data %>% select("Vertical_Segment", "Lithography",
                                "Recommended_Customer_Price", "nb_of_Cores", "nb_of_Threads",
                                "Processor_Base_Frequency", "Cache", "Max_Memory_Size",
                                "Max_Memory_Bandwidth", "Execute_Disable_Bit"
)
temp <- cpu_data

## Plot data pattern
aggr_plot <- aggr(cpu_data, col = c("navyblue", "red"),
                  numbers = TRUE,
                  sortVars = TRUE,
                  labels = names(cpu_data),
                  cex.axis = .7,
                  gap = 3,
                  ylab = c("Histogram of Missing data", "Pattern"),
                  cex.numbers = 0.1
)

## Remove any missing data in Recommended_Customer_Price
cpu_data <- cpu_data[!is.na(cpu_data$Recommended_Customer_Price), ]
print(apply(is.na(cpu_data), 2, sum))

print(length((cpu_data$Recommended_Customer_Price)))

## Convert any string data that need to be in integer type.

# Recommended_Customer_Price converting.
recommend_price_cleaning <- function(price_range) {
  if (grepl("-", price_range)) {
    range <- strsplit(price_range, "-")[[1]]
    return((as.double(range[1]) + as.double(range[2])) / 2)
  }
  return(price_range)
}

cpu_data$Recommended_Customer_Price <- gsub("\\$", "", cpu_data$Recommended_Customer_Price) 
cpu_data$Recommended_Customer_Price <- gsub(",", "", cpu_data$Recommended_Customer_Price)

cpu_data$Recommended_Customer_Price <- sapply(cpu_data$Recommended_Customer_Price, recommend_price_cleaning) 
cpu_data$Recommended_Customer_Price <- as.double(cpu_data$Recommended_Customer_Price) 

# Lithography converting.
cpu_data <- cpu_data %>%
  mutate(Lithography = gsub("nm|\\s", "", Lithography),
         Lithography = as.double(Lithography))

df2 <- cpu_data
# Processor_Base_Frequency converting (GHZ & MHZ) to MHZ.
Processor_Base_Frequency_cleaning <- function(string) { # Cleaning function
  if (!is.na(string)) {
    if (grepl("GHz", string)) {
      return(as.double(gsub("GHz|\\s", "", string)) * 1000)
    }
    return(as.double(gsub("MHz|\\s", "", string)))
  }
  return(NA)
}
cpu_data$Processor_Base_Frequency <- sapply(cpu_data$Processor_Base_Frequency,
                                            Processor_Base_Frequency_cleaning)

# Max_Memory_Size converting (TB & GB) to GB.
Max_Memory_Size_cleaning <- function(string) { # Cleaning function
  if (!is.na(string)) {
    if (grepl("TB", string)) {
      return(as.double(gsub("TB|\\s", "", string)) * 1024)
    }
    return(as.double(gsub("GB|\\s", "", string)))
  }
  return(NA)
}
cpu_data$Max_Memory_Size <- sapply(cpu_data$Max_Memory_Size,
                                   Max_Memory_Size_cleaning)

# Max_Memory_Bandwidth converting.
cpu_data <- cpu_data %>%
  mutate(Max_Memory_Bandwidth = gsub("GB/s|\\s", "", Max_Memory_Bandwidth),
         Max_Memory_Bandwidth = as.double(Max_Memory_Bandwidth))

# Cache converting, split into 2 columns represent cache_size, cache_type.
cpu_data <- cpu_data %>%
  mutate(Cache = sub(" ", "", Cache)) %>% # Remove the first space character.
  # Find the first " " character, merge everything from there to the end, then separate into 2 columns
  separate(Cache, sep = " ", into = c("Cache_size", "Cache_type"),
           remove = TRUE, extra = "merge", fill = "right")

Cache_size_cleaning <- function(string) { # Cleaning function
  if (!is.na(string)) {
    if (grepl("KB", string)) {
      return(as.double(gsub("KB", "", string)) / 1024)
    }
    return(as.double(gsub("MB", "", string)))
  }
  return(NA)
}
cpu_data$Cache_size <- sapply(cpu_data$Cache_size, Cache_size_cleaning)


# Numerical Variables filling
cpu_ata$Lithography <- na.locf(cpu_data$Lithography)
cpu_data$nb_of_Threads <- ifelse(is.na(cpu_data$nb_of_Threads), cpu_data$nb_of_Cores*2, cpu_data$nb_of_Threads)
cpu_data$Processor_Base_Frequency[is.na(cpu_data$Processor_Base_Frequency)] <- median(cpu_data$Processor_Base_Frequency, na.rm = TRUE)
cpu_data$Cache_size[is.na(cpu_data$Cache_size)] <- median(cpu_data$Cache_size, na.rm = TRUE)
cpu_data$Max_Memory_Size[is.na(cpu_data$Max_Memory_Size)] <- median(cpu_data$Max_Memory_Size, na.rm = TRUE)
cpu_data$Max_Memory_Bandwidth[is.na(cpu_data$Max_Memory_Bandwidth)] <- median(cpu_data$Max_Memory_Bandwidth, na.rm = TRUE)

write.csv(cpu_data, "Dataset\\Intel_CPUs_cleaned.csv", row.names = FALSE)


par(mfrow = c(1, 1))
hist(cpu_data$Recommended_Customer_Price, main = "Recommended_Customer_Price", xlab = "Recommended_Customer_Price", col="blue2", labels=TRUE)

# Convert to logarit.
cpu_data$Lithography <- log(cpu_data$Lithography)
cpu_data$Recommended_Customer_Price <- log(cpu_data$Recommended_Customer_Price)
cpu_data$nb_of_Cores <- log(cpu_data$nb_of_Cores)
cpu_data$nb_of_Threads <- log(cpu_data$nb_of_Threads)
cpu_data$Processor_Base_Frequency <- log(cpu_data$Processor_Base_Frequency)
cpu_data$Cache_size <- log(cpu_data$Cache_size)
cpu_data$Max_Memory_Size <- log(cpu_data$Max_Memory_Size)
cpu_data$Max_Memory_Bandwidth <- log(cpu_data$Max_Memory_Bandwidth)

# Categorical Variables, Filling with "Missing".
table(cpu_data$Vertical_Segment)
cpu_data$Cache_type[is.na(cpu_data$Cache_type)] = "Missing"
table(cpu_data$Cache_type)
cpu_data$Execute_Disable_Bit[is.na(cpu_data$Execute_Disable_Bit)] = "Missing"
table(cpu_data$Execute_Disable_Bit)

# Print out the number of NA
print(apply(is.na(temp), 2, sum))
print(apply(is.na(cpu_data), 2, sum))

# cpu_data %>% summary()
# Remove any unnecessary data,...
remove("aggr_plot")
remove("temp")
remove("recommend_price_cleaning")
remove("Cache_size_cleaning")
remove("Max_Memory_Size_cleaning")
remove("Processor_Base_Frequency_cleaning")

### Data visualization
## Statistics the numerical data
numerows <- c("Lithography", "Recommended_Customer_Price", "nb_of_Cores",
              "nb_of_Threads", "Processor_Base_Frequency", "Cache_size",
              "Max_Memory_Size", "Max_Memory_Bandwidth")

mean_ = apply(cpu_data[numerows], 2, mean, na.rm = TRUE)
sd_ = apply(cpu_data[numerows], 2, sd, na.rm = TRUE)
min_ = apply(cpu_data[numerows], 2, min, na.rm = TRUE)
first_quantile_ = apply(cpu_data[numerows], 2, quantile, na.rm = TRUE, probs = 0.25)
median_ = apply(cpu_data[numerows], 2, median, na.rm = TRUE)
third_quantile_ = apply(cpu_data[numerows], 2, quantile, na.rm = TRUE, probs = 0.75)
max_ = apply(cpu_data[numerows], 2, max, na.rm = TRUE)

numerical_summary <- cbind(mean_, sd_, min_, first_quantile_, median_, third_quantile_, max_)
rownames(numerical_summary) <- numerows
numerical_summary <- as.data.frame(numerical_summary)
View(numerical_summary)

# View(as.data.frame(cor(cpu_data[numerows])))

## Categorical data
View(as.data.frame(table(cpu_data$Vertical_Segment, dnn="Vertical_Segment")))
View(as.data.frame(table(cpu_data$Cache_type, dnn="Cache_type")))
View(as.data.frame(table(cpu_data$Execute_Disable_Bit, dnn="Execute_Disable_Bit")))

caterows <- c("Vertical_Segment", "Cache_type", "Execute_Disable_Bit")
categorical_summary <- as.data.frame(matrix(ncol=4))
for (i in caterows) {
  count <- length(cpu_data[[i]])
  uniq <- length(unique(cpu_data[[i]]))
  mod <- names(sort(-table(cpu_data[[i]])))[1]
  freq <- length(cpu_data[i][cpu_data[i] == mod])
  newrow <- c(count, uniq, mod, freq)
  categorical_summary <- rbind(categorical_summary, newrow)
}
categorical_summary <- categorical_summary[-1,]
colnames(categorical_summary) <- c("Count", "Unique", "Mode", "Frequency")
rownames(categorical_summary) <- caterows
View(categorical_summary)



## Statistic for linear regression
## Draw graphs
# Histogram for Recommended_Customer_Price
ggplot(cpu_data, aes(x = Recommended_Customer_Price)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  labs(title = "Histogram of Recommended_Customer_Price",
       x = "Recommended_Customer_Price", y = "Frequency")

# Draw boxplot for Recommended_Customer_Price vs Vertical_Segment
ggplot(cpu_data, aes(x = Vertical_Segment, y = Recommended_Customer_Price)) +
  geom_boxplot(fill=c("green2", "lemonchiffon1", 
                      "yellow1", "indianred1"), color = "black") +
  labs(title = "Boxplot of Recommended_Customer_Price vs Vertical_Segment",
       x = "Vertical_Segment", y = "Recommended_Customer_Price")

# Draw boxplot for Recommended_Customer_Price vs Cache_type
ggplot(cpu_data, aes(x = Cache_type, y = Recommended_Customer_Price)) +
  geom_boxplot(fill = c("darkslategray1", "hotpink1", 
                        "#192a40", "olivedrab2",
                        "salmon1"), color = "black") +
  labs(title = "Boxplot of Recommended_Customer_Price vs Cache_type",
       x = "Cache_type", y = "Recommended_Customer_Price")

# Draw boxplot for Recommended_Customer_Price vs Execute_Disable_Bit
ggplot(cpu_data, aes(x = Execute_Disable_Bit, y = Recommended_Customer_Price)) +
  geom_boxplot(fill = c("darkslategray1", "hotpink1", 
                        "olivedrab2"), color = "black") +
  labs(title = "Boxplot of Recommended_Customer_Price vs Execute_Disable_Bit",
       x = "Execute_Disable_Bit", y = "Recommended_Customer_Price")


# Danh sách các biến cần tính tương quan
variables <- c("Lithography", "nb_of_Cores", "nb_of_Threads", 
               "Processor_Base_Frequency", "Cache_size", 
               "Max_Memory_Size", "Max_Memory_Bandwidth")
# Tạo danh sách lưu hệ số tương quan
correlations <- numeric(length(variables))
# Tính toán hệ số tương quan
for (i in seq_along(variables)) {
  var <- variables[i]
  correlations[i] <- cor(cpu_data$Recommended_Customer_Price, cpu_data[[var]], use = "complete.obs")
}
# Xác định biến có tương quan lớn nhất
max_index <- which.max(abs(correlations))  # Giá trị tuyệt đối lớn nhất
max_variable <- variables[max_index]
# Tạo bảng dữ liệu
correlation_table <- data.frame(
  Variable = variables,
  Correlation = correlations,
  Highlight = ifelse(variables == max_variable, "*", "")  # Đánh dấu biến lớn nhất
)

# In bảng
print(correlation_table)


## Draw pairplot for numerical data
pairs(cpu_data[c("Recommended_Customer_Price", "Lithography")], pch=16,
      col="red2", main="Pairplot of Recommended_Customer_Price vs Lithography")
pairs(cpu_data[c("Recommended_Customer_Price", "nb_of_Cores")], pch=16,
      col="blue2", main="Pairplot of Recommended_Customer_Price vs nb_of_Cores")
pairs(cpu_data[c("Recommended_Customer_Price", "nb_of_Threads")], pch=16,
      col="green2", main="Pairplot of Recommended_Customer_Price vs nb_of_Threads")
pairs(cpu_data[c("Recommended_Customer_Price", "Processor_Base_Frequency")], pch=16,
      col="yellow2", main="Pairplot of Recommended_Customer_Price vs Processor_Base_Frequency")
pairs(cpu_data[c("Recommended_Customer_Price", "Cache_size")], pch=16,
      col="purple2", main="Pairplot of Recommended_Customer_Price vs Cache_size")
pairs(cpu_data[c("Recommended_Customer_Price", "Max_Memory_Size")], pch=16,
      col="orange2", main="Pairplot of Recommended_Customer_Price vs Max_Memory_Size")
pairs(cpu_data[c("Recommended_Customer_Price", "Max_Memory_Bandwidth")], pch=16,
      col="brown2", main="Pairplot of Recommended_Customer_Price vs Max_Memory_Bandwidth")

#################################################################################################
############################### LINEAR REGRESSION ###############################################
#################################################################################################

# Split dataset into train_set and test_set
set.seed(12345) # Ensure that generated number are all the same
train_size <- floor(0.8 * nrow(cpu_data)) # Take floor of 80% of data size
train_index <- sample(seq_len(nrow(cpu_data)), size = train_size) # Generate a vector of observation
train_index
train_set <- cpu_data[train_index, ]
test_set <- cpu_data[-train_index, ]
View(train_set)
View(test_set)

#Linear regression for Recommended_Customer_Price
model <- lm(formula = Recommended_Customer_Price ~ Cache_size, data = train_set)
summary(model)

main_model <- model


# Plot Histogram of residuals
ggplot(main_model, aes(x = main_model$residuals)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) +
  labs(title = "Histogram of Residuals",
       x = "Residuals", y = "Density") +
  stat_function(fun = dnorm, 
                args = list(mean = mean(main_model$residuals), 
                            sd = sd(main_model$residuals)),
                col = "red",
                linewidth = 1.5)

# Other plot
par(mfrow = c(2, 2))
plot(main_model)

# Check if residuals are independent.
durbinWatsonTest(main_model)

## Prediction
# Split the real price to another dataframe
predict_data <- cbind(test_set$Recommended_Customer_Price)
colnames(predict_data)[1] <- "Recommended_Customer_Price"
test_set <- select(test_set, -Recommended_Customer_Price) # Remove the price on the test_set.

predict_data <- cbind(predict_data, predict(main_model, newdata = test_set, interval = "prediction", level = 0.9))
predict_data <- data.frame(predict_data)
View(predict_data)
# Calculate the accuracy
correct_prediction <- predict_data %>% filter(Recommended_Customer_Price >= lwr & Recommended_Customer_Price <= upr)

accuracy = nrow(correct_prediction) / nrow(predict_data)
print(accuracy)

par(mfrow = c(1, 1))
plot(predict_data$Recommended_Customer_Price, predict_data$fit)

# Calculate the RMSE
Reg_rmse = sqrt(mean((predict_data$Recommended_Customer_Price - predict_data$fit)^2))
Reg_rmse



#########################################################################################
############################### ANOVA 1 NHÂN Tố  ########################################
#########################################################################################

df <- data.frame(
  Lithography=cpu_data$Lithography,
  Recommend_Customer_Price=cpu_data$Recommended_Customer_Price
)

hist(log(df$Recommend_Customer_Price), main = "Recommended_Customer_Price", xlab = "Recommended_Customer_Price", col="blue2", labels=TRUE)
df %>% select(Lithography) %>% is.na() %>% sum
#[1] 10

#Execute_Disable_Bit
#    Missing          No         Yes 
#  0.043043812 0.005380477 0.951575711

# Remove all NA
df <- df[!is.na(df$Lithography), ]

df %>% select(Lithography) %>% is.na() %>% sum
#[1] 0

df %>% select(Lithography) %>% table()
#Lithography
# 14  22  32  45  65  90 130 
#407 481 211 140  27  15  10

df %>% select(Lithography) %>% table() %>% length()
#[1] 7


#-----
# Check the normal distribution of RCP
Recommend_Customer_Price   <- df$Recommend_Customer_Price
Lithography                <- as.factor(df$Lithography) #group

Recommend_Customer_Price_Log   <- log(df$Recommend_Customer_Price)
png("Lithography_Price_Boxplot.png")
boxplot(Recommend_Customer_Price_Log ~ Lithography, col="blue")
dev.off()

png("qqnorm.png")
qqnorm(Recommend_Customer_Price_Log)
qqline(Recommend_Customer_Price_Log)
dev.off()

ad.test(Recommend_Customer_Price)
#         Anderson-Darling normality test

# data:  Recommend_Customer_Price
# A = 193.65, p-value < 2.2e-16

shapiro.test(Recommend_Customer_Price)
#         Shapiro-Wilk normality test

# data:  Recommend_Customer_Price
# W = 0.56555, p-value < 2.2e-16

# ----------- Anova 

av <- aov(Recommend_Customer_Price_Log ~ Lithography)
summary(av)
#                          Df Sum Sq Mean Sq F value   Pr(>F)    
# as.factor(Lithography)    6  114.4  19.066   11.74 7.64e-13 ***
# Residuals              1284 2085.4   1.624                     
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

tk <- TukeyHSD(av)

#   Tukey multiple comparisons of means
#     95% family-wise confidence level

# Fit: aov(formula = Recommend_Customer_Price_Log ~ as.factor(Lithography))

# $`as.factor(Lithography)`
#                diff         lwr          upr     p adj
# 22-14  -0.387636364 -0.64110129 -0.134171442 0.0001395
# 32-14  -0.318239062 -0.63749349  0.001015365 0.0513599
# 45-14  -0.788568380 -1.15730270 -0.419834058 0.0000000
# 65-14  -0.315928455 -1.06383426  0.431977346 0.8753154
# 90-14   0.782948098 -0.20650341  1.772399607 0.2272769
# 130-14  1.254544878  0.04991967  2.459170085 0.0348924
# 32-22   0.069397302 -0.24135933  0.380153934 0.9946533
# 45-22  -0.400932016 -0.76233386 -0.039530169 0.0185919
# 65-22   0.071707909 -0.67261016  0.816025974 0.9999569
# 90-22   1.170584462  0.18384205  2.157326872 0.0086049
# 130-22  1.642181242  0.43978023  2.844582250 0.0011399
# 45-32  -0.470329318 -0.88056123 -0.060097410 0.0129168
# 65-32   0.002310607 -0.76690218  0.771523390 1.0000000
# 90-32   1.101187160  0.09553339  2.106840931 0.0213286
# 130-32  1.572783940  0.35481551  2.790752375 0.0027359
# 65-45   0.472639925 -0.31839099  1.263670845 0.5724979
# 90-45   1.571516478  0.54907767  2.593955287 0.0001257
# 130-45  2.043113258  0.81124934  3.274977178 0.0000225
# 90-65   1.098876553 -0.11305546  2.310808566 0.1047813
# 130-65  1.570473333  0.17731620  2.963630469 0.0156251
# 130-90  0.471596780 -1.06480747  2.008001026 0.9716127
png("Tukey.png")
plot(tk, ordered=T)
dev.off()


# bartlett.test(Recommend_Customer_Price_Log ~ Lithography)
# > bartlett.test(Recommend_Customer_Price_Log ~ Lithography)

#         Bartlett test of homogeneity of variances

# data:  Recommend_Customer_Price_Log by Lithography
# Bartlett's K-squared = 19.947, df = 6, p-value = 0.002831


pairwise.t.test(Recommend_Customer_Price_Log, Lithography, p.adjust="bonferroni", pool.sd = T)
#         Pairwise comparisons using t tests with pooled SD 

# data:  Recommend_Customer_Price_Log and Lithography 

#     14      22      32      45      65      90     
# 22  0.00014 -       -       -       -       -      
# 32  0.06935 1.00000 -       -       -       -      
# 45  7.8e-09 0.02270 0.01536 -       -       -      
# 65  1.00000 1.00000 1.00000 1.00000 -       -      
# 90  0.41176 0.00999 0.02633 0.00013 0.15773 -      
# 130 0.04508 0.00122 0.00302 2.3e-05 0.01883 1.00000

# P value adjustment method: bonferroni

pairwise.t.test(Recommend_Customer_Price_Log, Lithography, p.adjust="BH", pool.sd = T)

#         Pairwise comparisons using t tests with pooled SD 

# data:  Recommend_Customer_Price_Log and Lithography 

#     14      22      32      45      65      90     
# 22  3.6e-05 -       -       -       -       -      
# 32  0.00533 0.56337 -       -       -       -      
# 45  7.8e-09 0.00227 0.00192 -       -       -      
# 65  0.26247 0.81488 0.99292 0.10224 -       -      
# 90  0.02745 0.00143 0.00239 3.6e-05 0.01127 -      
# 130 0.00376 0.00024 0.00050 1.1e-05 0.00209 0.42570

# P value adjustment method: BH 

library(agricolae)


print(LSD.test(av, "Lithography"))
# $statistics
#    MSerror   Df     Mean       CV
#   1.624169 1284 5.862757 21.73771

# $parameters
#         test p.ajusted      name.t ntr alpha
#   Fisher-LSD      none Lithography   7  0.05

# $means
#     Recommend_Customer_Price_Log       std   r         se      LCL      UCL
# 130                     7.387047 0.7826265  10 0.40300979 6.596417 8.177677
# 14                      6.132502 1.2895756 407 0.06317109 6.008572 6.256432
# 22                      5.744866 1.2467624 481 0.05810895 5.630867 5.858865
# 32                      5.814263 1.4437459 211 0.08773530 5.642143 5.986383
# 45                      5.343934 1.0492029 140 0.10770890 5.132629 5.555239
# 65                      5.816574 1.3240166  27 0.24526395 5.335412 6.297736
# 90                      6.915450 1.2621507  15 0.32905611 6.269904 7.560997
#          Min      Max      Q25      Q50      Q75
# 130 5.726848 8.213924 7.019980 7.556897 8.036517
# 14  3.044522 9.473550 5.385641 5.917549 7.074270
# 22  2.833213 8.878219 4.927254 5.638355 6.345636
# 32  2.263844 8.444622 4.787492 5.686975 6.997757
# 45  2.944439 7.950502 4.417940 5.513421 5.897154
# 65  3.610918 8.252707 4.762283 5.755742 6.906755
# 90  4.304065 8.213924 6.296361 7.347300 7.824115

# $comparison
# NULL

# $groups
#     Recommend_Customer_Price_Log groups
# 130                     7.387047      a
# 90                      6.915450      a
# 14                      6.132502      b
# 65                      5.816574     bc
# 32                      5.814263      c
# 22                      5.744866      c
# 45                      5.343934      c

# attr(,"class")
# [1] "group"
# > print(LSD.test(av, "Lithography"))
# $statistics
#    MSerror   Df     Mean       CV
#   1.624169 1284 5.862757 21.73771

# $parameters
#         test p.ajusted      name.t ntr alpha
#   Fisher-LSD      none Lithography   7  0.05

# $means
#     Recommend_Customer_Price_Log       std   r         se      LCL      UCL
# 130                     7.387047 0.7826265  10 0.40300979 6.596417 8.177677
# 14                      6.132502 1.2895756 407 0.06317109 6.008572 6.256432
# 22                      5.744866 1.2467624 481 0.05810895 5.630867 5.858865
# 32                      5.814263 1.4437459 211 0.08773530 5.642143 5.986383
# 45                      5.343934 1.0492029 140 0.10770890 5.132629 5.555239
# 65                      5.816574 1.3240166  27 0.24526395 5.335412 6.297736
# 90                      6.915450 1.2621507  15 0.32905611 6.269904 7.560997
#          Min      Max      Q25      Q50      Q75
# 130 5.726848 8.213924 7.019980 7.556897 8.036517
# 14  3.044522 9.473550 5.385641 5.917549 7.074270
# 22  2.833213 8.878219 4.927254 5.638355 6.345636
# 32  2.263844 8.444622 4.787492 5.686975 6.997757
# 45  2.944439 7.950502 4.417940 5.513421 5.897154
# 65  3.610918 8.252707 4.762283 5.755742 6.906755
# 90  4.304065 8.213924 6.296361 7.347300 7.824115

# $comparison
# NULL

# $groups
#     Recommend_Customer_Price_Log groups
# 130                     7.387047      a
# 90                      6.915450      a
# 14                      6.132502      b
# 65                      5.816574     bc
# 32                      5.814263      c
# 22                      5.744866      c
# 45                      5.343934      c


# The portion of the output that we're most interested in is the section titled $groups. 
# The types of lithinium that have different characters in the groups column are significantly different.


#########################################################################################
############################### ANOVA 1 NHÂN TỐ VỚI Lithography  ########################
#########################################################################################

cpu_data_clean <- read.csv("Dataset/Intel_CPUs_cleaned.csv", header = TRUE,
                           stringsAsFactors = FALSE, na.strings = c("", "N/A"))

df_1 <- data.frame(
  Recommend_Customer_Price=cpu_data_clean$Recommended_Customer_Price,
  Lithography=cpu_data_clean$Lithography
)


df_1$Lithography <- as.factor(df_1$Lithography)
df_1 %>% select(Lithography) %>% table()

ggqqplot(df_1, "Recommend_Customer_Price", facet.by = "Lithography")

df_1 %>% group_by(Lithography) %>% shapiro_test(Recommend_Customer_Price)

df_1_log <- df_1
df_1_log$Recommend_Customer_Price <- log(df_1_log$Recommend_Customer_Price)

ggqqplot(df_1_log, "Recommend_Customer_Price", facet.by = "Lithography")

df_1_log %>% group_by(Lithography) %>% shapiro_test(Recommend_Customer_Price)

# Using Levene Tesst
leveneTest(Recommend_Customer_Price_Log ~ Lithography)

Recommend_Customer_Price_Log <- df_1_log$Recommend_Customer_Price # nolint: object_name_linter, line_length_linter.
Lithography <- df_1_log$Lithography # nolint: object_name_linter.

boxplot(Recommend_Customer_Price_Log ~ Lithography, col="blue")

av <- aov(Recommend_Customer_Price_Log ~ Lithography)
summary(av)

tk <- TukeyHSD(av)
tk

plot(tk)

