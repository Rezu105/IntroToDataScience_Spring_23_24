cirrhosis_data<-read.csv("D:/AIUB/Semester 10/Introduction to Data Science [B]/Final Term/Final Project/dataset/cirrhosis.csv", header=TRUE, sep=",")
cirrhosis_data

colSums(is.na(cirrhosis_data))

#replacing missing values
library(DescTools)
mode_value <- Mode(cirrhosis_data$Drug, na.rm = TRUE)
cirrhosis_data$Drug[is.na(cirrhosis_data$Drug)] <- mode_value
print(mode_value)

mode_value <- Mode(cirrhosis_data$Ascites, na.rm = TRUE)
cirrhosis_data$Ascites[is.na(cirrhosis_data$Ascites)] <- mode_value
print(mode_value)

mode_value <- Mode(cirrhosis_data$Hepatomegaly, na.rm = TRUE)
cirrhosis_data$Hepatomegaly[is.na(cirrhosis_data$Hepatomegaly)] <- mode_value
print(mode_value)

mode_value <- Mode(cirrhosis_data$Spiders, na.rm = TRUE)
cirrhosis_data$Spiders[is.na(cirrhosis_data$Spiders)] <- mode_value
print(mode_value)

mean_value <- mean(cirrhosis_data$Cholesterol, na.rm = TRUE)
cirrhosis_data$Cholesterol[is.na(cirrhosis_data$Cholesterol)] <- mean_value
print(mean_value)

mean_value <- mean(cirrhosis_data$Copper, na.rm = TRUE)
cirrhosis_data$Copper[is.na(cirrhosis_data$Copper)] <- mean_value
print(mean_value)

median_value <- mean(cirrhosis_data$Alk_Phos, na.rm = TRUE)
cirrhosis_data$Alk_Phos[is.na(cirrhosis_data$Alk_Phos)] <- median_value
print(median_value)

mean_value <- mean(cirrhosis_data$SGOT, na.rm = TRUE)
cirrhosis_data$SGOT[is.na(cirrhosis_data$SGOT)] <- mean_value
print(mean_value)

mean_value <- mean(cirrhosis_data$Tryglicerides, na.rm = TRUE)
cirrhosis_data$Tryglicerides[is.na(cirrhosis_data$Tryglicerides)] <- mean_value
print(mean_value)

median_value <- median(cirrhosis_data$Platelets, na.rm = TRUE)
cirrhosis_data$Platelets[is.na(cirrhosis_data$Platelets)] <- median_value
print(median_value)

median_value <- median(cirrhosis_data$Prothrombin, na.rm = TRUE)
cirrhosis_data$Prothrombin[is.na(cirrhosis_data$Prothrombin)] <- median_value
print(median_value)

mode_value <- Mode(cirrhosis_data$Stage, na.rm = TRUE)
cirrhosis_data$Stage[is.na(cirrhosis_data$Stage)] <- mode_value
print(mode_value)

colSums(is.na(cirrhosis_data))





library(dplyr)
sapply(cirrhosis_data, class)

#HISTOGRAM SHOWCASE
cirrhosis_data$Albumin <- as.numeric(as.character(cirrhosis_data$Albumin))
cirrhosis_data$Copper <- as.numeric(as.character(cirrhosis_data$Copper))
cirrhosis_data$Alk_Phos <- as.numeric(as.character(cirrhosis_data$Alk_Phos))
cirrhosis_data$SGOT <- as.numeric(as.character(cirrhosis_data$SGOT))
cirrhosis_data$Tryglicerides <- as.numeric(as.character(cirrhosis_data$Tryglicerides))
cirrhosis_data$Platelets <- as.numeric(as.character(cirrhosis_data$Platelets))
cirrhosis_data$Prothrombin <- as.numeric(as.character(cirrhosis_data$Prothrombin))

par(mfrow = c(3, 3))

hist(cirrhosis_data$Albumin, main = "Histogram of Albumin", xlab = "Albumin", ylab = "Frequency", col = "lightblue")
hist(cirrhosis_data$Copper, main = "Histogram of Copper", xlab = "Copper", ylab = "Frequency", col = "lightgreen")
hist(cirrhosis_data$Alk_Phos, main = "Histogram of Alk_Phos", xlab = "Alk_Phos", ylab = "Frequency", col = "lightcoral")
hist(cirrhosis_data$SGOT, main = "Histogram of SGOT", xlab = "SGOT", ylab = "Frequency", col = "lightgoldenrod")
hist(cirrhosis_data$Tryglicerides, main = "Histogram of Tryglicerides", xlab = "Tryglicerides", ylab = "Frequency", col = "lightpink")
hist(cirrhosis_data$Platelets, main = "Histogram of Platelets", xlab = "Platelets", ylab = "Frequency", col = "lightcyan")
hist(cirrhosis_data$Prothrombin, main = "Histogram of Prothrombin", xlab = "Prothrombin", ylab = "Frequency", col = "lightgray")

par(mfrow = c(1, 1))


library(ggplot2)
library(gridExtra)  # or library(patchwork)

#BAR PLOT SHOWCASE
p1 <- ggplot(cirrhosis_data, aes(x = Drug)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Bar Plot of Drug Attribute", x = "Drug", y = "Count") +
  theme_minimal()

p2 <- ggplot(cirrhosis_data, aes(x = Sex)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Bar Plot of Sex Attribute", x = "Sex", y = "Count") +
  theme_minimal()

p3 <- ggplot(cirrhosis_data, aes(x = Status)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Bar Plot of Status Attribute", x = "Status", y = "Count") +
  theme_minimal()

p4 <- ggplot(cirrhosis_data, aes(x = Ascites)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Bar Plot of Ascites Attribute", x = "Ascites", y = "Count") +
  theme_minimal()

p5 <- ggplot(cirrhosis_data, aes(x = Hepatomegaly)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Bar Plot of Hepatomegaly", x = "Hepatomegaly", y = "Count") +
  theme_minimal()

p6 <- ggplot(cirrhosis_data, aes(x = Spiders)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Bar Plot of Spiders", x = "Spiders", y = "Count") +
  theme_minimal()

p7 <- ggplot(cirrhosis_data, aes(x = Edema)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Bar Plot of Edema", x = "Edema", y = "Count") +
  theme_minimal()

p8 <- ggplot(cirrhosis_data, aes(x = Stage)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Bar Plot of Stage", x = "Stage", y = "Count") +
  theme_minimal()

grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, ncol = 3)



#BOX PLOT SHOWCASE
par(mfrow = c(2, 3))

boxplot(cirrhosis_data$Bilirubin, main = "Boxplot of Bilirubin ")
boxplot(cirrhosis_data$Cholesterol, main = "Boxplot of Cholesterol ")
boxplot(cirrhosis_data$Albumin, main = "Boxplot of Albumin ")
boxplot(cirrhosis_data$Copper, main = "Boxplot of Copper ")
boxplot(cirrhosis_data$Alk_Phos, main = "Boxplot of Alk_Phos ")
boxplot(cirrhosis_data$SGOT, main = "Boxplot of SGOT ")

par(mfrow = c(1, 1))






#MULTIVARIATE DATA EXPLORATION

#VIOLIN PLOT SHOWCASE
library(ggplot2)

ggplot(cirrhosis_data, aes(x = factor(Status), y = SGOT, fill = factor(Status))) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), scale = "count", trim = FALSE) +
  geom_boxplot(width=0.1, fill="white", outlier.shape = NA) +
  labs(title = "Violin Plot of SGOT by Status", x = "Status", y = "SGOT") +
  theme_minimal()

ggplot(cirrhosis_data, aes(x = factor(Status), y = Alk_Phos, fill = factor(Status))) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), scale = "count", trim = FALSE) +
  geom_boxplot(width=0.1, fill="white", outlier.shape = NA) +
  labs(title = "Violin Plot of Alk_Phos by Status", x = "Status", y = "Alk_Phos") +
  theme_minimal()

ggplot(cirrhosis_data, aes(x = factor(Status), y = Tryglicerides, fill = factor(Status))) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), scale = "count", trim = FALSE) +
  geom_boxplot(width=0.1, fill="white", outlier.shape = NA) +
  labs(title = "Violin Plot of Tryglicerides by Status", x = "Status", y = "Tryglicerides") +
  theme_minimal()

ggplot(cirrhosis_data, aes(x = factor(Status), y = Platelets, fill = factor(Status))) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), scale = "count", trim = FALSE) +
  geom_boxplot(width=0.1, fill="white", outlier.shape = NA) +
  labs(title = "Violin Plot of Platelets by Status", x = "Status", y = "Platelets") +
  theme_minimal()






# SCATTER PLOT SHOWCASE
library(ggplot2)
filtered_data <- na.omit(cirrhosis_data[, c('Age', 'Bilirubin', 'Status')])
ggplot(filtered_data, aes(x = Age, y = Bilirubin, color = factor(Status))) +
  geom_point(alpha = 0.7) +
  scale_color_manual(values = c("blue", "green", "red"), 
                     labels = c("Censored", "Censored due to liver tx", "Death")) +
  labs(title = "Scatter Plot of Age vs Serum Bilirubin",
       x = "Age",
       y = "Serum Bilirubin (mg/dl)",
       color = "Status") +
  theme_minimal()

filtered_data <- na.omit(cirrhosis_data[, c('Copper', 'Alk_Phos', 'Status')])
ggplot(filtered_data, aes(x = Copper, y = Alk_Phos, color = factor(Status))) +
  geom_point(alpha = 0.7) +
  scale_color_manual(values = c("blue", "green", "red"), 
                     labels = c("Censored", "Censored due to liver tx", "Death")) +
  labs(title = "Scatter Plot of Copper vs Alk_Phos",
       x = "Copper",
       y = "Alk_Phos",
       color = "Status") +
  theme_minimal()

filtered_data <- na.omit(cirrhosis_data[, c('Cholesterol', 'Albumin', 'Status')])
ggplot(filtered_data, aes(x = Cholesterol, y = Albumin, color = factor(Status))) +
  geom_point(alpha = 0.7) +
  scale_color_manual(values = c("blue", "green", "red"), 
                     labels = c("Censored", "Censored due to liver tx", "Death")) +
  labs(title = "Scatter Plot of Cholesterol vs Albumin",
       x = "Cholesterol",
       y = "Albumin",
       color = "Status") +
  theme_minimal()

filtered_data <- na.omit(cirrhosis_data[, c('Tryglicerides', 'Platelets', 'Status')])
ggplot(filtered_data, aes(x = Tryglicerides, y = Platelets, color = factor(Status))) +
  geom_point(alpha = 0.7) +
  scale_color_manual(values = c("blue", "green", "red"), 
                     labels = c("Censored", "Censored due to liver tx", "Death")) +
  labs(title = "Scatter Plot of Tryglicerides vs Platelets",
       x = "Tryglicerides",
       y = "Platelets",
       color = "Status") +
  theme_minimal()






# SCATTER PLOT MATRIX SHOWCASE

library(ggplot2)
upper_attributes <- c('Age', 'Bilirubin', 'Cholesterol', 'Albumin', 'Status')
upper_data <- cirrhosis_data[, upper_attributes]
pairs(upper_data, col = cirrhosis_data$Status)


upper_attributes <- c('Copper', 'Alk_Phos', 'SGOT', 'Tryglicerides', 'Platelets', 'Status')
upper_data <- cirrhosis_data[, upper_attributes]
pairs(upper_data, col = cirrhosis_data$Status)





# RADAR CHART SHOWCSAE

library(fmsb)

filtered_data <- na.omit(cirrhosis_data)
radar_vars <- c('Age', 'Bilirubin', 'Cholesterol', 'Albumin', 'Copper')

scaled_data <- filtered_data %>%
  mutate(across(all_of(radar_vars), scales::rescale))

mean_values <- colMeans(scaled_data[radar_vars])

plot_data <- data.frame(rbind(rep(1, length(radar_vars)), rep(0, length(radar_vars)), mean_values))
colnames(plot_data) <- radar_vars

radarchart(plot_data,
           axistype = 1,
           pcol = rgb(0, 0, 1, 1),
           plwd = 2,
           plty = 1,
           cglcol = "darkgrey",  
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0, 1, 0.2),
           cglwd = 0.8,
           vlcex = 0.8)

title(main = "Radar Chart of Mean Values", line = 1, cex.main = 1.0)





filtered_data <- na.omit(cirrhosis_data)
radar_vars <- c('Bilirubin', 'Albumin', 'Prothrombin', 'Stage')

scaled_data <- filtered_data %>%
  mutate(across(all_of(radar_vars), scales::rescale))

mean_values <- colMeans(scaled_data[radar_vars])

plot_data <- data.frame(rbind(rep(1, length(radar_vars)), rep(0, length(radar_vars)), mean_values))
colnames(plot_data) <- radar_vars

radarchart(plot_data,
           axistype = 1,
           pcol = rgb(0, 0, 1, 1),
           plwd = 2,
           plty = 1,
           cglcol = "darkgrey",  
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0, 1, 0.2),
           cglwd = 0.8,
           vlcex = 0.8)

title(main = "Radar Chart of Mean Values", line = 1, cex.main = 1.0)
