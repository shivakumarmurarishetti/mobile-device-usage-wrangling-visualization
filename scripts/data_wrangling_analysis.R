# Load necessary libraries
library(ggplot2)
library(dplyr)
library(gridExtra)
library(viridis)
library(reshape2)
library(RColorBrewer)

# Load the dataset
data <- read.csv("cleaned_adw_dataset.csv", stringsAsFactors = FALSE)
#Inspect the structure and summary of the dataset:
# View structure
str(data)

# Summary statistics
summary(data)

#Use the summary() function or a more detailed summary with the psych package:
# Install psych package if not already installed
if (!require(psych)) install.packages("psych")

# Load psych library
library(psych)

# Detailed descriptive statistics
describe(data)

#Use table() to summarize counts:
# Summarize categorical variables
table(data$Gender)
table(data$Operating.System)
table(data$User.Behavior.Class)

# Grouped means by gender
data %>%
  group_by(Gender) %>%
  summarize(
    Avg_Usage_Time = mean(App.Usage.Time..min.day., na.rm = TRUE),
    Avg_Data_Usage = mean(Data.Usage..MB.day., na.rm = TRUE),
    Avg_Age = mean(Age, na.rm = TRUE)
  )

# Grouped means by operating system
data %>%
  group_by(Operating.System) %>%
  summarize(
    Avg_Usage_Time = mean(App.Usage.Time..min.day., na.rm = TRUE),
    Avg_Data_Usage = mean(Data.Usage..MB.day., na.rm = TRUE),
    Avg_Age = mean(Age, na.rm = TRUE)
  )
# Remove the User ID column
data <- data %>% select(-User.ID)

# ---- Prepare Data for Visualizations ----
# Ensure numeric columns for analysis
numeric_data <- data %>%
  select_if(is.numeric)

# Simplify device model names
data <- data %>%
  mutate(Device.Model = case_when(
    Device.Model == "Google Pixel 5" ~ "Google Pixel",
    Device.Model == "OnePlus 9" ~ "OnePlus",
    Device.Model == "Xiaomi Mi 11" ~ "Xiaomi Mi",
    Device.Model == "iPhone 12" ~ "iPhone",
    Device.Model == "Samsung Galaxy S21" ~ "Samsung",
    TRUE ~ Device.Model
  ))

# ---- 1. Box Plot for Numerical Features ----
# Melt data for box plot
melted_data <- melt(numeric_data)

# Abbreviate variable names
melted_data$variable <- factor(melted_data$variable,
                               levels = c("App.Usage.Time..min.day.", "Screen.On.Time..hours.day.",
                                          "Battery.Drain..mAh.day.", "Number.of.Apps.Installed",
                                          "Data.Usage..MB.day.", "Age"),
                               labels = c("App Usage Time", "Screen On Time", "Battery Drain",
                                          "Apps Installed", "Data Usage", "Age")
)

boxplot_plot <- ggplot(melted_data, aes(x = variable, y = value, fill = variable)) +
  geom_boxplot(outlier.color = "red", alpha = 0.7) +
  scale_fill_manual(values = c("#6baed6", "#fd8d3c", "#74c476", "#756bb1", "#ffeda0", "#41b6c4")) +
  labs(
    title = "Box Plot of Numerical Features",
    x = "Features",
    y = "Values"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.position = "none"
  )

# ---- 2. Device Model Analysis ----
device_model_plot <- ggplot(data, aes(x = Device.Model, fill = Device.Model)) +
  geom_bar(color = "black", alpha = 0.8) +
  scale_fill_manual(values = c(
    "Google Pixel" = "#e1a3ff", 
    "OnePlus" = "#8b73ff", 
    "Xiaomi Mi" = "#6baed6", 
    "iPhone" = "#fd8d3c", 
    "Samsung" = "#74c476"
  ))  +
  labs(
    title = "Device Model Analysis",
    x = "Device Model",
    y = "Total Devices"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

# ---- 3. Gender Analysis ----
gender_analysis_plot <- ggplot(data, aes(x = Gender, fill = Gender)) +
  geom_bar(color = "black", alpha = 0.8) +
  scale_fill_manual(values = c("Male" = "#e1a3ff", "Female" = "#8b73ff")) +
  labs(
    title = "Gender Distribution Analysis",
    x = "Gender",
    y = "Total Count"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.position = "none"
  )

# ---- 4. Operating System Analysis ----
operating_system_plot <- ggplot(data, aes(x = Operating.System, fill = Operating.System)) +
  geom_bar(color = "black", alpha = 0.8) +
  scale_fill_manual(values = c("#2b83ba", "#abdda4", "#fdae61", "#d7191c")) +
  labs(
    title = "Operating System Analysis",
    x = "Operating System",
    y = "Total Count"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.position = "none"
  )

# ---- # ---- 5. Distribution of Daily App Usage Time ----
daily_app_usage_histogram <- ggplot(data, aes(x = App.Usage.Time..min.day.)) +
  geom_histogram(binwidth = 50, aes(fill = ..count..), color = "black", alpha = 0.8) +  # Gradient based on count
  scale_fill_gradient(low = "#e1a3ff", high = "#8b73ff") +  # Gradient colors similar to gender analysis
  labs(
    title = "Distribution of Daily App Usage Time",
    x = "App Usage Time (min/day)",
    y = "Count",
    fill = "Frequency"  # Add a legend label for the gradient
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 12, face = "bold"),
    legend.position = "right",  # Display the gradient legend on the right
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10)
  )

# Display the plot
print(daily_app_usage_histogram)


# ---- 6. App Usage Distribution by Gender ----
app_usage_by_gender <- ggplot(data, aes(x = Gender, y = App.Usage.Time..min.day., fill = Gender)) +
  geom_boxplot(color = "black", outlier.color = "red", outlier.size = 2, alpha = 0.8) +
  scale_fill_manual(values = c("Male" = "#e1a3ff", "Female" = "#8b73ff")) +
  labs(
    title = "App Usage Distribution by Gender",
    x = "Gender",
    y = "App Usage Time (min/day)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 12, face = "bold"),
    legend.position = "none"
  )

# ---- 7. Average App Usage by Device Model ----
average_app_usage_by_device_model <- ggplot(data, aes(x = reorder(Device.Model, -App.Usage.Time..min.day.), 
                                                      y = App.Usage.Time..min.day., fill = Device.Model)) +
  geom_bar(stat = "summary", fun = "mean", width = 0.6, color = "black") +
  scale_fill_manual(values = c(
    "Google Pixel" = "#e1a3ff", 
    "OnePlus" = "#8b73ff", 
    "Xiaomi Mi" = "#6baed6", 
    "iPhone" = "#fd8d3c", 
    "Samsung" = "#74c476"
  )) +  # Relevant and consistent colors
  labs(
    title = "Average App Usage by Device Model",
    x = "Device Model",
    y = "Average App Usage Time (min/day)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 10, face = "bold", angle = 45, hjust = 1),
    legend.position = "none"  # No legend for simplicity
  )

# Display the plot
print(average_app_usage_by_device_model)



# ---- Combine and Display Plots ----
# Figure 1: Box Plot, Device Model, Gender Distribution, OS Analysis
grid.arrange(
  boxplot_plot,
  device_model_plot,
  gender_analysis_plot,
  operating_system_plot,
  nrow = 2
)

# Figure 2: Distribution of Daily App Usage Time
print(daily_app_usage_histogram)

# Figure 3: App Usage Distribution by Gender
print(app_usage_by_gender)

# Figure 4: Average App Usage by Device Model
print(average_app_usage_by_device_model)
# Compute the correlation matrix
correlation_matrix <- cor(numeric_data, use = "complete.obs")

# Melt the correlation matrix for ggplot
melted_corr <- melt(correlation_matrix)

# Heatmap with aligned colors
correlation_heatmap <- ggplot(melted_corr, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +  # Add white borders to tiles
  scale_fill_gradientn(colors = c("#6baed6", "#74c476", "#fd8d3c", "#756bb1"), name = "Correlation") +
  labs(
    title = "Correlation Heatmap of Numerical Features",
    x = "Features",
    y = "Features"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10)
  )

# Display the heatmap
print(correlation_heatmap)


#Compare key metrics by gender:
# Grouped means by gender
data %>%
  group_by(Gender) %>%
  summarize(
    Avg_Usage_Time = mean(App.Usage.Time..min.day., na.rm = TRUE),
    Avg_Screen_On_Time = mean(Screen.On.Time..hours.day., na.rm = TRUE),
    Avg_Data_Usage = mean(Data.Usage..MB.day., na.rm = TRUE)
  )

#Compare key metrics by operating system:
# Grouped means by operating system
data %>%
  group_by(Operating.System) %>%
  summarize(
    Avg_Usage_Time = mean(App.Usage.Time..min.day., na.rm = TRUE),
    Avg_Screen_On_Time = mean(Screen.On.Time..hours.day., na.rm = TRUE),
    Avg_Battery_Drain = mean(Battery.Drain..mAh.day., na.rm = TRUE)
  )

#Visualize the relationship between age and app usage:
# Scatterplot of Age vs. App Usage Time
# Scatterplot of Age vs. App Usage Time
age_vs_app_usage <- ggplot(data, aes(x = Age, y = App.Usage.Time..min.day., color = Gender)) +
  geom_point(alpha = 0.6, size = 2) +  # Reduce point size and adjust transparency
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "#6baed6") +  # Linear regression line
  scale_color_manual(values = c("Male" = "#e1a3ff", "Female" = "#8b73ff")) +  # Gender colors
  labs(
    title = "Age vs. App Usage Time",
    x = "Age",
    y = "App Usage Time (min/day)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 12, face = "bold"),
    legend.title = element_text(size = 12, face = "bold"),
    legend.position = "bottom"
  )

# Boxplot of App Usage Time by Operating System
app_usage_by_os <- ggplot(data, aes(x = Operating.System, y = App.Usage.Time..min.day., fill = Operating.System)) +
  geom_boxplot(outlier.color = "red", alpha = 0.8) +
  scale_fill_manual(values = c(
    "iOS" = "#e1a3ff",
    "Android" = "#8b73ff"
  )) +  # Operating system colors
  labs(
    title = "App Usage Time by Operating System",
    x = "Operating System",
    y = "App Usage Time (min/day)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    legend.position = "none"
  )

# Display plots
print(age_vs_app_usage)
print(app_usage_by_os)

# t-test for app usage time by gender
t_test_gender <- t.test(App.Usage.Time..min.day. ~ Gender, data = data)
print(t_test_gender)

# t-test for app usage time by operating system
t_test_os <- t.test(App.Usage.Time..min.day. ~ Operating.System, data = data)
print(t_test_os)

# Correlation between age and app usage time
correlation <- cor.test(data$Age, data$App.Usage.Time..min.day.)
print(correlation)
# Select columns for analysis
columns <- c("App.Usage.Time..min.day.", "Screen.On.Time..hours.day.",
             "Battery.Drain..mAh.day.", "Number.of.Apps.Installed",
             "Data.Usage..MB.day.", "Age")

# Ensure the columns are numeric
for (col in columns) {
  data[[col]] <- as.numeric(as.character(data[[col]]))
}

# Remove rows with missing values
data <- na.omit(data)

# Define appropriate binwidths and custom axis ticks for each plot
binwidths <- list(
  "App.Usage.Time..min.day." = 50,
  "Screen.On.Time..hours.day." = 1,
  "Battery.Drain..mAh.day." = 200,
  "Number.of.Apps.Installed" = 5,
  "Data.Usage..MB.day." = 200,
  "Age" = 5
)

axis_limits <- list(
  "App.Usage.Time..min.day." = list(
    x = seq(0, 600, 100),
    y = seq(0, 0.01, 0.002)
  ),
  "Screen.On.Time..hours.day." = list(
    x = seq(0, 12, 2),
    y = seq(0, 0.1, 0.02)
  ),
  "Battery.Drain..mAh.day." = list(
    x = seq(0, 3000, 500),
    y = seq(0, 0.005, 0.001)
  ),
  "Number.of.Apps.Installed" = list(
    x = seq(0, 100, 20),
    y = seq(0, 0.03, 0.005)
  ),
  "Data.Usage..MB.day." = list(
    x = seq(0, 2500, 500),
    y = seq(0, 0.02, 0.005)
  ),
  "Age" = list(
    x = seq(0, 100, 10),
    y = seq(0, 0.02, 0.005)
  )
)

# Define custom colors for each plot
custom_colors <- c("#e1a3ff", "#8b73ff", "#6baed6", "#2c7fb8", "#253494", "#762a83")

# Create individual plots
plot_list <- list()

for (i in seq_along(columns)) {
  col <- columns[i]
  col_title <- gsub("\\.", " ", col)  # Replace dots with spaces for better titles
  
  p <- ggplot(data, aes_string(x = col)) +
    geom_histogram(aes(y = ..density..), binwidth = binwidths[[col]], fill = custom_colors[i], color = "black", alpha = 0.8) +
    geom_density(color = "black", size = 1.2, adjust = 1.5) +  # Adjust density curve smoothness
    labs(
      title = col_title,
      x = paste(tail(strsplit(col_title, " ")[[1]], 2), collapse = " "),
      y = "Density"
    ) +
    scale_x_continuous(breaks = axis_limits[[col]]$x) +  # Custom x-axis limits
    scale_y_continuous(breaks = axis_limits[[col]]$y) +  # Custom y-axis limits
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 12),
      panel.grid.major = element_line(color = "gray80"),
      panel.border = element_rect(color = "black", fill = NA, size = 0.8)  # Add borders
    )
  
  plot_list[[i]] <- p
}

# Arrange plots in a 2x3 grid layout
grid.arrange(
  grobs = plot_list,
  nrow = 2, ncol = 3,
  top = textGrob("Numerical Feature Analysis", gp = gpar(fontsize = 20, fontface = "bold"))
)
