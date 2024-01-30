library(ggplot2)
library(stats)

data <- read.csv("smoking_dataset.csv")

str(data)
summary(data)

data$gender<- as.factor(data$gender)
data$hearing.left.<- as.factor(data$hearing.left.)
data$hearing.right.<-as.factor(data$hearing.right.)
data$Urine.protein<-as.factor(data$Urine.protein)
data$oral<-as.factor(data$oral)
data$tartar<-as.factor(data$tartar)
data$dental.caries<-as.factor(data$dental.caries)
data$smoking<-as.factor(data$smoking)

summary(data)

str(data)
# step 1 : data cleaning 

missing_values <- colSums(is.na(data))
#Display the count of missing values
print(missing_values)
#handling of missing values
data <- na.omit(data)

# box plots with outliers
for (col in names(data)) {
  # Check if the variable is numeric
  if (is.numeric(data[[col]])) {
    # Create a boxplot and display
    boxplot_plot <- ggplot(data, aes(y = .data[[col]])) + geom_boxplot(fill = "pink", color = "skyblue") +
      ggtitle(paste("Boxplot of", col))
    print(boxplot_plot)
  }
}
#Removing outliers:
winsorize <- function(x) {
  q <- quantile(x, c(0.01, 0.99), na.rm = TRUE)
  outliers <- x < q[1] | x > q[2]
  if (any(outliers)) {
  }
  x[x < q[1]] <- q[1]
  x[x > q[2]] <- q[2]
  return(x)
}

numeric_vars <- sapply(data, is.numeric)
data[, numeric_vars] <- lapply(data[, numeric_vars], winsorize)

# box plots without outliers
for (col in names(data)) {
  # Check if the variable is numeric
  if (is.numeric(data[[col]])) {
    # Create a boxplot and display
    boxplot_plot <- ggplot(data, aes(y = .data[[col]])) + geom_boxplot(fill = "purple", color = "orange") +
      ggtitle(paste("Boxplot of", col , "without outliers"))
    print(boxplot_plot)
  }
}

#----------------------------------------
#2. Descriptive Statistics:

for (col in names(data)) {
  cat("Summary for variable:", col, "\n")
  print(summary(data[[col]]))
  cat("\n")
}

summary(data)

#histogram and box plots of all numerical variables 
# Loop over all variables
for (col in names(data)) {
  # Check if the variable is numeric
  if (is.numeric(data[[col]])) {
    # Create a histogram and display
    hist_plot <- ggplot(data, aes(x = .data[[col]])) + geom_histogram(binwidth = 1, fill = "blue", color = "black") +
      ggtitle(paste("Histogram of", col))
    print(hist_plot)
    
    # Create a boxplot and display
    boxplot_plot <- ggplot(data, aes(y = .data[[col]])) + geom_boxplot(fill = "green", color = "black") +
      ggtitle(paste("Boxplot of", col))
    print(boxplot_plot)
  }
}


# List of categorical variables
categorical_vars <- c("gender", "hearing.left.", "hearing.right.", "Urine.protein", 
                      "oral", "tartar", "dental.caries", "smoking")

# Loop over each categorical variable
for (var in categorical_vars) {
  # Create a bar plot
  bar_plot <- ggplot(data, aes(x = .data[[var]], fill = ..x..)) +
    geom_bar() +
    scale_fill_viridis_c() +  # You can change the color palette if needed
    ggtitle(paste("Bar Plot of", var)) +
    theme_minimal()
  
  # Print the bar plot
  print(bar_plot)
}

# Loop through categorical variables
for (var in categorical_vars) {
  contingency_table <- table(data[[var]])
  cat("Contingency Table for", var, ":\n")
  print(contingency_table)
  cat("\n")
}

#----------------------------------------
# Step : 3 EDA 
# bar plot of non-numerical variables
for (var in categorical_vars) {
  # Create a bar plot
  print(ggplot(data, aes(x = smoking, fill = get(var))) +
          geom_bar(position = "dodge") +
          labs(title = paste("Bar Plot of Smoking vs", var),
               x = "Smoking",
               y = "Count") +
          theme_minimal())
}

#Box Plots: (To analyze relation b/w all numeric variable & smoking variable)

# Identify numeric variables excluding 'ID'
numeric_vars <- sapply(data[, -which(names(data) == "ID")], is.numeric)

# Extract numeric variable names
numeric_var_names <- names(numeric_vars)[numeric_vars]

# Loop through numeric variables
for (var in numeric_var_names) {
  # Create a box plot
  print(ggplot(data, aes(x = smoking, y = get(var), fill = smoking)) +
          geom_boxplot() +
          labs(title = paste("Box Plot of Smoking vs", var),
               x = "Smoking",
               y = var) +
          theme_minimal())
}


#Scatter plots of numerical variables 
for (var in names(data)[-1]) {
  if (is.numeric(data[[var]])) {
    # Create scatter plot
    plot(data[[var]], data$smoking, main = paste("Scatter Plot of", var, "and Smoking"),
         xlab = var, ylab = "Smoking", col = ifelse(data$smoking == "Yes", "red", "blue"))
  }
}

#----------------------------------------
#Step 4 : Statical Analysis


# Loop through numeric variables
for (var in names(data)[-1]){
  if (is.numeric(data[[var]])) {
    # Mean
    mean_value <- mean(data[[var]], na.rm = TRUE)
    
    # Median
    median_value <- median(data[[var]], na.rm = TRUE)
    
    # Mode
    get_mode <- function(v) {
      uniqv <- unique(v)
      uniqv[which.max(tabulate(match(v, uniqv)))]
    }
    mode_value <- get_mode(data[[var]])
    
    # Standard Deviation
    sd_value <- sd(data[[var]], na.rm = TRUE)
    
    # Variance
    var_value <- var(data[[var]], na.rm = TRUE)
    
    # Print results
    cat("Variable:", var, "\n")
    cat("Mean:", mean_value, "\n")
    cat("Median:", median_value, "\n")
    cat("Mode:", mode_value, "\n")
    cat("Standard Deviation:", sd_value, "\n")
    cat("Variance:", var_value, "\n\n")
  } 
}

#-------------------------------------
#Step-5

#Logistic Regression model
model <- glm(smoking ~ height.cm. + weight.kg. + triglyceride + hemoglobin + 
               ALT + Gtp + tartar + gender + dental.caries, data = data, family = "binomial")
summary(model)



