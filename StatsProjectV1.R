install.packages("faraway")
install.packages("corrplot")
install.packages("ggplot2")
install.packages("knitr")
install.packages("kableExtra")
install.packages("effsize")
library(effsize)
library(faraway)
library(corrplot)
library(ggplot2)
library(knitr)
library(kableExtra)
library(car)

# Read in data
data(pima)
#1. Descriptive Statistics

  summary(pima[c("pregnant", "glucose", "diastolic", "triceps", "insulin", "bmi", "diabetes", "age")])

#2. Correlation Tests

# Figure 1
  # Display the correlation plot with numerical values
  corrplot(cor(pima[c("age", "bmi", "glucose", "diastolic", "insulin", "test")]), method = "color", type = "upper", order = "hclust", 
           tl.col = "black", tl.srt = 45, addCoef.col = "black")

#3. Sort test variable for further analysis
# Assuming "test" is a binary variable (0 or 1) indicating diabetes status

  # Have to use t test because we are using test column as categorical variable and using 
  # age as our numerical variable. Can try using anova too but cannot use chisq. test, that uses 2 numerical 
  # and we dont have that
  # Subset data for each category
  age_test_positive <- pima$age[pima$test == 1] # age
  age_test_negative <- pima$age[pima$test == 0] # age
  bmi_test_positive <- pima$bmi[pima$test == 1] # bmi
  bmi_test_negative <- pima$bmi[pima$test == 0] # bmi
  glucose_test_positive <- pima$glucose[pima$test == 1] # glucose
  glucose_test_negative <- pima$glucose[pima$test == 0] # glucose

#4. Check for Normality
# Q-Q plot for BMI in test_positive
par(mfrow = c(1, 2))

# Figure 2
  # Q-Q plot for BMI in test_positive
  qqnorm(bmi_test_positive, main = "Q-Q Plot for BMI in Test Positive", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles", col = "lightblue", cex = 1.2)
  # Add the line behind the points
  qqline(bmi_test_positive, col = "darkblue", lty = 2, panel.first = grid())
  # Q-Q plot for BMI in test_negative
  qqnorm(bmi_test_negative, main = "Q-Q Plot for BMI in Test Negative", xlab = "Theoretical Quantiles", ylab = "" , col = "pink", cex = 1.2)
  # Add the line behind the points
  qqline(bmi_test_negative, col = "darkred", lty = 2, panel.first = grid())
  
# Figure 3
  # Q-Q plot for Glucose in test_positive
  qqnorm(glucose_test_positive, main = "Q-Q Plot for Glucose in Test Positive", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles", col = "lightblue", cex = 1.2)
  # Add the line behind the points
  qqline(glucose_test_positive, col = "darkblue", lty = 2, panel.first = grid())
  # Q-Q plot for Glucose in test_negative
  qqnorm(glucose_test_negative, main = "Q-Q Plot for Glucose in Test Negative", xlab = "Theoretical Quantiles", ylab = "" , col = "pink", cex = 1.2)
  # Add the line behind the points
  qqline(glucose_test_negative, col = "darkred", lty = 2, panel.first = grid())

# Figure 4
  # Q-Q plot for Age in test_positive
  qqnorm(age_test_positive, main = "Q-Q Plot for Age in Test Positive", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles", col = "lightblue", cex = 1.2)
  # Add the line behind the points
  qqline(age_test_positive, col = "darkblue", lty = 2, panel.first = grid())
  # Q-Q plot for Age in test_negative
  qqnorm(age_test_negative, main = "Q-Q Plot for Age in Test Negative", xlab = "Theoretical Quantiles", ylab = "" , col = "pink", cex = 1.2)
  # Add the line behind the points
  qqline(age_test_negative, col = "darkred", lty = 2, panel.first = grid())

par(mfrow = c(1, 1))  # Reset the graphics layout

#5. Check for Homogeneity

  # Age
  # The p-value is 0.1362, which is greater than the typical significance level of 0.05
  # This means that, according to this test, the assumption of equal variances appears to be met.
  leveneTest(c(age_test_positive, age_test_negative), group = c(rep("Positive", length(age_test_positive)), rep("Negative", length(age_test_negative))))
  
  # BMI
  # The p-value is 0.1059, which is greater than the typical significance level of 0.05. 
  # This means that, according to this test, the assumption of equal variances appears to be met.
  leveneTest(c(bmi_test_positive, bmi_test_negative), group = c(rep("Positive", length(bmi_test_positive)), rep("Negative", length(bmi_test_negative))))
  
  # Glucose
  # The small p-value (5.512e-06) suggests that the variances of glucose levels between the test-positive and test-negative groups are significantly different. 
  # This violates the assumption of homogeneity of variances for parametric tests like the t-test.
  # Since homogeneity of variance for glucose is violated, we can set equal variance = False
  leveneTest(c(glucose_test_positive, glucose_test_negative), group = c(rep("Positive", length(glucose_test_positive)), rep("Negative", length(glucose_test_negative))))
  
  # Construct a dataframe
  levene_table <- rbind(
    c("Age", 1, 2.2252, 0.1362),
    c("Glucose", 1, 20.945, 5.512e-06),
    c("BMI", 1, 2.6202, 0.1059)
  )
  colnames(levene_table) <- c("Variable", "Df", "F value", "Pr(>F)")
  # Create a table using kable
  levene_table_kable <- kable(levene_table, format = "html", escape = FALSE) %>%
    kable_styling("striped", full_width = FALSE) %>%
    add_header_above(c(" " = 1, "Levene's Test Results" = 3))

# Print the table
print(levene_table_kable)

#6. Testing for significance 
# Used T.test
  
  # Perform t-test for age
  # Did not fully account for normalization of Age variable because didn't want to split the type of t-test between variables
  t_test_age <- t.test(age_test_positive, age_test_negative)
  # Perform t-test for bmi
  t_test_bmi <- t.test(bmi_test_positive, bmi_test_negative)
  # Perform Welch's t-test for Glucose, adjust for homogeneity 
  t_test_glucose_welch <- t.test(glucose_test_positive, glucose_test_negative, var.equal = FALSE)

# Figure 6
  # Welch's Two Sample t-test results
  t_test_results <- rbind(
    c("Age", t = 6.9207, df = 575.78, p_value = 1.202e-11, ci_lower = 4.209236, ci_upper = 7.545092),
    c("BMI", t = 8.6193, df = 573.47, p_value = 2.2e-16, ci_lower = 3.735811, ci_upper = 5.940864),
    c("Glucose", t = 13.752, df = 461.33, p_value = 2.2e-16, ci_lower = 26.80786, ci_upper = 35.74707)
  )
  colnames(t_test_results) <- c("Variable", "t", "df", "p-value", "95% CI Lower", "95% CI Upper")
  # Create a table using kable
  t_test_table <- kable(t_test_results, format = "html", escape = FALSE) %>%
    kable_styling("striped", full_width = FALSE) %>%
    add_header_above(c(" " = 1, "Welch's T-Test Results" = 5))

# Print the table
print(t_test_table)

# Figure 7 - Effect Size

  # Calculate Cohen's d for age
  cohen_d_age <- cohen.d(age_test_positive, age_test_negative)
  # Calculate Cohen's d for BMI
  cohen_d_bmi <- cohen.d(bmi_test_positive, bmi_test_negative)
  # Calculate Cohen's d for glucose
  cohen_d_glucose <- cohen.d(glucose_test_positive, glucose_test_negative)
  # Effect size results
  effect_size_results <- rbind(
    c("Age", d_estimate = 0.5142448, ci_lower = 0.3634140, ci_upper = 0.6650756, interpretation = "medium"),
    c("BMI", d_estimate = 0.641366, ci_lower = 0.4893184, ci_upper = 0.7934137, interpretation = "medium"),
    c("Glucose", d_estimate = 1.105307, ci_lower = 0.9467144, ci_upper = 1.2638992, interpretation = "large")
  )
  colnames(effect_size_results) <- c("Variable", "d estimate", "95% CI Lower", "95% CI Upper", "Interpretation")
  # Create a table using kable
  effect_size_table <- kable(effect_size_results, format = "html", escape = FALSE) %>%
    kable_styling("striped", full_width = FALSE) %>%
    add_header_above(c(" " = 1, "Effect Size Results" = 4))
  
# Print the table
print(effect_size_table)

# Done with analysis on first dataset
#######################################################
# Begining the analysis on second datset. 

# Read in the data
file <- file.choose()
data<- read.csv(file,header=TRUE)
# Subset by women (since other data was only women)
women_data<- subset(data, Gender=="F")

#1. Descriptive Statistics
  
  summary(women_data[c("AGE","HbA1c", "Chol", "BMI", "CLASS")])


#2. Correlation Tests
par(mfrow = c(1, 1))  # Reset the graphics layout

# Figure 8
  # Display the correlation plot with numerical values
  women_data$test <- as.numeric(factor(women_data$CLASS, levels = c("N", "P", "Y"), labels = c(0, 1, 2)))
  corrplot(cor(women_data[c("AGE","HbA1c", "Chol", "BMI",  "test")]), method = "color", type = "upper", order = "hclust", 
           tl.col = "black", tl.srt = 45, addCoef.col = "black")


# Subset data for each category
  women_age_test_N <- women_data$AGE[data$CLASS == "N"]
  women_age_test_P <- women_data$AGE[data$CLASS == "P"]
  women_age_test_Y <- women_data$AGE[data$CLASS == "Y"]
  
  women_BMI_test_N <- women_data$BMI[data$CLASS == "N"]
  women_BMI_test_P <- women_data$BMI[data$CLASS == "P"]
  women_BMI_test_Y <- women_data$BMI[data$CLASS == "Y"]
  
  women_HbA1c_test_N <- women_data$HbA1c[data$CLASS == "N"]
  women_HbA1c_test_P <- women_data$HbA1c[data$CLASS == "P"]
  women_HbA1c_test_Y <- women_data$HbA1c[data$CLASS == "Y"]

# Making sure we aren't violating assumption of normality:
# Q-Q plot for BMI in test_positive
par(mfrow = c(1, 3))

# Figure 9 - Age
  
  # Tails seem to be skewed for age
  # Q-Q plot for Age in CLASS "Y"
  qqnorm(women_age_test_Y, main = "Q-Q Plot for Age - Diabetic", xlab = "Theoretical Quantiles", ylab = "" , col = "lightblue", cex = 1.2)
  qqline(women_age_test_Y, col = "darkblue", lty = 2, panel.first = grid())
  # Q-Q plot for Age in CLASS "N"
  qqnorm(women_age_test_N, main = "Q-Q Plot for Age - Negative", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles", col = "gray", cex = 1.2)
  qqline(women_age_test_N, col = "black", lty = 2, panel.first = grid())
  # Q-Q plot for Age in CLASS "P"
  qqnorm(women_age_test_P, main = "Q-Q Plot for Age - Prediabetic", xlab = "Theoretical Quantiles", ylab = "" , col = "pink", cex = 1.2)
  qqline(women_age_test_P, col = "darkred", lty = 2, panel.first = grid())

# Figure 10 - BMI
  
  # Q-Q plot for BMI in CLASS "Y"
  qqnorm(women_BMI_test_Y, main = "Q-Q Plot for BMI - Diabetic", xlab = "Theoretical Quantiles", ylab = "" , col = "lightblue", cex = 1.2)
  qqline(women_BMI_test_Y, col = "darkblue", lty = 2, panel.first = grid())
  # Q-Q plot for BMI in CLASS "N"
  qqnorm(women_BMI_test_N, main = "Q-Q Plot for BMI - Negative", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles", col = "gray", cex = 1.2)
  qqline(women_BMI_test_N, col = "black", lty = 2, panel.first = grid())
  # Q-Q plot for BMI in CLASS "P"
  qqnorm(women_BMI_test_P, main = "Q-Q Plot for BMI - Prediabetic", xlab = "Theoretical Quantiles", ylab = "" , col = "pink", cex = 1.2)
  qqline(women_BMI_test_P, col = "darkred", lty = 2, panel.first = grid())

 # Figure 11 - HbA1c
  
  # Q-Q plot for HbA1c in CLASS "Y"
  qqnorm(women_HbA1c_test_Y, main = "Q-Q Plot for HbA1c - Diabetic", xlab = "Theoretical Quantiles", ylab = "" , col = "lightblue", cex = 1.2)
  qqline(women_HbA1c_test_Y, col = "darkblue", lty = 2, panel.first = grid())
  # Q-Q plot for HbA1c in CLASS "N"
  qqnorm(women_HbA1c_test_N, main = "Q-Q Plot for HbA1c - Negative", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles", col = "gray", cex = 1.2)
  qqline(women_HbA1c_test_N, col = "black", lty = 2, panel.first = grid())
  # Q-Q plot for HbA1c in CLASS "P"
  qqnorm(women_HbA1c_test_P, main = "Q-Q Plot for HbA1c - Prediabetic", xlab = "Theoretical Quantiles", ylab = "" , col = "pink", cex = 1.2)
  qqline(women_HbA1c_test_P, col = "darkred", lty = 2, panel.first = grid())

par(mfrow = c(1, 1))  # Reset the graphics layout

# Figure 12, Checking for homogeneity

  # Homogeneity of variances check for AGE
  leveneTest(c(women_age_test_N, women_age_test_P, women_age_test_Y),group = c(rep("N", length(women_age_test_N)),rep("P", length(women_age_test_P)),rep("Y", length(women_age_test_Y))))
  # Homogeneity of variances check for BMI
  leveneTest(c(women_BMI_test_N, women_BMI_test_P, women_BMI_test_Y),group = c(rep("N", length(women_BMI_test_N)),rep("P", length(women_BMI_test_P)),rep("Y", length(women_BMI_test_Y))))
  # Homogeneity of variances check for HbA1c
  leveneTest(c(women_HbA1c_test_N, women_HbA1c_test_P, women_HbA1c_test_Y),group = c(rep("N", length(women_HbA1c_test_N)),rep("P", length(women_HbA1c_test_P)),rep("Y", length(women_HbA1c_test_Y))))

  levene_results <- data.frame(
    Variable = c("Age", "BMI", "HbA1c"),
    Df = c(2, 2, 2),
    F_value = c(4.2043, 22.453, 13.77),
    Pr_value = c(0.01555, 5.311e-10, 1.598e-06)
  )
  
  # Create a kable table without Significance
  levene_table <- kable(levene_results, format = "html", escape = FALSE) %>%
    kable_styling("striped", full_width = FALSE) %>%
    add_header_above(c(" ", "Levene's Test Results" = 3))
  
# Display the table
print(levene_table)

##################### . Testing for significance 
#6. Testing for significance 
# Figure 13
  # All of our tests have violated the homogeneity test.
  # Perform anova for age
  # We have more than 2 comparisons so Anova has to be considered
  # Also every test failed homogeneity so we have to use Welches Anova test
  # The following test is supposed to mimic Welch test
  
  # Age
  oneway.test(AGE ~ CLASS, data = women_data, var.equal = FALSE)
  # BMI
  oneway.test(BMI ~ CLASS, data = women_data, var.equal = FALSE)
  # HbA1c
  oneway.test(HbA1c ~ CLASS, data = women_data, var.equal = FALSE)
  
  # Store the results in a data frame
  welch_anova_results <- data.frame(
    Variable = c("Age", "BMI", "HbA1c"),
    F_value = c(70.186, 386.12, 349.53),
    Num_df = c(2.000, 2.000, 2.00),
    Denom_df = c(37.988, 42.961, 149.41),
    P_value = c(1.748e-13, 2.2e-16, 2.2e-16)
  )
  # Print the table using kable
  welch_anova_table <- kable(welch_anova_results, format = "html", escape = FALSE) %>%
    kable_styling("striped", full_width = FALSE) %>%
    add_header_above(c(" " = 1, "Welch's ANOVA-like Test Results" = 4))
  
  
# Print the table
print(welch_anova_table)
  
