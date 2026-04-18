
# Libraries ---------------------------------------------------------------

library(dplyr)
library(summarytools)
library(ggplot2)
library(tidyr)
library(knitr)
library(kableExtra)
library(truncnorm) 
library(estimatr) 
library(broom)
library(modelsummary)
library(fixest)
library(reshape2)
library(webshot2)
library(stringr)
library(webshot)
library(htmltools)
library(broom)

# Block Randomization -----------------------------------------------------


assign_blocks <- function(data, value_col, num_blocks = 10) {
  percentiles <- quantile(data[[value_col]], probs = seq(0.1, 1, by = 0.1), type = 6) #blocks=10
  data$block <- cut(data[[value_col]], 
                    breaks = c(-Inf, percentiles),
                    labels = 1:num_blocks,
                    include.lowest = TRUE)
  data$block <- as.numeric(as.character(data$block))  
  return(data)
}

block_randomize <- function(data, block_col) {
  
  blocks <- unique(data[[block_col]])
  treatment <- numeric(nrow(data))
  #randomization within each block
  for(block in blocks) {
    block_indices <- which(data[[block_col]] == block)
    block_size <- length(block_indices)
    block_treatments <- rep(c(0, 1), each = floor(block_size/2))
  
    if(block_size %% 2 != 0) {
      block_treatments <- c(block_treatments, sample(c(0, 1), 1))
    }
    
    #permute treatments within block
    block_treatments <- sample(block_treatments)
    treatment[block_indices] <- block_treatments
  }
  data$treatment <- treatment
  return(data)
}


# Uploading Data ----------------------------------------------------------


set.seed(1000)
# participants_pre <- read.csv("C:/Users/user/OneDrive - mail.tau.ac.il/כלכלה/ניסויים וסקרים/data_update.csv")
# participants_pre <- assign_blocks(participants_pre, value_col = "Instagram_before")
# participants_post <- block_randomize(participants_pre, block_col = "block") 
# write.csv(participants_post, "C:/Users/user/OneDrive - mail.tau.ac.il/כלכלה/ניסויים וסקרים/data_updated_fin.csv", row.names = FALSE)

participants <- read.csv("C:/Users/user/OneDrive - mail.tau.ac.il/כלכלה/ניסויים וסקרים/data_update_fin.csv")
participants <- participants %>% select(-email) #anonymous 

#head(participants)


# Data Setting: mutating feelings and treatment  --------------------------

participants <- participants %>%
  mutate(treatment_label = ifelse(treatment == 1, "Treatment", "Control")) %>% 
  mutate(PositiveFeel_before = (happiness_before + productivity_before + creativity_before)/3) %>% 
  mutate(NegativeFeel_before = (boredom_before + fatigue_before + anxiety_before + lonliness_before + disc_friends_before + disc_current_events_before )/6) 

# Covariates and Balance Table (pre) --------------------------------------

covariates <- c("female", "employed", "single", "iphone", "Instagram_before")
calculate_p_value <- function(covariate) {
  if (is.numeric(participants[[covariate]])) {
    t_test <- t.test(participants[[covariate]] ~ participants$treatment_label)
    return(t_test$p.value)
  } else {
    chi_sq_test <- chisq.test(table(participants[[covariate]], participants$treatment_label))
    return(chi_sq_test$p.value)
  }
}

# Balance table
balance_table <- participants %>%
  group_by(treatment_label) %>%
  summarize(across(all_of(covariates), mean, na.rm = TRUE)) %>%
  pivot_longer(-treatment_label, names_to = "Covariate", values_to = "Mean") %>%
  pivot_wider(names_from = treatment_label, values_from = Mean, names_prefix = "Mean_") %>%
  mutate(
    SMD = abs(Mean_Treatment - Mean_Control) / sd(participants$Instagram_before, na.rm = TRUE),
    p_value = sapply(covariates, calculate_p_value)
  )

if ("Mean_NA" %in% colnames(balance_table)) {
  balance_table <- balance_table %>%
    select(-Mean_NA)
}

# column headers
balance_table %>%
  kable(
    col.names = c("Covariate", "Mean (Control)", "Mean (Treatment)", "Standardized Mean Difference", "P-value"),
    digits = 3,
    caption = "Balance Table: Covariate Comparison (Including Instagram Before Treatment)"
  ) %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE) %>%
  add_header_above(c(" " = 1, "Group Means" = 2, " " = 2))  

# Descriptive Statistics --------------------------------------------------

# Summary statistics
print(dfSummary(participants, 
                varnumbers = FALSE, 
                valid.col = FALSE, 
                graph.magnif = 0.8), 
      method = 'render',
      headings = FALSE,
      bootstrap.css = FALSE)

# Scatter plot for PositiveFeel vs Instagram_before
ggplot(participants, aes(x = Instagram_before, y = PositiveFeel_before)) +
  geom_point(color = "darkgreen") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Positive Feel vs Instagram Before", x = "Instagram Before", y = "Positive Feel Before") +
  theme_minimal()

# Scatter plot for NegativeFeel vs Instagram_before
ggplot(participants, aes(x = Instagram_before, y = NegativeFeel_before)) +
  geom_point(color = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Negative Feel vs Instagram Before", x = "Instagram Before", y = "Negative Feel Before") +
  theme_minimal()


# reshaped data for easier plotting
participants_melted <- melt(participants, id.vars = "Instagram_before", 
                            measure.vars = c("PositiveFeel_before", "NegativeFeel_before"))

# Scatter plot for Negativefeel and Postivefeel vs Instagram_before
ggplot(participants_melted, aes(x = Instagram_before, y = value, color = variable)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Instagram Use Before vs Positive/Negative Feel Before", 
       x = "Instagram Before", y = "Feel Score Before") +
  theme_minimal() +
  scale_color_manual(values = c("PositiveFeel_before" = "darkgreen", "NegativeFeel_before" = "red"))

#Scatter plot for Relationship status vs Instagram_before
ggplot(participants, aes(x = Instagram_before, fill = as.factor(single))) +
  geom_density(alpha = 0.5) +
  labs(title = "Instagram Use Before by Relationship Status", 
       x = "Instagram Before", y = "Density") +
  theme_minimal() +
  scale_fill_manual(values = c("0" = "darkblue", "1" = "coral"), labels = c("Not Single", "Single"))

#Scatter plot for Age  vs Instagram_before
ggplot(participants, aes(x = Instagram_before, y = age)) +
  geom_point(color = "#1f77b4", alpha = 0.6, size = 3) +  
  geom_smooth(method = "lm", color = "red", se = FALSE) +  
  labs(title = "Instagram Usage vs Age",
       x = "Instagram Use Before",
       y = "Age") +
  theme_minimal()

#Scatter plot for Followers  vs Instagram_before
ggplot(participants, aes(x = Instagram_before, y = instagram_followers)) +
  geom_point(color = "coral", alpha = 0.6, size = 3) +  
  geom_smooth(method = "lm", color = "red", se = FALSE) +  
  labs(title = "Instagram Usage vs Followers",
       x = "Instagram Use Before",
       y = "Followers") +
  theme_minimal()


# Distribution of Instagram use before by gender
ggplot(participants, aes(x = Instagram_before, fill = as.factor(female))) +
  geom_histogram(
    binwidth = 30, 
    color = "black", 
    alpha = 0.7, 
    position = "dodge"
  ) +
  theme_minimal() +
  scale_fill_manual(
    values = c("blue", "pink"),
    labels = c("Males", "Females")
  ) +
  labs(
    title = "Distribution of Instagram use Before by Gender",
    x = "Instagram Before",
    y = "Count",
    fill = "Gender"
  ) +
  theme(
    legend.title = element_text(face = "bold"),
    legend.position = "top"
  )
# Instagram Use Before by Group (treatment) 
#distribution by group 
ggplot(data = participants, aes(x = Instagram_before, fill = factor(treatment))) +
  geom_histogram(
    binwidth = 20,  
    color = "black", 
    alpha = 0.5, 
    position = "identity"
  ) +
  theme_minimal() +
  scale_fill_manual(
    values = c("darkturquoise", "purple"),
    labels = c("Control", "Treatment"),
    name = "Group"
  ) +
  scale_x_continuous(
    breaks = seq(0, max(participants$Instagram_before, na.rm = TRUE) + 50, by = 50), 
    limits = c(0, max(participants$Instagram_before, na.rm = TRUE) + 50)
  ) +
  labs(
    title = "Distribution of Instagram Use Before Treatment by Group",
    x = "Instagram Use Before Treatment (minutes/day)",
    y = "Count"
  ) +
  theme(
    legend.title = element_text(face = "bold"),
    legend.position = "top",
    axis.text.x = element_text(size = 10),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

#Aggregated distribution
ggplot(data = participants, aes(x = Instagram_before)) +
  geom_histogram(
    binwidth = 20,  
    color = "black", 
    fill = "lightsalmon",  
    alpha = 0.5
  ) +
  theme_minimal() +
  scale_x_continuous(
    breaks = seq(0, max(participants$Instagram_before, na.rm = TRUE) + 50, by = 50), 
    limits = c(0, max(participants$Instagram_before, na.rm = TRUE) + 50)
  ) +
  labs(
    title = "Distribution of Instagram Use Before Treatment",
    x = "Instagram Use Before Treatment (minutes/day)",
    y = "Count"
  ) +
  theme(
    axis.text.x = element_text(size = 10),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )




# ---- plots ----
# Feelings  -------------------------------------------------------------------

#Positive Feelings treatment group before and after
treatment_pos <- participants %>%
  filter(treatment_label == "Treatment") %>%  # Assuming "Treatment" is the label for the treatment group
  select(treatment_label, PositiveFeel_before, PositiveFeel_after)

feelings_positive <- treatment_pos %>%
  pivot_longer(
    cols = c(PositiveFeel_before, PositiveFeel_after),
    names_to = "Feeling_Time",
    values_to = "Value"
  )

# histogram with overlay
ggplot(data = feelings_positive, aes(x = Value, fill = Feeling_Time)) +
  geom_histogram(
    binwidth = 0.25, 
    color = "black", 
    alpha = 0.5, 
    position = "identity"
  ) +
  theme_minimal() +
  scale_fill_manual(
    values = c("lightskyblue", "lightgreen"),
    labels = c("Before Treatment", "After Treatment")
  ) +
  labs(
    title = "Distribution of Positive Feelings (Treatment Group)",
    x = "Positive Feelings (1-5)",
    y = "Count"
  ) +
  theme(
    legend.title = element_text(face = "bold"),
    legend.position = "top",
    axis.text.x = element_text(size = 10),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

treatment_neg <- participants %>%
  filter(treatment_label == "Treatment") %>%  
  select(treatment_label, NegativeFeel_before, NegativeFeel_after)

feelings_negative <- treatment_neg %>%
  pivot_longer(
    cols = c(NegativeFeel_before, NegativeFeel_after),
    names_to = "Feeling_Time",
    values_to = "Value"
  )

# Plot histogram with overlay
ggplot(data = feelings_negative, aes(x = Value, fill = Feeling_Time)) +
  geom_histogram(
    binwidth = 0.25, 
    color = "black", 
    alpha = 0.5, 
    position = "identity"
  ) +
  theme_minimal() +
  scale_fill_manual(
    values = c("plum1", "tomato1"),
    labels = c("Before Treatment", "After Treatment")
  ) +
  labs(
    title = "Distribution of Negative Feelings (Treatment Group)",
    x = "Positive Feelings (1-5)",
    y = "Count"
  ) +
  theme(
    legend.title = element_text(face = "bold"),
    legend.position = "top",
    axis.text.x = element_text(size = 10),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )
# Instagram bin(20-min Interval) before vs after by Group -----------------
participants <- participants %>%
  mutate(Instagram_bin = cut(Instagram_before, 
                             breaks = seq(0, ceiling(max(Instagram_before, na.rm = TRUE)) + 30, by = 20),
                             include.lowest = TRUE))

ggplot(participants %>% filter(!is.na(Instagram_bin)), aes(x = Instagram_bin, 
                                                           y = Instagram_after, 
                                                           color = treatment_label)) +
  stat_summary(fun = mean, geom = "point") +
  stat_summary(fun = mean, geom = "line", aes(group = treatment_label)) +
  theme_minimal() +
  labs(title = "Relationship between Instagram before and after treatment",
       x = "Instagram_before Bins (20-minute intervals)", 
       y = "Instagram_after", 
       color = "Group") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




# Instagram bin(20-min Interval) before vs after only treatment gr --------

ggplot(participants %>% filter(treatment_label == "Treatment" & !is.na(Instagram_bin)), 
       aes(x = Instagram_bin)) +
  stat_summary(aes(y = Instagram_before, color = "Before"), fun = mean, geom = "point") +
  stat_summary(aes(y = Instagram_after, color = "After"), fun = mean, geom = "point") +
  geom_line(aes(y = Instagram_before, color = "Before", group = 1), stat = "summary", fun = mean) +
  geom_line(aes(y = Instagram_after, color = "After", group = 1), stat = "summary", fun = mean) +
  theme_minimal() +
  scale_color_manual(values = c("Before" = "darkblue", "After" = "dodgerblue")) +
  labs(title = "Instagram Usage Before and After Treatment (Treatment Group Only)",
       x = "Instagram_before (20-minute intervals)", 
       y = "Mean Instagram Usage", 
       color = "Time Period") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Instagram bin(20-min Interval) before vs after only control grou --------

ggplot(participants %>% filter(treatment_label == "Control"), 
       aes(x = Instagram_bin)) +
  stat_summary(aes(y = Instagram_before, color = "Before"), fun = mean, geom = "point") +
  stat_summary(aes(y = Instagram_after, color = "After"), fun = mean, geom = "point") +
  geom_line(aes(y = Instagram_before, color = "Before", group = 1), stat = "summary", fun = mean) +
  geom_line(aes(y = Instagram_after, color = "After", group = 1), stat = "summary", fun = mean) +
  theme_minimal() +
  scale_color_manual(values = c("Before" = "firebrick4", "After" = "red")) +
  labs(title = "Instagram Usage Before and After Treatment (Control Group Only)",
       x = "Instagram_before Bins (20-minute intervals)", 
       y = "Mean Instagram Usage", 
       color = "Time Period") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Instagram bin(block Interval) before vs after only treatment gro --------
participants <- participants %>%
  group_by(block) %>%
  mutate(
    block_label = paste0(
      "Block ", block, " [", 
      min(Instagram_before, na.rm = TRUE), "-", 
      max(Instagram_before, na.rm = TRUE), "]"
    )
  ) %>%
  ungroup()

participants$block_label <- factor(
  participants$block_label,
  levels = participants %>%
    filter(treatment_label == "Treatment") %>%
    distinct(block_label) %>%
    arrange(as.numeric(sub("Block (\\d+).*", "\\1", block_label))) %>%
    pull(block_label)
)

# Plot with ordered x-axis
ggplot(participants %>% filter(treatment_label == "Treatment"), 
       aes(x = block_label)) +
  stat_summary(aes(y = Instagram_before, color = "Before"), fun = mean, geom = "point") +
  stat_summary(aes(y = Instagram_after, color = "After"), fun = mean, geom = "point") +
  geom_line(aes(y = Instagram_before, color = "Before", group = 1), stat = "summary", fun = mean) +
  geom_line(aes(y = Instagram_after, color = "After", group = 1), stat = "summary", fun = mean) +
  theme_minimal() +
  labs(
    title = "Change in Instagram Usage for Treatment Group by Blocks",
    x = "Block (Min-Max Instagram Before)",
    y = "Instagram Usage (Minutes)",
    color = "Usage Time"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = c("Before" = "darkblue", "After" = "dodgerblue"))


# Instagram bin(block Interval) before vs after only control group --------
ggplot(participants %>% filter(treatment_label == "Control"), 
       aes(x = block_label)) +
  stat_summary(aes(y = Instagram_before, color = "Before"), fun = mean, geom = "point") +
  stat_summary(aes(y = Instagram_after, color = "After"), fun = mean, geom = "point") +
  geom_line(aes(y = Instagram_before, color = "Before", group = 1), stat = "summary", fun = mean) +
  geom_line(aes(y = Instagram_after, color = "After", group = 1), stat = "summary", fun = mean) +
  theme_minimal() +
  labs(
    title = "Change in Instagram Usage for Control Group by Blocks",
    x = "Block (Min-Max Instagram Before)",
    y = "Instagram Usage (Minutes)",
    color = "Usage Time"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = c("Before" = "firebrick4", "After" = "red"))



# Instagram Before and After by Gender ------------------------------------
ggplot(participants, aes(x = Instagram_before, y = Instagram_after, color = as.factor(female))) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(
    title = "Relationship between Instagram Before and After by Gender",
    x = "Instagram Before",
    y = "Instagram After",
    color = "Gender"
  ) +
  scale_color_manual(values = c("0" = "lightskyblue", "1" = "pink"))



# ---- Models ----
# Regression: Instagram After, Negative, Positive feelings --------------------------------

model_positive_block <- feols(PositiveFeel_after ~ treatment + PositiveFeel_before + female + employed + single + factor(age_group)  + iphone | block, 
                              data = participants)

model_negative_block <- feols(NegativeFeel_after ~ treatment + NegativeFeel_before + female + employed + single + factor(age_group) + iphone | block, 
                              data = participants)

model_instagram_block <- feols(Instagram_after ~ treatment + Instagram_before + female + employed + single + factor(age_group) + iphone | block, data = participants)
summary(model_instagram_block)

#results
modelsummary(
  list("Positive Feelings" = model_positive_block, 
       "Negative Feelings" = model_negative_block, 
       "Instagram Use After Treatment" = model_instagram_block),
  statistic = "conf.int",
  stars = TRUE,
  title = "Regression Results"
)

combined_models_block <- bind_rows(
  tidy(model_positive_block, conf.int = TRUE) %>% mutate(Model = "Positive Feelings"),
  tidy(model_negative_block, conf.int = TRUE) %>% mutate(Model = "Negative Feelings"),
  tidy(model_instagram_block, conf.int = TRUE) %>% mutate(Model = "Instagram Use After Treatment")
) 

ggplot(combined_models_block, aes(x = estimate, y = term, color = Model)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, 
                 position = position_dodge(width = 0.5)) +
  labs(
    title = "Regression Coefficients with 95% Confidence Intervals",
    x = "Coefficient Estimate",
    y = "Variables",
    color = "Outcome Variable"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold"),
    legend.title = element_text(face = "bold")
  )


# Create the model summary table
reg_table <- modelsummary(
  list("Positive Feelings" = model_positive_block, 
       "Negative Feelings" = model_negative_block, 
       "Instagram Use After Treatment" = model_instagram_block),
  statistic = "conf.int",
  stars = TRUE,
  title = "Regression Results",
  output = "reg_table.html"  # Save as HTML first
)

# Convert HTML table to PNG image
webshot("reg_table.html", "regression_results.png", vwidth = 1000, vheight = 800)



# Regression: Future gray -------------------------------------------------

# multiple covariates
model_future_gray <- feols(
  Future_gray ~ Instagram_before + female + employed + single + factor(age_group)| block, 
  data = participants %>% filter(treatment_label == "Treatment")
)
summary(model_future_gray)
etable(model_future_gray)
modelsummary(model_future_gray)

# Simple model 
model_future_gray_simple <- feols(
  Future_gray ~ Instagram_before, 
  data = participants %>% filter(treatment_label == "Treatment")
)
summary(model_future_gray_simple)

data_treatment <- participants %>%
  filter(treatment_label == "Treatment") %>%
  select(Future_gray, Instagram_before, Instagram_after, block_label) %>%
  drop_na()

data_treatment <- data_treatment %>%
  mutate(Future_gray = factor(Future_gray, levels = c(0, 1), labels = c("No", "Yes")))


ggplot(data_treatment, aes(x = Instagram_before, y = Instagram_after, color = Future_gray)) +
  geom_point(alpha = 0.7) +  
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +  
  labs(
    title = "Instagram Usage Before vs. After Treatment",
    x = "Instagram Usage Before Treatment (minutes/day)",
    y = "Instagram Usage After Treatment (minutes/day)",
    color = "Future Gray"
  ) +
  theme_minimal()

block_summary <- data_treatment %>%
  group_by(block_label) %>%
  summarise(
    percent_gray = mean(as.numeric(Future_gray) - 1) * 100,  # Convert factor to numeric (Yes=1, No=0)
    total = n()
  ) %>%
  ungroup()


ggplot(block_summary, aes(x = factor(block_label), y = percent_gray, fill = factor(block_label))) +
  geom_col(alpha = 0.7, show.legend = FALSE) +  # Bar plot
  geom_text(aes(label = paste0(round(percent_gray, 1), "%")), 
            vjust = -0.5, size = 4, color = "black") +  # Add percentages
  labs(
    title = "Percentage of Future Gray = 1 by Block",
    x = "Block",
    y = "Percentage of Future Gray (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1) 
  )




# Regression: Gaming, Streaming, News -------------------------------------


missing_stats <- participants %>%
  filter(treatment_label == "Treatment") %>%
  summarize(
    missing_other_social = sum(is.na(other_social_media_after)),
    missing_news = sum(is.na(news_apps_after)),
    missing_gaming = sum(is.na(gaming_streaming_after))
  )
print(missing_stats)

model_gaming <- feols(
  gaming_streaming_after ~ treatment + gaming_streaming_before + female + employed + single + factor(age_group) | block, 
  data = participants
)
summary(model_gaming)
etable(model_gaming)

# Regression: Other Social Media ------------------------------------------

model_other_social <- feols(
  other_social_media_after ~ treatment + other_social_media_before + female + employed + single + factor(age_group) | block, 
  data = participants
)

summary(model_other_social)
etable(model_other_social)

ggplot(participants, 
       aes(x = block_label, y = other_social_media_after, color = treatment_label)) +
  stat_summary(fun = mean, geom = "point") +  # Mean value of social media usage after treatment
  stat_summary(fun = mean, geom = "line", aes(group = treatment_label)) +  # Connect the points
  theme_minimal() +
  labs(
    title = "Other Social Media Usage After Treatment by Blocks",
    x = "Block (Min-Max Social Media Before)",
    y = "Social Media Usage After (Minutes)",
    color = "Group"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = c("Control" = "red", "Treatment" = "dodgerblue"))




# Treatment Bar Plot  -----------------------------------------------------

extract_model_results <- function(model, model_name) {
  results <- tidy(model, conf.int = TRUE) %>%
    filter(term == "treatment") %>%  # Keep only treatment effect
    mutate(model = model_name)
  return(results)
}

results_list <- list(
  extract_model_results(model_other_social, "Other Social Media"),
  extract_model_results(model_positive_block, "Positive Feelings"),
  extract_model_results(model_negative_block, "Negative Feelings"),
  extract_model_results(model_instagram_block, "Instagram Usage")
)

results_combined <- bind_rows(results_list)

results_combined <- results_combined %>%
  mutate(p_label = case_when(
    p.value < 0.001 ~ "< 0.001",
    TRUE ~ sprintf("%.3f", p.value)
  ))

ggplot(results_combined, aes(x = factor(model, levels = c("Instagram Usage", "Other Social Media", "Positive Feelings", "Negative Feelings")), 
                             y = estimate, fill = model)) +
  geom_col(width = 0.6, alpha = 0.7, show.legend = FALSE) +  # Bar plot for estimate
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, color = "black") +  # CI lines
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +  # Reference line at zero
  geom_text(aes(label = paste0("Est: ", round(estimate, 2), "\n p=", p_label)), 
            vjust = -0.5, size = 5, color = "black") +  # Show estimates & p-values
  ylim(min(results_combined$conf.low) - 10, max(results_combined$conf.high) + 10) +  # Adjust scale
  labs(
    title = "Treatment Effect on Different Outcomes",
    x = "Outcome",
    y = "Effect Estimate"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title = element_text(face = "bold", size = 12),
    axis.text = element_text(face = "bold", size = 10)
  )

# Interactions : Gender, Age, Relationship status -------------------------

model_positive_gender <- feols(PositiveFeel_after ~ treatment * female, data = participants)
model_negative_gender <- feols(NegativeFeel_after ~ treatment * female, data = participants)
model_instagram_gender <- feols(Instagram_after ~ treatment * female, data = participants)

model_positive_age <- feols(PositiveFeel_after ~ treatment * factor(age_group), data = participants)
model_negative_age <- feols(NegativeFeel_after ~ treatment * factor(age_group), data = participants)
model_instagram_age <- feols(Instagram_after ~ treatment * factor(age_group), data = participants)

model_positive_single <- feols(PositiveFeel_after ~ treatment * single, data = participants)
model_negative_single <- feols(NegativeFeel_after ~ treatment * single, data = participants)
model_instagram_single <- feols(Instagram_after ~ treatment * single, data = participants)


results_gender <- bind_rows(
  tidy(model_positive_gender, conf.int = TRUE) %>% mutate(Model = "Positive Feelings"),
  tidy(model_negative_gender, conf.int = TRUE) %>% mutate(Model = "Negative Feelings"),
  tidy(model_instagram_gender, conf.int = TRUE) %>% mutate(Model = "Instagram Use")
) %>%
  filter(term == "treatment")  # Keep only treatment effect

ggplot(results_gender, aes(x = Model, y = estimate, fill = Model)) +
  geom_col(width = 0.5, alpha = 0.7, show.legend = FALSE) +  # Bar plot for estimates
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, color = "black") +  # CI lines
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +  # Reference line at zero
  labs(
    title = "Effect of Treatment by Gender",
    x = "Outcome Variable",
    y = "Treatment Effect Estimate"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(face = "bold")
  )

modelsummary(
  list(
    "Positive Feelings (Gender)" = model_positive_gender, 
    "Negative Feelings (Gender)" = model_negative_gender, 
    "Instagram Use (Gender)" = model_instagram_gender),
  statistic = "conf.int",
  stars = TRUE,
  title = "Regression Results: Interaction Effects"
)

modelsummary(
  list(
    "Positive Feelings (Age)" = model_positive_age,
    "Negative Feelings (Age)" = model_negative_age,
    "Instagram Use (Age)" = model_instagram_age
  ),
  statistic = "conf.int",
  stars = TRUE,
  title = "Regression Results: Interaction Effects"
)

modelsummary(
  list(
    "Positive Feelings (Single)" = model_positive_single,
    "Negative Feelings (Single)" = model_negative_single,
    "Instagram Use (Single)" = model_instagram_single
  ),
  statistic = "conf.int",
  stars = TRUE,
  title = "Regression Results: Interaction Effects"
)




# Heterogeneous: gender ---------------------------------------------------

results_instagram <- tidy(model_instagram_gender, conf.int = TRUE) 

# coefficient estimates
beta0 <- results_instagram$estimate[results_instagram$term == "(Intercept)"]  # Control (Male)
beta1 <- results_instagram$estimate[results_instagram$term == "female"]        # Control (Female)
beta2 <- results_instagram$estimate[results_instagram$term == "treatment"]     # Treatment (Male)
beta3 <- results_instagram$estimate[results_instagram$term == "treatment:female"]  # Treatment (Female interaction)

# p-values
p_vals <- results_instagram$p.value
p0 <- p_vals[results_instagram$term == "(Intercept)"]
p1 <- p_vals[results_instagram$term == "female"]
p2 <- p_vals[results_instagram$term == "treatment"]
p3 <- p_vals[results_instagram$term == "treatment:female"]

cumulative_results <- tibble(
  group = c("Control (Male)", "Control (Female)", "Treatment (Male)", "Treatment (Female)"),
  estimate = c(beta0, beta0 + beta1, beta0 + beta2, beta0 + beta1 + beta2 + beta3),
  conf.low = c(beta0, beta0 + beta1, beta0 + beta2, beta0 + beta1 + beta2 + beta3) - 1.96 * results_instagram$std.error,
  conf.high = c(beta0, beta0 + beta1, beta0 + beta2, beta0 + beta1 + beta2 + beta3) + 1.96 * results_instagram$std.error,
  p_value = c(p0, p1, p2, p3)  # Add p-values
)

ggplot(cumulative_results, aes(x = group, y = estimate, fill = group)) +
  geom_col(width = 0.6, alpha = 0.7, show.legend = FALSE) +  # Bar plot for estimate
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, color = "black") +  # CI lines
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +  # Reference line at zero
  geom_text(aes(label = paste0("Est: ", round(estimate, 2), "\n pv=", round(p_value, 3))), 
            vjust = -0.5, size = 4, color = "black") +  # Show estimates & p-values
  labs(
    title = "Cumulative Effects of Instagram Usage by Gender and Treatment",
    x = "Group",
    y = "Instagram Usage Effect Estimate"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(face = "bold")
  )




# Heterogeneous: relationship ---------------------------------------------

results_instagram <- tidy(model_instagram_single, conf.int = TRUE)

# coefficient estimates
beta0 <- results_instagram$estimate[results_instagram$term == "(Intercept)"]  # Control (Not Single)
beta1 <- results_instagram$estimate[results_instagram$term == "single"]       # Control (Single)
beta2 <- results_instagram$estimate[results_instagram$term == "treatment"]    # Treatment (Not Single)
beta3 <- results_instagram$estimate[results_instagram$term == "treatment:single"]  # Treatment (Single interaction)

# se and p-values 
se0 <- results_instagram$std.error[results_instagram$term == "(Intercept)"]
se1 <- results_instagram$std.error[results_instagram$term == "single"]
se2 <- results_instagram$std.error[results_instagram$term == "treatment"]
se3 <- results_instagram$std.error[results_instagram$term == "treatment:single"]

p0 <- results_instagram$p.value[results_instagram$term == "(Intercept)"]
p1 <- results_instagram$p.value[results_instagram$term == "single"]
p2 <- results_instagram$p.value[results_instagram$term == "treatment"]
p3 <- results_instagram$p.value[results_instagram$term == "treatment:single"]

cumulative_results <- tibble(
  group = c("Control (Not Single)", "Control (Single)", "Treatment (Not Single)", "Treatment (Single)"),
  estimate = c(beta0, beta0 + beta1, beta0 + beta2, beta0 + beta1 + beta2 + beta3),
  conf.low = c(beta0 - 1.96 * se0, (beta0 + beta1) - 1.96 * sqrt(se0^2 + se1^2), 
               (beta0 + beta2) - 1.96 * sqrt(se0^2 + se2^2), 
               (beta0 + beta1 + beta2 + beta3) - 1.96 * sqrt(se0^2 + se1^2 + se2^2 + se3^2)),
  conf.high = c(beta0 + 1.96 * se0, (beta0 + beta1) + 1.96 * sqrt(se0^2 + se1^2), 
                (beta0 + beta2) + 1.96 * sqrt(se0^2 + se2^2), 
                (beta0 + beta1 + beta2 + beta3) + 1.96 * sqrt(se0^2 + se1^2 + se2^2 + se3^2)),
  p_value = c(p0, p1, p2, p3)  # Add p-values for each group
)

ggplot(cumulative_results, aes(x = group, y = estimate, fill = group)) +
  geom_col(width = 0.6, alpha = 0.7, show.legend = FALSE) +  # Bar plot for estimate
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, color = "black") +  # CI lines
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +  # Reference line at zero
  geom_text(aes(label = paste0("Est: ", round(estimate, 2), "\n pv=", round(p_value, 3))), 
            vjust = -0.5, size = 5, color = "black") +  # Add estimates and p-values to bars
  labs(
    title = "Cumulative Effects of Instagram Usage by Single Status and Treatment",
    x = "Group",
    y = "Instagram Usage Effect Estimate"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(face = "bold")
  )
# Heterogeneous: brand ----------------------------------------------------

model_instagram_iphone <- feols(Instagram_after ~ treatment * iphone, data = participants)

results_instagram <- tidy(model_instagram_iphone, conf.int = TRUE)

beta0 <- results_instagram$estimate[results_instagram$term == "(Intercept)"]  # Control (Android)
beta1 <- results_instagram$estimate[results_instagram$term == "iphone"]        # Control (iPhone)
beta2 <- results_instagram$estimate[results_instagram$term == "treatment"]     # Treatment (Android)
beta3 <- results_instagram$estimate[results_instagram$term == "treatment:iphone"]  # Treatment (iPhone interaction)

# p-values
p_vals <- results_instagram$p.value
p0 <- p_vals[results_instagram$term == "(Intercept)"]
p1 <- p_vals[results_instagram$term == "iphone"]
p2 <- p_vals[results_instagram$term == "treatment"]
p3 <- p_vals[results_instagram$term == "treatment:iphone"]

# standard errors
se_vals <- results_instagram$std.error
se0 <- se_vals[results_instagram$term == "(Intercept)"]
se1 <- se_vals[results_instagram$term == "iphone"]
se2 <- se_vals[results_instagram$term == "treatment"]
se3 <- se_vals[results_instagram$term == "treatment:iphone"]


cumulative_results <- tibble(
  group = c("Control (Android)", "Control (iPhone)", "Treatment (Android)", "Treatment (iPhone)"),
  estimate = c(beta0, beta0 + beta1, beta0 + beta2, beta0 + beta1 + beta2 + beta3),
  conf.low = c(beta0 - 1.96 * se0, 
               (beta0 + beta1) - 1.96 * se1, 
               (beta0 + beta2) - 1.96 * se2, 
               (beta0 + beta1 + beta2 + beta3) - 1.96 * se3),
  conf.high = c(beta0 + 1.96 * se0, 
                (beta0 + beta1) + 1.96 * se1, 
                (beta0 + beta2) + 1.96 * se2, 
                (beta0 + beta1 + beta2 + beta3) + 1.96 * se3),
  p_value = c(p0, p1, p2, p3)  # Add p-values
)

cumulative_results <- cumulative_results %>%
  mutate(p_label = case_when(
    p_value < 0.001 ~ "< 0.001",
    TRUE ~ sprintf("%.3f", p_value)
  ))

ggplot(cumulative_results, aes(x = group, y = estimate, fill = group)) +
  geom_col(width = 0.6, alpha = 0.7, show.legend = FALSE) +  # Bar plot for estimate
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, color = "black") +  # CI lines
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +  # Reference line at zero
  geom_text(aes(label = paste0("Est: ", round(estimate, 2), "\n pv=", p_label)), 
            vjust = -0.5, size = 4, color = "black") +  # Show estimates & p-values
  labs(
    title = "Cumulative Effects of Instagram Usage by iPhone and Treatment",
    x = "Group",
    y = "Instagram Usage Effect Estimate"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(face = "bold")
  )




# Heterogeneous: age ----------------------------------------------------
# Extract model results
results_instagram <- tidy(model_instagram_age, conf.int = TRUE)

# Get baseline (Intercept)
beta0 <- results_instagram$estimate[results_instagram$term == "(Intercept)"]

# Extract age group coefficients dynamically
age_levels <- results_instagram$term[grepl("^factor\\(age_group\\)", results_instagram$term)]
age_estimates <- sapply(age_levels, function(x) {
  val <- results_instagram$estimate[results_instagram$term == x]
  if (length(val) == 0) 0 else val
})

# Extract treatment effect
beta_treatment <- results_instagram$estimate[results_instagram$term == "treatment"]

# Extract interaction terms dynamically
interact_terms <- paste0("treatment:", age_levels)
interact_estimates <- sapply(interact_terms, function(x) {
  val <- results_instagram$estimate[results_instagram$term == x]
  if (length(val) == 0) 0 else val
})

# Compute estimates for each group
estimates <- c(beta0, beta0 + age_estimates, beta0 + beta_treatment, beta0 + age_estimates + beta_treatment + interact_estimates)

# Confidence intervals
se_values <- sapply(results_instagram$term, function(x) {
  val <- results_instagram$std.error[results_instagram$term == x]
  if (length(val) == 0) 0 else val
})

conf.low <- estimates - 1.96 * c(0, se_values[age_levels], se_values["treatment"], se_values[interact_terms])
conf.high <- estimates + 1.96 * c(0, se_values[age_levels], se_values["treatment"], se_values[interact_terms])

# P-values
p_values <- sapply(results_instagram$term, function(x) {
  val <- results_instagram$p.value[results_instagram$term == x]
  if (length(val) == 0) 1 else val
})

p_values <- c(p_values["(Intercept)"], p_values[age_levels], p_values["treatment"], p_values[interact_terms])

# Construct cumulative results
cumulative_results <- tibble(
  group = c("Control (18-25)", paste0("Control (", age_levels, ")"), "Treatment (18-25)", paste0("Treatment (", age_levels, ")")),
  estimate = estimates,
  conf.low = conf.low,
  conf.high = conf.high,
  p_value = p_values
)

# Format p-values
cumulative_results <- cumulative_results %>%
  mutate(p_label = case_when(
    p_value < 0.001 ~ "< 0.001",
    TRUE ~ sprintf("%.3f", p_value)
  ))

# Plot
ggplot(cumulative_results, aes(x = group, y = estimate, fill = group)) +
  geom_col(width = 0.6, alpha = 0.7, show.legend = FALSE) +  
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, color = "black") +  
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +  
  geom_text(aes(label = paste0("Est: ", round(estimate, 2), "\n p=", p_label)), 
            vjust = -0.5, size = 4, color = "black") +  
  labs(
    title = "Cumulative Effects of Instagram Usage by Age Group and Treatment",
    x = "Group",
    y = "Instagram Usage Effect Estimate"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(face = "bold")
  )

  




# Balance table with compliance -------------------------------------------
participants_clean <- participants %>% 
  filter(!is.na(Instagram_after))

# balance table after removing NA values
balance_table <- participants_clean %>%
  group_by(treatment_label) %>%
  summarize(across(all_of(covariates), mean, na.rm = TRUE)) %>%
  pivot_longer(-treatment_label, names_to = "Covariate", values_to = "Mean") %>%
  pivot_wider(names_from = treatment_label, values_from = Mean, names_prefix = "Mean_") %>%
  mutate(
    SMD = abs(Mean_Treatment - Mean_Control) / sd(participants_clean$Instagram_before, na.rm = TRUE),
    p_value = sapply(covariates, calculate_p_value)
  )

if ("Mean_NA" %in% colnames(balance_table)) {
  balance_table <- balance_table %>%
    select(-Mean_NA)
}

balance_table %>%
  kable(
    col.names = c("Covariate", "Mean (Control)", "Mean (Treatment)", "Standardized Mean Difference", "P-value"),
    digits = 3,
    caption = "Balance Table: Covariate Comparison (Excluding NA in Instagram After)"
  ) %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE) %>%
  add_header_above(c(" " = 1, "Group Means" = 2, " " = 2))




# Hetreoscdasity - constant variance --------------------------------------

participants_clean$residuals <- residuals(model_instagram_block)
participants_clean$fitted_values <- fitted(model_instagram_block)

ggplot(participants_clean, aes(x = fitted_values, y = residuals)) +
  geom_point(alpha = 0.6, color = "blue") +  # Scatter plot of residuals
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Reference line at zero
  labs(
    title = "Residuals vs. Fitted Values for Instagram Usage Model",
    x = "Fitted Values (Predicted Instagram Usage)",
    y = "Residuals"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(face = "bold")
  )

