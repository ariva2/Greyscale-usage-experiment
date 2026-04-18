# The Impact of Grayscale Screen Display on Social Media Usage

## Project Overview
This research project investigates the influence of visual design—specifically color—on user behavior and engagement patterns on social media, with a focus on **Instagram**. The study explores whether changing a smartphone's screen display from color to **grayscale (black and white)** affects usage habits and the subjective well-being of young adults (ages 18-30).

The project was conducted as a randomized field experiment at **Tel Aviv University**.

## Research Question
> Does switching the screen display from color to grayscale affect usage patterns on Instagram, other apps, and the subjective feelings of users?

## Key Findings
* **Significant Reduction in Usage**: Switching to grayscale led to a statistically significant decrease in Instagram usage, with an average reduction of **23.46 minutes per day** [cite: 90, 554].
* **Spillover Effect**: Usage of other social media apps also decreased by approximately **30.15 minutes** (nearly significant at p < 0.1) [cite: 568, 636].
* **Subjective Well-being**: The short-term (24-hour) intervention did not show a statistically significant change in users' positive or negative emotions [cite: 91, 555].
* **User Adoption**: While effective in reducing time, users were generally reluctant to adopt grayscale permanently, with most blocks showing 0% interest in future use, except for medium-level users [cite: 606, 608].

## Methodology
### Experimental Design
- **Participants**: 57 young adults aged 18-30 [cite: 88, 334].
- **Randomization**: Block randomization based on initial Instagram usage levels (10 blocks) to ensure balanced groups [cite: 329, 332].
- **Intervention**: The treatment group switched their phones to grayscale for 24 hours, while the control group continued using their phones normally [cite: 89, 164].
- **Data Collection**: Initial and follow-up surveys via Qualtrics, including manual entry of screen time data and screenshots for verification [cite: 171, 188].

### Statistical Analysis
The analysis was performed using **R** and involved:
- **Linear Regression (Fixed Effects)**: Controlling for gender, employment status, relationship status, age group, and phone type (iOS vs. Android) [cite: 222, 545].
- **Balance Tables**: To verify that the randomization successfully balanced the groups [cite: 342].
- **Interaction Effects**: Testing for heterogeneity across demographics [cite: 683].

## Technical Implementation (R)
The provided script `grayscale_colors_exp.R` includes the following workflows:
- **Data Preprocessing**: Cleaning and calculating composite feeling scores (Positive: happiness, productivity, creativity; Negative: boredom, fatigue, anxiety, etc.) [cite: 845, 846].
- **Visualizations**: Distribution histograms, scatter plots with regression lines, and treatment effect bar charts using `ggplot2`.
- **Econometric Modeling**: Using the `fixest` library for high-dimensional fixed-effects regressions.

## Repository Structure
- `data_update_fin.csv`: (Not included for privacy) The anonymized experimental data.
- `grayscale_colors_exp.R`: The complete analysis script used to generate results and plots.
- `Research_Report.pdf`: The full detailed research paper (Hebrew).

## Conclusions
The results support the hypothesis that visual stimuli are a powerful driver of social media engagement. Grayscale settings provide a practical, accessible, and effective tool for individuals seeking to reduce smartphone dependency and "digital addiction," even if the immediate impact on emotional well-being is neutral in the short term.

## References
- Allcott, H., et al. (2020). *The welfare effects of social media*.
- Holte, A. J., & Ferraro, F. R. (2020). *True colors: Grayscale setting reduces screen time*.
- Israeli Internet Association (2024). *Social network usage data in Israel*.
