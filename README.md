# The Impact of Grayscale Screen Display on Social Media Usage

## Project Overview
This research investigates the influence of visual design—specifically color—on user behavior and engagement patterns on social media, with a focus on **Instagram**. The study explores whether changing a smartphone's screen display from color to **grayscale (black and white)** affects usage habits and the subjective well-being of young adults aged 18-30.

The project was conducted as a randomized field experiment at **Tel Aviv University**.

## Research Question
> Does switching the screen display from color to grayscale affect usage patterns on Instagram, other apps, and the subjective feelings of users?

## Key Findings
* **Significant Reduction in Usage**: Switching to grayscale led to a statistically significant decrease in Instagram usage, with an average reduction of **23.46 minutes per day** (Research Report, p. 18).
* **Spillover Effect**: Usage of other social media apps also decreased by approximately **30.15 minutes**, showing a near-significant trend (Research Report, p. 19).
* **Subjective Well-being**: The short-term (24-hour) intervention did not show a statistically significant change in users' positive or negative emotions (Research Report, p. 3).
* **User Adoption**: While effective in reducing time, users were generally reluctant to adopt grayscale permanently; for instance, most participants showed 0% interest in future use, with the exception of certain medium-usage groups (Research Report, p. 20).

## Methodology
### Experimental Design
* **Participants**: The study analyzed 57 young adults aged 18-30 (Research Report, p. 3).
* **Randomization**: Participants were assigned to 10 blocks based on their initial Instagram usage levels to ensure balanced treatment and control groups (Research Report, p. 13).
* **Intervention**: The treatment group switched their phones to grayscale for 24 hours, while the control group continued using their phones in color (Research Report, p. 6).
* **Data Collection**: Data was gathered through surveys using Qualtrics, including manual entry of screen time data and screenshots for verification (Research Report, p. 7).

### Statistical Analysis
The analysis was performed using **R** and included:
* **Linear Regression (Fixed Effects)**: Models controlled for gender, employment status, relationship status, age group, and phone type (Research Report, p. 9).
* **Balance Tables**: Statistical tests (Standardized Mean Difference and P-values) were used to verify that no structural biases existed between the groups prior to the intervention (Research Report, p. 13).
* **Heterogeneity Testing**: The study analyzed whether effects differed across demographic sub-groups, such as relationship status or gender (Research Report, p. 24).

## Technical Implementation (R)
The analysis script (`grayscale_colors_exp.R`) performs the following:
* **Data Preprocessing**: Cleaning and calculating composite feeling scores (e.g., PositiveFeel based on happiness, productivity, and creativity).
* **Visualizations**: Generation of distribution histograms, scatter plots with regression lines, and treatment effect bar charts using `ggplot2`.
* **Econometric Modeling**: Utilization of the `fixest` library for high-dimensional fixed-effects regressions.

## Conclusions
The findings support the hypothesis that visual stimuli are a primary driver of social media engagement. Changing screen display settings to grayscale provides a practical and accessible tool for reducing smartphone dependency, even if the immediate impact on emotional well-being remains neutral in the short term (Research Report, p. 22).

## References
* Allcott, H., Braghieri, L., Eichmeyer, S., & Gentzkow, M. (2020). *The welfare effects of social media*.
* Holte, A. J., & Ferraro, F. R. (2020). *True colors: Grayscale setting reduces screen time in college students*.
* Holte, A. J., Giesen, D. T., & Ferraro, F. R. (2023). *Color me calm: Grayscale phone setting reduces anxiety and problematic smartphone use*.
* Israeli Internet Association. (2024). *Use of social networks and online services in Israel*.
