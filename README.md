# US Bikeshare Data Product Analysis

An end-to-end user behavior research and product data analysis project for a US-based bike-sharing service.

##  Business Problem
The objective of this project is to analyze historical ride-sharing data for Chicago, New York, and Washington to identify distinct behavioral patterns between annual subscribers (`Members`) and casual riders (`Casual`). These insights are used to optimize user acquisition costs (CAC) and improve conversion rates into long-term subscriptions (LTV maximization).

##  Tech Stack & Methods
- **Language:** R
- **Libraries:** `tidyverse` (data wrangling), `ggplot2` (visualization), `lubridate` (time-series manipulation)
- **Reporting:** RMarkdown
- **Analysis Types:** Exploratory Data Analysis (EDA), Cohort Analysis, Time-Series Breakdown

##  Key Metrics & Findings
- **Segment Differentiation:** Proved that `Members` heavily use the service for daily commuting (sharp utilization spikes at 8 AM and 5 PM on weekdays). In contrast, `Casual` riders prefer leisure trips (long-duration sessions mostly on weekends).
- **Optimization Opportunities:** Identified specific trigger hours and drop-off stations with high casual traffic, providing the product marketing team with precise target configurations for localized trigger campaigns.
