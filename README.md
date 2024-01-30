# Quantium Chips Category - Trial Store Analysis

## Project Overview
This project, as part of Quantium’s retail analytics team, aims to assess the impact of new trial layouts in selected stores for the Chips category. The analysis involves a comparison of trial stores against control stores to provide data-driven recommendations on whether the new layout should be rolled out to all stores.
<br>This project repo contains the task 2 and 3 of the Quantium Virtual Internship program which can be found on [Forage task 2](https://www.theforage.com/virtual-experience/NkaC7knWtjSbi6aYv/quantium/data-analytics-rqkb/experimentation-and-uplift-testing) and [Forage task 3](https://www.theforage.com/virtual-experience/NkaC7knWtjSbi6aYv/quantium/data-analytics-rqkb/analytics-and-commercial-application)
## Objective
The primary objective is to evaluate the performance of trial layouts in stores 77, 86, and 88 by comparing them with carefully selected control stores. Key performance metrics include total sales revenue, number of customers, and average transactions per customer.

## Data Description
- `QVI_data`: Dataset derived from [Task 1](https://github.com/Sun-of-a-beach/Quantium-Virtual-Internship---Retail-Strategy-and-Analytics-Task-1), containing monthly sales data for each store.

## Repository Structure
- `Quantium Data Analysis Task 2/`: Directory containing the `.csv` files used in the analysis called `QVI_data.csv`. Also contains all the R files include R script, .md files.
- `Quantium_Data_Analysis_Task_2.R`: R script containing the analysis.
- `Quantium Data Analysis Task 2/Quantium_Data_Analysis_Task_2_files/figure-gfm`: Directory containing visualizations.
- `Quantium_Data_Analysis_Task_2.md`: This is a fully knitted R.md file which contains the R notebook and can be previewed on Github. This is located inside `Quantium Data Analysis Task 2/`.

## Analysis Workflow
1. **Select Control Stores**: Identify control stores that closely match the trial stores based on key metrics. This involves data exploration, visualization, and statistical analysis.
2. **Assess Trial Performance**: Analyze the performance of trial stores in comparison with their control counterparts to understand the impact of the new layouts.
3. **Collate Findings**: Summarize the findings for each store and prepare a recommendation for the Category Manager, Julia.

## Visualization
Visualizations play a crucial role in presenting insights to the client. The following charts are included to support the analysis:

![Sales Comparison](images/sales_comparison.png)
![Customer Count Comparison](images/customer_count_comparison.png)

## Key Insights and Recommendations
- **Control Store Selection**: Details on how control stores were selected and the rationale behind the selection criteria.
- **Trial Store Assessment**: Insights from the comparison of trial stores against control stores, including any significant differences observed in sales or customer behavior.
- **Strategic Recommendation**: Based on the analysis, a clear recommendation is provided on whether the trial layout should be adopted across all stores.

## How to Run
1. Clone the repository and Run the Script in R or open with R-studio:
   ```sh
   git clone https://github.com/Sun-of-a-beach/Quantium-Virtual-Internship---Retail-Strategy-and-Analytics-Task-2.git
   ```
