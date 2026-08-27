# Nonparametric-Sequential-Control-Charts 📊

This repository contains R code for implementing and analyzing nonparametric sequential control charts. These charts are essential tools in statistical process control (SPC) for monitoring data streams and detecting shifts or anomalies in real-time.

## Table of Contents 📝

- [Description](#description)
- [Features](#features)
- [Tech Stack](#tech-stack)
- [Installation](#installation)
- [Usage](#usage)
- [Project Structure](#project-structure)
- [Contributing](#contributing)
- [License](#license)
- [Important Links](#important-links)
- [Footer](#footer)

## Description ℹ️

The `Nonparametric-Sequential-Control-Charts` project focuses on the development and evaluation of sequential control charts that do not rely on specific distributional assumptions about the data. This makes them robust and applicable in a wider range of scenarios compared to traditional parametric methods. The repository appears to house research code for analyzing the performance (e.g., Average Sample Number (ASN), Average Number of Steady State (ANSS)) of various sequential control chart types, including SPRT (Sequential Probability Ratio Test) sign charts, EWMA (Exponentially Weighted Moving Average) sign charts, CUSUM (Cumulative Sum) sign charts, SSRT chart, and Sequential Sukhatme charts.

## Features ✨

- **Nonparametric Charts**: Implements control charts that do not assume a specific data distribution, increasing applicability.
- **Sequential Monitoring**: Utilizes sequential testing procedures to detect shifts in process parameters efficiently.
- **Performance Analysis**: Includes scripts for calculating and comparing performance metrics like ASN and ANSS for different chart types.
- **Variety of Chart Implementations**: Covers multiple types of sequential control charts, such as:
  - SPRT Sign Charts (one-sided and two-sided)
  - EWMA Sign Charts
  - CUSUM Sign Charts
  - SSRT Charts
  - Sequential Sukhatme Charts
- **Simulation-Based Evaluation**: Employs simulation techniques to assess chart performance under various conditions.
- **Regression Analysis**: Provides scripts for deriving regression equations related to chart boundaries and performance.

## Tech Stack 💻

- **Language**: R

## Installation 🛠️

This project consists of R scripts. To run these scripts, you will need to have R installed on your system. There are no external package dependencies explicitly mentioned or detected in the provided code snippets. It is recommended to have a recent version of R.

1.  **Install R**: If you don't have R installed, download it from the [CRAN website](https://cran.r-project.org/).
2.  **Download Repository**: Clone or download the repository to your local machine.
3.  **Run Scripts**: Navigate to the respective R script directories and execute the scripts using your R environment (e.g., RStudio, base R console).

```bash
# Example: Running a script from the terminal
Rscript path/to/your/script.R
```

## Usage 🚀

The scripts in this repository are designed for analyzing the performance of different nonparametric sequential control charts. They can be used to:

-   **Evaluate Chart Performance**: Understand how effective different charts are in detecting process shifts by examining metrics like ASN and ANSS.
-   **Compare Performance**: Compare the efficiency and power of TSST, Miller's SSRT, and new proposed SSRT; also SPRT, EWMA, CUSUM, SSRT, and sequential Sukhatme charts under various scenarios.
-   **Determine Optimal Parameters**: Analyze regression equations to find optimal boundaries and parameters for control charts.
-   **Conduct Simulations**: Perform simulations to generate data and test the behavior of control charts.

### Real-World Use Cases 🌍

Nonparametric sequential control charts are valuable in various industries where data distribution is unknown or variable, including:

-   **Healthcare**: Monitoring patient outcomes or hospital performance where data may not follow normal distributions.
-   **Manufacturing**: Quality control for processes where product characteristics might not be normally distributed.
-   **Environmental Monitoring**: Tracking pollution levels or climate data that can exhibit complex, non-normal patterns.
-   **Finance**: Analyzing financial data for detecting anomalies or shifts in market behavior.

### Example of Usage (Conceptual) 📈

While specific usage commands depend on the R environment, the general pattern involves running individual R scripts to perform specific analyses. For example, to analyze the ASN for an EWMA sign chart using Markov Chains, you would execute:

```R
# Assuming you are in the correct directory or have set the working path
source("Ch 2_ Two Sided SPRT sign Charts/ASN_delta and ANSS_delta of EWMA sign chart using Markov Chain.R")
# The script will then perform the calculations and likely print results or save data.
```

Similarly, for other analyses, you would run the corresponding `.R` files:

-   `Ch 2_ Two Sided SPRT sign Charts/ASN_delta and ANSS_delta of Shewhart sign chart.R`
-   `Ch 3_New SSRT Test/2_Size and power of new SSRT.R`
-   `Ch 5_Sequential Sukhatme Chart/6_Normal_Seq Sukhatme Chart.R`

## Project Structure 📁

The repository is organized into directories representing different chapters or types of analyses:

-   `Ch 2_ Two Sided SPRT sign Charts/`: Contains scripts related to two-sided SPRT sign charts, analyzing metrics like ASN and ANSS for EWMA, Shewhart, and SPRT variations.
-   `Ch 3_New SSRT Test/`: Focuses on new SSRT (Sequential Signed-Rank Test) methods, including simulation data, size and power analysis, and regression equations.
-   `Ch 4_SSRT Chart/`: Deals with SSRT charts, including regression equations, steady-state performance metrics for Shewhart, CUSUM, and EWMA charts, and overall SSRT chart performance.
-   `Ch 5_Sequential Sukhatme Chart/`: Explores the Sequential Sukhatme Chart, including simulation data, regression equations, and steady-state performance comparisons with Shewhart, CUSUM, and EWMA charts.

## Contributing 🤝

Contributions are welcome! If you have suggestions for improvements, bug fixes, or new features, please:

1.  Fork the repository.
2.  Create a new branch (`git checkout -b feature/YourFeature`).
3.  Make your changes.
4.  Commit your changes (`git commit -m 'Add some YourFeature'`).
5.  Push to the branch (`git push origin feature/YourFeature`).
6.  Open a Pull Request.

Please ensure your code adheres to R best practices and the existing structure.

## License 📜

This project is not explicitly licensed. Please refer to the GitHub repository for any licensing information provided by the owner.

## Important Links 🔗

-   **Repository URL**: [https://github.com/Ravindraedeore/Nonparametric-Sequential-Control-Charts](https://github.com/Ravindraedeore/Nonparametric-Sequential-Control-Charts)

## Footer 🌟

--- 

This README was generated based on the code analysis of the `Nonparametric-Sequential-Control-Charts` repository.

-   **Repository**: [Nonparametric-Sequential-Control-Charts](https://github.com/Ravindraedeore/Nonparametric-Sequential-Control-Charts)
-   **Author**: Ravindraedeore
-   **Contact**: Please refer to the repository owner's profile for contact information.

Liked this project? Show your appreciation by giving it a ⭐ star, opening an 💡 issue, or submitting a 🚀 pull request!


---
**<p align="center">Generated by [ReadmeCodeGen](https://www.readmecodegen.com/)</p>**
