# Quadratic Spline Interpolation and Simplex Method Calculator

An interactive web application built with **R** and **Shiny** that performs **Quadratic Spline Interpolation** and solves **Linear Programming** problems using the **Simplex Method**. This tool is designed to help users visualize and compute mathematical solutions efficiently through a user-friendly interface.

## 🔗 Project Repository

[GitHub Repository](https://github.com/driee17/cmsc150-rstudio-webapp)

---

## 🛠 Tech Stack

- **Frontend/UI:** Shiny (R)
- **Backend Logic:** R

---

## 🚀 Features

- Input custom data points for real-time **Quadratic Spline Interpolation** visualization.
- Define objective functions and constraints for solving **Linear Programming** problems using the **Simplex Method**.
- Responsive and interactive UI powered by Shiny.
- Clean data display and result output for easy interpretation.

---

## 🧠 Project Goals

This project was developed to:

- Demonstrate numerical methods concepts through a practical and visual web interface.
- Gain hands-on experience in full-stack development using R and Shiny.
- Explore algorithm implementation from scratch in an academic context.

## 📦 Prerequisites

Before you run the app, make sure you have the following installed:

- [R](https://cran.r-project.org/) (version 4.0 or later recommended)
- [RStudio](https://posit.co/download/rstudio-desktop/)
- Required R packages:
  - `shiny`
  - `shinyMatrix` (for generating flexible matrices in UI)
  - `bslib` (for theming)
  - `shinycssloaders` (for loading animations in backend)
  - Any additional packages specified in the source files

You can install the required packages in R using:

```r
install.packages(c("shiny", "shinyMatrix", "bslib", "shinycssloaders"))
```

## 💻 Running the Application

1. Clone the Repository

```
git clone https://github.com/driee17/cmsc150-rstudio-webapp.git
cd cmsc150-rstudio-webapp
```

2. Open the App in RStudio or VSCode
3. Open the `CuetoEx10.R` in RStudio
4. Run the App
    - Click the Run App button in RStudio, or run the following command in the console: 
        `source("CuetoEx10.R")` 
    - The app will launch in your default web browser