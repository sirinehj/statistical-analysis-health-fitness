# Statistical Analysis of Health-Related Physical Fitness

This project explores the statistical analysis of Health-Related Physical Fitness (HRPF) indicators using data from the National Fitness Award (2015–2019). The study employs rigorous statistical methodology to analyze, compare, and model physical fitness outcomes through parametric and non-parametric hypothesis tests, as well as regression analysis.

---

## Table of Contents
1. Objectives
2. Dataset
3. Statistical Methodology
    - Phase 1: Assumption Checking
    - Phase 2: Parametric Hypothesis Tests](#phase-2---parametric-hypothesis-tests
    - Phase 3: Non-Parametric Tests](#phase-3---non-parametric-tests
    - Phase 4: Regression Analysis](#phase-4---regression-analysis
4. Tools & Technologies
5. Key Findings

---

## Objectives

- Assess normality and distributional properties of fitness variables.
- Compare men vs women using appropriate statistical tests.
- Compare sample results to theoretical reference values.
- Study the effect of age, BMI categories, and VO₂ categories.
- Evaluate relationships between variables.
- Build regression models to explain physical fitness outcomes.

---

## Dataset

**Source:** National Fitness Award (2015–2019)  
**Population:** Korean adults  
**Type:** Cross-sectional observational data  

**Preprocessing:**
- Removal of implausible values.
- Creation of derived variables (age groups, BMI categories, VO₂ categories).
- Handling of missing values.

---

## Statistical Methodology

### 🔹 Phase 1 — Assumption Checking
1. **Normality Tests**
   - Shapiro–Wilk test
   - Visual inspection (histograms, QQ-plots)
   - Anderson-Darling test (large dataset)
   - Purpose: Determine suitability for parametric or non-parametric tests.

### 🔹 Phase 2 — Parametric Hypothesis Tests
2. **Test F de Fisher (Homogeneity of Variances)**  
   Comparison of variances between men and women. Used to choose appropriate t-tests:
   - **BMI**
   - **Sit-and-Reach**
   - **VO₂ estimated**

3. **Independent t-tests (Men vs Women)**  
   | Variable        | Test Used       | Rationale                 |
   |-----------------|-----------------|---------------------------|
   | BMI             | Welch t-test    | Variances unequal         |
   | Sit-and-Reach   | Student t-test  | Variances equal           |
   | VO₂ estimated   | Student t-test  | Variances equal           |

4. **One-sample t-tests (Reference Comparison)**  
   Comparison of sample means to theoretical reference values:  
   | Variable        | Reference Value |
   |-----------------|-----------------|
   | BMI             | 22.8            |
   | Sit-and-Reach   | 17.56           |
   | VO₂ estimated   | 37.3            |

5. **Chi-square Test on Variance**  
   *Not applied*: No theoretical variance provided in the reference article.

6. **ANOVA (Analysis of Variance)**  
   - **Factors studied:**
     - Age groups
     - BMI categories
     - VO₂ categories
   - **Dependent variables:**
     - BMI
     - Sit-and-Reach
     - VO₂ estimated

7. **Proportion Test**  
   - Test of sex distribution.
   - Comparison against theoretical proportion (50% / 50%).

8. **Pearson Correlation Test**  
   Evaluates relationships between:
   - BMI & VO₂
   - BMI & Sit-and-Reach
   - VO₂ & Sit-and-Reach

### 🔹 Phase 3 — Non-Parametric Tests
Applied when normality assumptions are violated.  

9. **Mann–Whitney U Test**  
   Non-parametric alternative to independent t-test. Used for group comparisons (Men vs Women) when distributions are non-normal.  

10. **Kruskal–Wallis Test**  
    Non-parametric alternative to ANOVA. Used for comparisons across more than two groups.  

### 🔹 Phase 4 — Regression Analysis
11. **Multiple Linear Regression**  
    - **Objective:** Explain physical fitness outcomes using predictors such as Age, Sex, BMI, and Flexibility.
    - **Model evaluated:**  
      `VO₂ ~ Age + Sex + BMI + Sit-and-Reach`
    - **Model diagnostics:**
      - Residual analysis
      - Multicollinearity check (VIF)
      - Evaluation of model significance

---

## Tools & Technologies

- **R** programming language
- Key libraries:
  - `tidyverse`
  - `ggplot2`
  - `kableExtra`
  - `broom`
  - `car`

---

## Key Findings (Summary)

- Significant sex differences were found in BMI, flexibility, and VO₂.
- Sample population showed lower fitness levels compared to theoretical reference values.
- Age strongly impacts VO₂ but has minimal influence on BMI and flexibility.
- No meaningful linear correlations were observed between BMI, VO₂, and flexibility.
- Regression analysis confirms Age and Sex are major determinants of VO₂.

---
