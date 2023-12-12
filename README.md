## Air Traffic Analysis

**Introduction:**
Our analysis, conducted in R, delves into the Air Traffic Passenger Statistics dataset from DataSF, covering San Francisco air traffic from 1999 to 2023. With 15 variables and 34,878 observations, it offers deep insights into air travel dynamics.

**Objectives:**
1. Explore passenger trends using time series models.
2. Develop predictive models for monthly and yearly passenger numbers and flight counts over 3 to 5 years. Investigate seasonality's impact on the airline industry.

**Analysis:**
1. **Residual Analysis:**
   - Detected a distinct seasonal trend, indicating the need for time series models.
   - Explored white noise assumption, revealing deviations from expected patterns.

2. **Detrending Time Series:**
   - Fitted linear and quadratic regression models, highlighting the need to detrend.
   - Evaluated residuals, revealing a quadratic pattern in the data.

3. **Model Specification/Selection:**
   - Attempted AR(1) and AR(2) models, observing seasonal patterns.
   - Selected ARIMA(1,0,1)(0,1,1)12 as the best-fit model based on extensive analysis.

**Conclusion:**
The ARIMA(1,0,1)(0,1,1)12 model, implemented in R, was chosen for its accuracy and robustness in forecasting air traffic trends. Despite the pandemic, our model predicts a continued upward trend. This insight aids airlines in planning for future demand.

**Appendix:**
Exhibits [2.1], [3.5], [4.11], and [4.18] illustrate key findings and model selections. Detailed statistical analyses are available in the R code and output sections.

**Note:**
While advanced black-box functions in R could potentially yield better results, we opted for simplicity and project scope. The chosen methodology aligns with project objectives, offering meaningful insights into air traffic dynamics. Continuous monitoring is advised for adapting to dynamic situations.
