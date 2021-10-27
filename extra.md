### Complementary contributions

Apart from the methodologies described in the report we also implemented:
- **Bayesian linear regression with MCMC**: this approach could be preferred to the classical linear model of section 4 when the main goal is to quantify the uncertainty in the parameters. However we didn't think that this aspect was critical for our dataset, hence we excluded it in the report.
- **Decision trees**: When trying different techniques to predict the final grade we also tested  some models based on decision trees. On the first hand we implemented random forest, which simply aggregates many decision trees and outputs the average. On the other hand we used Gradient Boosting, where the trees are aggregated in such a way that they correct the deficiencies of the previous. We decided not to include this in the report because the performance was low compared to the other results presented.
