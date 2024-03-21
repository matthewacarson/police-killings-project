import pandas as pd
import statsmodels.api as sm
import numpy as np
import matplotlib.pyplot as plt

# Load the data
data = pd.read_csv("C:\\Users\\madou\\OneDrive - UCLA IT Services\\1)_PS-Honors\\HP_PC\\police_killings_github_HP\\all_tracts.csv")

# Specify the formula
formula = "luof_boolean ~ Income_1k"

# Fit the logistic regression model
model = sm.Logit.from_formula(formula, data=data)

# Get the results
results = model.fit()

# Print the summary of the regression results
print(results.summary())


# Generate a range of values for Income_1k
income_range = np.linspace(data['Income_1k'].min(), data['Income_1k'].max(), 100)

# Predict probabilities for each value of Income_1k
predicted_probabilities = results.predict(exog=dict(Income_1k=income_range))

# Plot the predicted probabilities
plt.plot(income_range, predicted_probabilities, label='Predicted Probabilities', color='blue')
plt.xlabel('Income_1k')
plt.ylabel('Probability of luof_boolean')
plt.title('Predicted Probabilities vs. Income_1k')
plt.legend()
plt.show()


# Calculate observed probabilities for each percentile group
observed_probabilities = data.groupby('income_bins_100')['luof_boolean'].mean()

# Plot the predicted probabilities
plt.plot(income_range, predicted_probabilities, label='Predicted Probabilities', color='blue')

# Plot observed probabilities
plt.scatter(data.groupby('income_bins_100')['Income_1k'].mean(), observed_probabilities, color='red', label='Observed Probabilities')

plt.xlabel('Income_1k')
plt.ylabel('Probability of luof_boolean')
plt.title('Predicted vs. Observed Probabilities by Income Percentile')
plt.legend()
plt.show()

1e3
