% Assuming you have your data loaded and it's named 'data'

% Extracting predictors and response variables
X = alltracts.Income_1k;  % Predictor variable
y = alltracts.luof_boolean;  % Response variable

% Fit logistic regression model
beta = fitmnr(alltracts.Income_1k, alltracts.Income_1k+1); % Adding 1 to response to make it 1-based

% Display coefficients
disp('Coefficients:');
disp(beta);

% Predict probabilities for new data
new_data = randn(10, 3); % New data with the same number of predictors
predicted_probs = mnrval(beta, new_data);

% Display predicted probabilities
disp('Predicted probabilities:');
disp(predicted_probs);