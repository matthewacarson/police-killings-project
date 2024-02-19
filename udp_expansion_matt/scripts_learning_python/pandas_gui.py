import pandas as pd
from pandasgui import show
# %%
# Create a sample DataFrame
data = {
    'Name': ['Alice', 'Bob', 'Charlie'],
    'Age': [25, 30, 35],
    'City': ['New York', 'Los Angeles', 'Chicago']
}
df = pd.DataFrame(data)
# %%
# Display the DataFrame in a GUI-based spreadsheet
show(df)
