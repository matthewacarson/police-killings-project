# Assuming typology_input.columns is your Index object

# Convert the Index object to a list
column_names = typology_input.columns.to_list()

# Specify the file path where you want to save the .txt file
file_path = "column_names.txt"

# Open the file in write mode
with open(file_path, "w") as file:
    # Write each column name to the file
    for name in column_names:
        file.write(name + "\n")


print("Column names saved to", file_path)
