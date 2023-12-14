# AEM-7010-PS1-Group5
Repository containing codes for the problem set 3. The data file has not been included in the repository.

## Table of Contents

- [Replication Instructions](#replication-instructions)
- [Code Checks](#code-checks)

## Replication Instructions

Run order:

- `Code/PS3_Q1.R` 
- `Code/PS3_Q2.R` 
- `Code/PS3_Q3.R` 

Please change the directories to the directory where the data is stored.

## Code Checks

### `PS3_Q1.R`

Inputs: `OTC_Headache.csv`

Outputs: Table: 1) Cleaned data used by the subsequent codes, 2) Summary statistics (Table 1)

Runs: Yes

Note: The output is in the `output/` folder under the name `sumstats.tex`

### `PS3_Q2.R`

Inputs: 
- `data_clean.csv`

Outputs: Logit demand model (Table 2)

Runs: Yes

Note: The output is in the `output/` folder under the names `model_summary1.tex`, `model_summary2.tex`, `model_summary3.tex`

### `PS3_Q3.R`

Inputs: Regression results from the previous results in Q2 - `model_summary1.tex`, `model_summary2.tex`, `model_summary3.tex`

Outputs: 
- Table 3, 4

Runs: Yes
