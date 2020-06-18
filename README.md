# Patenting-Patterns-in-Artificial-Intelligence

The repository presented here is related to the paper entitled "Patenting Patterns in Artificial Intelligence: 
Identifying National and International Breeding Grounds", submitted to the journal "World Patent Information". Short description of the paper:

## This repository includes:
1. [One R code](https://github.com/matheusleusin/Patenting-Patterns-in-Artificial-Intelligence/blob/master/Code_for_calculating_and_ploting_indicators.R) for calculating the indexes proposed in the paper (National Breeding Ground and International Breeding Ground indexes) and to plot them as the figures used in the paper;
2. [Data](https://github.com/matheusleusin/Patenting-Patterns-in-Artificial-Intelligence/tree/master/data) necessary for the calculations related to the indexes;
3. [Data](https://github.com/matheusleusin/Patenting-Patterns-in-Artificial-Intelligence/tree/master/data/Data_comparison) used for making the comparison between three distinct queries used for searching AI patents: ours, based on keywords related to AI techniques and proposed on this paper,  and two other strategies based on IPC codes, applied by C.-Y. Tseng, P.-H. Ting (2013) and H. Fujii, S. Managi (2018).
4. [One R code](https://github.com/matheusleusin/Patenting-Patterns-in-Artificial-Intelligence/blob/master/Comparison_AI_patents_identification_strategies.R) for analyzing the aforementioned comparison, which consists of a code for merging our classification of patents being about AI or not with the appln_ids to which they are related.

### Papers cited for the IPC-based strategies on AI: 
1. [C.-Y. Tseng, P.-H. Ting, Patent analysis for technology development of artificial intelligence: A country-level comparative study, Innovation 15(4) (2013) 463-475.](https://www.tandfonline.com/doi/abs/10.5172/impp.2013.15.4.463?casa_token=QtYuBqAC9HUAAAAA:b9_WEwyejUub_SSNcDwrNON0qqyePt7x6sK-EbuNVTxDcm3loeO9DV1_7YCFDbAvcfQpLQjIw1zJFMs)
2. [H. Fujii, S. Managi, Trends and priority shifts in artificial intelligence technology invention: A global patent analysis, Economic Analysis and Policy 58 (2018) 60-69.](https://www.sciencedirect.com/science/article/pii/S0313592617302539?casa_token=X1FH2BIiAvYAAAAA:ot96qmUKvB5IUu80Lg1OwRzN57hOXhhhSaWK0XYHt1Zg3Bm3SSjFpwFl6NPQVOALnaDvVOnRzw)

## Detailed description of each data file:

Data related to the code ["Code_for_calculating_and_ploting_indicators.R"](https://github.com/matheusleusin/Patenting-Patterns-in-Artificial-Intelligence/blob/master/Code_for_calculating_and_ploting_indicators.R):

[countries.csv](https://github.com/matheusleusin/Patenting-Patterns-in-Artificial-Intelligence/blob/master/data/countries.csv) →

[InfopartialTSummbycountry.csv](https://github.com/matheusleusin/Patenting-Patterns-in-Artificial-Intelligence/blob/master/data/InfopartialTSummbycountry.csv) →

[InfototalT1SummAllYears.csv](https://github.com/matheusleusin/Patenting-Patterns-in-Artificial-Intelligence/blob/master/data/InfototalT1SummAllYears.csv) →

[Info_Titles.csv](https://github.com/matheusleusin/Patenting-Patterns-in-Artificial-Intelligence/blob/master/data/Info_Titles.csv) →

[Info_Abstracts.xlsx](https://github.com/matheusleusin/Patenting-Patterns-in-Artificial-Intelligence/blob/master/data/Info_Abstracts.xlsx) →

[Info_Full dataset.csv](https://github.com/matheusleusin/Patenting-Patterns-in-Artificial-Intelligence/blob/master/data/Info_Full%20dataset.csv) →

[Info_Priorities.csv](https://github.com/matheusleusin/Patenting-Patterns-in-Artificial-Intelligence/blob/master/data/Info_Priorities.csv) →

Data related to the code ["Comparison_AI_patents_identification_strategies.R"](https://github.com/matheusleusin/Patenting-Patterns-in-Artificial-Intelligence/blob/master/Comparison_AI_patents_identification_strategies.R):

merged.csv → 

MyQuery1_100.csv → 

Query2 →

Query3 →

NotIn1_Query2_unique_100.csv →

NotIn1_Query3_unique_100.csv → 

