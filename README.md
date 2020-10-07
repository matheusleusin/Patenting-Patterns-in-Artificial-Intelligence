# Patenting-Patterns-in-Artificial-Intelligence

This repository refers to the paper "Patenting patterns in Artificial Intelligence: Identifying national and international breeding grounds", published at World Patent Information, Volume 62, September 2020, doi: https://doi.org/10.1016/j.wpi.2020.101988.

The paper proposes two novel patent-based indicators to differentiate structural differences in the patterns of intellectual property (IP) protection observed across countries. The proposed indicators are appliued for the exemplary case of AI. In particular, we consider (i) to what extent countries specialize in AI and are relevant markets for corresponding IP protection (‘National Breeding Ground’); and (ii) to what extent countries attract AI from abroad for IP protection and extend the protection of their own AI-related IP to foreign markets (‘International Breeding Ground’). A short description of the steps followed in the paper is presented in the figure below.

<p align="center">
  <img src="https://github.com/matheusleusin/Patenting-Patterns-in-Artificial-Intelligence/blob/master/Fig2.png" width="750" />
</p>

---
**Keywords:** *Artificial Intelligence; Digitalisation; Patent analysis; International comparison; Specialisation.*

For citation of the **paper**, the following information can be used: 
Leusin, Matheus Eduardo, Jutta Günther, Björn Jindra, and Martin G. Moehrle. "Patenting patterns in Artificial Intelligence: Identifying national and international breeding grounds." *World Patent Information 62 (2020): 101988*.

For citation of this **repository**, the following information can be used: 
Leusin, Matheus Eduardo, Jutta Günther, Björn Jindra, and Martin G. Moehrle. *GitHub repository of the paper "Patenting patterns in Artificial Intelligence: Identifying national and international breeding grounds"*. Available in: https://github.com/matheusleusin/Patenting-Patterns-in-Artificial-Intelligence.

For citation of the paper, the following information can be used: 


## This repository includes:
1. [One R code](https://github.com/matheusleusin/Patenting-Patterns-in-Artificial-Intelligence/blob/master/Code_for_calculating_and_ploting_indicators.R) for calculating the indexes proposed in the paper (National Breeding Ground and International Breeding Ground indexes) and to plot them as the figures used in the paper;
2. [Data](https://github.com/matheusleusin/Patenting-Patterns-in-Artificial-Intelligence/tree/master/data_main_analysis) necessary for the calculations related to the indexes;
3. [Data](https://github.com/matheusleusin/Patenting-Patterns-in-Artificial-Intelligence/tree/master/data_comparison) used for making the comparison between three distinct queries used for searching AI patents: ours, based on keywords related to AI techniques and proposed on this paper,  and two other strategies based on IPC codes, applied by C.-Y. Tseng, P.-H. Ting (2013) and H. Fujii, S. Managi (2018);
4. [One R code](https://github.com/matheusleusin/Patenting-Patterns-in-Artificial-Intelligence/blob/master/Comparison_AI_patents_identification_strategies.R) for analyzing the aforementioned comparison, which consists of a code for merging our classification of patents being about AI or not with the appln_ids to which they are related;

### Papers cited for the IPC-based strategies on AI: 
1. [C.-Y. Tseng, P.-H. Ting, Patent analysis for technology development of artificial intelligence: A country-level comparative study, Innovation 15(4) (2013) 463-475.](https://www.tandfonline.com/doi/abs/10.5172/impp.2013.15.4.463?casa_token=QtYuBqAC9HUAAAAA:b9_WEwyejUub_SSNcDwrNON0qqyePt7x6sK-EbuNVTxDcm3loeO9DV1_7YCFDbAvcfQpLQjIw1zJFMs)
2. [H. Fujii, S. Managi, Trends and priority shifts in artificial intelligence technology invention: A global patent analysis, Economic Analysis and Policy 58 (2018) 60-69.](https://www.sciencedirect.com/science/article/pii/S0313592617302539?casa_token=X1FH2BIiAvYAAAAA:ot96qmUKvB5IUu80Lg1OwRzN57hOXhhhSaWK0XYHt1Zg3Bm3SSjFpwFl6NPQVOALnaDvVOnRzw)

## Detailed description of each data file:

**Data related to the code ["Code_for_calculating_and_ploting_indicators"](https://github.com/matheusleusin/Patenting-Patterns-in-Artificial-Intelligence/blob/master/Code_for_calculating_and_ploting_indicators.R), located in the folder [data_main_analysis](https://github.com/matheusleusin/Patenting-Patterns-in-Artificial-Intelligence/tree/master/data_main_analysis):**

[countries.csv](https://github.com/matheusleusin/Patenting-Patterns-in-Artificial-Intelligence/blob/master/data_main_analysis/countries.csv) → this file is used for replacing the two letters abbreviation of countries by their real names;

[InfopartialTSummbycountry.csv](https://github.com/matheusleusin/Patenting-Patterns-in-Artificial-Intelligence/blob/master/data_main_analysis/InfopartialTSummbycountry.csv) → this file contain information about the  number of priority patents per country. It is used for calculating the indicator named "Total Patents_Country", presented in the equation 2 of the paper;

[InfototalT1SummAllYears.csv](https://github.com/matheusleusin/Patenting-Patterns-in-Artificial-Intelligence/blob/master/data_main_analysis/InfototalT1SummAllYears.csv) → this file contain information about the  number of priority patents of all countries, per year. It is used for calculating the indicator named "Global Number of Patents", presented in the equation 2 of the paper;

[Info_Titles.csv](https://github.com/matheusleusin/Patenting-Patterns-in-Artificial-Intelligence/blob/master/data_main_analysis/Info_Titles.csv) → this file contain all the **titles** of the AI patents identified using our keyword-based search strategy presented in the paper;

[Info_Abstracts.xlsx](https://github.com/matheusleusin/Patenting-Patterns-in-Artificial-Intelligence/blob/master/data_main_analysis/Info_Abstracts.xlsx) → this file contain all the **abstracts** of the AI patents identified using our keyword-based search strategy presented in the paper;

[Info_Full dataset.csv](https://github.com/matheusleusin/Patenting-Patterns-in-Artificial-Intelligence/blob/master/data_main_analysis/Info_Full%20dataset.csv) → this file contain general information of all AI patents identified using our keyword-based search strategy presented in the paper (i.e. regardless of them being priorities or not);

[Info_Priorities.csv](https://github.com/matheusleusin/Patenting-Patterns-in-Artificial-Intelligence/blob/master/data_main_analysis/Info_Priorities.csv) → this file contain general information of priority AI patents identified using our keyword-based search strategy presented in the paper;

**Data related to the code ["Comparison_AI_patents_identification_strategies"](https://github.com/matheusleusin/Patenting-Patterns-in-Artificial-Intelligence/blob/master/Comparison_AI_patents_identification_strategies.R), located in the folder [data_comparison](https://github.com/matheusleusin/Patenting-Patterns-in-Artificial-Intelligence/tree/master/data_comparison):**

[merged.csv](https://github.com/matheusleusin/Patenting-Patterns-in-Artificial-Intelligence/blob/master/data_comparison/merged.csv) → this file contains our classification for 300 patents regarding they being about AI or not (100 patents from our dataset, and 100 patents from each of the 2 others datasets considered, based in IPC codes). It is used for measuring the accuracy of each strategy regarding the identification of AI patents; 

[MyQuery1_100.csv](https://github.com/matheusleusin/Patenting-Patterns-in-Artificial-Intelligence/blob/master/data_comparison/MyQuery1_100.csv) → this file contain information about the first 100 patents from our dataset;

[Query2](https://github.com/matheusleusin/Patenting-Patterns-in-Artificial-Intelligence/blob/master/data_comparison/Query%202.csv) → this file contain information about all appln_ids collected based on the strategy proposed in *H. Fujii, S. Managi (2018)*;

[Query3](https://github.com/matheusleusin/Patenting-Patterns-in-Artificial-Intelligence/blob/master/data_comparison/Query%203.csv) → this file contain information about all appln_ids collected based on the strategy proposed in *C.-Y. Tseng, P.-H. Ting (2013)*;

[NotIn1_Query2_unique_100.csv](https://github.com/matheusleusin/Patenting-Patterns-in-Artificial-Intelligence/blob/master/data_comparison/NotIn1_Query2_unique_100.csv) → this file contain information about the first 100 patents from the dataset collected based on the strategy proposed in *H. Fujii, S. Managi (2018)* that are identified in their dataset and not in ours;

[NotIn1_Query3_unique_100.csv](https://github.com/matheusleusin/Patenting-Patterns-in-Artificial-Intelligence/blob/master/data_comparison/NotIn1_Query3_unique_100.csv) → this file contain information about the first 100 patents from the dataset collected based on the strategy proposed in *C.-Y. Tseng, P.-H. Ting (2013)* that are identified in their dataset and not in ours;

