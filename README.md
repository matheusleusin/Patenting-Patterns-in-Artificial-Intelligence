# Patenting-Patterns-in-Artificial-Intelligence

The code presented here is related to the paper entitled "Patenting Patterns in Artificial Intelligence: 
Identifying National and International Breeding Grounds", submitted to the journal "World Patent Information".

# The code includes:
1. One R code for calculating the indexes proposed in the paper (National Breeding Ground and International Breeding Ground indexes) and to plot them as the figures used in the paper;
2. Data necessary for these calculations;
3. Data used for making the comparison between three distinct queries used for searching AI patents: ours, based on keywords related to AI techniques and proposed on this paper,  and two other strategies, applied by C.-Y. Tseng, P.-H. Ting (2013) and H. Fujii, S. Managi (2018), which are based on IPC codes.
4. One R code for analyzing the aforementioned comparison, which consists of a code for merging our classification of patents being about AI or not with the appln_ids to which they are related.

### Papers cited for the IPC-based strategies on AI: 
C.-Y. Tseng, P.-H. Ting, Patent analysis for technology development of artificial intelligence: A country-level comparative study, Innovation 15(4) (2013) 463-475.
H. Fujii, S. Managi, Trends and priority shifts in artificial intelligence technology invention: A global patent analysis, Economic Analysis and Policy 58 (2018) 60-69.

# A detailed description of each data file is provided below

Data related to the code "Code_for_calculating_and_ploting_indicators.R":
countries.csv →
DataFig1topics2.csv →
InfopartialTSummbycountry.csv →
InfototalT1SummAllYears.csv →
Query 3_Full dataset.csv →
Query 4 - Priorities.csv →

Data related to the code "Comparison_AI_patents_identification_strategies.R":

merged.csv → 
MyQuery1_100.csv → 
NotIn1_Query3_unique_100.csv →
NotIn1_Query4_unique_100.csv → 

