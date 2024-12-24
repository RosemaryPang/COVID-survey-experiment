This Github repository is the replication code for "International Mudslinging in Vain: An Experiment on COVID-Related Propaganda and Overseas Nationals’ Support for Autocratic Government"

By Jia Li and M. Rosemary Pang

The original name for "Wave1.csv" is "COVID_survey_0918-2.csv". This wave of survey was conducted in Summer 2020 (August/September) in a public university in the U.S. We received 506 responses. 139 responses are incomplete which leaves us a sample size of 367 subjects. After dropping incomplete survey responses, we have 92 observations in the control group, 92 observations in the positive group, 93 observations, in the negative group, and 90 observations in the additive group. 

The original name for "Wave2.csv" is "COVID_survey_wave2_0809_2.csv". This wave of survey was conducted from late March to late July 2021 across the U.S. We received 1658 responses nationwide. 856 responses are incomplete, 21 responses who completed the survey have never been to the United States due to travel restrictions during the pandemic, which leaves us a sample size of 781 in the second wave. After dropping incomplete survey responses, we have 194 observations in the control group, 196 observations in the positive group, 195 observations, in the negative group, and 196 observations in the additive group. 

Please contact the corresponding author, Rosemary Pang (mrpang@umass.edu), if you have any questions or want more information. 



Content of "CovidSurvey_Full.R" script

1. Set working environment: Line 9 

2. Reading Wave 1 Data: Line 25

3. Reading Wave 2 Data: Line 37

4. Recode Wave 1 Variables: Line 51 
   1) Generate COVID assess PCA: Line 99 - 111
   2) Generate Ideology PCA: Line 188 - 200 
  
5. Recode Wave 2 Variables: Line 256 
   1) Generate COVID assess PCA: Line 312 - 324
   2) Generate Ideology PCA: Line 396 - 408

6. Figure 1: Correlation matrix of DV: Line 508

7. Main model: positive as base group: Line 549
   1) Wave 1: Line 550
      A) With control: Line 555 - 611
      B) Without control: Line 614 - 643
   2) Wave 2: Line 647
      A) With control: Line 652 - 708
      B) Without control: Line 711 - 740

8. Figure 2: Average Treatment Effects: Line 743
   1) China overall: Line 780 - 819
   2) Chinese government: Line 822 - 892
   3) China COVID response: Line 895 - 965
   4) China COVID response Students: Line 968 - 1038
   5) US COVID response: Line 1041 - 1111
   6) US COVID response Students: Line 1114 - 1184

9. Appendix C: Outcomes by group: Line 1189
   1) Assign values: Line 1191 - 1312
   2) Figure C-1: Line 1315 - 1375
   3) Figure C-2: Line 1379 - 1440
   4) Figure C-3: Line 1445 - 1506

10. Appendix D: Summary Statistics: Line 469
    1) Wave 1: Line 470 - 486
    2) Wave 2: Line 489 - 505   

11. Appendix E: Heterogeneous Effects: Line 1510
    1) Interactive models (ideology): Line 1511
       A) Wave 1 with control: Line 1514 - 1571
       B) Wave 1 without control: Line 1573 - 1603
       C) Wave 2 with control: Line 2183 - 2242
       D) Wave 2 without control: Line 2244 - 2274
    2) Plot interactive models (ideology): 
       A) Wave 1 China overall: Line 1606 - 1748
       B) Wave 1 Chinese government: Line 1750 - 1891
       C) Wave 1 China COVID: Line 1893 - 2036
       D) Wave 1 China COVID students: Line 2040 - 2180
       E) Wave 2 China overall: Line 2277 - 2419
       F) Wave 2 Chinese government: Line 2421 - 2559
       G) Wave 2 China COVID: Line 2561 - 2699
       h) Wave 2 China COVID students: Line 2703 - 2843
    3) Interactive models (health kit): Line 2846
       A) Wave 1 with control: Line 2849 - 2905
       B) Wave 1 without control: Line 2908 - 2937
       C) Wave 2 with control: Line 3352 - 3410
       D) Wave 2 without control: Line 3413 - 3442
    4) Plot interactive models (health kit): 
       A) Wave 1 China overall & Chinese government: Line 2940 - 3147 
       B) Wave 1 China COVID & to students: Line 3149 - 3348
       C) Wave 2 China overall & Chinese government: Line 3554 - 3652
       D) Wave 2 China COVID & to students: Line 3654 - 3853
    5) Interactive models (COVID assess): Line 3857
       A) Wave 1 with control: Line 3860 - 3917
       B) Wave 1 without control: Line 3919 - 3948
       C) Wave 2 with control: Line 4250 - 4309
       D) Wave 2 without control: Line 4311 - 4340
    6) Plot interactive models (COVID assess): 
       A) Wave 1 US COVID response: Line 3952 - 4100 
       B) Wave 1 US COVID response student: Line 4102 - 4246
       C) Wave 2 US COVID response: Line 4344 - 4492
       D) Wave 2 US COVID response student: Line 4494 - 4638
 
12. Appendix F: Complete Regression Results 
    1) Main Model
       A) Control group as baseline: Line 4642
          a) Wave 1 (Appendix F-1): Line 4644 - 4679
          b) Wave 1 (Appendix F-2): Line 4682 - 4732
          c) Wave 2 (Appendix F-3): Line 4736 - 4772
          d) Wave 2 (Appendix F-4): Line 4775 - 4825
       B) Negative group as baseline: Line 4829
          a) Wave 1 (Appendix F-1): Line 4831 - 4866
          b) Wave 1 (Appendix F-2): Line 4868 - 4917
          c) Wave 2 (Appendix F-3): Line 4920 - 4954
          d) Wave 2 (Appendix F-4): Line 4957 - 5006
    2) Heterogeneous effect: Ideology
       A) Control group as baseline: Line 5011
          a) Wave 1 (Appendix F-5): Line 5013 - 5048
          b) Wave 1 (Appendix F-6): Line 5051 - 5106
          c) Wave 2 (Appendix F-7): Line 5109 - 5144
          d) Wave 2 (Appendix F-8): Line 5147 - 5202
       B) Negative group as baseline: Line 5205
          a) Wave 1 (Appendix F-5): Line 5207 - 5242
          b) Wave 1 (Appendix F-6): Line 5245 - 5300 
          c) Wave 2 (Appendix F-7): Line 5304 - 5339
          d) Wave 2 (Appendix F-8): Line 5342 - 5398
    3) Heterogeneous effect: Health Kit
       A) Control group as baseline: Line 5406
          a) Wave 1 (Appendix F-9): Line 5408 - 5442
          b) Wave 1 (Appendix F-10): Line 5444 - 5498
          c) Wave 2 (Appendix F-11): Line 5503 - 5536
          d) Wave 2 (Appendix F-12): Line 5539 - 5593
       B) Negative group as baseline: Line 5597
          a) Wave 1 (Appendix F-9): Line 5599 - 5633
          b) Wave 1 (Appendix F-10): Line 5636 - 5690
          c) Wave 2 (Appendix F-11): Line 5693 - 5727
          d) Wave 2 (Appendix F-12): Line 5730 - 5784
    4) Heterogeneous effect: COVID assess in U.S.
       A) Control group as baseline: Line 5790
          a) Wave 1 (Appendix F-13): Line 5792 - 5826
          b) Wave 1 (Appendix F-14): Line 5829 - 5883
          c) Wave 2 (Appendix F-15): Line 5886 - 5920
          d) Wave 2 (Appendix F-16): Line 5923 - 5977
       B) Negative group as baseline: Line 5981
          a) Wave 1 (Appendix F-13): Line 5983 - 6017
          b) Wave 1 (Appendix F-14): Line 6020 - 6074
          c) Wave 2 (Appendix F-15): Line 6077 - 6111
          d) Wave 2 (Appendix F-16): Line 6114 - 6168

13. Appendix G: Political Ideology
    1) Generate PCA of ideology: Line 6173 - 6242
    2) Figure G-1: Line 6245 - 6280
    3) Figure G-2: Line 6282 - 6304
    4) Figure G-3: Line 6307 - 6341
    5) Figure G-4: Line 6344 - 6363

13. Appendix H: COVID assess
    1) Generate PCA of COVID assess: Line 6367 - 6425
    2) Figure H-1: Line 6428 - 6467
    3) Figure H-2: Line 6470 - 6492
    4) Figure H-3: Line 6495 - 6525
    5) Figure H-4: Line 6528 - 6547


  
 
