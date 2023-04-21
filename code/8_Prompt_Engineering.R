library(tidyverse)

#This code prepares a series of prompts for ChatGPT for each Type

readxl::read_xls("")








A geodemographics company is trying to explain the characteristics of a neighborhood to a new customer. They present data comparing this neighborhood to the national average. A score of 100 means the neighborhood is equivalent to the national average, a score of 150 means the neighborhhood one and a half times the national average, a score of 200 means the neighborhood is twice the national average, a score of 50 means the neighborhood is half of the national average, a score of 300 means the neighborhood is three times the national average. Their neighborhood has the following charactersitics, described in #DATA# below. Data are presented for each charactersitic followed by a colon, and then a score. A good description of a neighborhood describes characteristics that have scores which are greater than 120 or less than 80. 



#DATA#
men under 5 years: 87
men 5 to 9 years: 92
men 10 to 14 years: 95
men 15 to 17 years: 100
men 18 and 19 years: 82
men 20 years: 77
men 21 years: 76
men 22 to 24 years: 75
men 25 to 29 years: 75
men 30 to 34 years: 76
men 35 to 39 years: 84
men 40 to 44 years: 92
men 45 to 49 years: 105
men 50 to 54 years: 116
men 55 to 59 years: 126
men 60 and 61 years: 134
men 62 to 64 years: 137
men 65 and 66 years: 143
men 67 to 69 years: 146
men 70 to 74 years: 147
men 75 to 79 years: 156
men 80 to 84 years: 138
men 85 years and over: 110
women under 5 years: 85
women 5 to 9 years: 90
women 10 to 14 years: 95
women 15 to 17 years: 98
women 18 and 19 years: 72
women 20 years: 74
women 21 years: 73
women 22 to 24 years: 71
women 25 to 29 years: 75
women 30 to 34 years: 75
women 35 to 39 years: 80
women 40 to 44 years: 90
women 45 to 49 years: 100
women 50 to 54 years: 109
women 55 to 59 years: 118
women 60 and 61 years: 121
women 62 to 64 years: 126
women 65 and 66 years: 125
women 67 to 69 years: 129
women 70 to 74 years: 128
women 75 to 79 years: 129
women 80 to 84 years: 118
women 85 years and over: 89
commutes to work using public transit public transportation (excluding taxicab): 5
commute length Less than 5 minutes: 105
commute length 5 to 9 minutes: 65
commute length 10 to 14 minutes: 71
commute length 15 to 19 minutes: 81
commute length 20 to 24 minutes: 88
commute length 25 to 29 minutes: 100
commute length 30 to 34 minutes: 93
commute length 35 to 39 minutes: 111
commute length 40 to 44 minutes: 98
commute length 45 to 59 minutes: 110
commute length 60 to 89 minutes: 101
commute length 90 or more minutes: 118
annual income less than $10,000: 122
annual income $10,000 to $14,999: 141
annual income $15,000 to $19,999: 153
annual income $20,000 to $24,999: 146
annual income $25,000 to $29,999: 142
annual income $30,000 to $34,999: 137
annual income $35,000 to $39,999: 139
annual income $40,000 to $44,999: 133
annual income $45,000 to $49,999: 130
annual income $50,000 to $59,999: 125
annual income $60,000 to $74,999: 119
annual income $75,000 to $99,999: 106
annual income $100,000 to $124,999: 88
annual income $125,000 to $149,999: 71
annual income $150,000 to $199,999: 51
annual income $200,000 or more: 30
with retirement income: 134
households with 1 or more persons with a disability: 153
households in poverty: 119
households with a desktop or laptop with no other type of computing device: 167
households with a smartphone with no other type of computing device: 145
households with a tablet or other portable wireless computer with no other type of computing device: 182
households with no computer: 199
households with a dial-up with no other type of internet subscription: 254
households with a cellular data plan with no other type of internet subscription: 152
broadband such as cable, fiber optic or dsl with no other type of internet subscription: 135
households with satellite internet service: 191
households with satellite internet service with no other type of internet subscription: 391
households with no internet access: 192
households with a broadband internet subscription: 86
households without an internet subscription: 136
people in extreme poverty: 107
people living in poverty : 126
Men employed in agriculture, forestry, fishing and hunting, and mining: 269
Men employed in transportation and warehousing, and utilities: 106
Men employed in finance and insurance, and real estate, and rental and leasing: 36
Men employed in professional, scientific, and technical services: 29
Men employed in management of companies and enterprises: 36
Men employed in administrative and support and waste management services: 77
Men employed in educational services, and health care and social assistance: 61
Men employed in arts, entertainment, and recreation, and accommodation and food services: 52
Women employed in agriculture, forestry, fishing and hunting, and mining: 183
Women employed in transportation and warehousing, and utilities: 90
Women employed in finance and insurance, and real estate, and rental and leasing: 65
Women employed in professional, scientific, and technical services: 44
Women employed in management of companies and enterprises: 54
Women employed in administrative and support and waste management services: 67
Women employed in educational services, and health care and social assistance: 89
Women employed in arts, entertainment, and recreation, and accommodation and food services: 75
housing units that are vacant: 269
housing units that are renter occupied: 52
housing units that are for rent: 46
housing units that are for sale only: 148
housing units that are for seasonal, recreational, or occasional use: 465
housing units that are for migrant workers: 233
buildings that are one single family detached housing: 151
housing units that are single family attached : 15
buildings that have 2 units: 25
buildings that have 3 or 4 units: 16
buildings that have 5 to 9 units: 11
buildings that have 10 to 19 units: 7
buildings that have 20 to 49 units: 7
buildings that have 50 or more units: 3
housing units that are mobile home: 474
housing that is built 2014 or later: 87
housing that is built 2010 to 2013: 113
housing that is built 2000 to 2009: 135
housing that is built 1990 to 1999: 162
housing that is built 1980 to 1989: 134
housing that is built 1970 to 1979: 136
housing that is built 1960 to 1969: 103
housing that is built 1950 to 1959: 86
housing that is built 1940 to 1949: 112
housing that is built 1939 or earlier: 126
housing units that are have no bedroom: 84
housing units that are have 1 bedroom: 74
housing units that are have 2 bedrooms: 136
housing units that are have 3 bedrooms: 155
housing units that are have 4 bedrooms: 93
housing units that are have 5 or more bedrooms: 86
rent is less than $100: 50
rent is $100 to $149: 91
rent is $150 to $199: 66
rent is $200 to $249: 93
rent is $250 to $299: 114
rent is $300 to $349: 173
rent is $350 to $399: 166
rent is $400 to $449: 170
rent is $450 to $499: 124
rent is $500 to $549: 123
rent is $550 to $599: 78
rent is $600 to $649: 73
rent is $650 to $699: 52
rent is $700 to $749: 44
rent is $750 to $799: 34
rent is $800 to $899: 24
rent is $900 to $999: 13
rent is $1,000 to $1,249: 10
rent is $1,250 to $1,499: 5
rent is $1,500 to $1,999: 3
rent is $2,000 to $2,499: 2
rent is $2,500 to $2,999: 2
rent is $3,000 to $3,499: 2
rent is $3,500 or more: 2
proportion of income spent on housing less than 10.0 percent: 75
proportion of income spent on housing 10.0 to 14.9 percent: 60
proportion of income spent on housing 15.0 to 19.9 percent: 45
proportion of income spent on housing 20.0 to 24.9 percent: 39
proportion of income spent on housing 25.0 to 29.9 percent: 36
proportion of income spent on housing 30.0 to 34.9 percent: 34
proportion of income spent on housing 35.0 to 39.9 percent: 37
proportion of income spent on housing 40.0 to 49.9 percent: 38
proportion of income spent on housing 50.0 percent or more: 35
race white alone: 136
race black or african american alone: 6
race american indian and alaska native alone: 79
race hispanic or latino: 12
men living alone: 116
women living alone: 87
living with same-sex spouse: 73
living with biological child: 87
living with adopted child: 123
living with stepchild: 132
living with grandchild: 125
Is in group quarters: 23
Is married-couple family: 122
men never married: 74
women never married: 62
no schooling completed: 68
9th grade level of education: 157
10th grade level of education: 198
11th grade level of education: 169
12th grade, no diploma level of education: 101
GED or alternative credential level of education: 183
Some college, less than 1 year level of education: 121
Some college, 1 or more years, no degree level of education: 97
Associate's degree level of education: 112
Bachelor's degree level of education: 53
Master's degree level of education: 48
Professional school degree level of education: 35
Doctorate degree level of education: 34
teenagers who speak other indo-european languages (15-17): 99
teenagers who speak asian and pacific island languages (15-17): 6
teenagers who speak other languages (15-17): 9
adults who speak other indo-european languages (18-64): 37
adults who speak asian and pacific island languages (18-64): 7
adults who speak other languages (18-64): 9
seniors who speak other indo-european languages (65+): 38
seniors who speak asian and pacific island languages  (65+): 8
seniors who speak other languages  (65+): 26
Two or more races: 38
Three or more races: 19

In the thrid person, write a description of the neighborhood in no more than 500 words. Don't mention the specific scores from the #DATA#, instead use descriptive words to illustrate rates that are above or below the national average.

