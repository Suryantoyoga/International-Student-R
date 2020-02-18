# International-Student-R
Dashboard of Development Australia International Student from 2002 to 2018

the code result has been published in https://yogaps.shinyapps.io/International-Student-R/

## Library needed:
library(readr)
library(dplyr)
library(forcats)
library(ggplot2)
library(plotly)
library(shiny)
library(shinydashboard)

## Total international student enrolments (first graph)
Using line charts, with year as x-axis and total number of students as y-axis
From the graph, it can be seen that there was a decline trend between 2009 to 2012, some of the causes are student visa regulation changes, raising of Australian dollar and bad experience from international student.
As international student is one of the biggest source state income, government tried their best to overcome the problem. From 2013 to 2018, the chart raises dramatically with international student from China became the biggest contributor (40% growth since 2012)

## Enrolments by sector and state in 2018 (second graph)
The relationship between sector and state, mapped using heat maps. The darker color represent large number of students, while lighter color represent fewer students.
The heat maps shows that students from Victoria, New South Wales, and Queensland prefer to take higher education sector. 
Northern Territory has the smallest number of students compared to other states.
The gap between higher education students and other sector of education in ACT is big, it has different pattern than the other State

## Top 10 Nationalities by enrolments
This graph can help goverment to devise marketing strategies, and prepare international students who will stay in Australia. 
There have been many changes in the number of international students from each country over the past 18 years. For example, Indonesia in 2002 ranks second with 21.000 students. The number didn't changes much and make Indonesia ranks last in 2018. 
India and Nepal have significant growth across the year and make them rank 2 and 3 from the bottom in 2002.
China is always number one in term of number of students, moreover they also have the biggest growth since 2002. This reason makes them ranks first in 2018 and leaving India (2nd position) with huge gap.
