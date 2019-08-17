# Social Network Analysis

## Description of the data set
In the 1990s New York City police investigated a cocaine crime ring by tapping phone conversations between suspected members of a gang. In this dataset, 28 individuals who either placed and/or received a call from one of the others in the circle of dealers. The left most column indicates the person who placed the call, and the column names indicate the person who was called. More about this dataset can be found in this paper: “Understanding the structure of a drug trafficking organization: a conversational analysis” by M Natarajan. The dataset was downloaded from UCINET.

## Tasks Performed
* Used the data matrix to build tall table using dplyr and tidyr
* Calculated Statistics such as the number of calls placed, maximum number of calls received and maximum number of calls placed  
* Computed and displayed the degree centrality, betweenness centrality, etc. of each person
* Generated network graphs for the connections, outbound calls and inbound calls using networkD3 package