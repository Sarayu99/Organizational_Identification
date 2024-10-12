# Organizational_Identification

This repository contains the code base for the work, '*Locally Ensconced and Globally Integrated: How Positions in Network Structure Relate to a Language-Based Model of Organizational Identification*'. 

**Input Files**
---
The text corpora consists of email data and hr data from three firms - Tech firm, Design firm, and the Staffing firm. Out of the three firms, the email data are hashed for the Design and Staffing firm. 

**Main Folders**
----
A description of the main folders in this repository. <br>
**<ins>Each folder has its own *README.md* file detailing the folder's contents.<ins>**

* *Design_Firm*, *Staffing Firm*, and *Tech Firm* folders : contain code to generate the Organizational Identification measure, as well as the Network Measures for each firm.

* *Analysis* folder : contains the code to run the main analyses and generate the tables in the paper.

The order of execution is the following -

1. Generate the Identification and Network measures for each firm. For this, the program files in the three folders - *Design_Firm*, *Staffing Firm*, and *Tech Firm* need to be executed. The measures can be generated for the firms in any order, however, within each firm's folder there are specific instructions on how to execute the programs.
2. Next, all the processed data are pooled and the regression analyses are conducted. For this, the program files in the *Analysis* folder need to be executed.

