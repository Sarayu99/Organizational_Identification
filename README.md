# Organizational_Identification

This repository contains the code base for the work, '*Locally Ensconced and Globally Integrated: How Positions in Network Structure Relate to a Language-Based Model of Organizational Identification*'. 

**Input Data**
---
Input : The text corpora consists of email data and hr data from three firms - Tech firm, Design firm, and the Staffing firm. Out of the three firms, the email data are hashed for the Design and Staffing firm. 

The input email data are first cleaned and processed, following which training of GloVe and fine-tuning of Mittens is conducted.

*Note: All file names, as well as input and output file paths have been modified to protect the data use agreements.*

**Main Folders**
----
A description of the main folders in this repository. <br>
**<ins>Each folder has its own *README.md* file detailing the folder's contents.</ins>**

* *Analysis* folder : Contains the code to run the main analyses and generate the tables in the paper.

* *Design_Firm*, *Staffing Firm*, and *Tech Firm* folders : Contains code to generate the Organizational Identification measure generated as shown in the image below, as well as the Network Measures for each firm.

<img width="630" alt="identification_workflow" src="https://github.com/user-attachments/assets/6bbdedce-350c-464b-89f1-b76401c41f1b">

For additional details on the training of the GloVe model, please see [this](https://nlp.stanford.edu/projects/glove/) and also refer to the [GloVe GitHub repo](https://github.com/stanfordnlp/GloVe). <br>
For additional details about fine-tuning Mittens, please refer to the [Mittens GitHub repo](https://github.com/roamanalytics/mittens).


**Order of Execution**
----

The order of execution is as follows,

1. Generate the Identification and Network measures for each firm. For this, the program files in the three folders - *Design_Firm*, *Staffing Firm*, and *Tech Firm* need to be executed. The measures can be generated for the firms in any order, however, within each firm's folder there are specific instructions on how to execute the programs.
2. Next, all the processed data are pooled and the regression analyses are conducted. For this, the program files in the *Analysis* folder need to be executed.

**References**
----
[1] Jeffrey Pennington, Richard Socher, and Christopher D. Manning. 2014. GloVe: Global Vectors for Word Representation. <br>

[2] Nicholas Dingwall and Christopher Potts. 2018. Mittens: An Extension of GloVe for Learning Domain-Specialized Representations. (NAACL 2018)

