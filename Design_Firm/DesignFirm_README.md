This folder contains the code for the Design Firm.

Execute the programs in the following order -

* 1_mittens_finetune_design.ipynb : This notebook walks through the steps to finetune mittens by starting with the initial company embeddings that were trained from the Design firm corpus.
* 2_output_cossims_design.ipynb : This notebook processes the identification measures.
Note: The design firm emails are hashed and prepared, training of GloVe is done by reading the hashed emails and running a script similar to demo.sh that is obtained directly from the stanfordnlp/GloVe Github repository. The output of the training process is the company embeddings for the design firm. These embeddings are supplied as the input to the fine-tuning process; fine-tuning of Mittens is done in program 1_mittens_finetune_design.ipynb, and the final identification measures are processed in program 2_output_cossims_design.ipynb. Easiest way to replicate is to start from 1_mittens_finetune_design.ipynb and then run 2_output_cossims_design.ipynb. 

