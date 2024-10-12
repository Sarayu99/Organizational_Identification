This folder contains the code for the Design Firm.

Execute the programs in the following order -

* 1_mittens_finetune_design.ipynb : This notebook walks through the steps to finetune mittens by starting with the initial company embeddings that were trained from the Design firm corpus.
* 2_output_cossims_design.ipynb : This notebook processes the identification measures.<br>
* 3_design_network_embed.ipynb : Generate the network measures.
* 4_design_community_diversity.ipynb : Generate the network measures.

Note: The design firm emails are hashed and prepared, training of GloVe is done by reading the hashed emails and running a script similar to demo.sh that is obtained directly from the stanfordnlp/GloVe Github repository. The output of the training process is the company embeddings for the design firm. These embeddings are supplied as the input to the fine-tuning process - the same process of training was implemented for the tech firm and the staffing firm too. Fine-tuning of Mittens is done in program 1_mittens_finetune_design.ipynb, and the final identification measures are processed in program 2_output_cossims_design.ipynb. Lastly, the network measures are generated using the programs 3_design_network_embed.ipynb and 4_design_community_diversity.ipynb. Easiest way to replicate is to start from 1_mittens_finetune_design.ipynb and then run 2_output_cossims_design.ipynb, followed by the network measure programs.

