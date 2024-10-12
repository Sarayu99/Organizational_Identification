This folder contains the code for the Staffing Firm.

Execute the programs in the following order -

* 1_build_corpus_staffing.ipynb : This notebook builds the Staffing firm data that can be passed to Glove to train company embeddings from scratch.
* 2_glove_email_staffing.sh : This is the main script to train a GloVe model on the data. This file returns the company embeddings.
* 3_mittens_finetuning_staffing.ipynb : This notebook walks through the steps to finetune mittens by starting with the initial company embeddings that were trained from the Staffing firm corpus.
* 4_output_cossims_staffing.ipynb : This notebook processes the identification measures.
* 5_num_counts_staffing.ipynb : This notebook walks through the steps to calculate the number of 'i' and 'we' counts for the staffing firm corpus
* 6_staffing_network_embed.ipynb : Generate network measures.
* 7_staffing_community_bridging.ipynb : Generate network measures.

Note: Preprocessing is done in program 1_build_corpus_staffing.ipynb, training of GloVe is done in program 2_glove_email_staffing.sh, fine-tuning of Mittens is done in program 3_mittens_finetuning_staffing.ipynb, the final identification measures are processed in program 4_output_cossims_staffing.ipynb, and the counts of pronouns are calculated in 5_num_counts_staffing.ipynb. Finally, the network measures are generated using the programs 6_staffing_network_embed.ipynb and 7_staffing_community_bridging.ipynb.
