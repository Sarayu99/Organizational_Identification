This folder contains the code for the Tech Firm.

Execute the programs in the following order -

* 1_build_corpus_tech.ipynb : This notebook builds the Tech firm data that can be passed to Glove to train company embeddings from scratch.
* 2_glove_email_tech.sh : This is the main script to train a GloVe model on the data. This file returns the company embeddings.
* 3_mittens_finetune_tech.ipynb : This notebook walks through the steps to finetune mittens by starting with the initial company embeddings that were trained from the Tech firm corpus.
* 4_output_cossims_tech.ipynb : This notebook processes the identification measures.


Note: Preprocessing is done in program 1_build_corpus_tech.ipynb, training of GloVe is done in program 2_glove_email_tech.sh, fine-tuning of Mittens is done in program 3_mittens_finetune_tech, and the final identification measures are processed in program 4_output_cossims_tech.ipynb.

