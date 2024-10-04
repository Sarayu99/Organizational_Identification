This folder contains the code for the Tech Firm.

Order of execution of programs -

* Training/1_build_corpus_tech.ipynb : This notebook builds the Tech firm data that can be passed to Glove to train company embeddings from scratch.
* Training/glove_email_tech.sh : This is the main script to train a GloVe model on the data. This file returns the company embeddings.
* Finetuning/2_mittens_finetune_tech.ipynb : This notebook walks through the steps to finetune mittens by starting with the initial company embeddings that were trained from the Tech firm corpus

