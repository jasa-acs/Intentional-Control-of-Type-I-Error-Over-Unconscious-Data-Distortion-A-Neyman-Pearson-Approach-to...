## wordk10.RData contains 10 word-sequences, corresponding to the scenario where the number of topics K=10.
## Each word sequence contains the top 20 words in one topic after performing LDA with K=10, and over 50 repetitions, so of dimension 20*50.
## The words we have are in Chinese, then we add English translation manually.

### data files Table1.txt and wordk10.RData are by-products of SearchingK.R


load("wordk10.RData")
