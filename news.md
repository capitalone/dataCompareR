# dataCompareR 0.1.0

Intial release

# dataCompareR 0.1.1

Mainly fixes compatibility with new versions of dplyr by using `mutate_all` rather than `mutate_each`. Also fixes some other minor bugs and typos in documentation. 

# dataCompareR 0.1.2

Mainly bug fixes, as follows:

- Allow data frames with zero rows to be compared successfully, rather than erroring
- Fix bug that prevented very large data frame comparisons because of an integer overflow when calculating the number of cells
- Allow user to set number of comparisons to show when creating a report

