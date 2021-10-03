# dataCompareR 0.1.4

Bug fix and minor improvement:

- Removed references to deprecated `dplyr::funs` function
- Added a separator when creating a single index from multiple columns to prevent a loss of uniqueness

---

# dataCompareR 0.1.3

Mainly bug fixes and test changes, including some to address CRAN check issues:

- Allows single-column data frames to be compared, rather than erroring
- No longer produces deprecated code warnings when running tests
- Removed unnecessary console output during tests
- Changed an if condition to meet guidelines around not using && operator with logical vectors
- Fixed issue with Travis CI tests not passing
- Added pkgdown documentation

---

# dataCompareR 0.1.2

Mainly bug fixes, as follows:

- Allow data frames with zero rows to be compared successfully, rather than erroring
- Fix bug that prevented very large data frame comparisons because of an integer overflow when calculating the number of cells
- Allow user to set number of comparisons to show when creating a report

---

# dataCompareR 0.1.1

Mainly fixes compatibility with new versions of dplyr by using `mutate_all` rather than `mutate_each`. Also fixes some other minor bugs and typos in documentation. 

---

# dataCompareR 0.1.0

Initial release
