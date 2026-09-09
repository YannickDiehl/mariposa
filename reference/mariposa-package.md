# mariposa: 'SPSS'-Compatible Statistical Tools for Survey Data

Statistical analysis of survey data with full support for survey
weights, grouped operations, and 'tidyverse' integration. Provides 80
functions for data import/export ('SPSS', 'Stata', 'SAS', 'Excel') with
label roundtripping and tagged NA preservation, label management
(variable labels, value labels, type conversions, missing value
declaration), data transformation (recoding, dummy coding,
standardization, centering), descriptive statistics, codebook
generation, hypothesis testing, correlation analysis, post-hoc
comparisons, weighted statistics, scale analysis, regression,
non-parametric tests, exact tests, factorial ANOVA, and ANCOVA. Every
analysis offers compact print() and detailed summary() output with
toggleable sections. Statistical results are validated against 'SPSS'
version 29 within documented per-tier tolerances (see the compatibility
vignette for per-function status). Methods follow the published
algorithms of IBM Corp. (2023, "IBM SPSS Statistics Algorithms"), the
Lilliefors-corrected normality test of Dallal and Wilkinson (1986)
[doi:10.1080/00031305.1986.10475419](https://doi.org/10.1080/00031305.1986.10475419)
, and the adjusted standardized residuals of Haberman (1973)
[doi:10.2307/2529686](https://doi.org/10.2307/2529686) . Designed for
survey researchers, social scientists, and students working with complex
survey designs.

## See also

Useful links:

- <https://YannickDiehl.github.io/mariposa/>

- <https://github.com/YannickDiehl/mariposa>

- Report bugs at <https://github.com/YannickDiehl/mariposa/issues>

## Author

**Maintainer**: Yannick Diehl <yannick.diehl@gmail.com>

Authors:

- Yannick Diehl <yannick.diehl@gmail.com>
