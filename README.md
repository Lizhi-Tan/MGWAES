# `MGWAES`：Multi-strategy genome-wide association and evolutionary analysis
## Installation

Installing dependency packages:

```R
# from cran
if(!require("plinkFile")) install.packages("plinkFile")
if(!require("vcfR")) install.packages("vcfR")
if(!require("GHap")) install.packages("GHap")
if(!require("grid")) install.packages("grid")
if(!require("data.table")) install.packages("data.table")
if(!require("effects")) install.packages("effects")
if(!require("emmeans")) install.packages("emmeans")
if(!require("plyr")) install.packages("plyr")
if(!require("hglm")) install.packages("hglm")
if(!require("CCA")) install.packages("CCA")
if(!require("lme4qtl")) install.packages("lme4qtl")
if(!require("ggplot2")) install.packages("ggplot2")
if(!require("paletteer")) install.packages("paletteer")
```


Data Preparation
-------


### Phenotype Data

MGWAES can process initially raw phenotypes, e.g., different states under a time series, for derivation of multiple related traits.MGWAES also supports single phenotypes and multiple phenotypes that have been processed for use in GWAS. The first column of phenotype data should be the sample id corresponding to the genotype data, followed by the phenotype dataMissing data should be indicated by “NA”. 


### Genotype Data

MGWAES accepts the PLINK and vcf format.Users can convert PLINK and VCF formats by themselves as inputs to different modules.

## Main function

> MGWAES has 5 main modules, including:

- `Dynamic model fitting for time series`
  - MGWAES provides three different models for egg production rate, and users can add and change different types of mathematical models to fit their data.
  - Users can directly observe the generated average fitting curves. We also provide AIC and BIC for evaluating the model fitting results.
    
- `Haplotype-based GWAS`
  - MGWAES constructed a recoded Haplotype-based GWAS model for mapping significant haplotype blocks in the genome-wide landscape.
  - We preset 5 SNPs as a fixed window for haplotype coding, which can be customized by the user with different lengths or modified to a sliding window.
  - Rare haplotypes cause extremely high false positives in the model. We preset a 5% threshold to filter rare haplotypes, and users can customize different thresholds based on genomic haplotypes.


- `Canonical Correlation Analysis(CCA)-based GWAS`
  - MGWAES constructed multi-trait GWAS under different time states using CCA. this module was able to localize specific acting SNPs under different time phases/different phenotypes.
  - We added a module to calculate typical coefficients under different time stages. By calculating different typical coefficients for each trait, the contribution of different genotypes to a specific trait is assessed.

- `Genome-wide prediction using haplotypes`
  - MGWAES included fixed effects (HPPS) and random effects models (HBLUP). According to the assessment, the results of random effects were slightly better than fixed effects.
  - Both random effects and fixed effects models outperformed the widely used GBLUP model.
  - Phenotype prediction was based on the haplotype GWAS module, and users were required to run HGWAS to obtain significant haplotype blocks , which were then used for phenotype prediction.

- `Analysis on different time periods interactions`
  - This module includes interactions modeled under different time phases to examine the specific alleles of the localized time-specific SNPs.
  - Post-hoc analysis of significant SNPs obtained by CCA-GWAS. Users can also specify any SNP for analysis.
 

## Contact
tanlizhi@cau.edu.cn(lizhi)



