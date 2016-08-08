# Kinship analysis by ICS

We developed a new approach for pairwise kinship analysis in forensic genetics based on chromosomal sharing between two individuals. 
We defined "index of chromosome sharing" (ICS) calculated using 174,254 single nucleotide polymorphism (SNP) loci typed by SNP microarray and genetic length of the shared segments from the genotypes of two individuals. 
To investigate the expected ICS distributions from first- to fifth-degree relatives and unrelated pairs, we used computationally generated genotypes to consider the effect of linkage disequilibrium and recombination. 
The distributions were used for probabilistic evaluation of the pairwise kinship analysis, such as likelihood ratio (LR) or posterior probability, without allele frequencies and haplotype frequencies.
All programs used for simulations and statistical analyses were written by the statistical software R version 3.1.2. 


# Prerequisites

* R
 (Download from the website of R Development Core Team, http://www.R-project.org.)

* HAPS file
 (The HAPS file describes the SNPs and contains the estimated haplotypes by SHAPEIT. Before the data analysis, the HAPS file should be transrated into the text file.
See the website of SHAPEIT, https://mathgen.stats.ox.ac.uk/genetics_software/shapeit/shapeit.html#output for more information.)

* SNP genotypes of samples
 (The files are CSV files. We show an example file, Examplefile_samplegenotype.csv.)


# List of 174,254 SNPs

* Chr.ver5 (R Workspace)


# Code

* 1  Generation of the pedigree genotypes
* 2  Calculation of ics (Th)
* 3  Estimation of the ICS distributions 
* 4  AIC calculation
* 5  QQ plot 
* 6  Analysis of actual sample data
* 7  Z test
* 8  Probabilistic evaluation


# References

* Morimoto C, Manabe S, Kawaguchi T, Kawai C, Fujimoto S, Hamano Y, Yamada R, Matsuda F, Tamaki K. Pairwise kinship analysis by the index of chromosome sharing using high-density single nucleotide polymorphisms. PLOS ONE. 2016;11(7): e0160287.
* R Development Core Team. R: a language and environment for statistical computing. R Foundation for Statistical Computing. 2006.
* Delaneau O, Coulonges C, Zagury JF. Shape-IT: new rapid and accurate algorithm for haplotype inference. BMC Bioinformatics. 2008; 9: 540.