Fold
====

Fold is a web application for the analysis of the output of hairpin-bisulphite sequencing data; spectifically the programme reconstrcuts, visualises, and generates statistics on the double-stranded CpG methylation patterns of the original cohort of DNA molecules.
This is achived by first 'realigning' the top and bottom strand of the molecule about the hairpin, in which the programme attempts to manage 'PCR slippage', and other sequencing errors.
Then algorithm then identifies and categorises CpG dyads, which is possible due to the previous bisulphite conversion of unmethylated cytosine to uracil (and so recognised as tyrosine when seuquenced).
For example, fully methylated dyads are those regions in where the reconstructed top strand is C-G and the bottom is G-C. Similarly, fully unmethylated dyads are those where the top is T-G and the bottom is G-T.
In addition, the programme calculates a metric: 'Ratio of Concordance Principle' which quantifies the concordance of methylation between the top and bottom strands of the DNA molecule (0=complete discordance, 1=random concordance, inf=complete concordance).
This metric represents the preference of the summation of epigenetic mechanisms of the cell to either maintain or obscure methylation patterns of the DNA in the cells at the time the sample was taken.

The fucntions of Fold was written in R and the web application is written in PHP. The live web application can be found at [http://www.gregoryleeman.com/fold](http://www.gregoryleeman.com/fold), and the repository can be found at [https://github.com/gregoryleeman/fold](https://github.com/gregoryleeman/fold).
