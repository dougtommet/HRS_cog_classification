To: support@statmodel.com
Subject: KNOWNCLASS with WEIGHT: class proportions appear to be estimated without the weights (Mplus 9, Mac)

Dear Mplus support,

With KNOWNCLASS and WEIGHT, Mplus seems to estimate the class logits from the unweighted class counts, while the class-specific parameters and all standard errors use the weights. I think this may be a bug, but I may be missing a documented reason for it.

I first saw this in an applied TYPE = COMPLEX MIXTURE model (MLR, ALGORITHM = INTEGRATION) that has 3 known classes, survey weights, strata, and clusters. The class logits (1.819, 0.768) reproduce the unweighted class counts exactly. I refit the model with all weights set to 1, starting from the weighted solution. The class logits were unchanged (1.819, 0.768), but their standard errors changed (0.077 and 0.070 weighted; 0.061 and 0.054 unweighted). All 78 class-specific thresholds changed as well, by up to 0.79. So the weights reach the thresholds and the standard errors of the logits, but not the logit point estimates. Under weighted pseudo-maximum likelihood with known classes, the class proportions should be the weighted sample proportions.

The attached files reproduce the pattern with simulated data (N = 3,000; 3 classes; the weight is correlated with class). Each model has its own input and data file:

- m1_noweight: KNOWNCLASS, no weight (reference)
- m2_weight: KNOWNCLASS with WEIGHT, TYPE = MIXTURE
- m3_complex: KNOWNCLASS with WEIGHT and CLUSTER, TYPE = COMPLEX MIXTURE
- m4_training: TRAINING (all memberships known) with WEIGHT
- m5_latent: latent classes with WEIGHT (control; classes far apart)

repex_results.txt compares each estimate with the weighted and the unweighted sample proportions and class means, which I computed directly from the data. In m2 and m3, the class proportions match the unweighted targets, while the class-specific means match the weighted targets. The R script that generates the data and inputs is included (repex_knownclass_weights.R), but the .inp and .dat files run on their own.

Could you tell me whether this is intended? If it is, what is the reasoning, and is there a recommended way to get weighted class proportions with KNOWNCLASS? For now, I fix the class logits at the weighted proportions, which I compute outside Mplus.

Thank you,

Rich Jones

Richard N. Jones, Sc.D.
Brown University
