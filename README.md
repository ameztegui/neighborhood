# Plasticity

An R package to define and characterize neighborhoods and estimate their effects on forest dynamics. The neighborhood theory of forest dynamics establishes that neighborhood effects
(i.e. spatial explicit processes such as seed dispersal, shading, facilitation, competition) operate at small spatial extents of the individual tree and its neighbors but play a central role in forest dynamics by contributing to patch formation at stand scales and dynamics of the landscape.

One of the main highlights of the neighborhood theory of forest dynamics is the creation of the neighborhood competition index NCI (Canham and Uriarte, 2006). This index quantifies the amount of competition that a tree receives as a function of the number of neighbors, their identity (either species or functional group), their size and their distance to the target tree. The NCI assumes that a given neighbor tree exerts more competition the bigger it is, and the closer to the target tree it stands.

This equation has been extensively used to characterize the competitive environment of forest ecosystems, and also to gain insight into the ecological fundaments of neighborhood dynamics, approaching issues such as symetric vs. asymetric competition, competition along environmental gradients or determinants of competition vs. facilitation. Functional approaches to the neighborhood theory of forest dynamics have also been developed.

However, fitting the so-called, NCI-like equations is not straightforward, since they require simultaneous estimation of large amount of parameters. The best way to approacj these equations is through maximum likelihood statistics, using simulated annealing to find the parameter estimates that maximize the likelihood. This approach has been implemented in the `likelihood` package, especifically in the `likelihood::anneal` function. The functions developed in the `neighborhood` package aim at easening the process of data wrangling and tidying in order to estimate nci-like functions using `anneal`.

At the current moment, the package is still under development. If you want to contribute with a function to calculate any other of the indices included in that paper (or elsewhere), please feel free to contact me or do a pull request in GitHub.

## Installation
To install the package `neighborhood` in your computer, please run the following code:

```
devtools::install_github("ameztegui/neighborhood")
```

## Dependencies
The package is written using the `tidyverse`, and so requires to install this package, or at least two of their components: `dplyr`and `tidyr`.  If you don't have them you can install them with the following code

```
install.packages("tidyverse")

```

## License
The package is released under a MIT license, so you are free to use, distribute or modify it, under appropriate attribution (see [LICENSE.txt](LICENSE.txt) for details). 


## Citation

If you use `neighborhood`, please cite it as:

Ameztegui, A (2020) neighborhood: An R package to determine the neighborhood competitive environment of trees. GitHub repository, https://github.com/ameztegui/neighborhood
See [CITATION.txt](CITATION.txt) for further details. 
