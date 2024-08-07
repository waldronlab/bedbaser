# bedbaser

bedbaser is an R api for [BEDbase](https://bedbase.org) that provides access
to the [BEDbase API](https://api.bedbase.org) and includes convenience
functions, such for creating GRanges and GRangesList objects.

## Install bedbaser

Install bedbaser using `BiocManager`.

```
if (!"BiocManager" %in% rownames(installed.packages())) {
    install.packages("BiocManager")
}
BiocManager::install("bedbaser", dependencies = TRUE)
```
