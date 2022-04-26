## Driver script to automate analysis and reporting

Rscript src/R/clean.R

Rscript src/R/plot_growth.R
Rscript src/R/aft.R
Rscript src/R/mixed.R

Rscript -e "rmarkdown::render('presentation_Kuhlers.Rmd')"
Rscript -e "rmarkdown::render('paper_Kuhlers.Rmd')"
