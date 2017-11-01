presentation.html: pkginstalled.txt mzes_proj_members.RDS
	Rscript -e "rmarkdown::render('presentation.rmd')"

pkginstalled.txt:
	Rscript req.R

mzes_proj_members.RDS:
	Rscript scrape.R
