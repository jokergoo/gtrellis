.onAttach = function(libname, pkgname) {
    version = packageDescription(pkgname, fields = "Version")

  	msg = paste0("========================================
", pkgname, " version ", version, "
Bioconductor page: http://bioconductor.org/packages/gtrellis/
Github page: https://github.com/jokergoo/gtrellis
Documentation: http://bioconductor.org/packages/gtrellis/

If you use it in published research, please cite:
Gu, Z. gtrellis: an R/Bioconductor package for making genome-level 
  Trellis graphics. BMC Bioinformatics 2016.
========================================
")	

    packageStartupMessage(msg)
}
