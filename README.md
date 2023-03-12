# HPM-UP_v1.0

How to install the HPM-UP package?

1. Run Rstudio as an administrator
(The current versoin of HPM-UP was developed by R 4.2.2)


2. You must have some essential packages. If not, you can install it with the code below:

install.packages("devtools")
install.packages("car")
install.packages("caret")
install.packages("randomForest")
install.packages("reactable")
install.packages("reticulate")
install.packages("skimr")
install.packages("tidytable")
install.packages("tidyverse")
install.packages("yardstick")


3. Run the code below:
devtools::install_github("hsilab/hpmup", ref="test_1")


4. If Rstudio generates an error below,

Error: package or namespace load failed for 'ncpm' in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]):
namespace 'package name' 0.x.xx is already loaded, but >= 0.x.xx is required

Then, you need to remove the package and install a latest version with the code below:

remove.packages("package name")
install.packages("package name")


5. Once you successfully run devtools::install_github("hsilab/hpmup"), then, the HPM-UP package will be installed.


6. Finally, you can run HPM-UP with:
library(hpmpd)
HPMPD()
