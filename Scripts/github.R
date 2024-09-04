

library(usethis)
use_git()
use_git_config(user.name = "xyron-mg", user.email = "mglogoski@gmail.com")
usethis::git_default_branch_configure()
usethis::create_github_token()

install.packages("gitcreds")
library(gitcreds)
gitcreds::gitcreds_set()
