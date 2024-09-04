

library(usethis)
use_git()
use_git_config(user.name = "xyron-mg", user.email = "mglogoski@gmail.com")
usethis::git_default_branch_configure()


library(gitcreds)
gitcreds::gitcreds_set()
gitcreds_get()
#testing add

usethis::git_remotes()

library(usethis)
use_git()
use_git_config(user.name = "xyron-mg", user.email = "mglogoski@gmail.com")
usethis::git_default_branch_configure()

