

library(usethis)
use_git()
use_git_config(user.name = "xyron-mg", user.email = "mglogoski@gmail.com")
usethis::git_default_branch_configure()


library(gitcreds)
gitcreds::gitcreds_set()
gitcreds_get()
#testing add

usethis::git_remotes()


#How to check and push changes to Git repository
# Check the current branch name in the Git repository
system("git branch", intern = TRUE)

# Assuming the current branch is `main`, push it to the remote repository
# This will set the upstream branch so that future pushes can be done with just `git push`
system("git push -u origin main")

# If you need to create a new `master` branch locally and push it to the remote
system("git checkout -b master")  # Create and switch to a new branch named `master`
system("git push -u origin master")  # Push the `master` branch to the remote repository

# If you want to push the current branch to the remote repository
system("git push -u origin HEAD")  # Push the current branch to the remote