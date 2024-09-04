

# Load the usethis package to access functions for configuring Git and GitHub in R
library(usethis)

# Initialize a Git repository in the current R project directory
use_git()

# Set up the Git configuration with your username and email
# These details will be associated with your commits
use_git_config(user.name = "xyron-mg", user.email = "mglogoski@gmail.com")

# Configure the default branch name for new Git repositories
# This sets the default branch to "main" if your Git is still using "master" by default
usethis::git_default_branch_configure()

# Load the gitcreds package to manage Git credentials
library(gitcreds)

# Set up your GitHub credentials
# This will prompt you to enter your GitHub Personal Access Token (PAT) to authenticate GitHub operations
gitcreds::gitcreds_set()

# Check the stored GitHub credentials to ensure they are set correctly
gitcreds_get()

# After the initial setup, you can add and commit changes as usual in your project.
# This is a simple example to test adding a change to the repository:
# Create a file or modify an existing one, then use the following Git commands:
# system("git add .")
# system("git commit -m 'Initial commit'")

# Check the remote repositories configured for this project
# This will show the URL of the GitHub repository you have set up as a remote
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