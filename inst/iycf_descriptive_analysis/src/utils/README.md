# utility_repo
Repo that contain all utility scripts (check_kobo, utils_cleaning, utils_analysis, utils_audit) etc.

## Installation

The way to use these utils scripts is to initialize them as a [git submodule](https://git-scm.com/book/en/v2/Git-Tools-Submodules) in your project repo (preferably inside _src_).

### follow these steps to install:

Open your shell and move to your _src_ directory:

- ```cd src```

Clone the __utility_repo__ submodule into a folder named `utils`:

- ```git submodule add https://github.com/REACH-WoU-Regional/utility_repo.git utils```

After this you should see two changes to your base repository. Run `git status` to see them: you should have new files named _.gitmodules_ and _utils_. 

Check that everything is okay by using the following command:

- ```git submodule status```

The output should be something like:

``` 
66361b5ef3498cfce3435c680b69e9f5f45d2af1 utils (heads/main) 
```

As you can see by default the __utility_repo__ submodule is on branch `main`. Please create your own dev branch to implement your changes to utils:

- ``` cd utils```
- ```git checkout -b dev_DB``` (or provide a different name for your dev branch)

Now you can add changes to __utility_repo__, for example you can move here your existing utils scripts (if they're not already included in this repo). 

Now change your directory out of `utils` back to your base repository folder:

- ```cd ..```
 
and run `git submodule status` again. The output should change to:

```
+56cd7caf704941c60a9f7589c64aad5310fb1855 utils (heads/dev_DB)
```

There's a plus sign at the start of the output, and the branch name changed. If you run `git status` again: you should see something like this:

```
Changes not staged for commit:
  (...)
        modified:   utils (new commits)
```

Git noticed that you switched to a new branch on the submodule and regards this as a new commit. This is because git is tracking changes inside submodules separately from the base repository.

### how to switch

You will need to update the paths to your util scripts in your _init.R_ (or anywhere else where you're sourcing them):

For example: ```source("./src/utils_cleaning.R")``` -> change to ```source("./src/utils/utils_cleaning.R")```

Of course, if your previous _utils.R_ contained some functions that are not included in __utility_repo__, you should move them there.

## Usage

 After working on your branch of __utility_repo__, remember to make commits inside the submodule. Every time you want to make a commit in your base project repo, BEFORE that you should go into your `src/utils` and make new commits there, then move back to the base repo and commit there, adding the changes that were mede in `utils`.
 
 If you need to pull remote changes from __utility_repo__, the command is:
 
 - `git submodule update` if you're in your base project repo
