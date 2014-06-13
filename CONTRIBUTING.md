## Contributing

Thanks for your interest in contributing to Switchboard. This document
provides a set of guidelines adapted from
[cowboy](https://github.com/extend/cowboy). Follow the guide to the
best of your ability, but err on the side of getting your
contributions out there. If you have questions, you can ask them on
the mailing lists.


### Introduction

This document describes the usages and rules to follow when contributing
to this project.

`git` is a distributed source code versioning system. This document refers
to three different repositories hosting the source code of the project.
`Your local copy` refers to the copy of the repository that you have on
your computer. The remote repository `origin` refers to your fork of the
project's repository that you can find in your GitHub account. The remote
repository `upstream` refers to the official repository for this project.

Following this document will ensure prompt merging of your work in the
`master` branch of the project.

Before implementing a new feature, you should create a new issue for
discussion on your plans. The feature might have been rejected
already, or the implementation might already be decided. If your idea
is less concrete, you can also use the mailing lists for discussion
on your idea.


### Cloning

You must fork the project's repository to your GitHub account by clicking
on the `Fork` button.

Then, from your fork's page, copy the `Git Read-Only` URL to your clipboard.
You must perform the following commands in the folder you choose, replacing
`$URL` by the URL you just copied, `$UPSTREAM_URL` by the `Git Read-Only`
project of the official repository, and `$PROJECT` by the name of this project.

``` bash
$ git clone "$URL"
$ cd $PROJECT
$ git remote add upstream $UPSTREAM_URL
```

### Branching

Before starting working on the code, you must update to `upstream`. The
project is always evolving, and as such you should always strive to keep
up to date when submitting patches to make sure they can be merged without
conflicts.

To update the current branch to `upstream`, you can use the following commands.

``` bash
$ git fetch upstream
$ git rebase upstream/master
```

It may ask you to stash your changes, in which case you stash with:

``` bash
$ git stash
```

And put your changes back in with:

``` bash
$ git stash pop
```

You should use these commands both before working on your patch and before
submitting the pull request. If conflicts arise it is your responsability
to deal with them.

You must create a new branch for your work. First, ensure you are on `master`.
You must update `master` to `upstream` before doing anything. Then create a
new branch `$BRANCH` and switch to it.

``` bash
$ git checkout -b $BRANCH
```

You MUST use a an insightful branch name.

If you later need to switch back to an existing branch `$BRANCH`, you can use:

``` bash
$ git checkout $BRANCH
```

### Source editing

The following rules must be followed:
 *  Indentation uses spaces for tabs, with 4 columns per indentation.
 *  Lines must not span more than 80 columns

The following rules should be followed:
 *  Write small functions whenever possible
 *  Avoid having too many clauses containing clauses containing clauses


### Committing

You must ensure that all commits pass all tests and do not have extra
Dialyzer warnings.

Running tests is fairly straightforward. Note that you need at least
Erlang/OTP R16B01 for the SSL tests to run.

``` bash
make tests
```

/* will integrate eunit with common test. */

Running Dialyzer requires some initial setup. You need to build the PLT
file that Dialyzer will use for its analysis. This is a one-time operation.
Dialyzer will take care of updating that file when needed.

``` bash
make build-plt
```

Once that is done, you can run Dialyzer.

``` bash
make dialyze
```

You must put all the related work in a single commit. Fixing a bug is one
commit, adding a feature is one commit, adding two features is two commits.

You must write a proper commit title and message. The commit title MUST be
at most 72 characters; it is the first line of the commit text. The second
line of the commit text MUST be left blank. The third line and beyond is the
commit message. You should write a commit message. If you do, you MUST make
all lines smaller than 80 characters. You should explain what the commit
does, what references you used and any other information that helps
understanding your work.


### Submitting the pull request

You must push your branch `$BRANCH` to GitHub, using the following command:

``` bash
$ git push origin $BRANCH
```

You must then submit the pull request using the GitHub interface.
Include an explanatory message and refer to any previous issues
related to this patch.

