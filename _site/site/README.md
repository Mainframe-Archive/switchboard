# Switchboard Site

## Building

The Switchboard site is built by [jekyll](http://jekyllrb.com/), and
hosted using [GitHub Pages](http://pages.github.com). Check it out
[here](http://switchboard.spatch.co).

To build the site locally, you must have
[jekyll installed](http://jekyllrb.com/docs/installation/). If you're
editing the documentation, it's nice to build and serve the documentation
locally:

```bash
jekyll serve --watch
```

The API Docs are built into `site/doc` when you run `make docs`. It
replaces all the current documenation, resulting in `git` reporting
back that all the documentation has changed on build. You can `git-add`
only the files which have changed, and reset the rest. If you're only
editing files in `site`, there's no reason to rebuild the API Docs.


## Pushing

To deploy the documentation, `site` must be pushed to
`gh-pages`. `git-subtree` does the job (commands run
from root of the Switchboard repository):

```bash
git subtree push --prefix $(SITE_DIR) origin $(DOCS_BRANCH)
# or, a shortcut:
make update-docs
```

`gh-pages` should now have the `site` directory as it's root,
and should consist of all the commits made to `site` from the
checked out branch.
