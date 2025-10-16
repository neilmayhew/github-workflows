# github-workflows

Utility to manage GitHub workflows for a user

```
Usage: github-workflows [-t|--token SECRET] [-p|--page-size INT] [-f|--forks] [-a|--add REPO]
                        [-v|--verbose] [-b|--obscure] [-n|--noop] COMMAND

Available options:
  -h,--help                Show this help text
  -t,--token SECRET        The access token to be used for the Bearer authentication
  -p,--page-size INT       The page size to be used when using the GitHub API (default: 20)
  -f,--forks               Include forks
  -a,--add REPO            Add REPO to the list of repos to be processed (may be repeated)
  -v,--verbose             Output progress messages
  -b,--obscure             Obscure private repo names in output messages
  -n,--noop                Don't make any modifications
  -s,--save-repos FILE     Save a copy of the repo metadata to FILE as YAML

Available commands:
  export                   Export workflow metadata
  reenable                 Re-enable any workflows that were disabled due to inactivity

If the token isn't provided via the command line it will be taken from the GH_TOKEN
environment variable.
```

```
Usage: github-workflows export (-o|--output FILE)

  Export workflow metadata

Available options:
  -o,--output FILE         Write workflows to FILE
  -h,--help                Show this help text
```

```
Usage: github-workflows reenable [-w|--within DAYS]

  Re-enable any workflows that were disabled due to inactivity

Available options:
  -w,--within DAYS         Also reenable any workflows that will be disabled within the
                           next DAYS days
  -h,--help                Show this help text
```

You can run `reenable` with the `-n` option first to see which workflows it would re-enable.

## Using CI to keep your workflows enabled

1. Fork this repo to your own account/organization

2. Add a repository secret called `GH_TOKEN` containing a personal access token (PAT)
   that has *Write* access to *Actions*

3. Enable workflows for the repo (initially disabled in forks) on the **Actions** tab

### How it works

The workflow has an optional step that runs the program to re-enable CI in all
your non-fork repos that have been disabled due to inactivity or will be soon.
The option is enabled when the workflow is run by the schedule or run manually
and the option is chosen. By default, the workflow is scheduled weekly.
