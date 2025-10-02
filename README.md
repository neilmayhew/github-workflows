# github-workflows

Utility to manage GitHub workflows for a user

```
Usage: github-workflows [-t|--token SECRET] [-p|--page-size INT] [-v|--verbose] [-n|--noop]
                        COMMAND

Available options:
  -h,--help                Show this help text
  -t,--token SECRET        The access token to be used for the Bearer authentication
  -p,--page-size INT       The page size to be used when using the GitHub API (default: 20)
  -v,--verbose             Output progress messages
  -n,--noop                Don't make any modifications

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
Usage: github-workflows reenable

  Re-enable any workflows that were disabled due to inactivity

Available options:
  -h,--help                Show this help text
```

You can run `reenable` with the `-n` option first to see which workflows it would re-enable.

## Using CI to keep your workflows enabled

1. Fork this repo to your own account/organization

2. Add a repository secret called `GH_TOKEN` containing a personal access token (PAT) that has *Write* access to *Actions*

3. Enable workflows for the repo (initially disabled in forks) on the **Actions** tab

### How it works

There's a scheduled workflow that remakes an empty commit to the `keep-alive` branch every month. This will ensure that this repo's CI isn't disabled due to inactivity.

The main workflow is scheduled every day and has an optional step that runs the program to re-enable CI in all your other (non-fork) repos. The option is enabled when the workflow is run by the schedule or run manually and the option is chosen.
