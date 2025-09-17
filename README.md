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
