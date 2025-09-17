# github-workflows

Utility to manage GitHub workflows for a user

```
Usage: github-workflows [-t|--token SECRET] [-p|--page-size INT] [-o|--output FILE]
                        [-v|--verbose] [-n|--noop] USERNAME

Available options:
  -h,--help                Show this help text
  -t,--token SECRET        The access token to be used for the Bearer authentication
  -p,--page-size INT       The page size to be used when using the GitHub API (default: 20)
  -o,--output FILE         Write workflows to FILE
  -v,--verbose             Output progress messages
  -n,--noop                Don't make any modifications
  USERNAME                 The name of the GitHub user

If the token isn't provided via the command line it will be taken from the GH_TOKEN environment variable.
```
