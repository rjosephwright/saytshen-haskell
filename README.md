# saytshen
A tool for running security compliance scans.

[![Build Status](https://travis-ci.org/rjosephwright/saytshen-haskell.svg?branch=master)](https://travis-ci.org/rjosephwright/saytshen-haskell)

```
A tool for running security compliance scans

Usage: saytshen (-s|--spec SPEC)

Available options:
  -s,--spec SPEC           Path to audit specification file
  -h,--help                Show this help text
```

Run `saytshen` with an audit specification as input, and it will write a CSV report to results.csv from the directory where it was run.

`saytshen` will exit with `0` if all audit steps passed compliance, or it will exit with `1` if any steps failed. Any other unexpected errors will result in an exit code of `255`.

```
> saytshen -s centos-7-audit.yml
> echo $?
0
```

Audit specifications in YAML format can be found in the `examples` directory.
