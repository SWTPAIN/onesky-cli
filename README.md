# oneup-cli

# Development

```bash
stack ghci # to enter repl
stack build # to compile to binary
stack exec oneup-cli # to run the compiled executive. it resolves the build path by stack
stack exec oneup-cli -- download --directory ./tmp/locales --all
```

# Usage

```bash
#Upload a specific langauge
$ stack exec oneup-cli -- upload --directory <path/to/locales/folder> --all oneup upload --directory <path/to/locales/folder> --language en

#Download all language files
#Note that we expect there is a subfolder in <path/to/locales/en> which is
used to find out target file names
$ stack exec oneup-cli -- download --directory <path/to/locales/folder> --all
```
