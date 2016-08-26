# hosts
Hosts file builder, written in haskell
```
Usage: hosts [OPTIONS...]
  -i input      --input=input          input file
  -o OUTPUT     --output=OUTPUT        output file. Defaults to stdout
  -w whitelist  --whitelist=whitelist  whitelist file
  -h host       --hostname=host        hostname to use. Tries to determine system hostname if unspecified
```

Hosts files are downloaded and combined in parallel and normalized to all direct to `0.0.0.0` for efficiency
