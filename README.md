# hosts
Hosts file builder, written in Haskell
```
Usage: hosts [OPTIONS...]
  -i INPUT      --input=INPUT          input file
  -o OUTPUT     --output=OUTPUT        output file. Defaults to stdout
  -w WHITELIST  --whitelist=WHITELIST  whitelist file. Defaults to ~/.hosts_whitelist.txt
  -h HOSTNAME   --hostname=HOSTNAME    hostname to use. Tries to determine system hostname if unspecified
```

Hosts files are downloaded and combined in parallel and normalized to all direct to `0.0.0.0` for efficiency  
The whiltelist file is for exact entries to not be included in the final output
