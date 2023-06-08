# Bash Implementation

This is mostly a joke, honestly. I discovered it was technically possible and I figured I should know something more than I do about bash scripting, so I figured I'd give it a shot.

On the plus side, the only dependencies are OpenSSH, `psql`, and `jq`, so that's pretty cool.

## Development
Auto-reload the server:
```bash
find src | entr -rs "./run.sh"
```
