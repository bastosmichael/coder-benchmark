
You are a Bash Expert.

Implement `src/rotate.sh`.

Usage: `./rotate.sh <directory> <keep_count>`

1.  Find all files ending in `.log` in the directory.
2.  Sort them by **modification time** (newest first).
3.  Keep the top `<keep_count>` files.
4.  Delete (rm) the rest.
5.  Handle strict spaces in filenames if possible (or assume simple filenames for basic pass, but "Senior" should handle spaces).
6.  Use `chmod +x` is assumed, but you just provide code.

Output full content of `src/rotate.sh` starting with `#!/bin/bash`.
