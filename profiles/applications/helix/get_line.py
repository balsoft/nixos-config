#!/usr/bin/env python3

import subprocess
import sys

def map_line_to_head(file_path, local_line):
    cmd = ["git", "diff", "-U0", "HEAD", "--", file_path]
    diff = subprocess.check_output(cmd, text=True).splitlines()

    offset = 0
    for line in diff:
        if line.startswith('@@'):
            # Parse @@ -old_start,old_len +new_start,new_len @@
            parts = line.split()
            old = parts[1][1:].split(',')
            new = parts[2][1:].split(',')

            old_s, old_l = int(old[0]), (int(old[1]) if len(old) > 1 else 1)
            new_s, new_l = int(new[0]), (int(new[1]) if len(new) > 1 else 1)

            # If your line is inside an added/modified block, it didn't exist in HEAD
            if new_s <= local_line < new_s + new_l:
                return "This line was added/modified and does not exist in HEAD."

            # If the change happened before your line, track the shift
            if new_s < local_line:
                offset += (new_l - old_l)

    return local_line - offset

def repo():
    origin = subprocess.check_output(["git", "remote", "get-url", "origin"], text=True)
    return origin.strip().replace("ssh://git@", "https://").removesuffix(".git")

def commit():
    commit = subprocess.check_output(["git", "rev-parse", "HEAD"], text=True)
    return commit.strip()

# Usage: get_line.py <filename> <line_number>
print(f"{repo()}/tree/{commit()}/{sys.argv[1]}#L{map_line_to_head(sys.argv[1], int(sys.argv[2]))}")
