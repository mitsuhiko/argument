"""Captures zsh completions for a command line.

Usage: python3 zsh_complete.py <dir with _demo> <command line>...

Starts an interactive zsh in a pseudo terminal, loads the completion
function from the given directory, overrides `compadd` to print all matches
and triggers completion for each command line.  Prints one line per match
in the form `<command line>\t<match>`.
"""
import os
import pty
import select
import sys
import time

SETUP = r"""
PS1='READY> '
unsetopt beep
fpath=(%(dir)s $fpath)
autoload -U compinit && compinit -u -D
zstyle ':completion:*' list-grouped false
zstyle ':completion:*' insert-tab false
zstyle ':completion:*' menu no
complete-and-mark() { zle complete-word; print -r -- "@@DONE@@" > /dev/tty }
zle -N complete-and-mark
bindkey '^I' complete-and-mark
demo() { :; }
compadd() {
  if [[ ${@[1,(i)(-|--)]} == *-(O|A|D)\ * ]]; then
    builtin compadd "$@"; return $?
  fi
  typeset -a __hits
  builtin compadd -A __hits "$@"
  local h
  for h in $__hits; do print -r -- "@@HIT@@:$h" > /dev/tty; done
  builtin compadd "$@"
}
"""


def read_until(fd, marker, timeout=10.0):
    buf = b""
    deadline = time.time() + timeout
    while marker not in buf:
        remaining = deadline - time.time()
        if remaining <= 0:
            raise TimeoutError(buf.decode("utf-8", "replace"))
        ready, _, _ = select.select([fd], [], [], remaining)
        if ready:
            try:
                chunk = os.read(fd, 4096)
            except OSError:
                break
            if not chunk:
                break
            buf += chunk
    return buf.decode("utf-8", "replace")


def main():
    directory = sys.argv[1]
    lines = sys.argv[2:]
    pid, fd = pty.fork()
    if pid == 0:
        os.environ["TERM"] = "dumb"
        os.execvp("zsh", ["zsh", "-f", "-i"])
    os.write(fd, (SETUP % {"dir": directory}).encode() + b"\n")
    os.write(fd, b"print -r -- @@SET''UP@@\n")
    read_until(fd, b"@@SETUP@@")
    # the first completion in a fresh shell loads the completion system
    # and its matches are not reliably captured, so warm it up first.
    for idx, line in enumerate([lines[0]] + lines):
        os.write(fd, line.encode() + b"\t")
        out = read_until(fd, b"@@DONE@@")
        hits = []
        for raw in out.splitlines():
            raw = raw.replace("\r", "")
            if "@@HIT@@:" in raw:
                hit = raw.split("@@HIT@@:", 1)[1]
                if hit and hit not in hits:
                    hits.append(hit)
        if idx > 0:
            for hit in hits:
                print("%s\t%s" % (line, hit), flush=True)
        # clear the line and wait for a fresh prompt
        os.write(fd, b"\x15print -r -- @@NE''XT@@\n")
        read_until(fd, b"@@NEXT@@")
    os.close(fd)
    try:
        os.kill(pid, 9)
    except ProcessLookupError:
        pass
    os.waitpid(pid, 0)


if __name__ == "__main__":
    main()
