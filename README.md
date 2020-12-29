# personal-emacs-dot-files

Personal Emacs Dot Files based on a bunch of other personal dot files and lot of googling and surfing, especially stack overflow and reddit sites.

If you're trying to start editing, writing and programming with Emacs (and say goodbye to Sublime Text or VIM, maybe you're interesting in this sort of bookmarks that I found helpful and inspiring:


## Things to remember

### Actions

M-/ Dynamic Expansion
C-u <number> up or down -> Move the cursor

Code folding: [How to achieve code folding effects in Emacs?](https://stackoverflow.com/questions/1085170/how-to-achieve-code-folding-effects-in-emacs)

    M-1 C-x $ and magic happens!

    C-x $ will bring your code back.

### Magit

    magit-refresh-all

refresh all Magit Buffers belonging to the current repository and also reverts all unmodified buffers that visit files being tracked in the current repository.


### GDB and Windows 10

You need msys2 or you are fucked up.

Trying to pass some input file to executable?

    gdb executable
    r <input file>

Some useful tips emacs and gdb: [LEGACY Crash Course in Debugging C Programs with Emacs GDB](https://paalijarvi.fi/tgblog/2020/03/29/legacy-crash-course-in-debugging-c-programs-with-emacs-gdb-annotate3/)
