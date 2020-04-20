# List of useful Emacs commands


`C-M i`: Ispell for a selected word
`C-c C-f`: Fold and unfold!

## Issues

## El paquet ivy i la creació de nous fitxers

Documentació del paquet ivy:

>This is useful e.g. when you call find-file to create a new file, but the desired name matches an existing file. In that case, using **C-j** would select that existing file, which isn't what you want - use this command instead. 

El procediment funciona tal com ho explica aquí: https://emacs.stackexchange.com/questions/28982/after-enabling-ivy-mode-dired-create-directory-does-not-allow-me-to-choose-an

>If it does match, then pressing enter will get ivy to expand the name to the completion. Here you want to call ivy-immediate-done which is bound to **C-M-j** by default after you have typed the name.
