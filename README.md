elisp-eval
==========

Eval expression in the plain buffer instead of minibuffer.

Commands
--------

-   `elisp-eval`

Eval expression in **eval-elisp** buffer instead of minibuffer.

This command makes the current buffer the target buffer of the
elisp-eval and displays a buffer named "**eval-elisp**" in another
window.

| Key          | Command                         | Description                     |
|--------------|---------------------------------|---------------------------------|
| C-\<return\> | elisp-eval–eval                 | eval and stay in window         |
| C-c C-c      | elisp-eval–eval-and-quit        | eval and exit window            |
| C-x 0        | elisp-eval-quit                 | bury buffer                     |
| C-x s        | elisp-eval-save-history         | save history                    |
| C-x C-s      | elisp-eval-save-current-element | save current element            |
| M-n          | elisp-eval-next-history-element | insert next history element     |
| M-p          | elisp-eval-prev-history-element | insert previous history element |
|              |                                 |                                 |

-   `elisp-eval-region-or-last-sexp` Eval active region or sexp at
    point.

-   `elisp-eval-cleanup-history` Cleanup history.

Customization
-------------

-   `elisp-eval-history-file` The filename to save history

-   `elisp-eval-history-max-size` Max size for history.
