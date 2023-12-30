# ceamx -- an emacs configuration

The vast majority of packages are installed via Nixpkgs.
Others can be installed with `package.el`.

## Tasks

- [ ] Restore `org-modern-indent`
- [ ] Create Nix home-manager module to be imported in some configuration so
      that package requirements are not separated.

      Currently this configuration is split across multiple unrelated projects
      and only work by coincidence. Ceamx could not easily be re-created on a
      new machine until a Nix module is co-located here.
- [ ] `org-support-shift-select`

## Notes

### `magit-delta`

As of <2023-06-20 Tue>, removed primarily because of the massive hit to performance.
See <https://github.com/dandavison/magit-delta/issues/9>.

Also it requires disabling `line-numbers` in configuration,
which isn't a big issue but was the initial reason I uninstalled.
See <https://github.com/dandavison/magit-delta/issues/13>.

## Naming Things

### Boolean Variables vs. Predicate Functions

The naming for booleans and predicates is different.

#### Example

```elisp
(defvar cmx-foo-flag t)
(defvar cmx-is-foo-enabled t)
(defun cmx-foo-p ()
  ;; sketchy logic (don't do this)
  (or cmx-foo-flag cmx-is-foo-enabled))
```

#### Explanation

From [Coding Conventions (GNU Emacs Lisp Reference Manual)](https://www.gnu.org/software/emacs/manual/html_node/elisp/Coding-Conventions.html):

> If the purpose of a function is to tell you whether a certain condition is
> true or false, give the function a name that ends in ‘p’ (which stands for
> “predicate”). If the name is one word, add just ‘p’; if the name is multiple
> words, add ‘\-p’. Examples are `framep` and `frame-live-p`. We recommend to
> avoid using this `-p` suffix in boolean variable names, unless the variable is
> bound to a predicate function; instead, use a `-flag` suffix or names like
> `is-foo`.

## Tips

### Copy the list of packages installed by elpaca as string

First, invoke `(elpaca-write-lockfile)`. In the output file, `defvar` its contents.

Assuming `(defvar cmx-elpaca-packages ...)`, then:

```emacs-lisp
;; FIXME: rewrite without llama
(use-package llama)
(kill-new (mapconcat (##symbol-name (car %1)) cmx-elpaca-packages "\n"))
```

### Avoid `add-to-list`; prefer `push` or `cl-pushnew`

From the documentation for `add-to-list`:

> This is handy to add some elements to configuration variables,
> but please do not abuse it in Elisp code, where you are usually
> better off using push or cl-pushnew.
>
> If you want to use add-to-list on a variable that is not
> defined until a certain package is loaded, you should put the
> call to add-to-list into a hook function that will be run only
> after loading the package.  eval-after-load provides one way to
> do this.  In some cases other hooks, such as major mode hooks,
> can do the job.


### `setopt`, `setq`, and `customize-variable`

- Use `setopt` for user-customizable options, which may run setter functions.
- Use `setq` for regular variables which are not marked as customizable.

From [Setting Variables (GNU Emacs Lisp Reference Manual)](https://www.gnu.org/software/emacs/manual/html_node/elisp/Setting-Variables.html#index-setopt)

> This is like `setq` (see above), but meant for user options. This
> macro uses the Customize machinery to set the variable(s) (see
> [Defining Customization Variables][1]). In particular, `setopt` will
> run the setter function associated with the variable.
>
> [1]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Variable-Definitions.html

`setopt` is preferred even over the traditionally-recommended `customize-variable`:

> Unlike `defcustom` and related customization commands, such as
> `customize-variable`, `setopt` is meant for non-interactive use, in
> particular in the user init file. For that reason, it doesn’t record
> the standard, saved, and user-set values, and doesn’t mark the
> variable as candidate for saving in the custom file.

RMS disagrees:

> Since it isn't meant for the command line options that `getopt' examines,
> the name `setopt' is misleading.  (It already misled me!)
> This name should be changed to something longer and clearer.

More background:

- https://lists.gnu.org/archive/html/emacs-devel/2022-02/msg00585.html

### Enable/disable mode with argument

[How to choose between nil and 0, or t and 1 when setting variables or enabling modes](https://emacs.stackexchange.com/questions/2423/how-to-choose-between-nil-and-0-or-t-and-1-when-setting-variables-or-enabling-m)

In short, use whatever the mode function accepts, or the package
installation instruction suggests.

Note that a `nil` arg is *not* a reliable way to disable a mode. Instead, use `-1`.

For example, here's an excerpt from the info page on `savehist-mode`:

> If called from Lisp, toggle the mode if ARG is <toggle>.
> Enable the mode _if ARG is nil_, omitted, or is a positive number.
> Disable the mode if ARG is a negative number.
