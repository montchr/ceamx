# ceamx -- an emacs configuration

> generally no real magic in this world exists

—Eli Zaretskii

## Package Management

Packages are installed with Elpaca.

Previously, I have used `package.el` or Nixpkgs to manage packages.

### Nixpkgs

As a longtime Nix user, I actually think this is the easiest approach, with the
least amount of fiddling necessary.  Yes, that's right, I think Nix is *less*
fiddly than any other approach.

But: I would prefer using standalone/portable Emacs-specific package management
so Nix is not a hard requirement.  This is primarily because I am eagerly
anticipating the stable release of the official Emacs for Android, or at least
some established norms/idioms/best-practices for configuration on Android.

Currently I don't see any clear path towards supporting Nix there, at least not
for a while.  I would prefer to have interoperability between Emacs for Android
and Nix for Android instead of the GNU-signed Termux app, but AFAIK that would
require building both Emacs and Nix for Android APKs from source to self-sign,
which I don't want to do right now.

Oh yeah, and there's also WSL when I am forced to use Microsoft Windows for something
(like Adobe Acrobat).  Ideally I would use the NixOS on WSL setup but that's a
little bit of a project to integrate into my wasteland of a system configuration
repo.

### package.el

When I forget what pain feels like, I remind myself by attempting to use
`package.el`.  Soon I find that nothing works in a sane manner, and, the worst
part: it's barely configurable as Emacs should be.  This results in many nasty
hacks.  I do not think `package.el` is ready for daily usage in my life.

I tried leaning into package.el because:

1. It is built into Emacs, allowing for portability and predictable behavior
   across machines.  Ostensibily.
2. I thought using it would be a matter of "back-to-the-basics"/KISS.

But:

While the documentation seems thorough, I frequently need to dive into its
source code to figure out why some weird behavior is happening.  Several
configurations I've referenced apply advices to its internal functions to hammer
package.el into a usable machine.

Somehow, even though both `package.el` and `use-package` are part of Emacs, they
do not work well together.  TODO: add links to issues here

package.el forces the use of `user-custom-file`, with no option to specify a
different file or otherwise change this behavior.  I am absolutely not interested
in committing `custom.el` whenever the state of my installed packages changes.  I
like the idea of a lockfile, as every package manager should use one (except
Nix, which transcends such barbaric practices entirely), but the current state
of this behavior makes that impossible without resorting to hacks.  TODO: mention
the snippet I recently came across (saved to bookmark manager).

I've left a lot of comments throughout this configuration's explaining some of
`package.el`'s unintutive and sometimes downright terrible or broken behavior.
TODO: resurface these, which were probably deleted.

I imagine one day these issues will be resolved and stabilized in future
versions of Emacs, but until then, I would only be a reluctant user and find myself
still prone to indecision in this field.

On that note, I have since switched back to Elpaca.

## Notes

### Garbage Collection

While browsing many other user configs, I have noticed several slightly
different approaches to managing garbage collection, especially with the intent
of reducing startup time.

One approach is to offload this configuration to a package, safely hiding the
details away.  This package is `gcmh`, written by Andrea Corallo, perhaps better
known for gccemacs / native-comp.

As it turns out, as a direct response to a Reddit thread sharing `gcmh`, Eli
Zaretskii recommends caution in this field.  The package creator also weighs in.
Basically, Zaretskii recommends not overthinking things.

[eli-zaretskii comments on Garbage Collector Magic Hack](https://old.reddit.com/r/emacs/comments/bg85qm/garbage_collector_magic_hack/eln27qh/):

> My problem with the advice to make the GC threshold at such high values begins
> the moment you start publishing your personal tweaks as general advice to
> others.  IMO, this requires at least a lot of caveats, because your advice is
> likely to be followed by people whose workflows and system configurations are
> very different.  Simply put, you might get others in trouble by promoting your
> personal hacks as "magic".

From what I gather, `gcmh` has evolved a bit, so the previous criticism no
longer applies.

And, again, with more detail about fiddling carelessly with `gc-cons-threshold`:

[eli-zaretskii comments on An easy trick I found to improve Emacs start-up time](https://old.reddit.com/r/emacs/comments/yzb77m/an_easy_trick_i_found_to_improve_emacs_startup/iwz1vek/)

> The GC threshold setting after init is too high, IMNSHO, and its value seems
> arbitrary.
>
> If the OP thinks that Emacs will GC as soon as it allocates 100 MiB, then
> that's a grave mistake.  What really happens is the first time Emacs _considers
> doing GC_, if at that time more than 100 MiB have been allocated for Lisp
> objects, Emacs will GC.  And since neither Lisp programs nor the user have
> _any_ control on how soon Emacs will decide to check whether GC is needed, the
> actual amount of memory by the time Emacs checks could be many times the value
> of the threshold.
>
> My advice is to spend some time measuring the effect of increased GC threshold
> on operations that you care about and that take a long enough time to annoy,
> and use the lowest threshold value which produces a tangible improvement.
> Start with the default value, then enlarge it by a factor of 2 until you see
> only insignificant speedups.  I would not expect the value you arrive at to be
> as high as 100 MiB.

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
(defvar ceamx-foo-flag t)
(defvar ceamx-is-foo-enabled t)
(defun ceamx-foo-p ()
  ;; sketchy logic (don't do this)
  (or ceamx-foo-flag ceamx-is-foo-enabled))
```

#### Explanation

From [Coding Conventions (GNU Emacs Lisp Reference Manual)](https://www.gnu.org/software/emacs/manual/html_node/elisp/Coding-Conventions.html):

> If the purpose of a function is to tell you whether a certain condition is
> true or false, give the function a name that ends in ‘p’ (which stands for
> “predicate”.  If the name is one word, add just ‘p’; if the name is multiple
> words, add ‘\-p’.  Examples are `framep` and `frame-live-p`.  We recommend to
> avoid using this `-p` suffix in boolean variable names, unless the variable is
> bound to a predicate function; instead, use a `-flag` suffix or names like
> `is-foo`.

## Resources

### Emacs Lisp Manual: Tips and Conventions

[Tips (GNU Emacs Lisp Reference Manual)](https://www.gnu.org/software/emacs/manual/html_node/elisp/Tips.html)

> - [Emacs Lisp Coding Conventions]
> - [Key Binding Conventions]
> - [Emacs Programming Tips]
> - [Tips for Making Compiled Code Fast]
> - [Tips for Avoiding Compiler Warnings]
> - [Tips for Documentation Strings]
> - [Tips on Writing Comments]
> - [Conventional Headers for Emacs Libraries]
>
> [Emacs Lisp Coding Conventions]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Coding-Conventions.html
> [Key Binding Conventions]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Key-Binding-Conventions.html
> [Emacs Programming Tips]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Programming-Tips.html
> [Tips for Making Compiled Code Fast]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Compilation-Tips.html
> [Tips for Avoiding Compiler Warnings]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Warning-Tips.html
> [Tips for Documentation Strings]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Documentation-Tips.html
> [Tips on Writing Comments]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Comment-Tips.html
> [Conventional Headers for Emacs Libraries]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Library-Headers.html

## Tips

### Copy the list of packages installed by elpaca as string

First, invoke `(elpaca-write-lockfile)`.  In the output file, `defvar` its contents.

Assuming `(defvar ceamx-elpaca-packages ...)`, then:

```emacs-lisp
;; FIXME: rewrite without llama
(use-package llama)
(kill-new (mapconcat (##symbol-name (car %1)) ceamx-elpaca-packages "\n"))
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

> This is like `setq` (see above), but meant for user options.  This
> macro uses the Customize machinery to set the variable(s) (see
> [Defining Customization Variables][1].  In particular, `setopt` will
> run the setter function associated with the variable.
>
> [1]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Variable-Definitions.html

`setopt` is preferred even over the traditionally-recommended `customize-variable`:

> Unlike `defcustom` and related customization commands, such as
> `customize-variable`, `setopt` is meant for non-interactive use, in
> particular in the user init file.  For that reason, it doesn’t record
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

Note that a `nil` arg is *not* a reliable way to disable a mode.  Instead, use `-1`.

For example, here's an excerpt from the info page on `savehist-mode`:

> If called from Lisp, toggle the mode if ARG is <toggle>.
> Enable the mode _if ARG is nil_, omitted, or is a positive number.
> Disable the mode if ARG is a negative number.
