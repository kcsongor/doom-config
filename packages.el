;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; This is where you install packages, by declaring them with the `package!'
;; macro, then running 'doom refresh' on the command line. You'll need to
;; restart Emacs for your changes to take effect! Or at least, run M-x
;; `doom/reload'.
;;
;; WARNING: Don't disable core packages listed in ~/.emacs.d/core/packages.el.
;; Doom requires these, and disabling them may have terrible side effects.
;;
;; Here are a couple examples:

;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a particular repo, you'll need to specify
;; a `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, for whatever reason,
;; you can do so here with the `:disable' property:
;(package! builtin-package :disable t)

(package! hl-line :disable t)
(package! diredfl :disable t)

(package! polymode)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
                                        ;(package! builtin-package :recipe (:nonrecursive t))
                                        ;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
                                        ;(package! builtin-package :recipe (:branch "develop"))
(package! agda-input :pin "0895178")
(package! agda2-mode :pin "0895178")

(package! pushups
  :recipe (:local-repo "~/Dev/elisp/pushup/"))

(package! presenter
  :recipe (:local-repo "~/Dev/elisp/presenter/"))

(package! dark-mode
  :recipe (:local-repo "~/Dev/elisp/dark-mode/"))

(package! ghcid
  :recipe (:local-repo "~/Dev/elisp/ghcid/"))

(package! paredit)
(package! evil-paredit)

(package! which-key-posframe)

(package! ripgrep)
(package! deadgrep)
(package! suggest)
(package! diminish)

(package! structured-haskell-mode
  :recipe (:local-repo "~/Dev/elisp/structured-haskell-mode/elisp/"))

(package! tao-theme
  :recipe (:local-repo "~/Dev/elisp/tao-theme-emacs/"))

(package! dired-plus)
(package! ace-jump-mode)
(package! evil-snipe :disable t)
(package! yasnippet-snippets)

(package! ormolu)
(package! org-superstar)
(package! org-pdftools)
(package! magithub)

;; org-ref
(package! org-ref)
(package! helm-bibtex)
(package! bibtex-completion)

(package! org-agda-mode
  :recipe (:host github :repo "alhassy/org-agda-mode"))

(package! helm-swoop)
