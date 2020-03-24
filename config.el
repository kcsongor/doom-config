;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; refresh' after modifying this file!


;; These are used for a number of things, particularly for GPG configuration,
;; some email clients, file templates and snippets.


(add-to-list 'auth-sources "~/.authinfo")
(defvar theme-path "~/.emacs.d/themes/")

(add-to-list 'load-path theme-path)
;; (add-to-list 'load-path "~/Dev/elisp/")
(add-to-list 'custom-theme-load-path theme-path)

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; test
(setq doom-font (font-spec :family "iA Writer Mono S" :size 12 :weight 'regular)
      doom-variable-pitch-font (font-spec :family "iA Writer Duospace" :size 12 :weight 'regular))

(setq dark-theme 'monochrome)
(setq light-theme 'monochrome-light)

(theme-setup)

(map! :n "SPC t d" 'toggle-dark-mode
      :n "C-t" 'pop-tag-mark)

(setq doom-modeline-buffer-file-name-style 'relative-from-project)
(setq doom-modeline-icon nil)
(setq doom-modeline-buffer-state-icon nil)
(setq doom-modeline-major-mode-icon nil)
(setq doom-modeline-buffer-encoding nil)
(setq doom-modeline-indent-info nil)
(setq doom-modeline-github nil)
(setq doom-modeline-modal-icon nil)
(setq doom-modeline-height 20)

(setq display-line-numbers-type nil)

;; coq stuff
(after! company-coq
  (add-to-list 'company-coq-disabled-features 'prettify-symbols))

(after! flycheck
  (setq global-flycheck-mode nil)
  (setq flycheck-global-modes nil))

(after! magit
  (magit-auto-revert-mode -1))

(after! persp-mode
  (persp-set-keymap-prefix nil))

;;; evil
(setq evil-split-window-below t
      evil-vsplit-window-right t)

(after! projectile
  (map! :desc "projectile" :n "C-c p" 'projectile-command-map))

(after! which-key
  (which-key-setup-minibuffer)
  (setq which-key-show-early-on-C-h nil)
  (setq which-key-idle-delay 0.5)
  (setq which-key-idle-secondary-delay 0.05)
  (setq which-key-show-transient-maps t))


;; git navigation in file
(after! git-gutter
 (map! :n "M-j" 'git-gutter:next-hunk
       :n "M-k" 'git-gutter:previous-hunk
       :n "M-h" 'git-gutter:revert-hunk
       :n "M-l" 'git-gutter:stage-hunk))

(map! :n "C-x C-j" 'dired-jump
      :n "C-u" 'universal-argument)


(after! org
  (add-to-list 'org-modules 'org-habit t)
  (setq org-directory "~/org/"
        org-archive-location (concat org-directory "archive/%s::")
        org-ellipsis " ▼ "
        org-bullets-bullet-list '("•")
        org-agenda-span 14
        org-agenda-start-on-weekday 1
        org-agenda-start-day "today"
        ))

(evil-ex-define-cmd "bd" 'kill-this-buffer)

(after! evil
  (evil-define-command cs/show-file-info ()
    (let* ((nlines (count-lines (point-min) (point-max)))
           (file (buffer-file-name (buffer-base-buffer)))
           (line (line-number-at-pos)))
      (message "\"%s\" %d / %d lines" file line nlines)))
 (map! :n "C-g" 'cs/show-file-info))



;; ;; --------------------------------------------------------------------------------

(defmacro defsection (name &optional description &rest body)
  (declare (doc-string 2))
  (unless
      (and description (char-or-string-p description))
    (error (concat "Section " (prin1-to-string name) " has no docstring.")))
  `(progn ,@body))

(defsection ivy
  "Ivy settings"
 
  (after! ivy
    (map! :map ivy-minibuffer-map
          "<escape>" 'hydra-ivy/body)
    (map! :n "C-s" 'counsel-grep-or-swiper))

  (after! ivy-posframe
    (setq ivy-posframe-display-functions-alist
          '((swiper          . ivy-posframe-display-at-frame-top-center)
            (complete-symbol . ivy-posframe-display-at-frame-top-center)
            (counsel-M-x     . ivy-posframe-display-at-frame-top-center)
            (t               . ivy-posframe-display-at-frame-top-center))
          ivy-posframe-border-width 1)
    (ivy-posframe-mode 1)))

(defsection poly-mode
  "Poly-mode settings"
  (after! poly-mode
    (define-hostmode poly-latex-hostmode
      :mode 'LaTeX-mode)
    (define-hostmode poly-haskell-hostmode
      :mode 'haskell-mode)

    (define-innermode poly-haskell-innermode
      :mode 'haskell-mode
      :head-matcher "^\\\\begin{code}\n"
      :tail-matcher "^\\\\end{code}$"
      :tail-mode 'host
      :tail-mode 'host)

    (define-innermode poly-spec-innermode
      :mode 'haskell-mode
      :head-matcher "^\\\\begin{spec}\n"
      :tail-matcher "^\\\\end{spec}$"
      :tail-mode 'host
      :tail-mode 'host)

    (define-polymode hatex-mode
      :hostmode 'poly-latex-hostmode
      :innermodes '(poly-haskell-innermode poly-spec-innermode))))

(defsection workouts
  "Tracking home workouts."

  (map! :prefix ("C-c w" . "workouts")
        :desc "Log workout" "l" 'pushup/log
        :desc "Log pushups" "p" 'pushup/log-pushups
        :desc "Log pullups" "P" 'pushup/log-pullups
        :desc "Visit workouts" "v" 'pushup/visit))

(defsection coq
  "Proof stuff."

  (defsection undo
    "Fixing the behaviour of undo/redo in proof general."

    (map! :map coq-mode-map
          :n "C-r" 'coq-redo
          :n "u" 'coq-undo)

    (defun pg-in-protected-region-p ()
      (< (point) (proof-queue-or-locked-end)))

    (defmacro coq-wrap-edit (action)
      `(if (or (not proof-locked-span)
               (equal (proof-queue-or-locked-end) (point-min)))
           (,action)
         (,action)
         (when (pg-in-protected-region-p)
           (company-coq-proof-goto-point))))

    (defun coq-redo ()
      (interactive)
      (coq-wrap-edit undo-tree-redo))

    (defun coq-undo ()
      (interactive)
      (coq-wrap-edit undo-tree-undo))


    (add-hook! 'coq-mode-hook #'undo-tree-mode))

  (defsection navigation
    "Navigate proof state using a transient keymap."

    (map! :map coq-mode-map
          :n "C-c C-n" 'proof-next
          :n "SPC m ]" 'proof-next
          :n "C-c C-u" 'proof-previous
          :n "SPC m [" 'proof-previous)

    (defvar proof-step-transient-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "n") 'proof-assert-next-command-interactive)
        (define-key map (kbd "u") 'proof-undo-last-successful-command)
        (define-key map (kbd "p") 'proof-undo-last-successful-command)

        (define-key map (kbd "}") 'proof-process-buffer)
        (define-key map (kbd "x") 'proof-interrupt-process-goto-end)
        map))

    (defun proof-interrupt-process-goto-end ()
      (interactive)
      (proof-interrupt-process)
      (proof-goto-end-of-locked))

    (defun proof-next ()
      (interactive)
      (proof-assert-next-command-interactive)
      (message "Use n, u or ], [ for further steps.")
      (set-transient-map proof-step-transient-map 'proof-step-transient-keep))

    (defun proof-previous ()
      (interactive)
      (proof-undo-last-successful-command)
      (message "Use n, u or ], [ for further steps.")
      (set-transient-map proof-step-transient-map 'proof-step-transient-keep))

    (defun proof-step-transient-keep ()
      (let* ((key (this-command-keys))
             (binding (lookup-key proof-step-transient-map key)))
        binding)))

  (defsection auto-eval
    "Automatically evaluate up to point when . is pressed in insert mode.
     Ignored when editing a comment."

    (defun proof-insert-dot-and-eval ()
      (interactive)
      (if (proof-inside-comment (point))
          (insert ".")
        (unless (eq (char-before (point)) ?.)
          (insert ".")
          (let ((position (point)))
            (unless (proof-inside-comment (point))
              (company-coq-proof-goto-point)
              (sleep-for 0 100)
              (unless proof-shell-busy
                (goto-char position)))))))

    (map! :map coq-mode-map
          :i "." 'proof-insert-dot-and-eval)))

(defsection elisp
  "Editing elisp."

  (after! paredit
    (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
    (add-hook 'haskell-mode-hook 'enable-paredit-mode)
    (add-hook 'agda2-mode-hook 'enable-paredit-mode)
    (add-hook 'paredit-mode-hook 'evil-paredit-mode)))


(defhydra hydra-git ()
  ("j" git-gutter:next-hunk "next")
  ("k" git-gutter:previous-hunk "previous")
  ("d" git-gutter:popup-hunk "diff")
  ("l" git-gutter:stage-hunk "stage")
  ("h" git-gutter:revert-hunk "revert"))

(map! "C-c g" 'hydra-git/body)

(defsection refactoring
  "Tools for project navigation and refactoring."

  (defvar cs/error-called-from nil
    "The buffer that the next-error or previous-error functions were called from.")

  (defun cs/remember-current-buffer (&rest ...)
    (setq cs/error-called-from (current-buffer)))

  (advice-add 'next-error :before 'cs/remember-current-buffer)
  (advice-add 'previous-error :before 'cs/remember-current-buffer)

  (use-package! deadgrep
    :config
    (defun cs/deadgrep-visit-result (old-other)
      (if (member cs/error-called-from (deadgrep--buffers))
          (funcall old-other)
        (deadgrep-visit-result)))
    (advice-add 'deadgrep-visit-result-other-window :around 'cs/deadgrep-visit-result))

  (map! :prefix-map ("C-c r" . "refactoring")
        "d" 'deadgrep))

