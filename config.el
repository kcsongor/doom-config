;;; $DOOMDIR/config.el -*- mode: emacs-lisp; lexical-binding: t; -*-

(defmacro defsection (name &optional description &rest body)
  "Set up the general structure of this config file"
  (declare (doc-string 2))
  (unless
      (and description (char-or-string-p description))
    (warn (concat "Section " (prin1-to-string name) " has no docstring.")))
  `(progn ,@body))

(defsection general
  "General settings that don't fit into any specific bucket."

  (add-to-list 'auth-sources "~/.authinfo")
  (fringe-mode nil)

  (advice-add 'doom--recenter-on-load-saveplace-a :override (lambda ()))
  (remove-hook 'xref-after-jump-hook 'recenter)

  (add-to-list 'default-frame-alist '(internal-border-width . 24))

  (setq-default line-spacing 1)
  (setq widget-image-enable nil)

  (map! :n "C-u" 'universal-argument)

  (defsection hard-mode
    "Here, I retrain my muscle memory by temporarily disabling certain key
    bindings. Then, when I got used to the 'more optimal' way, re-enable the old ones."

    (advice-remove 'evil-window-left (lambda (&rest _) nil))
    (advice-remove 'evil-window-right (lambda (&rest _) nil))
    (advice-remove 'evil-window-up (lambda (&rest _) nil))
    (advice-remove 'evil-window-down (lambda (&rest _) nil))
    (map! :map evil-motion-state-map
          "h" 'evil-backward-char
          "j" 'evil-next-line
          "k" 'evil-previous-line
          "l" 'evil-forward-char))
  (defsection workspaces
    "Workspace management"

    (map! :n "C-s-[" '+workspace/swap-left
          :n "C-s-]" '+workspace/swap-right
          :n "s-[" '+workspace/switch-left
          :n "s-]" '+workspace/switch-right
          :n "s-}" (lambda () (interactive) (+workspace/new (concat (+workspace-current-name) "-copy") t))
          :n "s-r" '+workspace/rename)

    ;; (map! :leader "TAB TAB" nil)
    (map! :leader "TAB" '+ivy/switch-workspace-buffer)

    (add-hook 'pre-command-hook 'show-workspaces)

    (defun show-workspaces ()
      (interactive)
      (when (and (not (minibufferp))
                 (or (not (current-message))
                     (equal "Quit" (current-message))))
        (+workspace/display)))

    (defvar workspace-timer nil
      "Show workspace timer.")

    (setq workspace-timer (run-with-idle-timer 0.5 t 'show-workspaces))

    ;; cancel old timer
    (add-variable-watcher 'workspace-timer (lambda (&rest _) (cancel-timer workspace-timer)))
    )
  (defsection helpful
    "Help buffers."

    (setq better-jumper--buffer-targets "^\\*.*\\*$")
    (advice-add 'push-button :before (lambda (&rest _) (better-jumper--push)))
    (advice-add 'helpful-visit-reference :before (lambda (&rest _) (better-jumper--push))))
  (defsection which-key
    "Which-key."
    (after! which-key
      (which-key-setup-minibuffer)
      (setq which-key-show-early-on-C-h nil)
      (setq which-key-idle-delay 0.5)
      (setq which-key-idle-secondary-delay 0.05)
      (setq which-key-show-transient-maps t)))

  (defsection evil
    "General evil settings."

    (evil-escape-mode -1)
    (defsection maps
      "Evil key bindings"
      (map! :n "C-g" 'cs/show-file-info
            :n "<escape>" 'show-workspaces)
      (map! :leader
            "j j" 'cs/counsel-config-section
            "j m" 'cs/goto-major-mode-config)
      (map! :desc "Rename buffer file" :n "SPC b R" 'rename-current-buffer-file)
      (evil-ex-define-cmd "bd" 'kill-this-buffer)
      (evil-ex-define-cmd "q" 'kill-this-buffer)

      (setq evil-org-key-theme '(navigation insert textobjects calendar)))
    (defsection utilities
      "Evil utility functions"
      (evil-define-command cs/show-file-info ()
        (let* ((nlines (count-lines (point-min) (point-max)))
               (file (buffer-file-name (buffer-base-buffer)))
               (line (line-number-at-pos)))
          (message "\"%s\" %d / %d lines" file line nlines)))

      (defun rename-current-buffer-file ()
        "Renames current buffer and file it is visiting."
        (interactive)
        (let ((name (buffer-name))
              (filename (buffer-file-name)))
          (if (not (and filename (file-exists-p filename)))
              (error "Buffer '%s' is not visiting a file!" name)
            (let ((new-name (read-file-name "New name: " filename)))
              (if (get-buffer new-name)
                  (error "A buffer named '%s' already exists!" new-name)
                (rename-file filename new-name 1)
                (rename-buffer new-name)
                (set-visited-file-name new-name)
                (set-buffer-modified-p nil)
                (message "File '%s' successfully renamed to '%s'"
                         name (file-name-nondirectory new-name)))))))))
  (defsection system
    "Interacting with the operating system."

    (defsection yabai
      "Yabai window manager.
       TODO: check if it's installed, running, parse from skhd, etc."

      (define-minor-mode yabai-minor-mode
        "Minor mode for interacting with the yabai window manager."
        :keymap (make-sparse-keymap)
        :global t)

      (defun go-window-right (&rest _)
        (interactive)
        (condition-case nil
            (evil-window-right 1)
          (error (start-process-shell-command "" nil "yabai -m window --focus east"))))

      (defun go-window-left (&rest _)
        (interactive)
        (condition-case nil
            (evil-window-left 1)
          (error (start-process-shell-command "" nil "yabai -m window --focus west"))))

      (defun go-window-up (&rest _)
        (interactive)
        (condition-case nil
            (evil-window-up 1)
          (error (start-process-shell-command "" nil "yabai -m window --focus north"))))

      (defun go-window-down (&rest _)
        (interactive)
        (condition-case nil
            (evil-window-down 1)
          (error (start-process-shell-command "" nil "yabai -m window --focus south"))))

      (map! :map yabai-minor-mode-map
            :ni "M-L" 'go-window-right
            :ni "M-H" 'go-window-left
            :ni "M-J" 'go-window-down
            :ni "M-K" 'go-window-up)

      (yabai-minor-mode 1)))

  (defsection theme
    "Colours! (not too many)"

    (setq doom-font (font-spec :family "Input Mono" :size 12 :weight 'medium)
          doom-variable-pitch-font (font-spec :family "iA Writer Duospace" :size 12 :weight 'regular))


    (defvar theme-path "~/.emacs.d/themes/")
    (add-to-list 'load-path theme-path)
    (add-to-list 'custom-theme-load-path theme-path)

    (setq dark-theme 'tao-yin)
    (setq light-theme 'tao-yang)

    (setq tao-theme-use-height nil)

    (theme-setup)

    (map! :n "SPC t d" 'toggle-dark-mode)

    (defsection modeline
      "Doom modeline settings"

      (after! doom-modeline
        (setq doom-modeline-buffer-file-name-style 'relative-from-project)
        (setq doom-modeline-icon nil)
        (setq doom-modeline-buffer-state-icon nil)
        (setq doom-modeline-major-mode-icon nil)
        (setq doom-modeline-minor-modes nil)
        (setq doom-modeline-buffer-encoding nil)
        (setq doom-modeline-indent-info nil)
        (setq doom-modeline-github nil)
        (setq doom-modeline-modal-icon nil)
        (setq doom-modeline-height 20)
        (setq! +modeline-height 20))

      (setq window-divider-default-right-width 1)
      (setq window-divider-default-places 'right-only)
      (window-divider-mode)

      (defun mode-line-render (left right)
        (let* ((available-width (- (window-width) (length left) )))
          (format (format "%%s %%%ds" available-width) left right)))
      (setq-default header-line-format
                    '((:eval
                       (mode-line-render
                        (format-mode-line (list
                                           (propertize "☰" 'face `(:inherit mode-line-buffer-id)
                                                       'help-echo "Mode(s) menu"
                                                       'mouse-face 'mode-line-highlight
                                                       'local-map   mode-line-major-mode-keymap)
                                           " %b "
                                           (if (and buffer-file-name (buffer-modified-p))
                                               (propertize "(modified)" 'face `(:inherit face-faded)))))
                        (format-mode-line
                         (propertize "%4l:%2c  " 'face `(:inherit face-faded)))))))

      ;; Comment if you want to keep the modeline at the bottom
      ;; (setq-default header-line-format mode-line-format)
      (setq-default mode-line-format '(""))
      (defun set-face (face style)
        "Reset a face and make it inherit style."
        (set-face-attribute face nil
        :foreground 'unspecified :background 'unspecified
        :family     'unspecified :slant      'unspecified
        :weight     'unspecified :height     'unspecified
        :underline  'unspecified :overline   'unspecified
        :box        'unspecified :inherit    style))
      (defun set-modeline-faces ()

        ;; Mode line at top
        (set-face 'header-line                                 'face-strong)
        (set-face-attribute 'header-line nil
                            :underline (face-foreground 'default))
        (set-face-attribute 'mode-line nil
                            :height 10
                            :underline (face-foreground 'default)
                            :overline nil
                            :box nil
                            :foreground (face-background 'default)
                            :background (face-background 'default))
        (set-face 'mode-line-inactive                            'mode-line)
        (set-face-attribute 'cursor nil
                            :background (face-foreground 'default))
        ;; (set-face-attribute 'window-divider nil
        ;;                     :foreground (face-background 'mode-line))
        ;; (set-face-attribute 'window-divider-first-pixel nil
        ;;                     :foreground (face-background 'default))
        ;; (set-face-attribute 'window-divider-last-pixel nil
        ;;                     :foreground (face-background 'default))
        )
      (set-modeline-faces)

      (ace-window-display-mode -1))))

(defsection presentations
  "Giving presentations from emacs."

  (use-package! presenter)
  (after! presenter
    (map! :map presentation-mode-map
          :n "M-j" 'presenter/next
          :n "M-k" 'presenter/previous
          :n "M-i" (lambda () (interactive) (presenter/find-slide "imports"))
          :n "M-e" (lambda () (interactive) (presenter/find-slide "extensions"))
          :n "M-o" 'presenter/go-slide)
    (map! :leader
          "t p" 'presentation-mode)))
(defsection code-navigation
  "Moving around"

  (map! :n "C-t" 'pop-tag-mark)

  (setq evil-search-wrap nil)
  (setq display-line-numbers-type nil)
  (setq evil-split-window-below t
        evil-vsplit-window-right t)

  (after! flycheck
    (setq global-flycheck-mode nil)
    (setq flycheck-global-modes nil))

  (after! projectile
    (map! :desc "projectile" :n "C-c p" 'projectile-command-map))
  (after! persp-mode
    (persp-set-keymap-prefix nil))

  (map! :n "s" 'evil-ace-jump-char-mode
        :o "C-s" 'evil-ace-jump-char-mode
        :v "s" 'evil-substitute
        :n "C-s" 'ace-jump-mode-pop-mark)
  (defsection git
    "Git stuff."

    (after! git-gutter
      (map! :n "M-j" 'git-gutter:next-hunk
            :n "M-k" 'git-gutter:previous-hunk
            :n "M-h" 'git-gutter:revert-hunk
            :n "M-l" 'git-gutter:stage-hunk)))

  (after! magit
    (magit-auto-revert-mode -1))

  (defsection highlighting
    "Highlighting."
    (defvar-local custom-highlights '())

    (require 'files-x)

    (defun do-highlights ()
      (interactive)
      (hi-lock-mode)

      (let ((watchers (get-variable-watchers 'hi-lock-interactive-patterns)))
        (if (member 'update-highlights watchers)
            (progn
              (remove-variable-watcher 'hi-lock-interactive-patterns 'update-highlights)
              (setq hi-lock-interactive-patterns custom-highlights)
              (add-variable-watcher 'hi-lock-interactive-patterns 'update-highlights))
          (setq hi-lock-interactive-patterns custom-highlights)))

      (hi-lock-set-file-patterns custom-highlights)
      (message (format "Loaded %d highlight(s)" (length custom-highlights))))

    (add-variable-watcher 'hi-lock-interactive-patterns 'update-highlights)

    ;; (add-hook 'find-file-hook 'do-highlights)

    (defun update-highlights (_sym newval operation _where)
      (interactive)
      (when (and newval (eq operation 'set))
        (save-window-excursion
          (modify-dir-local-variable nil 'custom-highlights newval 'add-or-delete)
          (save-buffer)))))

  (defsection lsp
    "Language server"
    (after! lsp-mode
      (map! :n "g ?" 'lsp-ui-doc-glance
            :n "g ]" 'lsp-find-definition)
      (setq lsp-ui-sideline-enable nil)

      (defvar cs/enable-lsp nil
        "When non-nil (in dir-locals), LSP is automatically enabled.")

      (defun cs/enable-haskell-lsp ()
        (interactive)
        (when cs/enable-lsp
          (lsp)))

      (add-hook 'haskell-mode-hook
                'cs/enable-haskell-lsp)))
  )
(defsection org
  "Org mode"
  (after! org
    (add-to-list 'org-modules 'org-habit t)
    (setq org-directory "~/org/"
          org-archive-location (concat org-directory "archive/%s::")
          org-ellipsis " ▼ "
          org-bullets-bullet-list '("•")
          org-superstar-headline-bullets-list org-bullets-bullet-list
          org-agenda-span 7
          org-agenda-start-on-weekday 1
          org-agenda-start-day "today"
          org-agenda-sticky nil
          org-agenda-show-future-repeats nil
          )

    (doom-themes-org-config)

    (setq org-todo-keywords
          (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
               (sequence "READING(r)" "|" "READ(R)")
               (sequence "TEACHING" "CONSULTING" "TO-CLAIM" "|" "CLAIMED")
               (sequence "|" "CANCELLED(c@/!)" "MEETING" "TALK"))))
    (require 'org-inlinetask)

    (map! :map 'org-mode-map
          :n "C-M-x" 'org-latex-export-to-pdf)
    )

  (use-package! org-ref
    :config
    (setq org-ref-completion-library 'org-ref-helm-bibtex)))

(defsection eshell
  "Eshell"
  (defsection prompt
    "Customise the eshell prompt."
    (setq eshell-prompt-function
          (lambda ()
            (let ((white  `(face-attribute 'default :foreground))
                  (green  `(face-attribute 'success :foreground))
                  (yellow `(face-attribute 'warning :foreground)))
              (concat
               (propertize "["                'face white)
               (propertize (format-time-string "%H:%M" (current-time)) 'face yellow)
               (propertize "]"                'face white)
               (propertize (if (= (user-uid) 0) " # " " λ ") 'face green))
              ))))

  (defsection history
    "Fuzzy search history."

    ;; eshell-mode-map is buggy, so we need a hook here.
    (add-hook 'eshell-mode-hook
              (lambda () (map! :map eshell-mode-map :i "C-r" '+eshell/search-history)))

    (defun cs/zsh-history ()
      "Browse zsh history."
      (interactive)
      (let ((command (with-temp-buffer
                       (insert-file-contents-literally "~/.histfile")
                       (let ((history-list (delete-dups (split-string (buffer-string) "\n" t))))
                         (ivy-read "Command: " history-list)))))
        (when comman
          (insert command))))))

(defsection ivy
  "Ivy settings"
  )
(defsection helm
  "Helm stuff"
  (after! helm
    (setq helm-display-function 'helm-default-display-buffer)
    (map! :n "/" 'helm-swoop)
    (add-to-list 'display-buffer-alist
                 `(,(rx bos "*helm" (* not-newline) "*" eos)
                   (display-buffer-in-side-window)
                   (inhibit-same-window . t)
                   (window-height . 0.4)))
    (use-package! helm-swoop)))

(defsection dired
  "Directory editing."
  (after! dired
    (require 'dired+))
  (map! :n "C-x C-j" 'dired-jump))
(defsection compilation
  "Compilation settings"

  (require 'ansi-color)
  (defun endless/colorize-compilation ()
    "Colorize from `compilation-filter-start' to `point'."
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region
       compilation-filter-start (point))))

  (add-hook 'compilation-filter-hook #'endless/colorize-compilation))

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
      :innermodes '(poly-haskell-innermode poly-spec-innermode))
    ))

(defsection workouts
  "Tracking home workouts."

  (map! :prefix ("C-c w" . "workouts")
        :desc "Log workout" "l" 'pushup/log
        :desc "Log pushups" "p" 'pushup/log-pushups
        :desc "Log pullups" "u" 'pushup/log-pullups
        :desc "Visit workouts" "v" 'pushup/visit)

  (setq pushup-daily-target 500
        pushup-increment-function (lambda (x) x)))

(defsection coq
  "Proof stuff."

  (after! company-coq
    ;;
    (add-to-list 'company-coq-disabled-features 'prettify-symbols))

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
           (proof-goto-point))))

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

(defsection emacs-lisp-mode
  "Editing elisp."

  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'haskell-mode-hook 'enable-paredit-mode)
  (add-hook 'agda2-mode-hook 'enable-paredit-mode)
  (add-hook 'paredit-mode-hook 'evil-paredit-mode))

(defsection haskell-mode
  "Haskell stuff."

  (remove-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (remove-hook 'haskell-mode-hook 'structured-haskell-mode)

  (setq haskell-interactive-popup-errors nil)
  (setq haskell-process-suggest-language-pragmas nil)

  (after! flycheck
    (setq-default flycheck-disabled-checkers '(haskell-ghc haskell-hlint haskell-stack-ghc)))

  (after! 'structured-haskell-mode
    (define-key shm-map (kbd "M-e") 'shm/goto-parent-end))

  (use-package! ormolu
    :bind
    (:map haskell-mode-map
      ("C-c r r" . ormolu-format-buffer))
    :config
    (setq ormolu-process-path "ormolu"))

  (setq lsp-haskell-process-path-hie "cs")

  (defun regex-within (a b)
    (concat a "[^" b "]*" b))

  (defun cs/haskell-type-args (str)
    (let* ()
      (s-replace-regexp (s-join "\\|" (list (regex-within "(" ")") (regex-within "\\[" "\\]"))) (lambda (x) "asd") str)))


  (defun company-lsp--haskell-completion-snippet (item)
    "Function providing snippet with the haskell language.
It parses the function's signature in ITEM (a CompletionItem)
to expand its arguments."
    ;; (message item)
    (-when-let* ((kind (gethash "kind" item))
                 (is-function (= kind 3)))
      (let* ((detail (gethash "detail" item))
             (snippet
              (with-temp-buffer
                (insert (concat "(" detail ")"))
                (goto-char (point-min))
                (let* ((typ (-last-item (-split-on ':: (sexp-at-point))))
                       (after=> (-last-item (-split-on '=> typ)))
                       (pieces (-drop-last 1 (-map '-flatten (-split-on '-> after=>)))))
                  (-map-indexed (lambda (ix x) (format "${%s}" x)) pieces)))))
        (if snippet (concat " " (s-join " " snippet)) ""))))
  (after! company-lsp
    (add-to-list 'company-lsp--snippet-functions '("haskell" . company-lsp--haskell-completion-snippet))))

(defsection refactoring
  "Tools for project navigation and refactoring."

  (map! :map deadgrep-mode-map
        "<return>" 'cs/deadgrep-visit-result-push-mark
        :desc "Search tag" "C-c t" 'cs/deadgrep-search-tag
        :desc "Search" "C-c c" 'cs/deadgrep-change-term
        :desc "Change type" "C-c r" 'cs/deadgrep-search-type
        :desc "Change directory" "C-c d" 'cs/deadgrep-change-directory
        :n "C-j" 'deadgrep-forward-match
        :n "C-k" 'deadgrep-backward-match
        :desc "Change context" "C-c -" 'cs/deadgrep-change-context)

  (defvar cs/error-called-from nil
    "The buffer that the next-error or previous-error functions were called from.")

  (defun cs/remember-current-buffer (&rest _)
    (setq cs/error-called-from (current-buffer)))

  (advice-add 'next-error :before 'cs/remember-current-buffer)
  (advice-add 'previous-error :before 'cs/remember-current-buffer)

  (use-package! deadgrep
    :config
    (setq deadgrep-project-root-function 'projectile-project-root)

    (defun cs/deadgrep-visit-result (old-other)
      (if (member cs/error-called-from (deadgrep--buffers))
          (funcall old-other)
        (deadgrep-visit-result)))
    (advice-add 'deadgrep-visit-result-other-window :around 'cs/deadgrep-visit-result)

    (defun cs/deadgrep-visit-result-push-mark ()
      (interactive)
      (xref-push-marker-stack)
      (deadgrep-visit-result))

    (defun cs/deadgrep-search-tag (tagname)
      (interactive (find-tag-interactive "View tag other window: "))
      (save-excursion
        (setq deadgrep--search-term tagname)
        (rename-buffer
         (deadgrep--buffer-name deadgrep--search-term default-directory) t)
        (deadgrep-restart)))

    (defun cs/deadgrep-change-term ()
      (interactive)
      (save-excursion
        (goto-char (point-min))
        (re-search-forward "change")
        (deadgrep--search-term (point))))

    (defun cs/deadgrep-change-context (how-many)
      (interactive "nHow many lines before/after: ")
      (save-excursion
        (setq deadgrep--context `(,how-many . ,how-many))
        (deadgrep-restart)))

    (defun cs/deadgrep-change-directory ()
      (interactive)
      (deadgrep--directory nil))

    (defun cs/deadgrep-active-searches ()
      "Return active searches."
      (-map (lambda (buffer) (cadr (s-split " " (buffer-name buffer)))) (deadgrep--buffers)))

    (defun cs/deadgrep-new-search (term)
      "Start new search or open existing search buffer."
      (interactive
       (list (completing-read "Search term: "
                              (cs/deadgrep-active-searches)
                              nil nil (symbol-name (symbol-at-point)))))
      (let ((existing (-first (lambda (buffer) (s-contains? term (buffer-name buffer)))
                              (deadgrep--buffers))))
        (if existing
            (switch-to-buffer existing)
          (deadgrep term))))


    (defun cs/deadgrep-search-type (type)
      (interactive "sSearch type: ")
      (save-excursion
        (goto-char (point-min))
        (re-search-forward "Search type:")
        (re-search-forward type)
        (deadgrep--search-type (point))))

    )

    (map! :prefix-map ("C-c r" . "refactoring")
          "d" 'cs/deadgrep-new-search)
    )

(defsection windows
  "Window and buffer management"

  (map! :prefix ("C-c t" . "toggles")
        :desc "Dedicated window" "d" 'toggle-window-dedicated
        :desc "Fix window size" "s" 'fix-window-size
        :desc "Unfix window size" "u" 'unfix-window-size
        :desc "Fix window height" "h" 'fix-window-height
        :desc "Fix window width" "w" 'fix-window-width)

  (defun fix-window-size ()
    "Fix window height."
    (interactive)
    (setq window-size-fixed t))

  (defun fix-window-height ()
    "Fix window height."
    (interactive)
    (setq window-size-fixed 'height))

  (defun fix-window-width ()
    "Fix window width."
    (interactive)
    (setq window-size-fixed 'width))

  (defun unfix-window-size ()
    "Unfix window size."
    (interactive)
    (setq window-size-fixed nil))

  (defun toggle-window-dedicated ()
    "Control whether or not Emacs is allowed to display another
  buffer in current window."
    (interactive)
    (message
    (if (let ((window (get-buffer-window (current-buffer))))
          (set-window-dedicated-p window (not (window-dedicated-p window))))
        "%s: Can't touch this!"
      "%s is up for grabs.")
    (current-buffer))))

(defsection ghc
  "Working on ghc."
  (map! :map haskell-mode-map
        :n "C-j" 'flycheck-next-error
        :n "C-k" 'flycheck-previous-error
        :n "M-n" 'next-error
        :n "M-p" 'previous-error
        :n "M-RET" 'flycheck-buffer)

  (defun ghc-gen-trace ()
    (interactive)
    (save-window-excursion
     (async-shell-command (format "./ghc %s -fforce-recomp -ddump-tc-trace -ddump-rn-trace -ddump-to-file" (buffer-name))))))

;; Found this at
;; https://github.com/TOTBWF/BigMacs/blob/fd2b621a060227a5308fcbde69ea538db0bbeb5c/init.org
;; Cheers!
(defun create-file-template (regex template mode)
  (add-to-list 'auto-insert-alist
               `(,regex .  [(lambda () (yas-expand-snippet (yas-lookup-snippet ,template ',mode)))])))
(defsection agda2-mode
  "Types"

  (defun set-agda-input-method ()
    (interactive)
    (set-input-method "Agda"))

  ;; push marker stack before jumping to definition
  (advice-add 'agda2-goto-definition-keyboard :before 'xref-push-marker-stack)

  (map! :map agda2-mode-map
        :n "C-]" 'agda2-goto-definition-keyboard)

  (advice-add 'agda2-next-goal :after (lambda () (agda2-show-goal-if-visible nil)))
  (advice-add 'agda2-previous-goal :after (lambda () (agda2-show-goal-if-visible nil)))

  (map! :map agda2-mode-map
        :i "?" 'agda-insert-?
        :n "C-j" 'agda2-next-goal
        :n "C-k" 'agda2-previous-goal)

  (defsection utilities
    "General utilities for agda-mode."
    (defvar agda2-after-load-hook nil
      "Hook called after agda finished loading.")

    (defvar agda2-before-load-hook nil
      "Hook called before agda starts loading.")

    ;; the before hook is easy...
    (advice-add 'agda2-load :before (lambda () (run-hooks 'agda2-before-load-hook)))

    ;; ...so that we can go to the next goal after reloaded
    (defun agda2-watch-highlight-progress (_sym busy _op _where)
      (unless busy
        (run-hooks 'agda2-after-load-hook)))

    ;; hacky way to execute a "callback" after agda's done.
    (add-variable-watcher 'agda2-highlight-in-progress 'agda2-watch-highlight-progress)

    (defun agda2-info-buffer-window ()
      "Return the window of the info buffer (if visible)."
      (get-buffer-window agda2-info-buffer))
    )

  (defsection helper-fun
    "Help with defining helpers."
    (defun foo ()
      (interactive)
      (agda2-helper-function-type nil)
      (with-current-buffer agda2-info-buffer
        (sleep-for 0 10)
        (copy-region-as-kill (point-min) (point-max)))))
  (defsection goals
    "Goals."

    (define-minor-mode agda2-goal-minor-mode
      "Minor mode for working with agda goals."
      :keymap (make-sparse-keymap))

    (defun agda2-goal-minor-mode-on ()
      (unless agda2-goal-minor-mode
        (agda2-goal-minor-mode 1)))

    (defun agda2-goal-minor-mode-off ()
      (when agda2-goal-minor-mode
        (agda2-goal-minor-mode -1)))

    (defun agda-busy ()
      (or
       (bound-and-true-p agda2-in-progress)
       (bound-and-true-p agda2-highlight-in-progress)))

    (defun agda2-detect-goal ()
      (when (featurep 'agda2-mode)
        (unless (agda-busy)
          (if (agda2-goal-at (point))
              (agda2-goal-minor-mode-on)
            (agda2-goal-minor-mode-off)))))

    (after! agda2-mode
      (add-hook 'agda2-mode-hook
                (lambda () (add-hook 'post-command-hook 'agda2-detect-goal nil t)))
      (add-hook 'agda2-after-load-hook
                (lambda () 'agda2-detect-goal)))

    (defun agda2-show-goal-if-visible (arg)
      (when (agda2-info-buffer-window)
        (agda2-goal-and-context arg)))

    (add-hook 'agda2-goal-minor-mode-hook
              (lambda () (when agda2-goal-minor-mode
                           (agda2-show-goal-if-visible nil))))

    (setq agda2-fontset-name t)
    (map! :map agda2-goal-minor-mode-map
          :i "C-u" 'universal-argument
          :i "C-j" 'agda2-next-goal
          :i "C-k" 'agda2-previous-goal
          :ni "C-n" (lambda () (interactive) (agda2-goal-and-context 1))
          :ni "C-," 'agda2-goal-and-context)
    )

  (defsection clever-?
    "Reload when a ? is inserted."

    (defun agda-in-comment ()
      "Work out if we're in a comment. In that case, inserting ? is fine."
      (nth 4 (syntax-ppss)))

    (defun agda2-load-or-refine ()
      (if agda2-goal-minor-mode
          (agda2-refine nil)
        (agda2-load)))

    (defun agda-insert-? ()
      "Insert ? and reload."
      (interactive)
      (insert "?")
      (let ((thing (thing-at-point 'word)))
        (when (and (not (agda-in-comment))
                   (equal thing "?"))
          (setq agda-should-jump-to-next-goal t)
          (agda2-load-or-refine))))

    (advice-add 'agda2-refine :before (lambda (pmlambda) (setq agda-should-jump-to-next-goal t)))

    (defvar agda-should-jump-to-next-goal nil
      "Do we want to jump to the next goal?")
    (defvar agda-point-before-load nil
      "Point before (re)loading agda.")

    (add-hook 'agda2-before-load-hook
              (lambda () (setq agda-point-before-load (point))))

    (add-hook 'agda2-after-load-hook
              (lambda ()
                (when (and
                       ;; but only if we haven't moved since loading
                       (equal (+ 1 (point)) agda-point-before-load)
                       agda-should-jump-to-next-goal)
                  (setq agda-should-jump-to-next-goal nil)
                  (agda2-next-goal)))))
  (defsection minibuffer
    "Make sure that the minibuffer is using agda input method"

    (defun remove-agda-input-method ()
      (remove-hook 'minibuffer-setup-hook 'set-agda-input-method)
      (remove-hook 'minibuffer-exit-hook 'remove-agda-input-method))

    (defun set-agda-input-method-once ()
      (add-hook 'minibuffer-setup-hook 'set-agda-input-method)
      (add-hook 'minibuffer-exit-hook 'remove-agda-input-method))

    (advice-add 'agda2-compute-normalised-maybe-toplevel :before 'set-agda-input-method-once))

  (defsection org
    "Writing agda in org mode. What could go wrong?"
    (add-hook 'polymode-init-inner-hook
              #'evil-normalize-keymaps)
    (add-hook 'polymode-init-inner-hook
              'evil-normal-state)
    (add-hook 'polymode-after-switch-buffer-hook
              'polymode-switch-buffer-keep-evil-state-maybe)
    (use-package! org-agda-mode)))

(defsection art
  "Temporary stuff for art."


  (add-to-list 'auto-mode-alist '("\\.art\\'" . art-mode))

  (defvar art-keywords nil
    "Art language keywords.")

  (setq art-keywords
        '("with"
          "if"
          "else"
          "then"
          "when"))
  (setq art-macros
        (-map (lambda (b) (concat "\\<" (s-snake-case b) "\\>"))
              '("Try"
                "ListOverContext"
                "TableExtractList"
                "TableExtractFirst"
                "EvalOverList"
                "ListKeyArgs"
                "Missing")))
  (setq art-builtins
        (-map (lambda (b) (concat "\\<" (s-snake-case b) "\\>"))
              '("ColumnList"
                "VerticalLookupList"
                "VerticalLookupFirst"
                "RemoveDuplicates"
                "Get"
                "AmountAtCurrency"
                "LinearInterpolateTableColumns"
                "SumList"
                "MinList"
                "OrList"
                "LaxParseBool"
                "YesNoBool"
                "Months"
                "Years"
                "Days"
                "NumYears"
                "Period"
                "Min"
                "Max"
                "FlooredAt"
                "CappedAt"
                "ChangeCurrency"
                "Not"
                "Or"
                "FindClosest"
                "Abs"
                "Round2"
                "Elem"
                "Length")))

  (setq art-font-lock
        `(("//.*$" . font-lock-comment-face)
          ("\"\\(\\(\\\\\"\\)\\|[^\"]\\)*\"" . font-lock-string-face)
          (,(s-join "\\|" art-keywords) . font-lock-keyword-face)
          (,(s-join "\\|" art-macros) . font-lock-constant-face)
          (,(s-join "\\|" art-builtins) . font-lock-builtin-face)
          ("\\<\\*?\\[?[A-Z][a-z]*\\]?\\>" . font-lock-type-face)
          ("\\w\\(#\\w+\\)" . (1 font-lock-builtin-face))))

  (define-derived-mode art-mode prog-mode "Art"
    (setq-local font-lock-defaults '(art-font-lock t nil nil))
    (setq-local comment-start "// ")
    (setq-local comment-start-skip "// ")
    (setq-local comment-end "")))
(defsection config-file
  "Setup in _this_ file!"

  (after! dash
    (dash-enable-font-lock))

  (defvar cs/config-file
    (concat doom-private-dir "config.el"))

  (defun cs/open-config ()
    (interactive)
    (find-file cs/config-file)
    (widen))

  (defun cs/setup-config ()
    (outline-hide-body)
    (cs/set-counsel-outline-regexp-to-local)
    (map! :map local
          :n "<tab>" 'outline-show-entry
          :n "<C-tab>" 'outline-hide-entry
          :n "C-n" '(lambda () (interactive) (goto-char (cs/next-section)))
          :n "C-p" '(lambda () (interactive) (goto-char (cs/previous-section)))))

  (defun cs/get-current-line ()
    (require 's)
    (s-trim (substring-no-properties (thing-at-point 'line))))

  (defsection sections
    "Managing sections in my config file."

    (defun cs/set-counsel-outline-regexp-to-local ()
      (interactive)
      (when (--find (equal (car it) 'outline-regexp) file-local-variables-alist)
        (setq-local counsel-outline-settings
                    (--map-first
                     (equal (car it) major-mode)
                     (cons (car it) (let ((props (-clone (cdr it))))
                                      (plist-put! props
                                                  :outline-regexp outline-regexp
                                                  :outline-level 'cs/current-section-level)
                                      props))
                     counsel-outline-settings))))

    (defun cs/next-section ()
      (interactive)
      (let ((count (if (looking-at outline-regexp) 2 1)))
        (when (re-search-forward outline-regexp nil t count)
          ;;
          (match-beginning 2))))

    (defun cs/previous-section ()
      (interactive)
      (when (re-search-backward outline-regexp nil t)
        (match-beginning 2)))

    (defun cs/current-section-level ()
      (if (looking-at outline-regexp)
          (/ (- (match-end 1) (match-beginning 1)) 2)
        0))

    (defun cs/goto-major-mode-config ()
      (interactive)
      (let ((section major-mode))
        (cs/open-config)
        (goto-char (point-min))
        (re-search-forward (concat outline-regexp (format "%s" section)))
        (narrow-to-defun)
        (outline-show-entry)))

    (defun cs/counsel-config-section ()
      (interactive)
      (cs/open-config)
      (counsel-outline)
      (narrow-to-defun)
      (outline-hide-body)
      (outline-show-entry))))

;; Local Variables:
;; outline-regexp: "^\\([ ]*\\)\\((defsection \\)"
;; eval: (cs/setup-config)
;; End:
