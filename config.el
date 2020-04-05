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


  (map! :n "C-x C-j" 'dired-jump
        :n "C-u" 'universal-argument)

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
    (defsection maps
      "Evil key bindings"
      (map! :n "C-g" 'cs/show-file-info)
      (map! :leader
            "j j" 'cs/counsel-config-section
            "j m" 'cs/goto-major-mode-config)
      (map! :desc "Rename buffer file" :n "SPC b R" 'rename-current-buffer-file)
      (evil-ex-define-cmd "bd" 'kill-this-buffer))
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
  (defsection theme
    "Colours! (not too many)"

    (setq doom-font (font-spec :family "iA Writer Mono S" :size 12 :weight 'regular)
          doom-variable-pitch-font (font-spec :family "iA Writer Duospace" :size 12 :weight 'regular))


    (defvar theme-path "~/.emacs.d/themes/")
    (add-to-list 'load-path theme-path)
    (add-to-list 'custom-theme-load-path theme-path)

    (setq dark-theme 'monochrome)
    (setq light-theme 'monochrome-light)

    (theme-setup)

    (map! :n "SPC t d" 'toggle-dark-mode)

    (defsection modeline
      "Doom modeline settings"
      (setq doom-modeline-buffer-file-name-style 'relative-from-project)
      (setq doom-modeline-icon nil)
      (setq doom-modeline-buffer-state-icon nil)
      (setq doom-modeline-major-mode-icon nil)
      (setq doom-modeline-buffer-encoding nil)
      (setq doom-modeline-indent-info nil)
      (setq doom-modeline-github nil)
      (setq doom-modeline-modal-icon nil)
      (setq doom-modeline-height 20))))

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

    (add-hook 'find-file-hook 'do-highlights)

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
      (setq lsp-ui-sideline-enable nil))))
(defsection org
  "Org mode"
  (after! org
    (add-to-list 'org-modules 'org-habit t)
    (setq org-directory "~/org/"
          org-archive-location (concat org-directory "archive/%s::")
          org-ellipsis " ▼ "
          org-bullets-bullet-list '("•")
          org-agenda-span 14
          org-agenda-start-on-weekday 1
          org-agenda-start-day "today"
          )))

(defsection eshell
  "Eshell"
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
            )))

  (defun cs/zsh-history ()
    (interactive)
    (let ((command (with-temp-buffer
                     (insert-file-contents-literally "~/.histfile")
                     (let ((history-list (delete-dups (split-string (buffer-string) "\n" t))))
                       (ivy-read "Command: " history-list)))))
      (when comman
        (insert command))))

  (add-hook 'eshell-mode-hook
            (lambda () (map! :map eshell-mode-map :i "C-r" '+eshell/search-history))))

(defsection ivy
  "Ivy settings"
 
  (after! ivy
    (map! :map ivy-minibuffer-map
          "<escape>" 'hydra-ivy/body)
    (map! :n "C-s" 'counsel-grep-or-swiper)))

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
      :innermodes '(poly-haskell-innermode poly-spec-innermode))))

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

  (after! paredit
    (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
    (add-hook 'haskell-mode-hook 'enable-paredit-mode)
    (add-hook 'agda2-mode-hook 'enable-paredit-mode)
    (add-hook 'paredit-mode-hook 'evil-paredit-mode)))

(defsection haskell-mode
  "Haskell stuff."

  (remove-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (remove-hook 'haskell-mode-hook 'structured-haskell-mode)

  (after! flycheck
    (setq-default flycheck-disabled-checkers '(haskell-ghc haskell-hlint haskell-stack-ghc)))

  (after! 'structured-haskell-mode
   (define-key shm-map (kbd "M-e") 'shm/goto-parent-end)))

(defsection refactoring
  "Tools for project navigation and refactoring."

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

    (map! :map deadgrep-mode-map
          "<return>" 'cs/deadgrep-visit-result-push-mark
          :desc "Search tag" "C-c t" 'cs/deadgrep-search-tag
          :desc "Search" "C-c c" 'cs/deadgrep-change-term
          :desc "Change type" "C-c r" 'cs/deadgrep-search-type
          :desc "Change directory" "C-c d" 'cs/deadgrep-change-directory
          :n "C-j" 'deadgrep-forward-match
          :n "C-k" 'deadgrep-backward-match
          :desc "Change context" "C-c -" 'cs/deadgrep-change-context))

    (map! :prefix-map ("C-c r" . "refactoring")
          "d" 'cs/deadgrep-new-search))

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

  (advice-add 'agda2-next-goal :after (lambda () (agda2-goal-and-context nil)))
  (advice-add 'agda2-previous-goal :after (lambda () (agda2-goal-and-context nil)))

  (map! :map agda2-mode-map
        :i "?" 'agda-insert-?
        :n "C-j" 'agda2-next-goal
        :n "C-k" 'agda2-previous-goal)

  (defsection goals
    "Goals."

    (agda-goal-map :i "C-u" universal-argument)
    (agda-goal-map :i "C-j" agda2-next-goal)
    (agda-goal-map :i "C-k" agda2-previous-goal)
    (agda-goal-map :n "C-n" (lambda () (agda2-goal-and-context 1)))
    (agda-goal-map :n "C-," (lambda () (agda2-goal-and-context nil)))


    (defmacro agda-goal-map (mode key action)
      "Set up a mapping that only applied when"
      `(map! :map agda2-mode-map ,mode ,key
             (lambda ()
               (interactive)
               ;; TODO: pass through normally, not to insert moed
               (let ((orig-action (lookup-key evil-insert-state-map (kbd ,key))))
                 (if (agda2-goal-at (point))
                     (,action)
                   (call-interactively orig-action))))))
    )

  (defsection clever-?
    "Reload when a ? is inserted."

    (defun agda-in-comment ()
      "Work out if we're in a comment. In that case, inserting ? is fine."
      (nth 4 (syntax-ppss)))

    (defun agda-insert-? ()
      "Insert ? and reload."
      (interactive)
      (insert "?")
      (let ((thing (thing-at-point 'word)))
        (when (and (not (agda-in-comment))
                   (equal thing "?"))
          (agda2-load))))

    (defvar agda-point-before-load nil
      "Point before (re)loading agda.")

    ;; Remember where we were before loading
    (advice-add 'agda2-load :before (lambda () (setq agda-point-before-load (point))))

    ;; ...so that we can go to the next goal after reloaded
    (defun agda-goto-next-goal-after-load (_sym newval _op _where)
      (when (and (equal newval nil) ; no longer busy
                 ;; but only if we haven't moved since loading
                 (equal (+ 1 (point)) agda-point-before-load))
        (agda2-next-goal)))

    ;; hacky way to execute a "callback" after agda's done.
    (add-variable-watcher 'agda2-highlight-in-progress 'agda-goto-next-goal-after-load))
  (defsection minibuffer
    "Make sure that the minibuffer is using agda input method"

    (defun remove-agda-input-method ()
      (remove-hook 'minibuffer-setup-hook 'set-agda-input-method)
      (remove-hook 'minibuffer-exit-hook 'remove-agda-input-method))

    (defun set-agda-input-method-once ()
      (add-hook 'minibuffer-setup-hook 'set-agda-input-method)
      (add-hook 'minibuffer-exit-hook 'remove-agda-input-method))

    (advice-add 'agda2-compute-normalised-maybe-toplevel :before 'set-agda-input-method-once)))

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
