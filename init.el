(require 'package)

(setq package-archives
  '(("melpa" . "https://melpa.org/packages/")
    ("org" . "https://orgmode.org/elpa/")
    ("gnu" . "https://elpa.gnu.org/packages/")))

(unless package-archive-contents
  (package-refresh-contents))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(defun gq/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
		      (expand-file-name "~/.emacs.d/emacs-config.org"))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook 'gq/org-babel-tangle-config)))

(package-initialize)
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; (unless (package-installed-p 'quelpa)
;;   (with-temp-buffer
;;     (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
;;     (eval-buffer)
;;     (quelpa-self-upgrade)))

(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
	   (format "%.2f seconds"
		   (float-time
		   (time-subtract after-init-time before-init-time)))
	   gcs-done))

(add-hook 'emacs-startup-hook 'efs/display-startup-time)

(use-package no-littering)

(setq inhibit-startup-message t)

(scroll-bar-mode -1)

(tool-bar-mode -1)

;; (menu-bar-mode -1)

(set-fringe-mode 10)

(toggle-frame-maximized)

(set-frame-parameter (selected-frame) 'alpha '(90 . 50))

;; (setq visible-bell t)

(setq make-backup-files nil)

(use-package general
  :config
  (general-define-key
   "M-x" 'counsel-M-x
   "C-s" 'counsel-grep-or-swiper)

  ;; * Mode Keybindings
  ;; `general-define-key' is comparable to `define-key' when :keymaps is specified
  (general-define-key
   ;; NOTE: keymaps specified with :keymaps must be quoted
   :keymaps 'org-mode-map
   "<home>" 'beginning-of-line
   "<end>" 'end-of-line
   "<escape>" 'keyboard-escape-quit
   "C-M-j" 'counsel-switch-buffer
   )
  ;; `general-def' can be used instead for `define-key'-like syntax
  (general-def org-mode-map
               "C-c C-q" 'counsel-org-tag
               )

  ;; * Prefix Keybindings
  ;; :prefix can be used to prevent redundant specification of prefix keys
  (general-define-key
   :prefix "C-c"
   ;; bind "C-c a" to 'org-agenda
   "a" 'org-agenda
   "b" 'counsel-bookmark
   "c" 'org-capture)
  )

(setenv "LANG" "en_US.UTF-8")
(setenv "LC_ALL" "en_US.UTF-8")
(setenv "LC_CTYPE" "en_US.UTF-8")

;; (setq url-proxy-services '(("http" . "127.0.0.1:7890")
;; 			   ("https" . "127.0.0.1:7890")))

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-gruvbox-light t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package kaolin-themes
  :config
  ;;  (load-theme 'kaolin-dark t)
  ;;  (kaolin-treemacs-theme)
  )

(use-package base16-theme
  :config
  ;;  (load-theme 'base16-default-dark t)
  )

(use-package modus-themes
  :ensure
  :init
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-slanted-constructs t
        modus-themes-bold-constructs nil)

  ;; Load the theme files before enabling a theme
  (modus-themes-load-themes)
  :config
  ;; Load the theme of your choice:
  :bind ("<f5>" . modus-themes-toggle))

(use-package sublime-themes)

(column-number-mode)
(global-display-line-numbers-mode t)
(dolist
    (mode '(org-mode-hook
	    term-mode-hook
	    eshell-mode-hook
	    treemacs-mode-hook
	    org-agenda-mode-hook
	    ))
  (add-hook mode
	    (lambda () (display-line-numbers-mode 0))
	    )
  )

(use-package all-the-icons
  :after cnfonts
  :if (display-graphic-p)
  :hook (dired-mode . all-the-icons-dired-mode)
  )

(use-package all-the-icons-ivy
  :init (add-hook 'after-init-hook 'all-the-icons-ivy-setup)
  :after (all-the-icons ivy)
  )

(use-package all-the-icons-ivy-rich
  :init (all-the-icons-ivy-rich-mode 1)
  :after (all-the-icons ivy-rich)
  )

(use-package all-the-icons-ibuffer
   :init (all-the-icons-ibuffer-mode 1)
   :after (all-the-icons ibuffer)
   )

(use-package all-the-icons-dired
  :after (all-the-icons)
  :if (display-graphic-p)
  :hook (dired-mode . all-the-icons-dired-mode)
  )

(use-package doom-modeline
  :init
  (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 25)
  (setq doom-modeline-bar-width 3)
  (setq doom-modeline-window-width-limit fill-column)
  (setq doom-modeline-project-detection 'project)
  (setq doom-modeline-buffer-file-name-style 'auto)
  (setq doom-modeline-icon (display-graphic-p))
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-major-mode-color-icon t)
  (setq doom-modeline-buffer-state-icon t)
  (setq doom-modeline-buffer-modification-icon t)
  (setq doom-modeline-unicode-fallback t)
  (setq doom-modeline-minor-modes nil)
  (setq doom-modeline-enable-word-count t)
  (setq doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode))
  (setq doom-modeline-buffer-encoding t)
  (setq doom-modeline-indent-info nil)
  (setq doom-modeline-checker-simple-format t)
  (setq doom-modeline-number-limit 99)
  (setq doom-modeline-vcs-max-length 12)
  (setq doom-modeline-workspace-name t)
  (setq doom-modeline-persp-name t)
  (setq doom-modeline-display-default-persp-name nil)
  (setq doom-modeline-persp-icon t)
  (setq doom-modeline-lsp t)
  (setq doom-modeline-github t)
  (setq doom-modeline-github-interval (* 30 60))
  (setq doom-modeline-modal-icon t)
  (setq doom-modeline-mu4e t)
  (setq doom-modeline-gnus t)
  (setq doom-modeline-gnus-timer 2)
  (setq doom-modeline-gnus-excluded-groups '("dummy.group"))
  (setq doom-modeline-irc t)
  (setq doom-modeline-irc-stylize 'identity)
  (setq doom-modeline-env-version t)
  (setq doom-modeline-env-enable-python t)
  (setq doom-modeline-env-enable-ruby t)
  (setq doom-modeline-env-enable-perl t)
  (setq doom-modeline-env-enable-go t)
  (setq doom-modeline-env-enable-elixir t)
  (setq doom-modeline-env-enable-rust t)
  (setq doom-modeline-env-python-executable "python") ; or `python-shell-interpreter'
  (setq doom-modeline-env-ruby-executable "ruby")
  (setq doom-modeline-env-perl-executable "perl")
  (setq doom-modeline-env-go-executable "go")
  (setq doom-modeline-env-elixir-executable "iex")
  (setq doom-modeline-env-rust-executable "rustc")
  (setq doom-modeline-env-load-string "...")
  (setq doom-modeline-before-update-env-hook nil)
  (setq doom-modeline-after-update-env-hook nil)  
  )

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  (setq dashboard-banner-logo-title "笔耕不辍, 静水流深        ")
  (setq dashboard-startup-banner "~/.emacs.d/g.png")
  (setq dashboard-center-content t)
  (setq dashboard-set-footer nil)
  (setq dashboard-items '((recents  . 5)
			  (bookmarks . 5)
			  (projects . 5)
			  (agenda . 5)
			  (registers . 5)))
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-navigator t)
  (setq dashboard-set-init-info t)
  )

(use-package cnfonts
  :init
  (cnfonts-enable)
  (cnfonts-set-spacemacs-fallback-fonts)
  (setq cnfonts-profiles
	'("program" "org-mode" "read-book"))
  (setq cnfonts-personal-fontnames '(("Iosevka Comfy")
				     ()
				     ()))
  :bind
  (("C-=" . cnfonts-increase-fontsize)
   ("C--" . cnfonts-decrease-fontsize)
   )
  )

(use-package highlight-indent-guides
  :config (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))

(use-package treemacs
  :defer t
  :init (with-eval-after-load 'winum
	  (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
	  treemacs-deferred-git-apply-delay      0.5
	  treemacs-directory-name-transformer    #'identity
	  treemacs-display-in-side-window        t
	  treemacs-eldoc-display                 t
	  treemacs-file-event-delay              5000
	  treemacs-file-extension-regex          treemacs-last-period-regex-value
	  treemacs-file-follow-delay             0.2
	  treemacs-file-name-transformer         #'identity
	  treemacs-follow-after-init             t
	  treemacs-git-command-pipe              ""
	  treemacs-goto-tag-strategy             'refetch-index
	  treemacs-indentation                   2
	  treemacs-indentation-string            " "
	  treemacs-is-never-other-window         nil
	  treemacs-max-git-entries               5000
	  treemacs-missing-project-action        'ask
	  treemacs-move-forward-on-expand        nil
	  treemacs-no-png-images                 nil
	  treemacs-no-delete-other-windows       t
	  treemacs-project-follow-cleanup        nil
	  treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
	  treemacs-position                      'left
	  treemacs-read-string-input             'from-child-frame
	  treemacs-recenter-distance             0.1
	  treemacs-recenter-after-file-follow    nil
	  treemacs-recenter-after-tag-follow     nil
	  treemacs-recenter-after-project-jump   'always
	  treemacs-recenter-after-project-expand 'on-distance
	  treemacs-show-cursor                   nil
	  treemacs-show-hidden-files             nil
	  treemacs-silent-filewatch              nil
	  treemacs-silent-refresh                nil
	  treemacs-sorting                       'alphabetic-asc
	  treemacs-space-between-root-nodes      t
	  treemacs-tag-follow-cleanup            t
	  treemacs-tag-follow-delay              1.5
	  treemacs-user-mode-line-format         nil
	  treemacs-user-header-line-format       nil
	  treemacs-width                         35
	  treemacs-workspace-switch-cleanup      nil)
    (treemacs-resize-icons 18)
    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
		 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
	("M-0"       . treemacs-select-window)
	("C-x t 1"   . treemacs-delete-other-windows)
	("C-x t t"   . treemacs)
	("C-x t B"   . treemacs-bookmark)
	("C-x t C-t" . treemacs-find-file)
	("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :after treemacs projectile)

(use-package treemacs-magit
  :after treemacs magit)

(use-package centaur-tabs
  :load-path "~/.emacs.d/other/centaur-tabs"
  :config
  (setq centaur-tabs-style "bar"
	centaur-tabs-height 32
	centaur-tabs-set-icons t
	centaur-tabs-set-modified-marker t
	centaur-tabs-show-navigation-buttons t
	centaur-tabs-set-bar 'under
	x-underline-at-descent-line t)
  (centaur-tabs-headline-match)

  (setq centaur-tabs-gray-out-icons 'buffer)
  (centaur-tabs-enable-buffer-reordering)
  (setq centaur-tabs-adjust-buffer-order t)
  (centaur-tabs-mode t)
  (setq uniquify-separator "/")
  (setq uniquify-buffer-name-style 'forward)
  :hook
  (dashboard-mode . centaur-tabs-local-mode)
  (term-mode . centaur-tabs-local-mode)
  (calendar-mode . centaur-tabs-local-mode)
  (org-agenda-mode . centaur-tabs-local-mode)
  (helpful-mode . centaur-tabs-local-mode)
  :bind
  ("C-<prior>" . centaur-tabs-backward)
  ("C-<next>" . centaur-tabs-forward)
  ("C-c t s" . centaur-tabs-counsel-switch-group)
  ("C-c t p" . centaur-tabs-group-by-projectile-project)
  ("C-c t g" . centaur-tabs-group-buffer-groups)
  )

(global-set-key (kbd "C-x C-b") 'ibuffer)
(use-package ibuffer-vc)

(setq dired-guess-shell-alist-user '(("\\.pdf\\'" "open")
				     ("\\.doc\\'" "open")
				     ("\\.docx\\'" "open")
				     ("\\.ppt\\'" "open")
				     ("\\.pptx\\'" "open")
				     ("\\.xls\\'" "open")))
(add-hook 'dired-load-hook
	  (lambda ()
	    (setq dired-x-hands-off-my-keys nil)
	    (load "dired-x")
	    ;; Set dired-x global variables here.  For example:
	    ;; (setq dired-guess-shell-gnutar "gtar")
	    ))
(add-hook 'dired-mode-hook
	  (lambda ()
	    ;; Set dired-x buffer-local variables here.  For example:
	    ;; (dired-omit-mode 1)
	    ))

(use-package ranger)

(defun gq/org-mode-visual-fill ()
  (setq visual-fill-column-width 120
	visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :defer t
  :hook (org-mode . gq/org-mode-visual-fill))

;; (use-package smex)

(use-package ivy
  :diminish
  :bind
  (("C-s" . swiper)
   :map ivy-minibuffer-map
   ("TAB" . ivy-alt-done)
   ("C-l" . ivy-alt-done)
   ("C-j" . ivy-next-line)
   ("C-k" . ivy-previous-line)
   :map ivy-switch-buffer-map
   ("C-k" . ivy-previous-line)
   ("C-l" . ivy-done)
   :map ivy-reverse-i-search-map
   ("C-k" . ivy-previous-line)
   ("C-d" . ivy-reverse-i-search-kill)
   )
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :after (counsel ivy)
  :init (ivy-rich-mode 1)
  )

(use-package ivy-bibtex
  :init
  (setq ivy-re-builders-alist
	'((ivy-bibtex . ivy--regex-ignore-order)
	  (t . ivy--regex-plus)))
  (setq bibtex-completion-bibliography
	'("~/SynologyDrive/Library/bib/mybib.bib"))
  (setq bibtex-completion-library-path
	'("~/SynologyDrive/Library/pdf"))
  (setq bibtex-completion-pdf-field "File")
  (setq bibtex-completion-notes-path "~/SynologyDrive/Library/notes")
  (setq bibtex-completion-display-formats
    '((article       . "${=has-pdf=:1}${=has-note=:1} ${=type=:3} ${year:4} ${author:36} ${title:*} ${journal:40}")
      (inbook        . "${=has-pdf=:1}${=has-note=:1} ${=type=:3} ${year:4} ${author:36} ${title:*} Chapter ${chapter:32}")
      (incollection  . "${=has-pdf=:1}${=has-note=:1} ${=type=:3} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
      (inproceedings . "${=has-pdf=:1}${=has-note=:1} ${=type=:3} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
      (t             . "${=has-pdf=:1}${=has-note=:1} ${=type=:3} ${year:4} ${author:36} ${title:*}")))
  (setq bibtex-completion-pdf-symbol "⌘")
  (setq bibtex-completion-notes-symbol "✎")
  (setq bibtex-completion-pdf-extension '(".pdf" ".djvu"))
  (setq bibtex-completion-format-citation-functions
	'((org-mode      . bibtex-completion-format-citation-org-link-to-PDF)
	  (latex-mode    . bibtex-completion-format-citation-cite)
	  (markdown-mode . bibtex-completion-format-citation-pandoc-citeproc)
	  (default       . bibtex-completion-format-citation-default)))  
  (defun ivy-bibtex-my-publications (&optional arg)
    "Search BibTeX entries authored by “Jane Doe”.

With a prefix ARG, the cache is invalidated and the bibliography reread."
    (interactive "P")
    (when arg
      (bibtex-completion-clear-cache))
    (bibtex-completion-init)
    (ivy-read "BibTeX Items: "
	      (bibtex-completion-candidates)
	      :initial-input "Jane Doe"
	      :caller 'ivy-bibtex
	      :action ivy-bibtex-default-action))

  ;; Bind this search function to Ctrl-x p:
  :bind (("C-x p" . ivy-bibtex-my-publications))
  )

(use-package counsel
  :bind
  (("M-x" . counsel-M-x)
   ("C-x b" . counsel-ibuffer)
   ("C-x C-f" . counsel-find-file)
   :map minibuffer-local-map
   ("C-r" . 'counsel-minibuffer-history))
  ;; Don't start with ^
  :config
  (setq ivy-initial-inputs-alist nil)
  :init
  (setq counsel-find-file-ignore-regexp
	(concat "\\(?:\\`[#.]\\)" "\\|\\(?:\\`.+?[#~]\\'\\)")))

(use-package which-key
  :init (which-key-mode)
  :diminish
  :config (setq which-key-idle-delay 0))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package projectile
  :diminish projectile
  :config
  (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/projects")
    (setq projectile-project-search-path '("~/projects")))
  (setq projectile-switch-project-action #'projectile-dired)
  )

(use-package counsel-projectile
  :config
  (counsel-projectile-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook ((python-mode . lsp)
	 (lsp-mode . lsp-enable-which-key-integration))
  :init (setq lsp-keymap-prefix "C-c l"))

(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol)

(use-package lsp-ui
  :custom
  (lsp-ui-imenu-enable t)

  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-show-symbol t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-sideline-show-diagnostics nil)
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-sideline-code-actions-prefix "")

  (lsp-ui-peek-enable t)
  (lsp-ui-peek-peek-height 20)
  (lsp-ui-peek-list-width 50)
  (lsp-ui-peek-fontify 'on-demand)

  (lsp-ui-doc-enable nil)
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-doc-max-width 400)
  (lsp-ui-doc-max-height 20)
  (lsp-ui-doc-use-webkit t)
  :preface
  (defun ladicle/toggle-lsp-ui-doc ()
    (interactive)
    (if lsp-ui-doc-mode
	(progn
	  (lsp-ui-doc-mode -1)
	  (lsp-ui-doc--hide-frame))
      (lsp-ui-doc-mode 1)))
  :bind
  (:map lsp-mode-map
	("C-c C-r" . lsp-ui-peek-find-references)
	("C-c C-j" . lsp-ui-peek-find-definitions)
	("C-c i" . lsp-ui-peek-find-definitions)
	("C-c m" . lsp-ui-imenu)
	("C-c s" . lsp-ui-sideline-mode)
	("C-c d"   . ladicle/toggle-lsp-ui-doc))
  :hook
  (lsp-mode . lsp-ui-mode))

(use-package company)
(add-hook 'after-init-hook 'global-company-mode)

(use-package company-lsp
  :config
  (push 'company-lsp company-backends))

(use-package yasnippet
  :init
  (yas-global-mode 1))

(use-package yasnippet-snippets)

(use-package dap-mode
  :bind
  (("<f7>" . dap-step-in)
   ("<M-f7>" . dap-step-out)
   ("<f8>" . dap-next)
   ("<f9>" . dap-continue)
   ("<f5>" . dap-debug))
  :hook
  ((python-mode . dap-mode)
   (python-mode . dap-ui-mode))
  :config
  (require 'dap-python)  
  (setq dap-auto-configure-features '(sessions breakpoints locals controls tooltip repl))
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (tooltip-mode 1)
  (dap-ui-controls-mode 1))

(use-package magit
  :commands
  (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :bind
  ("C-x g" . magit-status)
  )

(setq org-support-shift-select 1)

(defun insert-zero-width-space () (interactive) (insert-char #x200b))
(defun my-latex-filter-zws (text backend info)
  (when (org-export-derived-backend-p backend 'latex)
    (replace-regexp-in-string "\x200B" "{}" text)))
(global-set-key (kbd "C-*") 'insert-zero-width-space)

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("el" . "src elisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("sh" . "src shell"))

(defface org-checkbox-done-text
  '((t (:foreground "#71696A" :strike-through t)))
  "Face for the text part of a checked org-mode checkbox.")
(font-lock-add-keywords
 'org-mode
 `(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)"
    1 'org-checkbox-done-text prepend)) 'append)

(add-hook 'org-mode-hook (lambda ()
			   "Beautify Org Checkbox Symbol"
			   (push '("[ ]" . "☐") prettify-symbols-alist)
			   (push '("[X]" . "☑") prettify-symbols-alist)
			   (push '("[-]" . "❍") prettify-symbols-alist)
			   (prettify-symbols-mode)))

(setq org-ellipsis " ↴ ")

(setq org-src-fontify-natively t)

(setq org-hide-emphasis-markers t)

(setq org-file-apps
      '(("\\.pdf\\'" . (lambda (file link) (org-pdftools-open link)))
	(directory . emacs)
	(auto-mode . emacs)
	("\\.mm\\'" . default)
	("\\.x?html?\\'" . default)
	("\\.pptx?\\'" . system)
	("\\.docx?\\'" . system)
	("\\.xlsx?\\'" . system)
	("\\.png?\\'" . system)))

(setq org-todo-keywords
      '((sequence "TODO" "NEXT" "|" "DONE" "CANCEL")))
(setq org-todo-keyword-faces
  '(("TODO" . (:foreground "red" :weight bold))
    ("NEXT" . "#E35DBF")
    ("DONE" . (:foreground "#1AA260" :weight bold))
    ("CANCEL" . (:foreground "#888888")))
  )

(custom-theme-set-faces
 'user
 '(variable-pitch ((t (:family "CMU Typewriter Text"))))
 '(fixed-pitch ((t ( :family "Fira Code" :height 160))))
 '(org-block ((t (:inherit fixed-pitch))))
 '(org-code ((t (:inherit (shadow fixed-pitch)))))
 '(org-document-info ((t (:foreground "dark orange"))))
 '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
 '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
 '(org-link ((t (:foreground "royal blue" :underline t))))
 '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-property-value ((t (:inherit fixed-pitch))) t)
 '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
 '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
 '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))

(setq org-agenda-start-with-log-mode t)
(setq org-log-done 'time)
(setq org-log-into-drawer t)

(setq org-tag-alist
      '((:startgroup)
	(:endgroup)
	("outlook" . ?o)
	("gmail" . ?g)
	)
      )

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)))
(setq org-confirm-babel-evaluate nil)

(use-package org-pomodoro
  :commands org-pomodoro
  )

(use-package org-pretty-tags
  :after orgmode
  )

(setq org-capture-templates
      '(
	("n" "Next" entry (file+headline "~/SynologyDrive/org/todo.org" "Inbox")
	 "* TODO %?\n SCHEDULED: %T \n %i\n  %a")
	("t" "Todo" entry (file+headline "~/SynologyDrive/org/todo.org" "Inbox")
	 "* TODO %?\n SCHEDULED: %^T\n")
	("f" "Todo with File" entry (file+headline "~/SynologyDrive/org/todo.org" "Inbox")
	 "* TODO %?\n SCHEDULED: %^T \n %i\n  %a")
	("j" "Journal" entry (file+datetree "~/SynologyDrive/org/journal.org")
	 "* %?\nEntered on %U\n  %i")
	("m" "Email" entry (file+headline "~/SynologyDrive/org/todo.org" "Email")
	 "* TODO %^{待办事项} %^g\n  SCHEDULED: %T DEADLINE: %^T \n  :PROPERTIES:\n  LINK:%i %a\n  :END:\n  %?")
      ))

(use-package org2blog
  :after org
  :config
  (setq org2blog/wp-blog-alist
	'(("myblog"
	   :url "http://galoisgu.com/wordpress/xmlrpc.php"
	   :username "guqun")))
  )

(use-package org-download
  :commands org)

(use-package org-kanban
  :commands org-kanban)

(use-package org-superstar)
(add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))

(use-package org-roam
  :hook (after-init . org-roam-mode)
  :custom (org-roam-directory "~/SynologyDrive/org/roam/")
  :bind
  (:map org-roam-mode-map
	(("C-c n l" . org-roam)
	 ("C-c n f" . org-roam-find-file)
	 ("C-c n g" . org-roam-graph))
	:map org-mode-map
	(("C-c n i" . org-roam-insert)
	 ("C-c n I" . org-roam-insert-immediate))
	))

(use-package org-roam-server
  :after org-roam
  :config
  (setq org-roam-server-host "127.0.0.1"
	org-roam-server-port 8080
	org-roam-server-authenticate nil
	org-roam-server-export-inline-images t
	org-roam-server-serve-files nil
	org-roam-server-served-file-extensions '("pdf" "mp4" "ogv")
	org-roam-server-network-poll t
	org-roam-server-network-arrows nil
	org-roam-server-network-label-truncate t
	org-roam-server-network-label-truncate-length 60
	org-roam-server-network-label-wrap-length 20))

(use-package org-fancy-priorities
  :hook
  (org-mode . org-fancy-priorities-mode)
  :config
  (setq org-fancy-priorities-list
	'((?A . "❗")(?B . "⬆")(?C . "⬇")(?D . "☕")
	  (?1 . "⚡")(?2 . "⮬")(?3 . "⮮")(?4 . "☕")(?I . "Important"))))

(use-package org-pdftools
  :hook (org-mode . org-pdftools-setup-link))

(use-package org-noter-pdftools
  :after org-noter
  :config
  (with-eval-after-load 'pdf-annot
    (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))

(use-package org-super-agenda
  :config
  (setq org-super-agenda-groups
	'((:name "Next Items"
		 :time-grid t
		 :tag ("NEXT" "outbox"))
	  (:name "Important"
		 :priority "A")
	  (:name "Quick Picks"
		 :effort< "0:30")
	  (:priority<= "B"
		       :scheduled future
		       :order 1)))
  )

(use-package org-mime
  :after org
  )

(require 'org-protocol)

(use-package lsp-python-ms
  :init (setq lsp-python-ms-auto-install-server t)
  :hook (python-mode . (lambda () (require 'lsp-python-ms) (lsp))))

(use-package pyenv-mode)

(require 'dap-python)

(add-hook
 'LaTeX-mode-hook
 (lambda ()
   (add-to-list
    'TeX-command-list
    '("XeLaTeX" "%`xelatex --synctex=1%(mode)%' %t" TeX-run-TeX nil t))))

(use-package auctex
  :defer t)

(defun toggle-selective-display (column)
  (interactive "P")
  (set-selective-display
   (or column
       (unless selective-display
	 (1+ (current-column))))))
(defun toggle-hiding (column)
  (interactive "P")
  (if hs-minor-mode
      (if (condition-case nil
	      (hs-toggle-hiding)
	    (error t))
	  (hs-show-all))
    (toggle-selective-display column)))
(global-set-key (kbd "C-+") 'toggle-hiding)
(global-set-key (kbd "C-\\") 'toggle-selective-display)
(add-hook 'prog-mode-hook #'hs-minor-mode)

(setq doc-view-resolution 200)

(use-package calfw
  :config
  (require 'calfw-org)
  (setq cfw:org-overwrite-default-keybinding t))

(use-package powerthesaurus)

(use-package pdf-tools
  :defer t
  :commands (pdf-view-mode pdf-tools-install)
  :mode ("\\.[pP][dD][fF]" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :config (pdf-tools-install :no-query)
  )

(use-package pdf-view-restore
  :after pdf-tools
  :config
  (add-hook 'pdf-view-mode-hook 'pdf-view-restore-mode))

(add-to-list 'load-path "/usr/local/Cellar/mu/1.4.14/share/emacs/site-lisp/mu/mu4e")
      (require 'mu4e)
      (setq mu4e-change-filenames-when-moving t)
      (setq mu4e-contexts
	    (list
	     ;; Gmail account
	     (make-mu4e-context
	      :name "gmail"
	      :vars '((mu4e-maildir . "~/.mail/gmail/")
		      (user-mail-address . "davidguqun@gmail.com")
		      (user-full-name    . "Qun Gu")
		      (mu4e-drafts-folder  . "/gmail/[Gmail]/Drafts")
		      (mu4e-sent-folder  . "/gmail/[Gmail]/Sent Mail")
		      (mu4e-refile-folder  . "/gmail/[Gmail]/All Mail")
		      (mu4e-trash-folder  . "/gmail/[Gmail]/Trash")
		      (mu4e-maildir-shortcuts . 
					      ((:maildir "/gmail/Inbox" :key ?i)
					       (:maildir "/gmail/[Gmail]/Sent Mail" :key ?s)
					       (:maildir "/gmail/[Gmail]/Trash" :key ?d)
					       (:maildir "/gmail/[Gmail]/All Mail" :key ?a)
					       (:maildir "/gmail/[Gmail]/Drafts" :key ?r)
					       (:maildir "/gmail/Edria" :key ?e)
					       )
					      )		
		      )
	      )

	     ;; Outlook account
	     (make-mu4e-context
	      :name "outlook"
	      :vars '((mu4e-maildir . "~/.mail/outlook/")
		      (user-mail-address . "guqun@outlook.com")
		      (user-full-name    . "Qun Gu")
		      (mu4e-drafts-folder  . "/outlook/Drafts")
		      (mu4e-sent-folder  . "/outlook/Sent")
		      (mu4e-refile-folder  . "/outlook/Archive")
		      (mu4e-trash-folder  . "/outlook/Deleted")
		      (mu4e-maildir-shortcuts . 
					      ((:maildir "/outlook/Inbox" :key ?i)
					       (:maildir "/outlook/Sent" :key ?s)
					       (:maildir "/outlook/Deleted" :key ?d)
					       (:maildir "/outlook/Archive" :key ?a)
					       (:maildir "/outlook/Drafts" :key ?r)
					       (:maildir "/outlook/Edria" :key ?e)))
		      )
	      )
	     )
	    )

      (setq mail-user-agent 'mu4e-user-agent)
      (setq mu4e-get-mail-command "mbsync -a")
      (setq mu4e-update-interval 300)             ;; update every 5 minutes
      (setq mu4e-compose-signature
	    (concat
	     "Qun Gu\n"
	     "Sent from Emacs"))
;;      (setq mu4e-use-fancy-chars t)

(use-package mu4e-views
  :after mu4e
  :defer nil
  :bind (:map mu4e-headers-mode-map
	    ("v" . mu4e-views-mu4e-select-view-msg-method) ;; select viewing method
	    ("M-n" . mu4e-views-cursor-msg-view-window-down) ;; from headers window scroll the email view
	    ("M-p" . mu4e-views-cursor-msg-view-window-up) ;; from headers window scroll the email view
	    ("f" . mu4e-views-toggle-auto-view-selected-message) ;; toggle opening messages automatically when moving in the headers view
	    )
  :config
  (setq mu4e-views-completion-method 'ivy) ;; use ivy for completion
  (setq mu4e-views-default-view-method "html") ;; make xwidgets default
  (mu4e-views-mu4e-use-view-msg-method "html") ;; select the default
  (setq mu4e-views-next-previous-message-behaviour 'stick-to-current-window) ;; when pressing n and p stay in the current window
  (setq mu4e-views-auto-view-selected-message t)) ;; automatically open messages when moving in the headers view

(use-package mu4e-alert
  :after mu4e
  :init
  (setq mu4e-alert-interesting-mail-query
     "flag:unread maildir:/Inbox"
     )
  (mu4e-alert-enable-mode-line-display)
  (defun gjstein-refresh-mu4e-alert-mode-line ()
    (interactive)
    (mu4e~proc-kill)
    (mu4e-alert-enable-mode-line-display)
    )
  (run-with-timer 0 60 'gjstein-refresh-mu4e-alert-mode-line)
  )

;; (use-package mu4e-marker-icons
;;   :init (mu4e-marker-icons-mode 1))

(require 'mu4e-org)

(setq smtpmail-default-smtp-server "smtp.office365.com"
      smtpmail-smtp-server "smtp.office365.com"
      smtpmail-smtp-service 587)
