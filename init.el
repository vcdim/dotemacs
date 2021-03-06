(require 'package)

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")
        ("gnu" . "https://elpa.gnu.org/packages/")))

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
(setq use-package-verbose t)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(setq package-enable-at-startup nil)

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

(set-fringe-mode 10)

(toggle-frame-maximized)

(set-frame-parameter (selected-frame) 'alpha '(90 . 50))

(display-battery-mode t)

(menu-bar-mode -1)

(setq make-backup-files nil)

(use-package general
  :config
  (general-define-key
   "M-x" 'counsel-M-x
   "C-s" 'counsel-grep-or-swiper
   "C-x b" 'counsel-switch-buffer
   "C-c C-j" 'org-journal-new-entry
   "C-s-<left>" 'shrink-window-horizontally
   "C-s-<right>" 'enlarge-window-horizontally
   "C-s-<down>" 'shrink-window
   "C-s-<up>" 'enlarge-window
   )

  ;; * Mode Keybindings
  ;; `general-define-key' is comparable to `define-key' when :keymaps is specified
  (general-define-key
   ;; NOTE: keymaps specified with :keymaps must be quoted
   :keymaps 'org-mode-map
   "<home>" 'beginning-of-line
   "<end>" 'end-of-line
   "<escape>" 'keyboard-escape-quit
   )
  ;; `general-def' can be used instead for `define-key'-like syntax
  (general-def org-mode-map
    "C-c C-q" 'counsel-org-tag
    )
  ;; * Prefix Keybindings
  ;; :prefix can be used to prevent redundant specification of prefix keys
  (general-define-key
   :prefix "C-c"
   "a" 'org-agenda-list
   "b" 'counsel-bookmark
   "c" 'org-capture
   )
  )

(setenv "LANG" "en_US.UTF-8")
(setenv "LC_ALL" "en_US.UTF-8")
(setenv "LC_CTYPE" "en_US.UTF-8")

(use-package osx-trash
  :defer t
  :config
  (when (eq system-type 'darwin)
    (osx-trash-setup))
  (setq delete-by-moving-to-trash t)
  )

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(server-start)

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(column-number-mode)
(global-display-line-numbers-mode t)
(dolist
    (mode '(org-mode-hook
            term-mode-hook
            eshell-mode-hook
            vterm-mode-hook
            treemacs-mode-hook
            org-agenda-mode-hook
            pdf-view-mode-hook
            dired-mode-hook
            xwidget-webkit-mode-hook
            mu4e-headers-mode-hook
            ))
  (add-hook mode
            (lambda () (display-line-numbers-mode 0))
            )
  )

(set-default 'truncate-lines t)

(use-package all-the-icons
  :after
  cnfonts
  :if
  (display-graphic-p)
  :hook
  (dired-mode . all-the-icons-dired-mode)
  )

(use-package all-the-icons-ivy
  :after
  (all-the-icons ivy)
  :hook
  (after-init . all-the-icons-ivy-setup)
  )

(use-package all-the-icons-ivy-rich
  :after (all-the-icons ivy-rich)
  :config (all-the-icons-ivy-rich-mode 1)
  )

(use-package all-the-icons-ibuffer
  :defer t
  :after (all-the-icons ibuffer)
  :config (all-the-icons-ibuffer-mode 1)
  )

(use-package all-the-icons-dired
  :diminish
  :if (display-graphic-p)
  :hook (dired-mode . all-the-icons-dired-mode)
  :config
  ;; FIXME: Refresh after creating or renaming the files/directories.
  ;; @see https://github.com/jtbm37/all-the-icons-dired/issues/34.
  (with-no-warnings
    (advice-add #'dired-do-create-files :around #'all-the-icons-dired--refresh-advice)
    (advice-add #'dired-create-directory :around #'all-the-icons-dired--refresh-advice)
    (advice-add #'wdired-abort-changes :around #'all-the-icons-dired--refresh-advice))

  (with-no-warnings
    (defun my-all-the-icons-dired--refresh ()
      "Display the icons of files in a dired buffer."
      (all-the-icons-dired--remove-all-overlays)
      ;; NOTE: don't display icons it too many items
      (if (<= (count-lines (point-min) (point-max)) 1000)
          (save-excursion
            (goto-char (point-min))
            (while (not (eobp))
              (when (dired-move-to-filename nil)
                (let ((file (file-local-name (dired-get-filename 'relative 'noerror))))
                  (when file
                    (let ((icon (if (file-directory-p file)
                                    (all-the-icons-icon-for-dir file
                                                                :face 'all-the-icons-dired-dir-face
                                                                :height 0.9
                                                                :v-adjust all-the-icons-dired-v-adjust)
                                  (all-the-icons-icon-for-file file :height 0.9 :v-adjust all-the-icons-dired-v-adjust))))
                      (if (member file '("." ".."))
                          (all-the-icons-dired--add-overlay (point) "  \t")
                        (all-the-icons-dired--add-overlay (point) (concat icon "\t")))))))
              (forward-line 1)))
        (message "Not display icons because of too many items.")))
    (advice-add #'all-the-icons-dired--refresh :override #'my-all-the-icons-dired--refresh)
    ))

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
                          ))
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
                                     ("Noto Sans SC")
                                     ()))
  (setq use-default-font-for-symbols nil)
  (setq cnfonts-use-face-font-rescale t)
  :bind
  (("C-=" . cnfonts-increase-fontsize)
   ("C--" . cnfonts-decrease-fontsize)
   )
  )

(use-package emojify
  :defer t
  :hook (after-init . global-emojify-mode))

(use-package highlight-indent-guides
  :defer t
  :hook
  (prog-mode . highlight-indent-guides-mode)
  )

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
  :after
  (treemacs projectile)
  )

(use-package treemacs-magit
  :after
  (treemacs magit)
  )

(use-package centaur-tabs
  :init
  (centaur-tabs-mode t)
  (setq centaur-tabs-style "bar"
        centaur-tabs-height 35
        centaur-tabs-set-icons t
        centaur-tabs-set-modified-marker t
        centaur-tabs-show-navigation-buttons nil
        centaur-tabs-set-bar 'left
        x-underline-at-descent-line t)
  (centaur-tabs-change-fonts "FiraGO" 180)
  (centaur-tabs-headline-match)

  (setq centaur-tabs-gray-out-icons 'buffer)
  (centaur-tabs-enable-buffer-reordering)
  (setq centaur-tabs-adjust-buffer-order t)
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

(use-package ibuffer-vc
  :defer t)

(use-package dired
  :ensure
  nil                                        ;; dired mode is not on melpa
  :defer t
  :commands
  (dired dired-jump)
  :custom
  ((dired-listing-swithes "-agho --group-directories-first")
   (counsel-dired-listing-swithes "-agho --group-directories-first"))
  :config
  (setq dired-guess-shell-alist-user
	'(("\\.pdf\\'" "open")
	  ("\\.doc\\'" "open")
	  ("\\.docx\\'" "open")
	  ("\\.ppt\\'" "open")
	  ("\\.pptx\\'" "open")
	  ("\\.xls\\'" "open")
	  ("\\.xlsx\\'" "open")))
  (when (string= system-type "darwin")       
    (setq dired-use-ls-dired nil))
  ;; :hook
  ;; (dired-load . (lambda ()
  ;; 		  (setq dired-x-hands-off-my-keys nil)
  ;; 		  (load "dired-x")))
  )

(use-package dired-single
  :init
  (defun my-dired-init ()
    "Bunch of stuff to run for dired, either immediately or when it's
loaded."
    ;; <add other stuff here>
    (define-key dired-mode-map [remap dired-find-file]
      'dired-single-buffer)
    (define-key dired-mode-map [remap dired-mouse-find-file-other-window]
      'dired-single-buffer-mouse)
    (define-key dired-mode-map [remap dired-up-directory]
      'dired-single-up-directory))

  ;; if dired's already loaded, then the keymap will be bound
  (if (boundp 'dired-mode-map)
      ;; we're good to go; just add our bindings
      (my-dired-init)
    ;; it's not loaded yet, so add our bindings to the load-hook
    (add-hook 'dired-load-hook 'my-dired-init))
  )

(use-package dired-hide-dotfiles
  :defer t
  :config
  (defun my-dired-mode-hook ()
    "My `dired' mode hook."
    ;; To hide dot-files by default
    (dired-hide-dotfiles-mode))

  ;; To toggle hiding
  (define-key dired-mode-map "." #'dired-hide-dotfiles-mode)
  :hook
  (dired-mode . my-dired-mode-hook)
  )

(defun gq/org-mode-visual-fill ()
  (setq visual-fill-column-width 120
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :defer t
  :hook (org-mode . gq/org-mode-visual-fill))

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
  :defer t
  :after
  (counsel ivy)
  :config
  (ivy-rich-mode 1)
  )

(use-package ivy-bibtex
  :defer t
  :after ivy
  :config
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

(use-package ivy-prescient
  :straight t
  :after ivy
  :config
  (ivy-prescient-mode)
  )

(use-package prescient
  :straight t
  :config
  (prescient-persist-mode +1))

(use-package selectrum
  :straight t
  :config
  (selectrum-mode +1)
  )

(use-package selectrum-prescient
  :after (selectrum prescient)
  :straight t
  :config
  (selectrum-prescient-mode +1)
  )

(use-package counsel
  :bind
  (("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   :map minibuffer-local-map
   ("C-r" . 'counsel-minibuffer-history))
  ;; Don't start with ^
  :config
  (setq ivy-initial-inputs-alist nil)
  (setq counsel-find-file-ignore-regexp
	(concat "\\(?:\\`[#.]\\)" "\\|\\(?:\\`.+?[#~]\\'\\)"))
  )

(use-package which-key
  :diminish
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0)
  )

(use-package helpful
  :defer t
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key)
  )

(use-package projectile
  :diminish
  projectile
  :config
  (projectile-mode)
  (when (file-directory-p "~/projects")
    (setq projectile-project-search-path '("~/projects")))
  (setq projectile-switch-project-action #'projectile-dired)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  )

(defun gq/setup-lsp-mode ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode)
  (lsp-enable-which-key-integration)
  )
(use-package lsp-mode
  :commands
  (lsp lsp-deferred)
  :hook
  ((python-mode . lsp)
   (lsp-mode . gq/setup-lsp-mode))
  :config
  (setq lsp-keymap-prefix "C-c l"))

(use-package lsp-ivy
  :defer
  t
  :commands
  lsp-ivy-workspace-symbol
  )

(use-package lsp-ui
  :defer
  t
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

(use-package lsp-treemacs
  :defer
  t
  :after
  lsp
  )

(use-package company
  :after
  lsp-mode
  :hook
  (prog-mode . company-mode)
  :bind
  (:map company-active-map
        ("<tab>" . company-complete-selection)
        )
  (:map lsp-mode-map
        ("<tab>" . company-indent-or-complete-common)
        )
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0)
  )

(use-package company-box
  :defer
  t
  :hook
  (company-mode . company-box-mode))

(use-package yasnippet
  :defer
  t
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :defer
  t
  )

(use-package dap-mode
  :defer t
  :commands
  dap-debug
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
  ;; (require 'dap-python)  
  (setq dap-auto-configure-features '(sessions breakpoints locals controls tooltip repl))
  ;; (dap-ui-mode 1)
  ;; (dap-tooltip-mode 1)
  ;; (tooltip-mode 1)
  ;; (dap-ui-controls-mode 1)
  ;; )
  )

(use-package magit
  :commands
  (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :bind
  ("C-x g" . magit-status)
  )

(defun gq/org-mode-setup ()
  (org-indent-mode)
  (visual-line-mode 1)

  ;; 待办事项图标
  (lambda ()
    "Beautify Org Checkbox Symbol"
    (push '("[ ]" . "☐") prettify-symbols-alist)
    (push '("[X]" . "☑") prettify-symbols-alist)
    (push '("[-]" . "❍") prettify-symbols-alist)
    (prettify-symbols-mode))

  (setq org-emphasis-regexp-components '("-[:multibyte:][:space:]('\"{" "-[:multibyte:][:space:].,:!?;'\")}\\[" "[:space:]" "." 1))
  (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)
  (org-element-update-syntax)
  )
(use-package org
  :ensure nil
  :hook
  (org-mode . gq/org-mode-setup)
  :config
  (with-eval-after-load 'org
    ;; 用 shift 进行选择
    (setq org-support-shift-select 1)     


    ;; 定义零宽字符与它的快捷键
    (defun insert-zero-width-space () (interactive) (insert-char #x200b))
    (defun my-latex-filter-zws (text backend info)
      (when (org-export-derived-backend-p backend 'latex)
        (replace-regexp-in-string "\x200B" "{}" text)))
    (global-set-key (kbd "C-*") 'insert-zero-width-space)

    ;; 快捷代码块
    (require 'org-tempo)
    (add-to-list 'org-structure-template-alist '("el" . "src elisp"))
    (add-to-list 'org-structure-template-alist '("py" . "src python"))
    (add-to-list 'org-structure-template-alist '("sh" . "src shell"))

    ;; 待办事项完成之后显示删除线
    (defface org-checkbox-done-text
      '((t (:foreground "#71696A" :strike-through t)))
      "Face for the text part of a checked org-mode checkbox.")
    (font-lock-add-keywords
     'org-mode
     `(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)"
        1 'org-checkbox-done-text prepend)) 'append)

    ;; 省略号自定义
    (setq org-ellipsis " ↴ ")
    ;; 源代码原生字体
    (setq org-src-fontify-natively t)
    ;; Tab 按键行为
    (setq org-src-tab-acts-natively t)
    ;; 隐藏标记符号
    (setq org-hide-emphasis-markers t)
    ;; 打开文件方式自定义
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
    ;; 待办事项状态
    (setq org-todo-keywords
          '((sequence "TODO(t)" "NEXT(n)" "DOING(i)""|" "DONE(d)" "CANCEL(c)")))
    (setq org-todo-keyword-faces
          '(("TODO" . (:foreground "red" :weight bold))
            ("NEXT" . "#E35DBF")
            ("DOING" . "brown")
            ("DONE" . (:foreground "#1AA260" :weight bold))
            ("CANCEL" . (:foreground "#888888")))
          )
    ;; 记录时间戳
    (setq org-agenda-start-with-log-mode t)
    (setq org-log-done 'time)
    (setq org-log-into-drawer t)
    ;; 标签预定义
    (setq org-tag-alist
          '((:startgroup)
            (:endgroup)
            ("outlook" . ?o)
            ("gmail" . ?g)
            )
          )
    )
  )

(custom-theme-set-faces
 'user
 '(variable-pitch ((t (:family "CMU Sans Serif" :height 200))))
 '(fixed-pitch ((t (:family "Fira Code" :height 160))))
 '(org-block ((t (:inherit fixed-pitch))))
 '(org-code ((t (:inherit (fixed-pitch)))))
 '(org-document-info ((t (:foreground "dark orange"))))
 '(org-block-begin-line ((t (:inherit (shadow fixed-pitch) :weight bold))))
 '(org-block-end-line ((t (:inherit (shadow fixed-pitch) :weight bold))))
 ;; '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
 '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
 '(org-link ((t (:foreground "royal blue" :underline t))))
 '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-property-value ((t (:inherit fixed-pitch))) t)
 '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
 '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
 '(org-verbatim ((t (:inherit (shadow fixed-pitch)))))
 )
(with-eval-after-load 'org
  (add-to-list 'org-emphasis-alist
               '("*" (:foreground "red" :weight bold)
                 ))
  )

(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)))
  (setq org-confirm-babel-evaluate nil)
  )

(use-package org-pomodoro
  :commands org-pomodoro
  )

(use-package org-pretty-tags
  :defer t
  )

(defun org-journal-find-location ()
  ;; Open today's journal, but specify a non-nil prefix argument in order to
  ;; inhibit inserting the heading; org-capture will insert the heading.
  (org-journal-new-entry t)
  (unless (eq org-journal-file-type 'daily)
    (org-narrow-to-subtree))
  (goto-char (point-max)))

(setq org-capture-templates
      '(
        ("n" "Next" entry (file+headline "~/SynologyDrive/org/inbox.org" "Tasks")
         "* NEXT %?\nSCHEDULED: %T\n\nReference: %a\n")
        ("t" "Todo" entry (file+headline "~/SynologyDrive/org/inbox.org" "Tasks")
         "* TODO %?\n")
        ("f" "Todo with File" entry (file+headline "~/SynologyDrive/org/inbox.org" "Tasks")
         "* TODO %?\nReference: %a\n")
        ("m" "Email" entry (file+headline "~/SynologyDrive/org/inbox.org" "Email")
         "* TODO %^{待办事项} %^g\nSCHEDULED: %T\n:PROPERTIES:\nLINK: %a\n:END:\n%?")
        ("d" "Diary" entry (file+olp+datetree "~/SynologyDrive/org/diary.org")
         "* %?\nEntered on %U\n%i")
        ("p" "org-protocol" entry (file "~/SynologyDrive/org/inbox.org")
         "* %^{Title}\nSource: [[%:link][%:description]]\n#+begin_quote\n%i\n#+end_quote\n%?\nCaptured On: %U\n")
        ("l" "org-protocol link" entry (file "~/SynologyDrive/org/inbox.org")
         "* [[%:link][%:description]] \nCaptured On: %U")
        ("j" "Journal entry" plain (function org-journal-find-location)
         "** %(format-time-string org-journal-time-format)%^{Title}\n%i%?"
         :jump-to-captured t :immediate-finish t)
        ))

(setq org-refile-targets '((nil :maxlevel . 9)
                           (("~/SynologyDrive/org/archive/email.org"
                             "~/SynologyDrive/org/archive/tasks.org") :maxlevel . 1)))
(advice-add 'org-refile :after 'org-save-all-org-buffers)

(use-package org2blog
  :commands (org2blog-user-interface)
  :config
  (setq org2blog/wp-blog-alist
        '(("myblog"
           :url "http://galoisgu.com/wordpress/xmlrpc.php"
           :username "guqun")))
  )

(use-package org-download
  :defer t)

(require 'org-habit)
(setq org-habit-show-done-always-green t) 
;;; 减少显示天数，使其可以放在任务条的左边
(setq org-habit-graph-column 1)
(setq org-habit-preceding-days 10)
(setq org-habit-following-days 2)
;;; 恢复默认日历行为
(setq org-habit-show-habits-only-for-today t)
(let ((agenda-sorting-strategy
       (assoc 'agenda org-agenda-sorting-strategy)))
  (setcdr agenda-sorting-strategy
          (remove 'habit-down (cdr agenda-sorting-strategy))))

(use-package org-kanban
  :commands org-kanban)

(use-package org-superstar
  :hook
  (org-mode . org-superstar-mode)
  :init
  ;; This is usually the default, but keep in mind it must be nil
  (setq org-hide-leading-stars nil)
  ;; This line is necessary.
  (setq org-superstar-leading-bullet ?\s)
  ;; If you use Org Indent you also need to add this, otherwise the
  ;; above has no effect while Indent is enabled.
  (setq org-indent-mode-turns-on-hiding-stars nil)
  (setq org-superstar-remove-leading-stars t)
  (setq org-superstar-item-bullet-alist '((45 . 8226) (43 . 9999) (42 . 9758)))
  )

(use-package org-roam
  :commands
  (org-roam org-roam-find-file org-roam-graph)
  :custom
  (org-roam-directory "~/SynologyDrive/org/roam/")
  (org-roam-dailies-directory "~/SynologyDrive/org/daily/")

  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry
           #'org-roam-capture--get-point
           "* %?"
           :file-name "daily/%<%Y-%m-%d>"
           :head "#+title: %<%Y-%m-%d>\n\n")))
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
  :defer t
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
          (?1 . "⚡")(?2 . "⮬")(?3 . "⮮")(?4 . "☕")(?I . "important"))))

(use-package org-noter
  :defer t
  )

(use-package org-pdftools
  :hook (org-mode . org-pdftools-setup-link))

(use-package org-noter-pdftools
  :defer t
  :after org-noter
  :config
  ;; Add a function to ensure precise note is inserted
  (defun org-noter-pdftools-insert-precise-note (&optional toggle-no-questions)
    (interactive "P")
    (org-noter--with-valid-session
     (let ((org-noter-insert-note-no-questions (if toggle-no-questions
                                                   (not org-noter-insert-note-no-questions)
                                                 org-noter-insert-note-no-questions))
           (org-pdftools-use-isearch-link t)
           (org-pdftools-use-freestyle-annot t))
       (org-noter-insert-note (org-noter--get-precise-info)))))

  ;; fix https://github.com/weirdNox/org-noter/pull/93/commits/f8349ae7575e599f375de1be6be2d0d5de4e6cbf
  (defun org-noter-set-start-location (&optional arg)
    "When opening a session with this document, go to the current location.
With a prefix ARG, remove start location."
    (interactive "P")
    (org-noter--with-valid-session
     (let ((inhibit-read-only t)
           (ast (org-noter--parse-root))
           (location (org-noter--doc-approx-location (when (called-interactively-p 'any) 'interactive))))
       (with-current-buffer (org-noter--session-notes-buffer session)
         (org-with-wide-buffer
          (goto-char (org-element-property :begin ast))
          (if arg
              (org-entry-delete nil org-noter-property-note-location)
            (org-entry-put nil org-noter-property-note-location
                           (org-noter--pretty-print-location location))))))))
  (with-eval-after-load 'pdf-annot
    (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))

(use-package org-super-agenda
  :defer t
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
  :defer t
  )

(with-eval-after-load 'org
  (require 'org-protocol)
  )

(use-package org-gcal
  :defer t
  :config
  (require 'json)
  (defun get-gcal-config-value (key)
    "Return the value of the json file gcal_secret for key"
    (cdr (assoc key (json-read-file "~/.emacs.d/gcal-secret.json")))
    )
  (setq org-gcal-client-id (get-gcal-config-value 'org-gcal-client-id)
        org-gcal-client-secret (get-gcal-config-value 'org-gcal-client-secret)
        org-gcal-fetch-file-alist
        '(("davidguqun@gmail.com" .  "~/SynologyDrive/org/gcal.org")
          ))
  )

(use-package org-journal
  :init


  (setq org-journal-file-type 'monthly)
  (setq org-journal-file-format "%Y-%m.org")
  (setq org-journal-dir "~/SynologyDrive/org/journal/")
  (setq org-journal-date-format "%A, %d %B %Y")
  (setq org-agenda-file-regexp "\\`\\\([^.].*\\.org\\\|[0-9]\\\{8\\\}\\\(\\.gpg\\\)?\\\)\\'")
  (add-to-list 'org-agenda-files org-journal-dir)
  (setq org-journal-enable-agenda-integration t)
  )

(use-package lsp-python-ms
  :defer
  t
  :config
  (setq lsp-python-ms-auto-install-server t)
  :hook (python-mode . (lambda () (require 'lsp-python-ms) (lsp))))

(use-package pyenv-mode
  :after python-mode
  )

(require 'dap-python)

(add-hook
 'LaTeX-mode-hook
 (lambda ()
   (add-to-list
    'TeX-command-list
    '("XeLaTeX" "%`xelatex --synctex=1%(mode)%' %t" TeX-run-TeX nil t))))

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

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

(use-package evil-nerd-commenter
  :defer t
  :bind
  ("M-/" . evilnc-comment-or-uncomment-lines)
  )

(setq doc-view-resolution 200)

(use-package calfw
  :defer t
  :commands
  (cfw:open-org-calendar)
  :config
  (require 'calfw-org)
  (setq cfw:org-overwrite-default-keybinding t)
  )

(use-package powerthesaurus
  :defer t
  :commands
  (powerthesaurus-lookup-word-at-point))

(use-package pdf-tools
  :defer t
  :commands (pdf-view-mode pdf-tools-install)
  :mode ("\\.[pP][dD][fF]" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  ;; :config (pdf-tools-install :no-query)
  )

(use-package pdf-view-restore
  :defer t
  :after pdf-tools
  :config
  (add-hook 'pdf-view-mode-hook 'pdf-view-restore-mode))

(use-package zotxt
  :defer t)

(use-package mu4e
  :defer t
  :commands
  (mu4e)
  :ensure nil
  :config
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
                  (smtpmail-smtp-user . "davidguqun@gmail.com")
                  (smtpmail-smtp-server . "smtp.gmail.com")
                  (smtpmail-smtp-service . 587)
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
                  (smtpmail-smtp-server . "smtp.office365.com")
                  (smtpmail-smtp-service . 587)
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
  (setq mu4e-headers-precise-alignment t)
  ;;      (setq mu4e-use-fancy-chars t)
)

(use-package mu4e-views
  :after mu4e
  :defer t
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
  :config
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

(with-eval-after-load 'mu4e
  (require 'mu4e-org)
)

(use-package mu4e-maildirs-extension
  :after mu4e
  :config
  (mu4e-maildirs-extension)
  )

(use-package mu4e-thread-folding
  :ensure
  nil
  :defer
  t
  :after mu4e
  :config
  (add-to-list 'mu4e-header-info-custom
               '(:empty . (:name "Qun Gu"
                                 :shortname "GQ"
                                 :function (lambda (msg) "  "))))
  (setq mu4e-headers-fields '((:empty         .    2)
                              (:human-date    .   12)
                              (:flags         .    6)
                              (:mailing-list  .   10)
                              (:from          .   22)
                              (:subject       .   nil)))
  (define-key mu4e-headers-mode-map (kbd "<tab>")     'mu4e-headers-toggle-at-point)
  (define-key mu4e-headers-mode-map (kbd "<left>")    'mu4e-headers-fold-at-point)
  (define-key mu4e-headers-mode-map (kbd "<S-left>")  'mu4e-headers-fold-all)
  (define-key mu4e-headers-mode-map (kbd "<right>")   'mu4e-headers-unfold-at-point)
  (define-key mu4e-headers-mode-map (kbd "<S-right>") 'mu4e-headers-unfold-all)
  )

(use-package vterm
  :commands
  vterm)

(use-package dash-at-point
  :defer t
  :config
  (add-to-list 'dash-at-point-mode-alist'(python-mode . "python"))
  )
