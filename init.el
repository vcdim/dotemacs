;; remove components from the 70s
(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

(set-frame-parameter (selected-frame) 'alpha '(90 . 50))
(setq make-backup-files nil)

;; setup basic package manager and repositories
(require 'package)
(setq package-archives
      '(("mepla" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
        ("org" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")
        ("elpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))

;; enable completions (ivy, counsel, ivy-rich, which-key)
(use-package smex)
(use-package ivy
  :diminish
  :bind (("C-s" . swiper))
  :config (ivy-mode 1))
(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history))
  ;; Don't start with ^
  :config (setq ivy-initial-inputs-alist nil)
  :init
  (setq counsel-find-file-ignore-regexp
        (concat "\\(?:\\`[#.]\\)" "\\|\\(?:\\`.+?[#~]\\'\\)")))
;; Must stay after counsel
(use-package ivy-rich
  :after counsel
  :init (ivy-rich-mode 1))
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
  (global-set-key (kbd "C-x p") 'ivy-bibtex-my-publications)
  )

(use-package ivy-posframe
  :init
  (setq ivy-posframe-display-functions-alist
      '((swiper          . ivy-display-function-fallback)
        (complete-symbol . ivy-posframe-display-at-point)
        (counsel-M-x     . ivy-posframe-display-at-window-bottom-left)
        (t               . ivy-posframe-display)))
  (ivy-posframe-mode 1)
  )

(use-package which-key
  :init (which-key-mode)
  :diminish
  :config (setq which-key-idle-delay 0))

;; customize fonts, mode-line, theme
(use-package all-the-icons)
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

  ;; Whether display the modification icon for the buffer.
  ;; It respects `doom-modeline-icon' and `doom-modeline-buffer-state-icon'.
  (setq doom-modeline-buffer-modification-icon t)

  ;; Whether to use unicode as a fallback (instead of ASCII) when not using icons.
  (setq doom-modeline-unicode-fallback nil)

  ;; Whether display the minor modes in the mode-line.
  (setq doom-modeline-minor-modes nil)

  ;; If non-nil, a word count will be added to the selection-info modeline segment.
  (setq doom-modeline-enable-word-count nil)

  ;; Major modes in which to display word count continuously.
  ;; Also applies to any derived modes. Respects `doom-modeline-enable-word-count'.
  ;; If it brings the sluggish issue, disable `doom-modeline-enable-word-count' or
  ;; remove the modes from `doom-modeline-continuous-word-count-modes'.
  (setq doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode))

  ;; Whether display the buffer encoding.
  (setq doom-modeline-buffer-encoding t)

  ;; Whether display the indentation information.
  (setq doom-modeline-indent-info nil)

  ;; If non-nil, only display one number for checker information if applicable.
  (setq doom-modeline-checker-simple-format t)

  ;; The maximum number displayed for notifications.
  (setq doom-modeline-number-limit 99)

  ;; The maximum displayed length of the branch name of version control.
  (setq doom-modeline-vcs-max-length 12)

  ;; Whether display the workspace name. Non-nil to display in the mode-line.
  (setq doom-modeline-workspace-name t)

  ;; Whether display the perspective name. Non-nil to display in the mode-line.
  (setq doom-modeline-persp-name t)

  ;; If non nil the default perspective name is displayed in the mode-line.
  (setq doom-modeline-display-default-persp-name nil)

  ;; If non nil the perspective name is displayed alongside a folder icon.
  (setq doom-modeline-persp-icon t)

  ;; Whether display the `lsp' state. Non-nil to display in the mode-line.
  (setq doom-modeline-lsp t)

  ;; Whether display the GitHub notifications. It requires `ghub' package.
  (setq doom-modeline-github nil)

  ;; The interval of checking GitHub.
  (setq doom-modeline-github-interval (* 30 60))

  ;; Whether display the modal state icon.
  ;; Including `evil', `overwrite', `god', `ryo' and `xah-fly-keys', etc.
  (setq doom-modeline-modal-icon t)

  ;; Whether display the mu4e notifications. It requires `mu4e-alert' package.
  (setq doom-modeline-mu4e t)

  ;; Whether display the gnus notifications.
  (setq doom-modeline-gnus t)

  ;; Wheter gnus should automatically be updated and how often (set to 0 or smaller than 0 to disable)
  (setq doom-modeline-gnus-timer 2)

  ;; Wheter groups should be excludede when gnus automatically being updated.
  (setq doom-modeline-gnus-excluded-groups '("dummy.group"))

  ;; Whether display the IRC notifications. It requires `circe' or `erc' package.
  (setq doom-modeline-irc t)

  ;; Function to stylize the irc buffer names.
  (setq doom-modeline-irc-stylize 'identity)

  ;; Whether display the environment version.
  (setq doom-modeline-env-version t)
  ;; Or for individual languages
  (setq doom-modeline-env-enable-python t)
  (setq doom-modeline-env-enable-ruby t)
  (setq doom-modeline-env-enable-perl t)
  (setq doom-modeline-env-enable-go t)
  (setq doom-modeline-env-enable-elixir t)
  (setq doom-modeline-env-enable-rust t)

  ;; Change the executables to use for the language version string
  (setq doom-modeline-env-python-executable "python") ; or `python-shell-interpreter'
  (setq doom-modeline-env-ruby-executable "ruby")
  (setq doom-modeline-env-perl-executable "perl")
  (setq doom-modeline-env-go-executable "go")
  (setq doom-modeline-env-elixir-executable "iex")
  (setq doom-modeline-env-rust-executable "rustc")

  ;; What to dispaly as the version while a new one is being loaded
  (setq doom-modeline-env-load-string "...")

  ;; Hooks that run before/after the modeline version string is updated
  (setq doom-modeline-before-update-env-hook nil)
  (setq doom-modeline-after-update-env-hook nil)  
  )
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))
(use-package doom-themes
  ;;  :init (load-theme 'doom-opera-light t)
  )
(use-package kaolin-themes
  :config
  ;;  (load-theme 'kaolin-dark t)
  ;;  (kaolin-treemacs-theme)
  )
(use-package base16-theme
  :config
  ;;  (load-theme 'base16-default-dark t)
  )
(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  (setq dashboard-banner-logo-title "笔耕不辍, 静水流深        ")
  (setq dashboard-startup-banner "~/.emacs.d/g.png")
  (setq dashboard-set-footer nil)
  )

(use-package cnfonts
  :init (cnfonts-enable)
  (cnfonts-set-spacemacs-fallback-fonts)
  (setq cnfonts-use-face-font-rescale t)
  (setq cnfonts-personal-fontnames
	'(("Operator Mono Book" "Operator Mono Lig")
	  ()
	  ()
	  ))
  )

(use-package all-the-icons-ivy
  :init (add-hook 'after-init-hook 'all-the-icons-ivy-setup))
(use-package all-the-icons-ivy-rich
  :init (all-the-icons-ivy-rich-mode 1))
(use-package all-the-icons-ibuffer
  :init (all-the-icons-ivy-rich-mode 1))
(use-package all-the-icons-dired
  :init (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(use-package highlight-indent-guides
  :config (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))

(use-package org-pretty-tags)

;; helpful
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; programming
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook ((python-mode . lsp)
	 (lsp-mode . lsp-enable-which-key-integration))
  :init (setq lsp-keymap-prefix "C-c l"))
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
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

(use-package exec-path-from-shell
  :config (when (memq window-system '(mac ns x))
	    (exec-path-from-shell-initialize)))

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
(use-package company)
(add-hook 'after-init-hook 'global-company-mode)
(use-package company-lsp
  :config
  (push 'company-lsp company-backends))
(use-package yasnippet
  :init
  (yas-global-mode 1))
(use-package yasnippet-snippets)
;; python
(use-package lsp-python-ms
  :init (setq lsp-python-ms-auto-install-server t)
  :hook (python-mode . (lambda () (require 'lsp-python-ms) (lsp))))
(use-package pyenv-mode)
(require 'dap-python)

;; treemacs, ibuffer
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
  ;; (setq centaur-tabs-gray-out-icons 'buffer)
  ;; (centaur-tabs-enable-buffer-reordering)
  ;; (setq centaur-tabs-adjust-buffer-order t)
  (centaur-tabs-mode t)
  (setq uniquify-separator "/")
  (setq uniquify-buffer-name-style 'forward)
  (defun centaur-tabs-buffer-groups ()
    "`centaur-tabs-buffer-groups' control buffers' group rules.

 Group centaur-tabs with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
 All buffer name start with * will group to \"Emacs\".
 Other buffer group by `centaur-tabs-get-group-name' with project name."
    (list
     (cond
      ;; ((not (eq (file-remote-p (buffer-file-name)) nil))
      ;; "Remote")
      ((or (string-equal "*" (substring (buffer-name) 0 1))
	   (memq major-mode '(magit-process-mode
			      magit-status-mode
			      magit-diff-mode
			      magit-log-mode
			      magit-file-mode
			      magit-blob-mode
			      magit-blame-mode
			      )))
       "Emacs")
      ((derived-mode-p 'prog-mode)
       "Editing")
      ((derived-mode-p 'dired-mode)
       "Dired")
      (((match-end )mq major-mode '(helpful-mode
				    help-mode))
       "Help")
      ((memq major-mode '(org-mode
			  org-agenda-clockreport-mode
			  org-src-mode
			  org-agenda-mode
			  org-beamer-mode
			  org-indent-mode
			  org-bullets-mode
			  org-cdlatex-mode
			  org-agenda-log-mode
			  diary-mode))
       "OrgMode")
      (t
       (centaur-tabs-get-group-name (current-buffer))))))
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
  ("C-c t g" . centaur-tabs-group-buffer-groups))

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
(use-package all-the-icons-ibuffer
  :init (all-the-icons-ibuffer-mode 1))
(use-package ibuffer-vc)

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

;; auctex
(use-package auctex
  :defer t
  )
(add-hook
 'LaTeX-mode-hook
 (lambda ()
   (add-to-list
    'TeX-command-list
    '("XeLaTeX" "%`xelatex --synctex=1%(mode)%' %t" TeX-run-TeX nil t))))

;; org
(setq org-default-notes-file "~/SynologyDrive/org/capture/notes.org")
(use-package org2blog)
(setq org2blog/wp-blog-alist
      '(("myblog"
         :url "http://galoisgu.com/wordpress/xmlrpc.php"
         :username "guqun")))
(use-package org-download)
(use-package org-kanban)
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
(use-package org-pdftools
  :hook (org-mode . org-pdftools-setup-link))
(use-package org-noter-pdftools
  :after org-noter
  :config
  (with-eval-after-load 'pdf-annot
    (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))

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
(require 'org-tempo)
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

;; zero width space
(defun insert-zero-width-space () (interactive) (insert-char #x200b))
(defun my-latex-filter-zws (text backend info)
  (when (org-export-derived-backend-p backend 'latex)
    (replace-regexp-in-string "\x200B" "{}" text)))

;; hideshow
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

;; email
(add-to-list 'load-path "/usr/local/Cellar/mu/1.4.14/share/emacs/site-lisp/mu/mu4e")
(require 'mu4e)
(setq mail-user-agent 'mu4e-user-agent)

(setq mu4e-drafts-folder "/Drafts")
(setq mu4e-sent-folder   "/Sent")
(setq mu4e-trash-folder  "/Deleted")
(setq mu4e-maildir-shortcuts
      '((:maildir "/Inbox" :key ?i)
	(:maildir "/Sent" :key ?s)
	(:maildir "/Deleted" :key ?d)
	(:maildir "/Archive" :key ?a)
	(:maildir "/Drafts" :key ?r)
	(:maildir "/Edria" :key ?e)	
	))
(setq mu4e-get-mail-command "offlineimap")
(setq mu4e-update-interval 300)             ;; update every 5 minutes
(setq
 user-mail-address "guqun@outlook.com"
 user-full-name  "Qun Gu"
 mu4e-compose-signature
 (concat
  "Qun Gu\n"
  "Sent from Emacs"))
(setq mu4e-use-fancy-chars t)

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
(use-package mu4e-marker-icons
  :init (mu4e-marker-icons-mode 1))

(setq smtpmail-default-smtp-server "smtp.office365.com"
      smtpmail-smtp-server "smtp.office365.com"
      smtpmail-smtp-service 587)

;; key bindings
(global-set-key (kbd "<home>") 'beginning-of-line)
(global-set-key (kbd "<end>") 'end-of-line)
(global-set-key (kbd "C-=") 'cnfonts-increase-fontsize)
(global-set-key (kbd "C--") 'cnfonts-decrease-fontsize)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(toggle-frame-maximized)
(setq doc-view-resolution 200)

(setenv "LANG" "en_US.UTF-8")
(setenv "LC_ALL" "en_US.UTF-8")
(setenv "LC_CTYPE" "en_US.UTF-8")


;; (setq url-proxy-services '(("http" . "127.0.0.1:7890")
;; 			   ("https" . "127.0.0.1:7890")))

;; make init.el clean
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)
