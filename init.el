;; remove things that looks in the 70s
(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

;; setup basic package manager and repositories
(require 'package)
(setq package-archives
      '(("mepla" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
        ("org" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")
        ("elpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
        ))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; enable completions (ivy, counsel, ivy-rich, which-key)
(use-package smex)
(use-package ivy
  :diminish
  :bind
  (("C-s" . swiper))
  :config
  (ivy-mode 1)
  )
(use-package counsel
  :bind
  (("M-x" . counsel-M-x)
   ("C-x b" . counsel-ibuffer)
   ("C-x C-f" . counsel-find-file)
   :map minibuffer-local-map
   ("C-r" . 'counsel-minibuffer-history)
   )
  :config
  (setq ivy-initial-inputs-alist nil)     ;; Don't start with ^
  )
(use-package ivy-rich                     ;; 必须放在 counsel 之后，否则会报错
  :init
  (ivy-rich-mode 1)
  )
(use-package which-key
  :init
  (which-key-mode)
  :diminish
  :config
  (setq which-key-idle-delay 0)
  )

;; customize fonts, mode-line, theme
(use-package all-the-icons)
(use-package doom-modeline
  :init
  (doom-modeline-mode 1)
  )
(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode)
  )
(use-package doom-themes
  :init
  (load-theme 'doom-opera-light t)
  )
;; (set-face-attribute 'default nil :family "WenQuanYi Micro Hei Mono")
(use-package cnfonts
  :init
  (cnfonts-enable)
  (cnfonts-set-spacemacs-fallback-fonts)
  (setq cnfonts-use-face-font-rescale t)
  )
(use-package all-the-icons-ivy
  :init
  (add-hook 'after-init-hook 'all-the-icons-ivy-setup)
  )
(use-package all-the-icons-ivy-rich
  :init
  (all-the-icons-ivy-rich-mode 1)
  )
(use-package all-the-icons-dired
  :init
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
  )

;; helpful
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key)
  )

;; key bindings
(global-set-key (kbd "<home>") 'beginning-of-line)
(global-set-key (kbd "<end>") 'end-of-line)

;; programming
(use-package lsp-mode
  :commands
  (lsp lsp-deferred)
  :hook
  ((python-mode . lsp)
   (lsp-mode . lsp-enable-which-key-integration)
   )
  :init
  (setq lsp-keymap-prefix "C-c l")
  )
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-ui
  :config
  (setq lsp-ui-doc-position 'at-point)
  (setq lsp-ui-doc-max-width 400)
  (setq lsp-ui-doc-max-height 20)
)

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package dap-mode
  :config
  (setq dap-auto-configure-features '(sessions locals controls tooltip))
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (tooltip-mode 1)
  (dap-ui-controls-mode 1)
  )

;; python
(use-package lsp-python-ms
  :init
  (setq lsp-python-ms-auto-install-server t)
  :hook
  (python-mode . (lambda () (require 'lsp-python-ms) (lsp))))

(use-package pyenv-mode)

(require 'dap-python)

;; treemacs
(use-package treemacs
  :defer t
  :init
  (with-eval-after-load 'winum
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
          treemacs-show-hidden-files             t
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
  :after treemacs projectile
  )

(use-package treemacs-icons-dired
  :after treemacs dired
  :config (treemacs-icons-dired-mode)
  )

(use-package treemacs-magit
  :after treemacs magit
  )

(use-package auctex
  :defer
  :config
  )
(add-hook
 'LaTeX-mode-hook
 (lambda ()
   (add-to-list 'TeX-command-list
		'("XeLaTeX" "%`xelatex --synctex=1%(mode)%' %t" TeX-run-TeX nil t)
		)
   )
 )

(use-package org2blog)
(setq org2blog/wp-blog-alist
      '(("myblog"
         :url "http://galoisgu.com/wordpress/xmlrpc.php"
         :username "guqun")))

(use-package org-download)
(use-package org-kanban)

(setq org-src-fontify-natively t)

