(require 'package)

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")
        ("gnu" . "https://elpa.gnu.org/packages/")))

(unless package-archive-contents
  (package-refresh-contents))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;; (load custom-file)

(defun gq/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "emacs-config.org" user-emacs-directory))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle)))
  )
(add-hook 'org-mode-hook
          (lambda () (add-hook 'after-save-hook 'gq/org-babel-tangle-config)))

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

;; 删除启动信息
(setq inhibit-startup-message t)
;; 不显示滚动条
(scroll-bar-mode -1)
;; 不显示工具条
(tool-bar-mode -1)
;; 不显示菜单栏
(menu-bar-mode -1)
;; 设置白边
(set-fringe-mode 10)
;; 全屏显示
(toggle-frame-maximized)
;; 设置透明度
(set-frame-parameter (selected-frame) 'alpha '(90 . 50))
;; 显示电池用量
(display-battery-mode t)
;; 显示列数
(column-number-mode)
;; 默认自动断行
(set-default 'truncate-lines t)
;; 设置可视警告
;; (setq visible-bell t)
;;
(setq show-trailing-whitespace t)
(when (eq system-type 'darwin)
  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'super)
  (global-unset-key (kbd "C-M-SPC"))
  (global-set-key (kbd "s-c") 'kill-ring-save)
  (global-set-key (kbd "s-v") 'yank)
  (global-set-key (kbd "s-x") 'kill-region)
  (global-set-key (kbd "s-a") 'mark-whole-buffer)
  (global-set-key (kbd "s-z") 'undo)
  (global-set-key (kbd "s-f") 'isearch-forward)
  (global-set-key (kbd "s-g") 'isearch-repeat-forward)
  (global-set-key (kbd "s-o") 'find-file)
  (global-set-key (kbd "s-o") 'mac-open-file)
  (global-set-key (kbd "s-n") 'find-file)
  (global-set-key (kbd "s-s") 'save-buffer)
  (global-set-key (kbd "s-S") 'mac-save-file-as)
  (global-set-key (kbd "s-p") 'mac-preview) ; requires mac-preview
  (global-set-key (kbd "s-w") 'kill-buffer)
  (global-set-key (kbd "s-m") 'iconify-frame)
  (global-set-key (kbd "s-q") 'save-buffers-kill-emacs)
  (global-set-key (kbd "s-.") 'keyboard-quit)
  (global-set-key (kbd "s-l") 'goto-line)
  (global-set-key (kbd "s-k") 'kill-buffer)
  (global-set-key (kbd "s-<up>")    'beginning-of-buffer)
  (global-set-key (kbd "s-<down>")  'end-of-buffer)
  (global-set-key (kbd "s-<left>")  'beginning-of-line)
  (global-set-key (kbd "s-<right>") 'end-of-line)
  (global-set-key [(meta down)]     'forward-paragraph)
  (global-set-key [(meta up)]       'backward-paragraph)
  )

;; 不需要备份文件
(setq make-backup-files nil)
;; 环境变量——系统编码
(setenv "LANG" "en_US.UTF-8")
(setenv "LC_ALL" "en_US.UTF-8")
(setenv "LC_CTYPE" "en_US.UTF-8")
;; 启动 emacs 服务，让 emacsclient 可用
(server-start)
;; 系统代理
;; (setq url-proxy-services '(("http" . "127.0.0.1:7890")
;;                            ("https" . "127.0.0.1:7890")))
(setq org-image-actual-width 800)

(use-package general
  :config
  (general-define-key
   "M-x" 'counsel-M-x
   "C-x d" 'counsel-dired
   "C-x b" 'counsel-switch-buffer
   "C-s" 'counsel-grep-or-swiper
   "C-x C-b" 'ibuffer
   "C-s-<left>" 'shrink-window-horizontally
   "C-s-<right>" 'enlarge-window-horizontally
   "C-s-<down>" 'shrink-window
   "C-s-<up>" 'enlarge-window
   "<escape>" 'keyboard-escape-quit
   "<home>" 'beginning-of-line
   "<end>" 'end-of-line
   "C-=" 'my/increase-font-size
   "C--" 'my/decrease-font-size
   )

  ;; `general-def' can be used instead for `define-key'-like syntax
  (general-def org-mode-map
    "C-c C-j" 'org-journal-new-entry
    "C-c C-q" 'counsel-org-tag
    )
  ;; * Prefix Keybindings
  ;; :prefix can be used to prevent redundant specification of prefix keys
  (general-define-key
   :prefix "C-c"
   "a" 'org-agenda-list
   "b" 'counsel-bookmark
   "c" 'org-capture
   "C-j" 'org-journal-new-entry
   )
  )

(use-package osx-trash
  :config
  (when (eq system-type 'darwin)
    (osx-trash-setup))
  (setq delete-by-moving-to-trash t)
  )

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-dracula t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(defvar my-current-font-size)
(setq my-current-font-size 5)

(defun my/increase-font-size ()
  (interactive)
  (setq my-current-font-size (min (+ my-current-font-size 1) 7))
  (my/set-fonts my-font-sizes-suite english-font chinese-font)
  )
(defun my/decrease-font-size ()
  (interactive)
  (setq my-current-font-size (max (- my-current-font-size 1) 0))
  (my/set-fonts my-font-sizes-suite english-font chinese-font)
  )
(defun my/set-fonts (my-font-sizes-suite english-font chinese-font)
  (setq my-font-sizes (nth my-current-font-size my-font-sizes-suite))
  (setq english-font-size (nth 0 my-font-sizes))
  (setq chinese-font-size (nth 1 my-font-sizes))
  (set-face-attribute 'default nil :font (font-spec :family english-font :size english-font-size))
  (set-fontset-font t 'kana (font-spec :family chinese-font :size chinese-font-size))
  (set-fontset-font t 'han (font-spec :family chinese-font :size chinese-font-size))
  (set-fontset-font t 'cjk-misc (font-spec :family chinese-font :size chinese-font-size))
  (set-fontset-font t 'bopomofo (font-spec :family chinese-font :size chinese-font-size))
;;  (set-fontset-font t 'symbol (font-spec :family symbol-font :size symbol-font-size))
  (set-fontset-font "fontset-default" '(#x2010 . #x20ff)
                    (font-spec :family chinese-font :size chinese-font-size) nil :append)
  (defun blaenk/set-char-widths (alist)
    (while (char-table-parent char-width-table)
      (setq char-width-table (char-table-parent char-width-table)))
    (dolist (pair alist)
      (let ((width (car pair))
            (chars (cdr pair))
            (table (make-char-table nil)))
        (dolist (char chars)
          (set-char-table-range table char width))
        (optimize-char-table table)
        (set-char-table-parent table char-width-table)
        (setq char-width-table table))))

  (blaenk/set-char-widths
   `((2 . (,(string-to-char "“") ,(string-to-char "”") ,(string-to-char "‘") ,(string-to-char "’")
           ,(string-to-char "…") ,(string-to-char "—") ,(string-to-char "（") ,(string-to-char "）")
                 ,(string-to-char "【") ,(string-to-char "】")
           ))))
  )

(defun my/prog-mode-font()
  (interactive)
  (setq chinese-font "STKaiti")
  (setq english-font "Iosevka")
  (setq my-font-sizes-suite
        '((10.0 10.0)
          (12.0 12.0)
          (14.0 14.0)
          (16.0 16.0)
          (19.0 20.0)
          (21.0 22.0)
          (23.0 24.0)
          (25.0 26.0)
          )
        )
  (my/set-fonts my-font-sizes-suite english-font chinese-font)
  )
(defun my/org-mode-font()
  (interactive)
  (setq chinese-font "STXihei")
  (setq english-font "JetBrains Mono")
  (setq my-font-sizes-suite
        '((8.0 10.0)
          (10.0 12.0)
          (11.0 14.0)
          (13.0 16.0)
          (16.0 20.0)
          (18.0 22.0)
          (20.0 24.0)
          (22.0 26.0)
          )
        )
  (my/set-fonts my-font-sizes-suite english-font chinese-font)
  )
(defun my/default-mode-font()
  (interactive)
  (setq chinese-font "STFangsong")
  (setq english-font "MesloLGS NF")
  (setq my-font-sizes-suite
        '((8.0 10.0)
          (10.0 12.0)
          (12.0 14.0)
          (14.0 16.0)
          (16.0 20.0)
          (18.0 22.0)
          (20.0 24.0)
          (24.0 28.0)
          )
        )
  (my/set-fonts my-font-sizes-suite english-font chinese-font)
  )
(defun my/text-mode-font()
  (interactive)
  (setq chinese-font "STKaiti")
  (setq english-font "Consola Mono")
  (setq my-font-sizes-suite
        '((8.0 10.0)
          (10.0 12.0)
          (12.0 14.0)
          (14.0 16.0)
          (16.0 18.0)
          (18.0 22.0)
          (20.0 24.0)
          (24.0 28.0)
          )
        )
  (my/set-fonts my-font-sizes-suite english-font chinese-font)
  )
(setq use-default-font-for-symbols nil)
(my/default-mode-font)

(use-package all-the-icons)

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

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package emojify
  :config
  (setq emojify-display-style 'image)
  ;; (setq emojify-emoji-set "twemoji-v2-22")
  )

(use-package highlight-indent-guides
  :defer t
  :hook
  (prog-mode . highlight-indent-guides-mode)
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
  (setq doom-modeline-env-version t)
  (setq doom-modeline-env-enable-python t)
  (setq doom-modeline-env-enable-ruby t)
  (setq doom-modeline-env-enable-perl t)
  (setq doom-modeline-env-enable-go t)
  (setq doom-modeline-env-enable-elixir t)
  (setq doom-modeline-env-enable-rust t)
  (setq doom-modeline-env-python-executable 'python-shell-interpreter)
  (setq doom-modeline-env-ruby-executable "ruby")
  (setq doom-modeline-env-perl-executable "perl")
  (setq doom-modeline-env-go-executable "go")
  (setq doom-modeline-env-elixir-executable "iex")
  (setq doom-modeline-env-rust-executable "rustc")
  (setq doom-modeline-env-load-string "...")
  (setq doom-modeline-before-update-env-hook nil)
  (setq doom-modeline-after-update-env-hook nil)
  )

;; (doom-modeline-def-modeline 'my-org-line
;;   '(bar buffer-info vcs buffer-encoding major-mode)
;;   '())

;; ;; Add to `doom-modeline-mode-hook` or other hooks
;; (defun setup-custom-doom-modeline ()
;;    (doom-modeline-set-modeline 'my-org-line 'default))
;; (add-hook 'org-mode-hook 'setup-custom-doom-modeline)

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
          treemacs-workspace-switch-cleanup      nil
          )
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

(use-package neotree
  :bind
  (("<f12>" . 'neotree-toggle))
  )

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

(use-package ibuffer-vc
  :defer t)

(use-package dired
  :ensure
  nil                                        ;; dired mode is not on melpa
  :defer t
  :commands
  (dired dired-jump)
  :config
  (when (string= system-type "darwin")
    (setq dired-use-ls-dired t
          insert-directory-program "/usr/local/bin/gls"
          dired-listing-switches "-aBhl --group-directories-first"))

        (setq counsel-dired-listing-switches "-agho --group-directories-first")
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
  ;;              (setq dired-x-hands-off-my-keys nil)
  ;;              (load "dired-x")))
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

(use-package dired-rainbow
  :config
  (progn
    (dired-rainbow-define-chmod directory "#6cb2eb" "d.*")
    (dired-rainbow-define html "#eb5286" ("css" "less" "sass" "scss" "htm" "html" "jhtm" "mht" "eml" "mustache" "xhtml"))
    (dired-rainbow-define xml "#f2d024" ("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json" "msg" "pgn" "rss" "yaml" "yml" "rdata"))
    (dired-rainbow-define document "#9561e2" ("docm" "doc" "docx" "odb" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub" "odp" "ppt" "pptx"))
    (dired-rainbow-define markdown "#ffed4a" ("org" "etx" "info" "markdown" "md" "mkd" "nfo" "pod" "rst" "tex" "textfile" "txt"))
    (dired-rainbow-define database "#6574cd" ("xlsx" "xls" "csv" "accdb" "db" "mdb" "sqlite" "nc"))
    (dired-rainbow-define media "#de751f" ("mp3" "mp4" "MP3" "MP4" "avi" "mpeg" "mpg" "flv" "ogg" "mov" "mid" "midi" "wav" "aiff" "flac"))
    (dired-rainbow-define image "#f66d9b" ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png" "psd" "eps" "svg"))
    (dired-rainbow-define log "#c17d11" ("log"))
    (dired-rainbow-define shell "#f6993f" ("awk" "bash" "bat" "sed" "sh" "zsh" "vim"))
    (dired-rainbow-define interpreted "#38c172" ("py" "ipynb" "rb" "pl" "t" "msql" "mysql" "pgsql" "sql" "r" "clj" "cljs" "scala" "js"))
    (dired-rainbow-define compiled "#4dc0b5" ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp" "hxx" "m" "cc" "cs" "cp" "cpp" "go" "f" "for" "ftn" "f90" "f95" "f03" "f08" "s" "rs" "hi" "hs" "pyc" ".java"))
    (dired-rainbow-define executable "#8cc4ff" ("exe" "msi"))
    (dired-rainbow-define compressed "#51d88a" ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
    (dired-rainbow-define packaged "#faad63" ("deb" "rpm" "apk" "jad" "jar" "cab" "pak" "pk3" "vdf" "vpk" "bsp"))
    (dired-rainbow-define encrypted "#ffed4a" ("gpg" "pgp" "asc" "bfe" "enc" "signature" "sig" "p12" "pem"))
    (dired-rainbow-define fonts "#6cb2eb" ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf"))
    (dired-rainbow-define partition "#e3342f" ("dmg" "iso" "bin" "nrg" "qcow" "toast" "vcd" "vmdk" "bak"))
    (dired-rainbow-define vc "#0074d9" ("git" "gitignore" "gitattributes" "gitmodules"))
    (dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*")
    ))

(use-package dired-collapse)

(use-package dired-subtree
  :bind (:map dired-mode-map
              ("i" . dired-subtree-insert)
              (";" . dired-subtree-remove)
              ("<tab>" . dired-subtree-toggle)
              ("<backtab>" . dired-subtree-cycle)))

(use-package dired-filter)

(use-package dired-narrow
  :bind (:map dired-mode-map
              ("C-f" . dired-narrow)))

(use-package dired-ranger)

(use-package dired-quick-sort
  :config (dired-quick-sort-setup))

(use-package diredfl
  :init
  (diredfl-global-mode))

(use-package dired-hacks-utils)

(use-package transpose-frame)

(defun gq/org-mode-visual-fill ()
  (setq visual-fill-column-width 120
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :defer t
  :hook (org-mode . gq/org-mode-visual-fill))

(use-package ivy
  :diminish t
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

(use-package marginalia
  :config
  (marginalia-mode))

(use-package embark
  :bind
  ("C-S-a" . embark-act))              ; pick some comfortable binding

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . embark-consult-preview-minor-mode))

(use-package prescient
  :straight t
  :config
  (prescient-persist-mode +1))

(use-package selectrum
  :straight t
  :config
  (selectrum-mode +1)
  )

(use-package orderless
  :custom (completion-styles '(orderless)))

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
  projectile-mode
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
  :delight
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
  (dap-ui-controls-mode 1)
  ;; )
  (dap-register-debug-template
   "Flutter :: Release"
   (list :type "flutter"
         :args "--release")
   )
  )

(use-package magit
  :commands
  (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :bind
  ("C-x g" . magit-status)
  )

(use-package diff-hl
  :config
  (global-diff-hl-mode)
  )

(defun gq/org-mode-setup ()
  (org-indent-mode)
  (visual-line-mode 1)
  (toggle-word-wrap 0)

  ;; 待办事项图标
  (lambda ()
    "Beautify Org Checkbox Symbol"
    (push '("[ ]" . "📌") prettify-symbols-alist)
    (push '("[X]" . "✅") prettify-symbols-alist)
    (push '("[-]" . "⏩") prettify-symbols-alist)
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
    ;; 源代码不要缩进
    (setq org-edit-src-content-indentation 0)


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
    (setq org-ellipsis " ⤵")
    ;; 源代码原生字体
    (setq org-src-fontify-natively t)
    ;; Tab 按键行为
    (setq org-src-tab-acts-natively t)
    ;; 隐藏标记符号
    (setq org-hide-emphasis-markers t)
    ;; 打开文件方式自定义
    (setq org-file-apps
          '
          (
           ("\\.pdf\\'" . system)
           (directory . emacs)
           (auto-mode . emacs)
           ("\\.mm\\'" . default)
           ("\\.x?html?\\'" . default)
           ("\\.pptx?\\'" . system)
           ("\\.docx?\\'" . system)
           ("\\.xlsx?\\'" . system)
           ("\\.ppt?\\'" . system)
           ("\\.doc?\\'" . system)
           ("\\.xls?\\'" . system)
           ("\\.png?\\'" . system)))
    ;; 待办事项状态
    (setq org-todo-keywords
          '((sequence "TODO(t)" "NEXT(n)" "DOING(i)""|" "DONE(d)" "CANCEL(c)")))
    ;; (setq org-todo-keyword-faces
    ;;       '(("TODO" . (:foreground "red" :weight bold))
    ;;         ("NEXT" . "#E35DBF")
    ;;         ("DOING" . "brown")
    ;;         ("DONE" . (:foreground "#1AA260" :weight bold))
    ;;         ("CANCEL" . (:foreground "#888888")))
    ;;       )
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

(with-eval-after-load 'org
  (setq org-emphasis-alist
        '(("*" (bold :foreground "magenta"))
            ("/" (italic :foreground "cyan"))
            ("_" underline)
            ("=" org-verbatim verbatim)
            ("~" org-code verbatim)
            ("+" (:strike-through t)))
        )
  )

(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (ledger . t)))
  (setq org-confirm-babel-evaluate nil)
  )

(use-package org-analyzer)

(setq org-latex-pdf-process
   '("xelatex -interaction nonstopmode -output-directory %o %f" "xelatex -interaction nonstopmode -output-directory %o %f" "xelatex -interaction nonstopmode -output-directory %o %f"))

(setq org-export-backends '(beamer html latex))

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
        ("t"
         "Todo"
         entry
         (file+headline
          "~/SynologyDrive/org/inbox.org"
          "Tasks")
         "* TODO %?\n")
        ("f"
         "Todo with File"
         entry
         (file+headline
          "~/SynologyDrive/org/inbox.org"
          "Tasks")
         "* TODO %?\nReference: %a\n")
        ("m"
         "Email"
         entry
         (file+headline
          "~/SynologyDrive/org/inbox.org"
          "Email")
         "* TODO %^{待办事项} %^g\nSCHEDULED: %T\n:PROPERTIES:\nLINK: %a\n:END:\n%?")
        ("d"
         "Diary"
         entry
         (file+olp+datetree
          "~/SynologyDrive/org/diary.org")
         "* 日记\n%?\nEntered on %U, %(format \"(Lat, Lng) = (%s, %s)\" osx-location-latitude osx-location-longitude)\n%i"
         :jump-to-captured t
         :immediate-finish t)
        ("j"
         "Journal entry"
         plain
         (function org-journal-find-location)
         "** %(format-time-string org-journal-time-format)%?\nSCHEDULED: %T\n"
         :jump-to-captured t
         )
        ("p"
         "org-protocol"
         entry
         (file+headline
          "~/SynologyDrive/org/inbox.org"
          "Web")
         "* %^{Title}\nSource: [[%:link][%:description]]\n#+begin_quote\n%i\n#+end_quote\n%?\nCaptured On: %U\n")
        ("l"
         "org-protocol link"
         entry
         (file+headline
          "~/SynologyDrive/org/inbox.org"
          "Web")
         "* [[%:link][%:description]] \n%?\nCaptured On: %U")
        ))

(setq org-refile-targets
      '((nil :maxlevel . 9)
        (("~/SynologyDrive/org/archive/emails.org"
          "~/SynologyDrive/org/archive/tasks.org"
          "~/SynologyDrive/org/archive/webposts.org"
          ) :maxlevel . 1)
        (("~/SynologyDrive/Study/"
          "~/SynologyDrive/Work/"
          "~/SynologyDrive/Life/") :maxlevel . 9)))
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
  :custom
  (org-download-method 'directory)
  (org-download-image-dir "images")
  (org-download-heading-lvl nil)
  (org-download-timestamp "%Y%m%d-%H%M%S_")
  (org-image-actual-width 300)
  (org-download-screenshot-method "/usr/local/bin/pngpaste %s")
  :bind
  ("C-M-y" . org-download-screenshot)
  :hook
  (dired-mode . org-download-enable)
  )

(require 'org-habit)
(setq org-habit-show-done-always-green t)
;;; 减少显示天数，使其可以放在任务条的左边
(setq org-habit-graph-column 55)
(setq org-habit-preceding-days 30)
(setq org-habit-following-days 1)
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
  :delight
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

(use-package org-journal
  :init

  (setq org-journal-file-type 'monthly)
  (setq org-journal-file-format "%Y-%m.org")
  (setq org-journal-dir "~/SynologyDrive/org/journal/")
  (setq org-journal-date-format "%A, %d %B %Y")
  (setq org-agenda-file-regexp "\\`\\\([^.].*\\.org\\\|[0-9]\\\{8\\\}\\\(\\.gpg\\\)?\\\)\\'")

  (setq org-agenda-files
        '(
          "~/SynologyDrive/org/inbox.org"
          "~/SynologyDrive/org/habits.org"
          "~/SynologyDrive/org/archive/tasks.org"
          "~/SynologyDrive/org/birthdays.org"
          ))

  (add-to-list 'org-agenda-files org-journal-dir)
  (setq org-journal-enable-agenda-integration t)
  )

(use-package deft
  :bind
  ("<f8>" . deft)
  :commands
  (deft)
  :config
  (setq deft-directory "~/SynologyDrive/org/roam"
        deft-extensions '("md" "org")
        deft-recursive t
        deft-use-filename-as-title nil
        deft-use-filter-string-for-filename t
        deft-file-naming-rules '((nospace . "-"))
        )
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
(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.8))
(setq pdf-view-use-scaling t)
(setq pdf-view-use-imagemagick nil)
(setq TeX-view-program-selection
      '((output-dvi "open")
        (output-pdf "open")
        (output-html "open")))

(use-package dart-mode
  :hook (dart-mode . flutter-test-mode))

(use-package flutter
  :after dart-mode
  :bind (:map dart-mode-map
              ("C-M-x" . #'flutter-run-or-hot-reload))
  :custom
  (flutter-sdk-path "~/software/flutter/"))

(use-package flutter-l10n-flycheck
  :after flutter
  :config
  (flutter-l10n-flycheck-setup))

(use-package lsp-dart
  :hook (dart-mode . lsp))

(use-package hover)

(use-package yaml-mode
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))
  )

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

(use-package zotero
  )

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

(use-package elfeed
  :config
  (add-hook 'elfeed-show-mode-hook
            (lambda ()
              (let ((inhibit-read-only t)
                    (inhibit-modification-hooks t))
                (setq-local truncate-lines nil)
                (setq-local shr-width 85)
                (set-buffer-modified-p nil))
              (setq-local left-margin-width 15)
              (setq-local right-margin-width 15)
              ))
  :bind ("C-x w" . elfeed)
  )

(use-package elfeed-org
  :init
  (elfeed-org)
  :config
  (setq rmh-elfeed-org-files (list "~/.emacs.d/elfeed.org"))
  )

(use-package elfeed-goodies
  :config
  (elfeed-goodies/setup)
  )

(use-package vterm
  :commands
  vterm)

(use-package dash-at-point
  :defer t
  :config
  (add-to-list 'dash-at-point-mode-alist'(python-mode . "python"))
  )

(use-package calibredb
  :defer t
  :config
  (setq calibredb-root-dir "~/SynologyDrive/Library/calibre")
  (setq calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir))
  (setq calibredb-library-alist '(("~/SynologyDrive/Library/calibre")
                                ))
  )

(use-package nov
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  )

(use-package djvu
  :ensure nil
  :defer t)

(use-package ledger-mode
  :config
  (add-hook 'ledger-mode-hook
            (lambda ()
              (setq-local tab-always-indent 'complete)
              (setq-local completion-cycle-threshold t)
              (setq-local ledger-complete-in-steps t)))
  )

(use-package osx-location
  :config
  (osx-location-watch))
