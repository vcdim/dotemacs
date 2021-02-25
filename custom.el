(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil))))
 '(fixed-pitch ((t (:family "Fira Code" :height 160))))
 '(org-block ((t (:inherit fixed-pitch))))
 '(org-block-begin-line ((t (:inherit (shadow fixed-pitch) :weight bold))))
 '(org-block-end-line ((t (:inherit (shadow fixed-pitch) :weight bold))))
 '(org-code ((t (:inherit (shadow fixed-pitch)))))
 '(org-document-info ((t (:foreground "dark orange"))))
 '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
 '(org-link ((t (:foreground "royal blue" :underline t))))
 '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-property-value ((t (:inherit fixed-pitch))) t)
 '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
 '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
 '(org-verbatim ((t (:inherit (shadow fixed-pitch)))))
 '(variable-pitch ((t (:family "CMU Sans Serif")))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(centaur-tabs-mode t nil (centaur-tabs))
 '(highlight-indent-guides-delay 0)
 '(highlight-indent-guides-method 'character)
 '(highlight-indent-guides-responsive 'stack)
 '(org-agenda-files
   '("~/SynologyDrive/org/habits.org" "~/SynologyDrive/org/birthdays.org" "~/SynologyDrive/org/todo.org"))
 '(org-export-backends '(beamer html latex))
 '(org-file-apps
   '((directory . emacs)
     (auto-mode . emacs)
     ("\\.mm\\'" . default)
     ("\\.x?html?\\'" . default)
     ("\\.pdf?\\'" . system)
     ("\\.pptx?\\'" . system)
     ("\\.docx?\\'" . system)
     ("\\.xlsx?\\'" . system)
     ("\\.png?\\'" . system)))
 '(org-latex-pdf-process
   '("xelatex -interaction nonstopmode -output-directory %o %f" "xelatex -interaction nonstopmode -output-directory %o %f" "xelatex -interaction nonstopmode -output-directory %o %f"))
 '(package-selected-packages
   '(evil-nerd-commenter company-box dired-hide-dotfiles counsel-projectile general sublime-themes modus-themes no-littering auto-package-update calfw-org calfw ghub powerthesaurus ranger org-pomodoro org-mime doct mu4e-overview mu4e-marker-icons ivy-posframe mu4e-alert mu4e-views mu4e org-pretty-tags ivy-bibtex base16-theme kaolin-themes centaur-tabs elpy yasnippet-snippets yasnippet highlight-indent-guides highlight-indent-guide dashboard pdf-view-restore org-noter-pdftools org-pdftools ibuffer-vc all-the-icons-ibuffer dired-narrow dired-ranger dired-subtree dired-rainbow dired-open dired-avfs dired-filter dired-list dired-collapse dired-hacks-utils dired+ company-capf company-mode company company-lsp which-key use-package treemacs-projectile treemacs-magit treemacs-icons-dired smex rainbow-delimiters pyenv-mode org2blog org-superstar org-roam-server org-kanban org-fancy-priorities org-download lsp-ui lsp-python-ms lsp-ivy helpful exec-path-from-shell doom-themes doom-modeline dap-mode counsel cnfonts auctex all-the-icons-ivy-rich all-the-icons-ivy all-the-icons-dired))
 '(send-mail-function 'smtpmail-send-it))
