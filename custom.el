(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil)))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(highlight-indent-guides-delay 0)
 '(highlight-indent-guides-method 'character)
 '(highlight-indent-guides-responsive 'stack)
 '(org-agenda-files
   '("~/SynologyDrive/org/gcal.org" "~/SynologyDrive/org/inbox.org" "~/SynologyDrive/org/habits.org" "~/SynologyDrive/org/birthdays.org" "/Users/gq/SynologyDrive/org/journal/2021-03.org"))
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
   '(dired emacs-color-themes zotxt yasnippet-snippets which-key vterm visual-fill-column treemacs-projectile treemacs-magit treemacs-icons-dired sublime-themes smex ranger rainbow-delimiters quelpa-use-package pyenv-mode powerthesaurus pdf-view-restore osx-trash org2blog org-trello org-superstar org-super-agenda org-roam-server org-pretty-tags org-pomodoro org-noter-pdftools org-mime org-kanban org-journal org-gcal org-fancy-priorities org-download no-littering mu4e-views mu4e-thread-folding mu4e-overview mu4e-marker-icons mu4e-maildirs-extension mu4e-alert modus-themes memoize lsp-ui lsp-python-ms lsp-origami lsp-ivy kaolin-themes ivy-posframe ivy-bibtex ibuffer-vc highlight-indent-guides helpful ghub general exec-path-from-shell evil-nerd-commenter emojify elpy doom-themes doom-modeline doct dired-single dired-hide-dotfiles dayone dashboard dash-at-point dap-mode counsel-projectile company-lsp company-emoji company-box company-auctex cnfonts centaur-tabs calfw-org calfw base16-theme auto-package-update all-the-icons-ivy-rich all-the-icons-ivy all-the-icons-ibuffer all-the-icons-dired))
 '(pos-tip-background-color "#E5E6DE")
 '(pos-tip-foreground-color "#4b5254")
 '(rustic-ansi-faces
   ["#FDF6E3" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#556b72"])
 '(send-mail-function 'smtpmail-send-it))
