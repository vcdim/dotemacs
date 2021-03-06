(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files
   '("~/SynologyDrive/org/gcal.org" "~/SynologyDrive/org/inbox.org" "~/SynologyDrive/org/habits.org" "~/SynologyDrive/org/birthdays.org"))
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
   '(dash-at-point vterm mu4e-maildirs-extension mu4e-alert mu4e-views zotxt pdf-view-restore powerthesaurus calfw evil-nerd-commenter centaur-tabs lsp-python-ms org-gcal dap-mode lsp-treemacs lsp-ui lsp-ivy lsp-mode use-package))
 '(send-mail-function 'smtpmail-send-it)
 '(tabbar-background-color "#357535753575")
 )
