(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-document-title ((t (:weight bold :height 1.5 :family "CMU Serif"))))
 '(outline-1 ((t (:weight bold :height 1.4 :family "CMU Serif"))))
 '(outline-2 ((t (:weight bold :height 1.25 :family "CMU Sans Serif"))))
 '(outline-3 ((t (:weight bold :height 1.125 :family "CMU Serif Upright Italic"))))
 '(outline-4 ((t (:weight bold :height 1.05 :family "CMU Bright"))))
 '(variable-pitch ((t (:family "Calibri")))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#2d3743" "#ff4242" "#74af68" "#dbdb95" "#34cae2" "#008b8b" "#00ede1" "#e1e1e0"])
 '(centaur-tabs-mode t nil (centaur-tabs))
 '(fci-rule-color "#5c5e5e")
 '(highlight-indent-guides-delay 0)
 '(highlight-indent-guides-method 'character)
 '(highlight-indent-guides-responsive 'stack)
 '(jdee-db-active-breakpoint-face-colors (cons "#0d0d0d" "#81a2be"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#0d0d0d" "#b5bd68"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#0d0d0d" "#5a5b5a"))
 '(objed-cursor-color "#cc6666")
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
   '(ivy-bibtex base16-theme kaolin-themes centaur-tabs elpy yasnippet-snippets yasnippet highlight-indent-guides highlight-indent-guide dashboard pdf-view-restore org-noter-pdftools org-pdftools ibuffer-vc all-the-icons-ibuffer dired-narrow dired-ranger dired-subtree dired-rainbow dired-open dired-avfs dired-filter dired-list dired-collapse dired-hacks-utils dired+ company-capf company-mode company company-lsp which-key use-package treemacs-projectile treemacs-magit treemacs-icons-dired smex rainbow-delimiters pyenv-mode org2blog org-superstar org-roam-server org-kanban org-fancy-priorities org-download lsp-ui lsp-python-ms lsp-ivy helpful exec-path-from-shell doom-themes doom-modeline dap-mode counsel cnfonts auctex all-the-icons-ivy-rich all-the-icons-ivy all-the-icons-dired))
 '(pdf-view-midnight-colors (cons "#c5c8c6" "#1d1f21"))
 '(rustic-ansi-faces
   ["#1d1f21" "#cc6666" "#b5bd68" "#f0c674" "#81a2be" "#c9b4cf" "#8abeb7" "#c5c8c6"])
 '(vc-annotate-background "#1d1f21")
 '(vc-annotate-color-map
   (list
    (cons 20 "#b5bd68")
    (cons 40 "#c8c06c")
    (cons 60 "#dcc370")
    (cons 80 "#f0c674")
    (cons 100 "#eab56d")
    (cons 120 "#e3a366")
    (cons 140 "#de935f")
    (cons 160 "#d79e84")
    (cons 180 "#d0a9a9")
    (cons 200 "#c9b4cf")
    (cons 220 "#ca9aac")
    (cons 240 "#cb8089")
    (cons 260 "#cc6666")
    (cons 280 "#af6363")
    (cons 300 "#936060")
    (cons 320 "#765d5d")
    (cons 340 "#5c5e5e")
    (cons 360 "#5c5e5e")))
 '(vc-annotate-very-old-color nil))
