;; 使用包管理器
(require 'package)

;; 配置国内源
;; (setq package-archives
;;       '(("mepla" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
;;         ("org" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")
;;         ("elpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")))

;; 配置国外源
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")
        ("gnu" . "https://elpa.gnu.org/packages/")))


;; 除非包已经缓存，否则刷新包
(unless package-archive-contents
  (package-refresh-contents))

;; 加载 org 包
(require 'org)

;; 加载 emacs-config.org
(org-babel-load-file "~/.emacs.d/emacs-config.org")
