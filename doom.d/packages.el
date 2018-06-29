;; -*- no-byte-compile: t; -*-
;;; config/default/packages.el
(disable-packages! irony irony-eldoc flycheck-irony company-irony)

(package! centered-cursor-mode)
(package! evil-magit)
(package! highlight-indent-guides)
(package! ivy-posframe)
(package! lsp-mode)
(package! lsp-go)
;; (package! lsp-ui)
(package! cquery)
(package! company-lsp)
(package! company-box)

(package! lsp-ui
  :recipe (:fetcher github
           :repo "emacs-lsp/lsp-ui"
           :files ("*")))

(package! dart-mode
  :recipe (:fetcher github
           :branch "lite"
           :repo "averrin/dart-mode"
           :files ("*")))
