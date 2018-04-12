;; -*- no-byte-compile: t; -*-
;;; config/default/packages.el

(package! ag)
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

;; (package! helm-dart
;;   :recipe (:fetcher github
;;            :repo "averrin/dart-mode"
;;            :files ("helm-dart.el")))

;; (package! company-dart
;;   :recipe (:fetcher github
;;            :repo "sid-kurias/company-dart"
;;            :files ("*")))
