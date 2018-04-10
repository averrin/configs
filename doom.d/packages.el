;; -*- no-byte-compile: t; -*-
;;; config/default/packages.el

;; (package! ranger)
(package! ag)
(package! centered-cursor-mode)
;; (package! evil-magit)
;; (package! indent-guide)
(package! highlight-indent-guides)
(package! ivy-posframe)
(package! lsp-mode)
(package! lsp-ui)
(package! company-lsp)
(package! company-box)
;;(package! company-box
;;  :recipe (:fetcher github
;;           :repo "sebastiencs/company-box"
;;           :files ("*")))

(package! dart-mode
  :recipe (:fetcher github
           :repo "averrin/dart-mode"
           :files ("*")))

(package! helm-dart
  :recipe (:fetcher github
           :repo "averrin/dart-mode"
           :files ("helm-dart.el")))

(package! company-dart
  :recipe (:fetcher github
           :repo "sid-kurias/company-dart"
           :files ("*")))
