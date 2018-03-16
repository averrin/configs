;; -*- no-byte-compile: t; -*-
;;; config/default/packages.el

;; (package! ranger)
(package! ag)
(package! centered-cursor-mode)
(package! evil-magit)
(package! indent-guide)
(package! ivy-posframe)

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
