;;; +dart.el --- description -*- lexical-binding: t; -*-

(def-package! lsp-mode)
(def-package! lsp-ui
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
)

(def-package! company-lsp
  :after lsp-mode
  :config
    (push 'company-lsp company-backends))

;; (if (string-equal system-name "spb-anabrodov")
(defun averrin//dart-mode-enable ()
  "Init dart-mode"
  (lsp-define-stdio-client
    lsp-dart-major-mode
    "dart"
    (lambda () default-directory)
    '("~/.pub-cache/bin/dart_language_server"))

  (push 'dart-mode flycheck-global-modes)
  (set (make-local-variable 'company-backends)
      '(company-lsp (company-dabbrev)))
  (flycheck-mode)
  (lsp-dart-major-mode-enable)
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  (setq lsp-ui-sideline-code-actions-prefix "💡 ")
)

(def-package! dart-mode
    :after company-lsp
    :hook (dart-mode . averrin//dart-mode-enable)
)

(defun my-set-projectile-root ()
  (when lsp--cur-workspace
    (setq projectile-project-root (lsp--workspace-root lsp--cur-workspace))))
(add-hook 'lsp-before-open-hook #'my-set-projectile-root)


(provide '+dart)
;;; +dart.el ends here
