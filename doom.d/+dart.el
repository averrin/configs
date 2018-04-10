;;; +dart.el --- description -*- lexical-binding: t; -*-

(def-package! lsp-mode)
(def-package! lsp-ui :after lsp-mode)
(def-package! company-lsp :after lsp-mode)
(push 'company-lsp company-backends)
(def-package! company-box :after company-lsp)

(lsp-define-stdio-client
 lsp-prog-major-mode
 "dart"
 (lambda () default-directory)
 '("~/.pub-cache/bin/dart_language_server"))

(add-hook! prog-mode #'lsp-prog-major-mode-enable)
(add-hook! prog-major-mode #'lsp-prog-major-mode-enable)
(add-hook! lsp-mode 'lsp-ui-mode)
(define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
(define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)

;; (if (string-equal system-name "spb-anabrodov")
    (def-package! dart-mode :after company-lsp)
    (def-package! helm-dart :after dart-mode)
    ;; (def-package! company-dart :after dart-mode)
    (add-hook! dart-mode
        (push 'dart-mode flycheck-global-modes)
    )
    (add-hook! dart-mode
        (set (make-local-variable 'company-backends)
            ;; '(company-dart (company-dabbrev company-lsp))))
            '(company-lsp (company-dabbrev))))

    (add-hook! dart-mode
        (flycheck-mode)
    )
    (add-hook! dart-mode #'lsp-prog-major-mode-enable)
    (add-hook! dart-mode 'lsp-prog-major-mode-enable)
;; )


(provide '+dart)
;;; +dart.el ends here
