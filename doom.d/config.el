;;
;; Plugins
;;

(load! "+bindings")
(load! "+icons-in-terminal")
(load! "+symbols")

(def-package! centered-cursor-mode)
(def-package! highlight-indent-guides)
(def-package! evil-magit)
(def-package! lsp-go)
(def-package! dired-single)
(def-package! spinner)
(def-package! dart-mode)
(add-hook 'dart-mode-hook 'lsp)

(with-eval-after-load "projectile"
  (add-to-list 'projectile-project-root-files-bottom-up "pubspec.yaml")
  (add-to-list 'projectile-project-root-files-bottom-up "BUILD"))

(setq lsp-auto-guess-root t)
;; (def-package! ivy-posframe
;;   :after (ivy)
;;   :config
;;   (setq ivy-display-function nil
;;         ivy-fixed-height-minibuffer nil)
;;   (push '(swiper . nil) ivy-display-functions-alist)
;;   (push '(t . ivy-posframe-display-at-frame-center) ivy-display-functions-alist)
;;   (setq ivy-posframe-parameters
;;         '((min-width . 120)
;;           (internal-border-width . 10))
;;         ivy-posframe-font (font-spec :family "Iosevka" :size 14 :width 'extra-condensed :weight 'normal :slant 'normal))
;;   (ivy-posframe-enable))

(def-package! cquery
  :hook (c-mode-common . +cc|init-cquery)
  :config
  (defun +cc|init-cquery ()
    (when (memq major-mode '(c-mode c++-mode))
      (flycheck-mode)
      (lsp-cquery-enable)))
  (setq cquery-executable "~/.local/bin/cquery"))


(def-package! lsp-mode)
(def-package! lsp-ui
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
)

;  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
 ; (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)

(def-package! company-lsp
  :after lsp-mode
  :config
    (push 'company-lsp company-backends))

(setq ccls-executable "/usr/sbin/ccls")

(def-package! clang-format
  :commands (clang-format-region)
  )

;; (def-package! ccls
;;   :init (add-hook! (c-mode c++-mode objc-mode) #'lsp-ccls-enable)
;;   :when (featurep! :lang cc)
;;   :after-call (c-mode c++-mode c-mode)
;;   :commands lsp-ccls-enable
;;   :hook ((c-mode . +lsp-ccls//c-modes)
;;          (c++-mode . +lsp-ccls//c-modes))
;;   :config
;;   ;; overlay is slow
;;   ;; Use https://github.com/emacs-mirror/emacs/commits/feature/noverlay
;;   (setq ccls-sem-highlight-method 'font-lock)
;;   (ccls-use-default-rainbow-sem-highlight)
;;   ;; https://github.com/maskray/ccls/blob/master/src/config.h
;;   (setq ccls-extra-init-params '(
;;           :completion (:detailedLabel t)
;;           :diagnostics (:frequencyMs 5000)
;;           :index (:reparseForDependency 1)))

;;   (with-eval-after-load 'projectile
;;     (add-to-list 'projectile-globally-ignored-directories ".ccls-cache"))

;;   (evil-set-initial-state 'ccls-tree-mode 'emacs)
;;   (set-company-backend! '(c-mode c++-mode) 'company-lsp)
;;   )

;; (load! +dart)


(setq lsp-auto-guess-root t)
(setq lsp-auto-configure t)
;; (if (string-equal system-name "spb-anabrodov")
;; (defun averrin//dart-mode-enable ()
;;   "Init dart-mode"
;; ;;   (lsp-define-stdio-client
;; ;;     lsp-dart-major-mode
;; ;;     "dart"
;; ;;     (lambda () default-directory)
;; ;;     '("~/.pub-cache/bin/dart_language_server"))
;; (lsp-define-stdio-client
;;  :name lsp-dart
;;  :language-id "dart"
;;  :command "~/.pub-cache/bin/dart_language_server")
;; (lsp-register-client
;;  (make-lsp-client :new-connection (lsp-stdio-connection "~/.pub-cache/bin/dart_language_server")
;;                   :major-modes '(dart-mode)
;;                   :server-id 'lsp-dart))

;;   ;; (push 'dart-mode flycheck-global-modes)
;;   (set (make-local-variable 'company-backends)
;;       '(company-lsp (company-dabbrev)))
;;   (flycheck-mode)
;;   ;; (lsp-dart-major-mode-enable)
;;   ;; (setq lsp-ui-sideline-code-actions-prefix "💡 ")
;; )

;; (def-package! dart-mode
;;     :after company-lsp
;;     :hook (dart-mode . averrin//dart-mode-enable)
;; )

(defun my-set-projectile-root ()
  (when lsp--cur-workspace
    (setq projectile-project-root (lsp--workspace-root lsp--cur-workspace))))
(add-hook 'lsp-before-open-hook #'my-set-projectile-root)

;; ;;
;; Config
;;
(setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))
(setq highlight-indent-guides-method 'character)
(add-hook! prog-mode 'highlight-indent-guides-mode)
(setq dired-listing-switches "-alFh")
(setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)

(defun save-all ()
  (interactive)
  (save-some-buffers t))

(defun full-auto-save ()
  (interactive)
  (save-excursion
    (dolist (buf (buffer-list))
      (set-buffer buf)
      (if (and (buffer-file-name) (buffer-modified-p))
          (basic-save-buffer)))))
(add-hook 'auto-save-hook 'full-auto-save)
(auto-save-visited-mode t)

(setq doom-font (font-spec :family "Iosevka" :size 14))
(setq doom-theme 'doom-tomorrow-night)
(doom-themes-visual-bell-config)

(global-centered-cursor-mode +1)
(setq doom-line-numbers-style 'relative)
(add-hook! dart-mode #'doom|enable-line-numbers)

(setq buffer-save-without-query t)
(setq frame-title-format "doom | %b")
(setq tab-width 2)
(setq web-mode-markup-indent-offset 2)
(setq helm-follow-mode-persistent 1)
(setq dartfmt-args (quote ("-l 120")))
(setq x-stretch-cursor t)
(setq word-wrap t)

(setq evil-snipe-scope 'buffer)
(setq evil-snipe-repeat-scope 'buffer)

(setq ein:jupyter-default-server-command "jupyter")
(setq ein:jupyter-default-notebook-directory "~/projects/my-comic-books/data")

(defun concatString (list)
  "A non-recursive function that concatenates a list of strings."
  (if (listp list)
      (let ((result ""))
        (dolist (item list)
          (if (stringp item)
              (setq result (concatenate 'string result (if (= (length item) 0) item (concat (if (= (position item list :test #'equal ) 0 ) item (substring item 0 1)) "/"))))))
        result)))

(defun +doom-modeline--buffer-file-name-relative (_file-path true-file-path &optional include-project)
  "Propertized `buffer-file-name' showing directories relative to project's root only."
  (let ((root (doom-project-root))
        (active (active)))
    (if (null root)
        (propertize "%b" 'face (if active 'doom-modeline-buffer-file))
      (let* ((modified-faces (if (buffer-modified-p) 'doom-modeline-buffer-modified))
             (relative-dirs (file-relative-name (file-name-directory true-file-path)
                                                (if include-project (concat root "../") root)))
             (relative-faces (or modified-faces (if active 'doom-modeline-buffer-path)))
             (file-faces (or modified-faces (if active 'doom-modeline-buffer-file))))
        (if (equal "./" relative-dirs) (setq relative-dirs ""))
        (concat (propertize (concatString (split-string relative-dirs "/")) 'face (if relative-faces `(:inherit ,relative-faces)))
                (propertize (file-name-nondirectory true-file-path)
                            'face (if file-faces `(:inherit ,file-faces))))
        )
      )
    )
  )

(setq +doom-modeline-buffer-file-name-style 'relative-from-project)

(def-modeline! 'main
  '(bar flycheck matches " " buffer-info "  " selection-info)
  '("  %l:%c %p  | " major-mode vcs))

(add-hook 'focus-out-hook 'save-all)

(setq show-trailing-whitespace t)
(add-hook! '(minibuffer-setup-hook doom-popup-mode-hook)
  (setq-local show-trailing-whitespace nil))

;; Magit config
(after! magit
  ;; Show differences at the word level when a hunk is selected.
  (setq magit-diff-refine-hunk t))
(add-hook! magit-mode (visual-line-mode +1))

(after! whitespace
  (advice-remove #'company-box--make-frame #'doom*fix-whitespace-mode-in-childframes)
  (advice-remove #'posframe--create-posframe #'doom*fix-whitespace-mode-in-childframes))
