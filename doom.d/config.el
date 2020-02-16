;;
;; Plugins
;;

(load! "+bindings")
(load! "+font")

(use-package! centered-cursor-mode)
(use-package! highlight-indent-guides)
(use-package! evil-magit)
(use-package! dired-single)
(use-package! spinner)
(use-package! dart-mode)
(add-hook 'dart-mode-hook 'lsp)
(use-package! diredfl)
(diredfl-global-mode t)
(use-package! perfect-margin)
;; (perfect-margin-mode 1)
(defcustom perfect-margin-ignore-regexps
  '("^minibuf" "^[*]")
  "List of strings to determine if window is ignored.
Each string is used as regular expression to match the window buffer name."
  :group 'perfect-margin)

(use-package! emojify)
(setq emojify-display-style "image")

(use-package! dimmer)
(dimmer-configure-which-key)
(dimmer-configure-helm)
(dimmer-mode t)
(setq dimmer-fraction 0.3)

(use-package! all-the-icons-ibuffer
  :ensure t
  :init (all-the-icons-ibuffer-mode 1))

(use-package! dired-rainbow
  :config
    (progn
        (dired-rainbow-define-chmod directory "#6cb2eb" "d.*")
        (dired-rainbow-define html "#eb5286" ("css" "less" "sass" "scss" "htm" "html" "jhtm" "mht" "eml" "mustache" "xhtml"))
        (dired-rainbow-define xml "#D8C360" ("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json" "msg" "pgn" "rss" "yaml" "yml" "rdata"))
        (dired-rainbow-define image "#f66d9b" ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png" "psd" "eps" "svg"))
        (dired-rainbow-define log "#c17d11" ("log"))
        (dired-rainbow-define shell "#f6993f" ("awk" "bash" "bat" "sed" "sh" "zsh" "vim"))
        (dired-rainbow-define interpreted "#4de0e5" ("py" "ipynb" "rb" "pl" "t" "msql" "mysql" "pgsql" "sql" "r" "clj" "cljs" "scala" "js" "dart"))
        (dired-rainbow-define compiled "#4DC0A1" ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp" "hxx" "m" "cc" "cs" "cp" "cpp" "go" "f" "for" "ftn" "f90" "f95" "f03" "f08" "s" "rs" "hi" "hs" "pyc" ".java"))
        (dired-rainbow-define compressed "#51d88a" ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
        (dired-rainbow-define vc "#0074d9" ("git" "gitignore" "gitattributes" "gitmodules"))
        (dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*")
        (dired-rainbow-define orig "#444444" ("orig"))
    )
  )
(defcustom perfect-margin-ignore-filters
  '(window-minibuffer-p)
  "List of functions to determine if window is ignored.
Each function is called with window as its sole arguemnt, returning a non-nil value indicate to ignore the window."
  :group 'perfect-margin)

(when (string= system-type "darwin")
  (setq dired-use-ls-dired nil))

(setq dired-k-human-readable t)

(with-eval-after-load "projectile"
  (add-to-list 'projectile-project-root-files-bottom-up "pubspec.yaml")
  (add-to-list 'projectile-project-root-files-bottom-up "BUILD"))

(setq lsp-auto-guess-root t)

(use-package! lsp-mode)
(use-package! lsp-ui
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
)

;  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
 ; (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)

(use-package! company-lsp
  :after lsp-mode
  :config
    (push 'company-lsp company-backends))

(setq ccls-executable "/usr/bin/ccls")
(setq vhdl-tool-bin-name "/usr/bin/vhdl-tool")

(use-package! clang-format
  :commands (clang-format-region)
  )


(setq lsp-auto-configure t)

;; ;;
;; Config
;;
(setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))
(setq highlight-indent-guides-method 'character)
(add-hook! prog-mode 'highlight-indent-guides-mode)
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
(evil-snipe-override-mode 1)
(push '(?\[ "[[{(]") evil-snipe-aliases)

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

(setq doom-modeline-buffer-file-name-style 'relative-from-project)
(setq doom-modeline-icon t)
(setq doom-modeline-env-version nil)

;;(after! doom-modeline 
;;(doom-modeline-def-modeline! 'main
;;  '(bar flycheck matches " " buffer-info "  " selection-info)
;;  '("  %l:%c %p  | " major-mode vcs))
;;)

(add-hook 'focus-out-hook 'save-all)
(add-hook 'after-focus-cahnge-function 'save-all)

(setq show-trailing-whitespace t)
(add-hook! '(minibuffer-setup-hook doom-popup-mode-hook)
  (setq-local show-trailing-whitespace nil))

(after! whitespace
  (advice-remove #'company-box--make-frame #'doom*fix-whitespace-mode-in-childframes)
  (advice-remove #'posframe--create-posframe #'doom*fix-whitespace-mode-in-childframes))
