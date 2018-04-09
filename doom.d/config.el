;;
;; Plugins
;;

(load! +bindings)

(def-package! centered-cursor-mode)
(def-package! evil-magit)
;; (def-package! indent-guide)
(def-package! highlight-indent-guides)
(def-package! ag
  :defer t
  :init
  (setq ag-highlight-search t
        ag-reuse-buffers t))

;; (if (string-equal system-name "spb-anabrodov")
    (def-package! dart-mode)
    (def-package! helm-dart :after dart-mode)
    (def-package! company-dart :after dart-mode)
    (add-hook! dart-mode
        (push 'dart-mode flycheck-global-modes)
    )
    (add-hook! dart-mode
        (set (make-local-variable 'company-backends)
            '(company-dart (company-dabbrev))))

    (add-hook! dart-mode
        (flycheck-mode)
    )
;; )

 (def-package! ivy-posframe
  :after (ivy)
  :config
  (setq ivy-display-function nil
        ivy-fixed-height-minibuffer nil)
  (push '(swiper . nil) ivy-display-functions-alist)
  (push '(t . ivy-posframe-display-at-frame-center) ivy-display-functions-alist)
  (setq ivy-posframe-parameters
        '((min-width . 120)
          (internal-border-width . 10))
        ivy-posframe-font (font-spec :family "Iosevka" :size 14 :width 'extra-condensed :weight 'normal :slant 'normal))
  (ivy-posframe-enable))

;;
;; Config
;;
(setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))
(setq highlight-indent-guides-method 'character)
(add-hook! prog-mode 'highlight-indent-guides-mode)
(setq dired-listing-switches "-alFh")
;(setq dired-use-ls-dired nil)
(defun save-all ()
  "Save hook"
  (interactive)
  (save-some-buffers t))

(setq doom-font (font-spec :family "Iosevka" :size 14))
(setq doom-theme 'doom-tomorrow-night)
(doom-themes-visual-bell-config)

(global-centered-cursor-mode +1)
;; (setq global-display-line-numbers-mode t)
;; (setq display-line-numbers-type 'relative)
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


(defun concatString (list)
  "A non-recursive function that concatenates a list of strings."
  (if (listp list)
      (let ((result ""))
        (dolist (item list)
          (if (stringp item)
              (setq result (concatenate 'string result (if (= (length item) 0) item (concat (if (= (position item list :test #'equal ) 0 ) item (substring item 0 1)) "/"))))))
        result)))

(defun +doom-modeline--buffer-file-name-relative (&optional include-project)
  "Propertized `buffer-file-name' showing directories relative to project's root only."
  (let ((root (doom-project-root))
        (active (active)))
    (if (null root)
        (propertize "%b" 'face (if active 'doom-modeline-buffer-file))
      (let* ((modified-faces (if (buffer-modified-p) 'doom-modeline-buffer-modified))
             (relative-dirs (file-relative-name (file-name-directory (file-truename buffer-file-name))
                                                (if include-project (concat root "../") root)))
             (relative-faces (or modified-faces (if active 'doom-modeline-buffer-path)))
             (file-faces (or modified-faces (if active 'doom-modeline-buffer-file))))
        (if (equal "./" relative-dirs) (setq relative-dirs ""))
        (concat (propertize (concatString (split-string relative-dirs "/")) 'face (if relative-faces `(:inherit ,relative-faces)))
                (propertize (file-name-nondirectory (file-truename buffer-file-name))
                            'face (if file-faces `(:inherit ,file-faces))))))))

(setq +doom-modeline-buffer-file-name-style 'relative-from-project)

(def-modeline! main
  (bar flycheck matches " " buffer-info "  " selection-info)
  ("  %l:%c %p  | " major-mode vcs))

(add-hook 'focus-out-hook 'save-all)

(setq show-trailing-whitespace t)
(add-hook! '(minibuffer-setup-hook doom-popup-mode-hook)
  (setq-local show-trailing-whitespace nil))

;; Magit config
(after! magit
  ;; Show differences at the word level when a hunk is selected.
  (setq magit-diff-refine-hunk t))
(add-hook! magit-mode (visual-line-mode +1))
