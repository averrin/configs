(defun averrin/dired-config()
  ;; Dired
  (add-hook 'dired-mode-hook 'ao/dired-omit-caller)

  (eval-after-load "dired-mode")

  (averrin/dired-colors)

  (global-set-key (kbd "C-j") (kbd "RET"))
  (define-key dired-mode-map "l" (kbd "RET"))
  (define-key dired-mode-map ";" 'next-multiframe-window)
)

(defun averrin/dired-init()

  (package-initialize)
  (require 'dired-x) ; Enable dired-x
  (require 'dired+)  ; Enable dired+
  (require 'dired-rainbow)  ; Enable dired+
  (setq-default dired-omit-files-p t)  ; Don't show hidden files by default
  (setq dired-omit-files (concat dired-omit-files "\\|^\\..+$\\|\\.pyc$"))

  (add-hook 'dired-mode-hook 'ao/dired-omit-caller)

  (setq diredp-hide-details-initially-flag nil)
  (setq dired-listing-switches "-aBhl  --group-directories-first")

  (averrin/dired-colors)
  )

(defvar ao/v-dired-omit t
  "If dired-omit-mode enabled by default. Don't setq me.")

(defun ao/dired-omit-switch ()
  "This function is a small enhancement for `dired-omit-mode', which will
   \"remember\" omit state across Dired buffers."
  (interactive)
  (if (eq ao/v-dired-omit t)
      (setq ao/v-dired-omit nil)
    (setq ao/v-dired-omit t))
  (ao/dired-omit-caller)
  (when (equal major-mode 'dired-mode)
    (revert-buffer)))

(defun ao/dired-omit-caller ()
  (if ao/v-dired-omit
      (setq dired-omit-mode t)
    (setq dired-omit-mode nil)))

(defun ao/dired-back-to-top()
  "Move to the first file."
  (interactive)
  (beginning-of-buffer)
  (dired-next-line 1))

(defun ao/dired-jump-to-bottom()
  "Move to last file."
  (interactive)
  (end-of-buffer)
  (dired-next-line -1))

(defun averrin/dired-colors()

  (dired-rainbow-define html "#00a7b7" ("htm" "html" "xhtml"))
  (dired-rainbow-define dart "#A8C6ED" ("dart"))
  (dired-rainbow-define gen "#555" ("g.dart"))
  (dired-rainbow-define orig "#555" ("orig"))
  (dired-rainbow-define-chmod executable-unix "#A4CC37" "-[rw-]+x.*")


  (let ((class '((class color) (min-colors 89)))
      ;; Palette colors.
      (yellow-1 "#fce94f") (yellow-2 "#ffd700") (yellow-3 "#c4a000") (yellow-3-5 "#aaaa11") (yellow-4 "#875f00")
      (orange-1 "#ffaf5f") (orange-2 "#ff8700") (orange-3 "#ff5d17") (orange-4 "#d75f00") (orange-5 "#af5f00")
      (magenta-1 "#ff7bbb") (magenta-2 "#ff4ea3") (magenta-3 "#ff1f8b")
      (green-1 "#afff00") (green-2 "#a1db00") (green-3 "#5faf00") (green-4 "#008700") (green-5 "#005f00")
      (cyan-1 "#87ffff") (cyan-2 "#87d7af") (cyan-3 "#00d7af") (cyan-4 "#00ac8a") (cyan-5 "#5faf87") (cyan-6 "#005f5f") (cyan-7 "#236f73")
      (blue-1 "#84edb9") (blue-2 "#1f5bff") (blue-3 "#005f87") (blue-4 "#005faf") (blue-5 "#0000af") (blue-6 "#00005f")
      (purple-1 "#d18aff") (purple-2 "#af5fff") (purple-3 "#9a08ff") (purple-4 "#6c0099")
      (red-1 "#ef2929")  (red-2 "#dd0000")  (red-3 "#a40000") (red-4 "#5f0000")
      (white-1 "#c6c6c6") (white-2 "#c6c6c6") (white-3 "#b2b2b2") (black-1 "#a8a8a8") (black-2 "#8a8a8a")
      (black-2-5 "#6c6c6c") (black-3 "#4e4e4e") (black-4 "#3a3a3a") (black-5 "#303030") (black-6 "#000000")
      (LIGHT_BG "#fdfde7") (white-0 "#eeeeee")
      (green-02 "#5fd700") (green-01 "#d7ff00") (green-0 "#d7ff5f") (green-00 "#d7ff87")
      (cyan-0 "#d7ffd7")
      (blue-01 "#c3c9f8") (blue-0 "#afd7ff") (blue-00 "#d7d7ff")
      (yellow-0 "#ffff87") (yellow-00 "#ffffaf")
      (purple-0 "#af87ff") (purple-00 "#e6a8df")
      (red-0 "#ff4b4b") (red-00 "#ffafaf")
      (magenta-0 "#ffafd7") (magenta-00 "#ffd7ff")
      (orange-0 "#ffaf87") (orange-00 "#ffd787") (orange-000 "#ffd7af")
      )
  (custom-set-faces
    `(dired-directory ((,class (:foreground ,red-1 :bold t))))

    `(dired-flagged ((,class (:foreground ,red-1))))
    `(dired-header ((,class (:foreground ,black-5 :background ,green-2 :bold t))))
    `(dired-ignored ((,class (:foreground ,black-1))))
    `(dired-mark ((,class (:foreground ,green-1))))
    `(dired-marked ((,class (:foreground ,green-2))))
    `(dired-perm-write ((,class (:foreground ,red-2 :bold t))))
    `(dired-symlink ((,class (:foreground ,magenta-2))))
    `(dired-warning ((,class (:foreground ,white-1 :background ,red-3 :bold t))))

    `(diredp-compressed-file-suffix ((,class (:foreground ,purple-2))))
    `(diredp-date-time ((,class (:foreground ,blue-1 :background ,black-4))))
    `(diredp-deletion ((,class (:foreground ,white-1, :background ,red-3))))
    `(diredp-deletion-file-name ((,class (:foreground ,red-2))))
    `(diredp-dir-heading ((,class (:foreground ,green-2 :bold t))))
    `(diredp-dir-priv ((,class (:foreground ,orange-2 :bold t))))
    `(diredp-dir-name ((,class (:foreground ,yellow-3 :bold t))))

    `(diredp-display-msg ((,class (:foreground ,orange-2))))
    `(diredp-executable-tag ((,class (:foreground ,green-2))))
    `(diredp-file-name ((,class (:foreground ,white-1))))
    `(diredp-file-suffix ((,class (:foreground ,orange-2))))
    `(diredp-flag-mark ((,class (:foreground ,white-1 :background ,magenta-3 :bold t))))
    `(diredp-flag-mark-line ((,class (:foreground ,black-5 :background ,magenta-1))))
    `(diredp-ignored-file-name ((,class (:foreground ,black-2))))
    `(diredp-link-priv ((,class (:foreground ,magenta-3))))
    `(diredp-mode-line-flagged ((,class (:foreground ,black-5 :background ,green-2))))
    `(diredp-mode-line-marked ((,class (:foreground ,white-1 :background ,magenta-3 bold t))))
    `(diredp-no-priv ((,class (:foreground ,white-1 :background ,black-4))))
    `(diredp-number ((,class (:foreground ,yellow-1))))

    `(diredp-other-priv ((,class (:foreground ,white-1))))
    `(diredp-rare-priv ((,class (:foreground ,white-1))))
    `(diredp-symlink ((,class (:foreground ,magenta-3))))
    `(diredp-read-priv ((,class (:foreground ,green-2))))
    `(diredp-write-priv ((,class (:foreground ,blue-2))))
    `(diredp-exec-priv ((,class (:foreground ,red-2))))
  )
)
)
