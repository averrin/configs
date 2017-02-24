(defun averrin/dired-config()
  ;; Dired
  (add-hook 'dired-mode-hook 'ao/dired-omit-caller)
  (define-key evil-normal-state-map (kbd "_") 'projectile-dired)
  (define-key evil-normal-state-map (kbd "-") 'dired-jump)
  (define-key evil-normal-state-map (kbd ",p") (kbd "\"xp"))

  (eval-after-load "dired-mode"
    (evilified-state-evilify dired-mode dired-mode-map
      "K" 'dired-up-directory
      "f" 'helm-find-files
      "h" 'diredp-up-directory-reuse-dir-buffer
      "l" 'diredp-find-file-reuse-dir-buffer
      "I" 'ao/dired-omit-switch
      "gg" 'ao/dired-back-to-top
      "G" 'ao/dired-jump-to-bottom))
  )

(defun averrin/dired-init()

  (package-initialize)
  (require 'dired-x) ; Enable dired-x
  (require 'dired+)  ; Enable dired+
  (setq-default dired-omit-files-p t)  ; Don't show hidden files by default
  (setq dired-omit-files (concat dired-omit-files "\\|^\\..+$\\|\\.pyc$"))

  (global-set-key (kbd "C-j") (kbd "RET"))
  (add-hook 'dired-mode-hook 'ao/dired-omit-caller)

  (setq diredp-hide-details-initially-flag nil)
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
