(defun averrin/evil-config()
  (global-evil-mc-mode 1)
  (evil-goggles-mode)
  (require 'diff-mode) ;; load diff-* faces
  (setq evil-goggles-faces-alist `(
    ( evil-delete . diff-removed )
    ( evil-yank . diff-changed )
    ( evil-paste-after . diff-added )
    ( evil-paste-before . diff-added )))

  (vhl/define-extension 'evil 'evil-paste-after 'evil-paste-before
                        'evil-paste-pop 'evil-move)
  (vhl/install-extension 'evil)

  (define-key evil-normal-state-map (kbd "C-SPC") 'evil-scroll-page-down)
  (define-key evil-normal-state-map (kbd "C-S-SPC") 'evil-scroll-page-up)
  (define-key evil-normal-state-map (kbd "m") 'bmkp-toggle-autonamed-bookmark-set/delete)
  (define-key evil-normal-state-map (kbd "gj") 'bmkp-next-autonamed-bookmark-repeat)
  (define-key evil-normal-state-map (kbd "gJ") 'bmkp-previous-autonamed-bookmark-repeat)
  (define-key evil-normal-state-map (kbd "q") 'kill-this-buffer)
  (define-key evil-normal-state-map (kbd "Q") 'delete-window)
  (define-key evil-normal-state-map (kbd "K") 'dired-jump)
  (define-key evil-normal-state-map (kbd "/") 'swiper)
  (define-key evil-normal-state-map (kbd "gp") (kbd "\"zp"))
  )
