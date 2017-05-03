(defun averrin/evil-config()
  (global-evil-mc-mode 1)

  (vhl/define-extension 'evil 'evil-paste-after 'evil-paste-before
                        'evil-paste-pop 'evil-move)
  (vhl/install-extension 'evil)

  (define-key evil-normal-state-map (kbd "C-SPC") 'evil-scroll-page-down)
  (define-key evil-normal-state-map (kbd "C-S-SPC") 'evil-scroll-page-up)
  (define-key evil-normal-state-map (kbd "gj") 'pop-global-mark)
  (define-key evil-normal-state-map (kbd "q") 'kill-this-buffer)
  (define-key evil-normal-state-map (kbd "Q") 'delete-window)
  (define-key evil-normal-state-map (kbd "K") 'dired-jump)
  (define-key evil-normal-state-map (kbd "/") 'swiper)
  (define-key evil-normal-state-map (kbd "gp") (kbd "\"zp"))
  )
