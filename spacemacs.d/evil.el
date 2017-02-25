(defun bb/evil-delete (orig-fn beg end &optional type _ &rest args)
  (interactive "<R><x><y>")
  (let (
        (text (filter-buffer-substring beg end))
        )
    (message "%s - %s | %s" beg end type)
    (apply orig-fn beg end type _ args)
    (unless (eq text nil)
      (evil-set-register ?z text)
      )
    (evil-set-register ?\" (evil-get-register ?0))
    ))

(defun averrin/evil-config()
  (global-evil-mc-mode 1)

  (vhl/define-extension 'evil 'evil-paste-after 'evil-paste-before
                        'evil-paste-pop 'evil-move)
  (vhl/install-extension 'evil)

  (define-key evil-normal-state-map (kbd "C-SPC") 'evil-scroll-page-down)
  (define-key evil-normal-state-map (kbd "C-S-SPC") 'evil-scroll-page-up)
  (define-key evil-normal-state-map (kbd "gj") 'pop-global-mark)
  (define-key evil-normal-state-map (kbd "q") 'kill-this-buffer)
  (define-key evil-normal-state-map (kbd "gp") (kbd "\"zp"))

  ;; (advice-add 'evil-delete :around 'bb/evil-delete)
  )
