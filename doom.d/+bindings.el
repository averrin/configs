;;; +bindings.el --- description -*- lexical-binding: t; -*-

;;
;; Bindings
;;
(defun window-half-height ()
     (max 1 (/ (1- (window-height (selected-window))) 2)))

   (defun scroll-up-half ()
     (interactive)
     (scroll-up (window-half-height)))

   (defun scroll-down-half ()
     (interactive)
     (scroll-down (window-half-height)))


(defun append-semicolon-insert ()
  (interactive)
  (if (looking-at-p ".*;[ \t]*$")
    (evil-next-line)
    (progn
         (end-of-line)
         (insert ";\n"))
  ))

(defun append-semicolon-normal ()
  (interactive)
  (if (looking-at-p ".*;[ \t]*$")
    (evil-next-line)
    (progn
        (end-of-line)
        (insert ";")
        (evil-force-normal-state)
        (evil-next-line))
  ))

(map! [remap evil-jump-to-tag] #'projectile-find-tag
      [remap find-tag]         #'projectile-find-tag
      [remap newline]          #'newline-and-indent

      :n "K" #'dired-jump
      :n ";" #'evil-window-next
      :i "C-j" #'append-semicolon-insert
      :n "C-j" #'append-semicolon-normal

      ;; :n "M-j" #'scroll-down-half
      ;; :n "M-k" #'scroll-up-half

      ;; My page scrolling
      :nv "C-SPC" #'evil-scroll-page-down
      :nv "C-S-SPC" #'evil-scroll-page-up

      (:after dired
        (:map dired-mode-map
          :n ";" #'evil-window-next
          :n "K" #'dired-single-up-directory
          :n "l" #'dired-single-buffer
        ))

      ;; --- <leader> -------------------------------------
      (:leader
        :desc "M-x"                     :nv "SPC"  #'execute-extended-command

        :nv "J" #'evil-avy-goto-line
        :nv "j" #'evil-avy-goto-subword-1

        ;; Most commonly used
        :desc "Save"                    :n "s" #'save-buffer
        :desc "Comment line"            :nv "l" #'evilnc-comment-or-uncomment-lines
        :desc "Replace regex"           :n "r" #'anzu-query-replace-regexp

        :desc "window"                  :n "w"  evil-window-map
        :desc "Next window"             :n ";" #'evil-window-next

        ;; :desc "Find in project"         :n "/"  #'swiper-all
        ;; :desc "Find word in project"    :n "*"  (λ! (swiper (symbol-name (symbol-at-point))))

        :desc "Find in project"         :n "/"  #'counsel-rg
        :desc "Find word in project"    :n "*"  (λ! (counsel-rg (symbol-name (symbol-at-point))))

        (:desc "flycheck" :prefix "e"
          :desc "Next error"            :n "n" #'flycheck-next-error
          :desc "Prev error"            :n "p" #'flycheck-previous-error
          )

        ;;TODO: only dart-mode
        (:desc "lang" :prefix "m"
          :desc "Jump to def"          :n "j" #'xref-find-definitions
          :desc "Find refs"          :n "r" #'xref-find-references
          :desc "Quick fix"          :n "q" #'lsp-ui-sideline-apply-code-actions
          :desc "Format"          :n "f" #'dartfmt
          )

        (:desc "workspace" :prefix "TAB"
          :desc "Display tab bar"          :n "TAB" #'+workspace/display
          :desc "New workspace"            :n "n"   #'+workspace/new
          :desc "Load workspace from file" :n "l"   #'+workspace/load
          :desc "Load last session"        :n "L"   (λ! (+workspace/load-session))
          :desc "Save workspace to file"   :n "s"   #'+workspace/save
          :desc "Autosave current session" :n "S"   #'+workspace/save-session
          :desc "Switch workspace"         :n "."   #'+workspace/switch-to
          :desc "Kill all buffers"         :n "x"   #'doom/kill-all-buffers
          :desc "Delete session"           :n "X"   #'+workspace/kill-session
          :desc "Delete this workspace"    :n "d"   #'+workspace/delete
          :desc "Load session"             :n "L"   #'+workspace/load-session
          :desc "Next workspace"           :n "]"   #'+workspace/switch-right
          :desc "Previous workspace"       :n "["   #'+workspace/switch-left
          :desc "Rename"                   :n "r"   #'+workspace/rename
          :desc "Switch to 1st workspace"  :n "1"   (λ! (+workspace/switch-to 0))
          :desc "Switch to 2nd workspace"  :n "2"   (λ! (+workspace/switch-to 1))
          :desc "Switch to 3rd workspace"  :n "3"   (λ! (+workspace/switch-to 2))
          :desc "Switch to 4th workspace"  :n "4"   (λ! (+workspace/switch-to 3))
          :desc "Switch to 5th workspace"  :n "5"   (λ! (+workspace/switch-to 4))
          :desc "Switch to 6th workspace"  :n "6"   (λ! (+workspace/switch-to 5))
          :desc "Switch to 7th workspace"  :n "7"   (λ! (+workspace/switch-to 6))
          :desc "Switch to 8th workspace"  :n "8"   (λ! (+workspace/switch-to 7))
          :desc "Switch to 9th workspace"  :n "9"   (λ! (+workspace/switch-to 8))
          :desc "Switch to last workspace" :n "0"   #'+workspace/switch-to-last)

        (:desc "buffer" :prefix "b"
          :desc "New empty buffer"        :n "n" #'evil-buffer-new
          :desc "Switch workspace buffer" :n "b" #'persp-switch-to-buffer
          :desc "Switch buffer"           :n "B" #'switch-to-buffer
          :desc "Kill buffer"             :n "k" #'kill-this-buffer
          :desc "Kill other buffers"      :n "o" #'doom/kill-other-buffers
          :desc "Save buffer"             :n "s" #'save-buffer
          :desc "Pop scratch buffer"      :n "x" #'doom/open-scratch-buffer
          :desc "Bury buffer"             :n "z" #'bury-buffer
          :desc "Next buffer"             :n "n" #'next-buffer
          :desc "Previous buffer"         :n "p" #'previous-buffer
          :desc "Sudo edit this file"     :n "S" #'doom/sudo-this-file)

        (:desc "code" :prefix "c"
          :desc "Format"                    :n  "f" #'lsp-format-buffer
          :desc "List errors"               :n  "x" #'flycheck-list-errors
          :desc "Evaluate buffer/region"    :n  "e" #'+eval/buffer
                                            :v  "e" #'+eval/region
          :desc "Evaluate & replace region" :nv "E" #'+eval:replace-region
          :desc "Build tasks"               :nv "b" #'+eval/build
          :desc "Jump to definition"        :n  "d" #'+lookup/definition
          :desc "Jump to references"        :n  "D" #'+lookup/references
          :desc "Open REPL"                 :n  "r" #'+eval/open-repl
          :desc "Comment line"              :n  "l" #'comment-line
                                            :v  "r" #'+eval:repl)

        (:desc "file" :prefix "f"
          :desc "Find file"                 :n "." #'find-file
          :desc "Sudo find file"            :n ">" #'doom/sudo-find-file
          :desc "Find file in project"      :n "/" #'projectile-find-file
          :desc "Find file from here"       :n "?" #'counsel-file-jump
          :desc "Find other file"           :n "a" #'projectile-find-other-file
          :desc "Open project editorconfig" :n "c" #'editorconfig-find-current-editorconfig
          :desc "Find directory"            :n "d" #'dired
          :desc "Find file in emacs.d"      :n "e" #'+default/find-in-emacsd
          :desc "Browse emacs.d"            :n "E" #'+default/browse-emacsd
          :desc "Recent files"              :n "r" #'recentf-open-files
          :desc "Recent project files"      :n "R" #'projectile-recentf
          :desc "Yank filename"             :n "y" #'+default/yank-buffer-filename
          (:when (featurep! :config private)
            :desc "Find file in private config" :n "p" #'+private/find-in-config
            :desc "Browse private config"       :n "P" #'+private/browse-config))

        (:desc "git" :prefix "g"
          :desc "Git status"            :n  "S" #'magit-status
          :desc "Git blame"             :n  "b" #'magit-blame
          :desc "Git diff"              :n  "d" #'magit-diff
          :desc "Git time machine"      :n  "t" #'git-timemachine-toggle
          :desc "Git stage hunk"        :n  "s" #'git-gutter:stage-hunk
          :desc "Git revert hunk"       :n  "r" #'git-gutter:revert-hunk
          :desc "Git revert buffer"     :n  "R" #'vc-revert
          :desc "List gists"            :n  "g" #'+gist:list
          :desc "Next hunk"             :nv "]" #'git-gutter:next-hunk
          :desc "Previous hunk"         :nv "[" #'git-gutter:previous-hunk)

        (:desc "insert" :prefix "i"
          :desc "From kill-ring"        :nv "y" #'counsel-yank-pop
          :desc "From evil registers"   :nv "r" #'counsel-evil-registers
          :desc "From snippet"          :nv "s" #'yas-insert-snippet)

        (:desc "notes" :prefix "n"
          :desc "Find file in notes"    :n  "n" #'+default/find-in-notes
          :desc "Browse notes"          :n  "N" #'+default/browse-notes
          :desc "Org capture"           :n  "x" #'org-capture
          :desc "Browse mode notes"     :n  "m" #'+org/browse-notes-for-major-mode
          :desc "Browse project notes"  :n  "p" #'+org/browse-notes-for-project)

        (:desc "project" :prefix "p"
          :desc "Browse project"          :n  "." #'+default/browse-project
          :desc "Find file in project"    :n  "/" #'projectile-find-file
          :desc "Find file in project"    :n  "f" #'projectile-find-file
          :desc "Run cmd in project root" :nv "!" #'projectile-run-shell-command-in-root
          :desc "Switch project"          :n  "p" #'projectile-switch-project
          :desc "Recent project files"    :n  "r" #'projectile-recentf
          :desc "List project tasks"      :n  "t" #'+ivy/tasks
          :desc "Invalidate cache"        :n  "x" #'projectile-invalidate-cache)

        (:desc "quit" :prefix "q"
          :desc "Save and quit"          :n "q" #'evil-save-and-quit
          :desc "Quit (forget session)"  :n "Q" #'+workspace/kill-session-and-quit)

        (:desc "toggle" :prefix "t"
          :desc "Flyspell"               :n "s" #'flyspell-mode
          :desc "Flycheck"               :n "f" #'flycheck-mode
          :desc "Line numbers"           :n "l" #'doom/toggle-line-numbers
          :desc "Frame fullscreen"       :n "F" #'toggle-frame-fullscreen
          :desc "Indent guides"          :n "i" #'highlight-indentation-mode
          :desc "Indent guides (column)" :n "I" #'highlight-indentation-current-column-mode
          :desc "Impatient mode"         :n "h" #'+impatient-mode/toggle
          :desc "Big mode"               :n "b" #'doom-big-font-mode
          :desc "Evil goggles"           :n "g" #'+evil-goggles/toggle
          :desc "org-tree-slide mode"    :n "p" #'+org-present/start))
)

;;; +bindings.el ends here
