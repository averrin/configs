;;; config/default/config.el -*- lexical-binding: t; -*-

;;
;; Plugins
;;

(def-package! centered-cursor-mode)
(def-package! evil-magit)
(def-package! indent-guide)

;; (if (string-equal system-name "spb-anabrodov")
    (def-package! dart-mode)
    (def-package! helm-dart :after dart-mode)
    (add-hook! dart-mode
        (push 'dart-mode flycheck-global-modes)
    )
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
;; disable :unless predicates with (sp-pair "'" nil :unless nil)
;; disable :post-handlers with (sp-pair "{" nil :post-handlers nil)
;; ...or specific :post-handlers with (sp-pair "{" nil :post-handlers '(:rem ("| " "SPC")))
(after! smartparens
  ;; Autopair quotes more conservatively; if I'm next to a word/before another
  ;; quote, I likely don't want another pair.
  (let ((unless-list '(sp-point-before-word-p
                       sp-point-after-word-p
                       sp-point-before-same-p)))
    (sp-pair "'"  nil :unless unless-list)
    (sp-pair "\"" nil :unless unless-list))
  )

(indent-guide-global-mode)
  ;; Expand {|} => { | }
  ;; Expand {|} => {
  ;;   |
  ;; }
  (dolist (brace '("(" "{" "["))
    (sp-pair brace nil
             :post-handlers '(("||\n[i]" "RET") ("| " "SPC"))
             ;; I likely don't want a new pair if adjacent to a word or opening brace
             :unless '(sp-point-before-word-p sp-point-before-same-p)))

(setq buffer-save-without-query t)
(setq frame-title-format "e » %b")
(setq tab-width 2)
(setq web-mode-markup-indent-offset 2)
(setq helm-follow-mode-persistent 1)
(setq dartfmt-args (quote ("-l 120")))
(setq x-stretch-cursor t)
(setq word-wrap t)
(setq +doom-modeline-buffer-file-name-style 'relative-from-project)
  ;; Don't do square-bracket space-expansion where it doesn't make sense to
  (sp-local-pair '(emacs-lisp-mode org-mode markdown-mode gfm-mode)
                 "[" nil :post-handlers '(:rem ("| " "SPC")))


(def-modeline! main
  (bar matches " " buffer-info "  " selection-info)
  ("  %l:%c %p  | " major-mode vcs flycheck))

(add-hook 'focus-out-hook 'save-all)


;;
;; Bindings
;;

(setq expand-region-contract-fast-key "V")

;;
(map! [remap evil-jump-to-tag] #'projectile-find-tag
      [remap find-tag]         #'projectile-find-tag
      [remap newline]          #'newline-and-indent

      ;; Ensure there are no conflicts
      :nmvo doom-leader-key nil
      :nmvo doom-localleader-key nil

      ;; --- Global keybindings ---------------------------
      ;; Make M-x available everywhere
      :gnvime "M-x" #'execute-extended-command
      :gnvime "A-x" #'execute-extended-command

      ;; A little sandbox to run code in
      :gnvime "M-;" #'eval-expression
      :gnvime "M-:" #'doom/open-scratch-buffer

      ;; Text-scaling
      :ne "M-+"       (λ! (text-scale-set 0))
      :ne "M-="       #'text-scale-increase
      :ne "M--"       #'text-scale-decrease

      :ne "C-`"       #'+popup/toggle
      :ne "C-~"       #'+popup/raise
      :ne "M-t"       #'+workspace/new
      :ne "M-T"       #'+workspace/display
      :ne "M-w"       #'delete-window
      :ne "M-W"       #'delete-frame
      :ne "C-M-f"     #'toggle-frame-fullscreen
      :ne "M-n"       #'evil-buffer-new
      :ne "M-N"       #'make-frame
      :ne "M-1"       (λ! (+workspace/switch-to 0))
      :ne "M-2"       (λ! (+workspace/switch-to 1))
      :ne "M-3"       (λ! (+workspace/switch-to 2))
      :ne "M-4"       (λ! (+workspace/switch-to 3))
      :ne "M-5"       (λ! (+workspace/switch-to 4))
      :ne "M-6"       (λ! (+workspace/switch-to 5))
      :ne "M-7"       (λ! (+workspace/switch-to 6))
      :ne "M-8"       (λ! (+workspace/switch-to 7))
      :ne "M-9"       (λ! (+workspace/switch-to 8))
      :ne "M-0"       #'+workspace/switch-to-last

      ;; My page scrolling
      :nv "C-SPC" #'evil-scroll-page-down
      :nv "C-S-SPC" #'evil-scroll-page-up

      :n ";" #'evil-window-next


      ;; --- <leader> -------------------------------------
      (:leader
        :desc "M-x"                     :nv "SPC"  #'execute-extended-command

        ;; Most commonly used
        :desc "Save"                    :n "s" #'save-buffer
        :desc "Comment line"            :n "l" #'comment-line

        :desc "window"                  :n "w"  evil-window-map

        ;; :desc "Find in project"         :n "/"  #'swiper-all
        ;; :desc "Find word in project"    :n "*"  (λ! (swiper (symbol-name (symbol-at-point))))

        :desc "Find in project"         :n "/"  #'counsel-ag
        :desc "Find word in project"    :n "*"  (λ! (counsel-ag (symbol-name (symbol-at-point))))

        (:desc "flycheck" :prefix "e"
          :desc "Next error"            :n "n" #'flycheck-next-error
          :desc "Prev error"            :n "p" #'flycheck-previous-error
          )

        (:desc "lang" :prefix "m"
          :desc "Jump to def"          :n "j" #'dart-jump-to-defn
          :desc "Quick fix"          :n "q" #'helm-dart-quick-fix
          :desc "Format"          :n "f" #'dartfmt
          :desc "Sort"          :n "s" #'dart-sort-members
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


      ;; --- Personal vim-esque bindings ------------------
      :n  "zx" #'kill-this-buffer
      :n  "ZX" #'bury-buffer
      :n  "]b" #'next-buffer
      :n  "[b" #'previous-buffer
      :n  "]w" #'+workspace/switch-right
      :n  "[w" #'+workspace/switch-left
      :m  "gt" #'+workspace/switch-right
      :m  "gT" #'+workspace/switch-left
      :m  "gd" #'+lookup/definition
      :m  "gD" #'+lookup/references
      :m  "gh" #'+lookup/documentation
      :n  "gp" #'+evil/reselect-paste
      :n  "gr" #'+eval:region
      :n  "gR" #'+eval/buffer
      :v  "gR" #'+eval:replace-region
      :v  "@"  #'+evil:macro-on-all-lines
      :n  "g@" #'+evil:macro-on-all-lines
      ;; repeat in visual mode (FIXME buggy)
      :v  "."  #'evil-repeat
      ;; don't leave visual mode after shifting
      :v  "<"  #'+evil/visual-dedent  ; vnoremap < <gv
      :v  ">"  #'+evil/visual-indent  ; vnoremap > >gv

      :nv "C-a"   #'evil-numbers/inc-at-pt
      :nv "C-S-a" #'evil-numbers/dec-at-pt


      ;; --- Plugin bindings ------------------------------
      ;; auto-yasnippet
      :i  [C-tab] #'aya-expand
      :nv [C-tab] #'aya-create

      ;; company-mode (vim-like omnicompletion)
      :i "C-SPC"  #'+company/complete
      :i "TAB"  #'+company/complete
      (:prefix "C-x"
        :i "C-l"   #'+company/whole-lines
        :i "C-k"   #'+company/dict-or-keywords
        :i "C-f"   #'company-files
        :i "C-]"   #'company-etags
        :i "s"     #'company-ispell
        :i "C-s"   #'company-yasnippet
        :i "C-o"   #'company-capf
        :i "C-n"   #'company-dabbrev-code
        :i "C-p"   #'+company/dabbrev-code-previous)
      (:after company
        (:map company-active-map
          ;; Don't interfere with `evil-delete-backward-word' in insert mode
          "C-w"        nil
          "C-o"        #'company-search-kill-others
          "C-n"        #'company-select-next
          "C-p"        #'company-select-previous
          "C-h"        #'company-quickhelp-manual-begin
          "C-S-h"      #'company-show-doc-buffer
          "C-S-s"      #'company-search-candidates
          "C-s"        #'company-filter-candidates
          "C-SPC"      #'company-complete-common
          "C-h"        #'company-quickhelp-manual-begin
          [tab]        #'company-complete-common-or-cycle
          [backtab]    #'company-select-previous
          [escape]     (λ! (company-abort) (evil-normal-state 1)))
        ;; Automatically applies to `company-filter-map'
        (:map company-search-map
          "C-n"        #'company-search-repeat-forward
          "C-p"        #'company-search-repeat-backward
          "C-s"        (λ! (company-search-abort) (company-filter-candidates))
          [escape]     #'company-search-abort))

      ;; counsel
      (:after counsel
        (:map counsel-ag-map
          [backtab]  #'+ivy/wgrep-occur      ; search/replace on results
          "C-SPC"    #'ivy-call-and-recenter ; preview
          "M-RET"    (+ivy-do-action! #'+ivy-git-grep-other-window-action)))


      ;; evil-commentary
      :n  "gc"  #'evil-commentary

      ;; evil-exchange
      :n  "gx"  #'evil-exchange

      ;; evil-matchit
      :nv [tab] #'+evil/matchit-or-toggle-fold

      ;; evil-magit
      (:after evil-magit
        :map (magit-status-mode-map magit-revision-mode-map)
        :n "C-j" nil
        :n "C-k" nil)

      ;; evil-mc
      (:prefix "gz"
        :nv "m" #'evil-mc-make-all-cursors
        :nv "u" #'evil-mc-undo-all-cursors
        :nv "z" #'+evil/mc-make-cursor-here
        :nv "t" #'+evil/mc-toggle-cursors
        :nv "n" #'evil-mc-make-and-goto-next-cursor
        :nv "p" #'evil-mc-make-and-goto-prev-cursor
        :nv "N" #'evil-mc-make-and-goto-last-cursor
        :nv "P" #'evil-mc-make-and-goto-first-cursor
        :nv "d" #'evil-mc-make-and-goto-next-match
        :nv "D" #'evil-mc-make-and-goto-prev-match)
      (:after evil-mc
        :map evil-mc-key-map
        :nv "C-n" #'evil-mc-make-and-goto-next-cursor
        :nv "C-N" #'evil-mc-make-and-goto-last-cursor
        :nv "C-p" #'evil-mc-make-and-goto-prev-cursor
        :nv "C-P" #'evil-mc-make-and-goto-first-cursor)

      ;; evil-multiedit
      :v  "R"     #'evil-multiedit-match-all
      :n  "M-d"   #'evil-multiedit-match-symbol-and-next
      :n  "M-D"   #'evil-multiedit-match-symbol-and-prev
      :v  "M-d"   #'evil-multiedit-match-and-next
      :v  "M-D"   #'evil-multiedit-match-and-prev
      :nv "C-M-d" #'evil-multiedit-restore
      (:after evil-multiedit
        (:map evil-multiedit-state-map
          "M-d" #'evil-multiedit-match-and-next
          "M-D" #'evil-multiedit-match-and-prev
          "RET" #'evil-multiedit-toggle-or-restrict-region)
        (:map (evil-multiedit-state-map evil-multiedit-insert-state-map)
          "C-n" #'evil-multiedit-next
          "C-p" #'evil-multiedit-prev))

      ;; evil-snipe
      (:after evil-snipe
        :map evil-snipe-parent-transient-map
        ;; switch to evil-easymotion/avy after a snipe
        "C-;" (λ! (require 'evil-easymotion)
                  (call-interactively
                   (evilem-create #'evil-snipe-repeat
                                  :bind ((evil-snipe-scope 'whole-buffer)
                                         (evil-snipe-enable-highlight)
                                         (evil-snipe-enable-incremental-highlight))))))

      ;; evil-surround

      ;; expand-region
      :v  "v"  #'er/expand-region
      :v  "V"  #'er/contract-region

      ;; flycheck
      :m  "]e" #'next-error
      :m  "[e" #'previous-error
      (:after flycheck
        :map flycheck-error-list-mode-map
        :n "C-n" #'flycheck-error-list-next-error
        :n "C-p" #'flycheck-error-list-previous-error
        :n "j"   #'flycheck-error-list-next-error
        :n "k"   #'flycheck-error-list-previous-error
        :n "RET" #'flycheck-error-list-goto-error)

      ;; flyspell
      :m  "]S" #'flyspell-correct-word-generic
      :m  "[S" #'flyspell-correct-previous-word-generic
      (:after flyspell
        ;; Press RET on misspelled words to correct them
        (:map flyspell-mouse-map
          "RET" #'flyspell-correct-word-generic
          "<mouse-1>" #'flyspell-correct-word-generic))

      ;; git-gutter
      :m  "]d" #'git-gutter:next-hunk
      :m  "[d" #'git-gutter:previous-hunk

      ;; git-timemachine
      (:after git-timemachine
        (:map git-timemachine-mode-map
          :n "C-p" #'git-timemachine-show-previous-revision
          :n "C-n" #'git-timemachine-show-next-revision
          :n "[["  #'git-timemachine-show-previous-revision
          :n "]]"  #'git-timemachine-show-next-revision
          :n "q"   #'git-timemachine-quit
          :n "gb"  #'git-timemachine-blame))

      ;; helm
      (:after helm
        (:map helm-map
          "ESC"        nil
          "C-S-n"      #'helm-next-source
          "C-S-p"      #'helm-previous-source
          "C-u"        #'helm-delete-minibuffer-contents
          "C-w"        #'backward-kill-word
          "C-r"        #'evil-paste-from-register ; Evil registers in helm! Glorious!
          "C-b"        #'backward-word
          [left]       #'backward-char
          [right]      #'forward-char
          [escape]     #'helm-keyboard-quit
          [tab]        #'helm-execute-persistent-action)

        (:after helm-files
          (:map helm-generic-files-map
            :e "ESC"     #'helm-keyboard-quit)
          (:map helm-find-files-map
            "C-w" #'helm-find-files-up-one-level
            "TAB" #'helm-execute-persistent-action))

        (:after helm-ag
          (:map helm-ag-map
            "<backtab>"  #'helm-ag-edit)))

      ;; hl-todo
      :m  "]t" #'hl-todo-next
      :m  "[t" #'hl-todo-previous

      ;; ivy
      (:after ivy
        :map ivy-minibuffer-map
        [escape] #'keyboard-escape-quit
        "C-SPC"  #'ivy-call-and-recenter
        "M-z"    #'undo
        "M-v"    #'yank
        "C-v"    #'yank
        "C-r"    #'evil-paste-from-register
        "C-k"    #'ivy-previous-line
        "C-j"    #'ivy-next-line
        "C-A-k"  #'ivy-scroll-down-command
        "C-A-j"  #'ivy-scroll-up-command
        "C-l"    #'ivy-alt-done
        "C-w"    #'ivy-backward-kill-word
        "C-u"    #'ivy-kill-line
        "C-b"    #'backward-word
        "C-f"    #'forward-word)

      ;; rotate-text
      :n  "!"  #'rotate-text

      ;; smart-forward
      :nv "K"  #'smart-up
      :m  "g]" #'smart-forward
      :m  "g[" #'smart-backward

      ;; swiper
      (:after swiper
        (:map swiper-map
          [backtab]  #'+ivy/wgrep-occur))

      ;; undo-tree -- undo/redo for visual regions
      :v "C-u" #'undo-tree-undo
      :v "C-r" #'undo-tree-redo

      ;; yasnippet
      (:after yasnippet
        (:map yas-keymap
          "C-e"           #'+snippets/goto-end-of-field
          "C-a"           #'+snippets/goto-start-of-field
          "<M-right>"     #'+snippets/goto-end-of-field
          "<M-left>"      #'+snippets/goto-start-of-field
          "<M-backspace>" #'+snippets/delete-to-start-of-field
          [backspace]     #'+snippets/delete-backward-char
          [delete]        #'+snippets/delete-forward-char-or-field)
        (:map yas-minor-mode-map
          :ig "<tab>" yas-maybe-expand
          :v  "<tab>" #'yas-insert-snippet))


      ;; --- Major mode bindings --------------------------
      (:after markdown-mode
        (:map markdown-mode-map
          ;; fix conflicts with private bindings
          "<backspace>" nil
          "<M-left>"    nil
          "<M-right>"   nil))


      ;; --- Built-in plugins -----------------------------
      (:after comint
        ;; TAB auto-completion in term buffers
        :map comint-mode-map [tab] #'company-complete)

      (:after debug
        ;; For elisp debugging
        :map debugger-mode-map
        :n "RET" #'debug-help-follow
        :n "e"   #'debugger-eval-expression
        :n "n"   #'debugger-step-through
        :n "c"   #'debugger-continue)

      (:map help-mode-map
        :n "[["  #'help-go-back
        :n "]]"  #'help-go-forward
        :n "o"   #'ace-link-help
        :n "q"   #'quit-window
        :n "Q"   #'ivy-resume)

      (:after vc-annotate
        :map vc-annotate-mode-map
        :n "q"   #'kill-this-buffer
        :n "d"   #'vc-annotate-show-diff-revision-at-line
        :n "D"   #'vc-annotate-show-changeset-diff-revision-at-line
        :n "SPC" #'vc-annotate-show-log-revision-at-line
        :n "]]"  #'vc-annotate-next-revision
        :n "[["  #'vc-annotate-prev-revision
        :n "TAB" #'vc-annotate-toggle-annotation-visibility
        :n "RET" #'vc-annotate-find-revision-at-line))


;;
;; Keybinding fixes
;;

;; This section is dedicated to "fixing" certain keys so that they behave
;; properly, more like vim, or how I like it.

(map! (:map input-decode-map
        [S-iso-lefttab] [backtab]
        (:unless window-system "TAB" [tab])) ; Fix TAB in terminal

      ;; I want C-a and C-e to be a little smarter. C-a will jump to
      ;; indentation. Pressing it again will send you to the true bol. Same goes
      ;; for C-e, except it will ignore comments and trailing whitespace before
      ;; jumping to eol.
      :i "C-a" #'doom/backward-to-bol-or-indent
      :i "C-e" #'doom/forward-to-last-non-comment-or-eol
      :i "C-u" #'doom/backward-kill-to-bol-and-indent

      ;; textmate-esque newline insertion
      :i  [M-return]    #'evil-open-below
      :i  [S-M-return]  #'evil-open-above
      ;; textmate-esque deletion
      :ig [M-backspace] #'doom/backward-kill-to-bol-and-indent
      ;; Emacsien motions for insert mode
      :i  "C-b" #'backward-word
      :i  "C-f" #'forward-word
      :n "K" #'dired-jump

      (:after dired
        (:map dired-mode-map
          :n ";" #'evil-window-next
          :n "h" #'dired-up-directory
          :n "K" #'dired-up-directory
          :n "l" #'dired-find-file
         ))

      ;; Restore common editing keys (and ESC) in minibuffer
      (:map (minibuffer-local-map
             minibuffer-local-ns-map
             minibuffer-local-completion-map
             minibuffer-local-must-match-map
             minibuffer-local-isearch-map
             read-expression-map)
        [escape] #'abort-recursive-edit
        "C-r" #'evil-paste-from-register
        "C-a" #'move-beginning-of-line
        "C-w" #'doom/minibuffer-kill-word
        "C-u" #'doom/minibuffer-kill-line
        "C-b" #'backward-word
        "C-f" #'forward-word
        "M-z" #'doom/minibuffer-undo)

      (:after evil
        (:map evil-ex-completion-map
          "C-a" #'move-beginning-of-line
          "C-b" #'backward-word
          "C-f" #'forward-word))

      (:map messages-buffer-mode-map
        "M-;" #'eval-expression
        "A-;" #'eval-expression)

      (:after tabulated-list
        (:map tabulated-list-mode-map
          [remap evil-record-macro] #'quit-window))

      (:after view
        (:map view-mode-map "<escape>" #'View-quit-all)))
