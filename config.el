;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

; Since I use fish shell, add this. From `doom doctor': Fish (and possibly other
; non-POSIX shells) is known to inject garbage output into some of the child
; processes that Emacs spawns. Many Emacs packages/utilities will choke on this
; output, causing unpredictable issues.
(setq shell-file-name (executable-find "bash"))

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Phil Piwonka"
      user-mail-address "philpiwonka@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:

;;; Font settings

;; Get fonts from here:
;; https://github.com/ryanoasis/nerd-fonts
;; For Mac:
;; brew tap homebrew/cask-fonts && brew install --cask font-blex-mono-nerd-font
(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 15)
      doom-big-font (font-spec :family "JetBrainsMono Nerd Font" :size 20))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-nord-aurora)
;; NOTE: doom-theme isn't needed to by set thanks to auto-dark
;; auto-dark will follow system preferences to flip between
;; two themes for day/night
(if (display-graphic-p)
    (progn
      (use-package! auto-dark
        :config
        (auto-dark-mode t)
        :custom
        (auto-dark-dark-theme 'doom-feather-dark)
        (auto-dark-light-theme 'doom-flatwhite)
        ))
    ;; else for terminals
  (setq doom-theme 'doom-feather-dark))

;; I don't like the menu bar
(menu-bar-mode -1)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

;; Make it so I can pull up M-x more easily
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-xm" 'execute-extended-command)

;; Make jumping around lists/parens easy
(global-set-key "\M-n" 'forward-list)
(global-set-key "\M-p" 'backward-list)

(global-set-key "\C-s" 'consult-line)

;; Display continuous lines
(setq-default truncate-lines nil)
;; truncate even even when screen is split into multiple windows
(setq-default truncate-partial-width-windows nil)

;; Various Preferences
(setq kill-whole-line t)

;; Aliases
;;
(defalias 'qrr 'query-replace-regexp)
(defalias 'edb 'ediff-buffers)
(defalias 'afm 'auto-fill-mode)
(defalias 'gits 'magit-status)
(defalias 'kr 'helm-show-kill-ring)

(setq confirm-kill-emacs 'y-or-n-p)

;;; ORG
(use-package! org
  :config
  (progn
    (defun pdp-org-archive-done-tasks ()
      (interactive)
      ; take all entries that are over 7 days old and archive them
      ; I just want to end the endless build up
      ;
      ; Mark items as DONE
      (org-ql-select (concat org-directory "inbox.org")
        '(and
              (todo "TODO")
              (not (ts :from -10)) ; older than 10 days
              (not (priority >= "C")) ; has no priority
              (not (scheduled)) ; isn't scheduled
              (not (deadline)) ; doesn't have a deadline
              (not (tags "email")) ; doesn't have an email tag
        )
        :action '(org-todo "DONE") ; mark it as done
        )

      ; Archive DONE items
      ; I was using org-ql-select but you had to run it multiple times to
      ; archive everything because the shifting of items in the file as things
      ; got archived confused it. From: https://stackoverflow.com/a/27043756
      (org-map-entries
       (lambda ()
         (org-archive-subtree)
         (setq org-map-continue-from (org-element-property :begin (org-element-at-point))))
       "/DONE" 'file)
      )
    (global-set-key "\C-cnh" 'pdp-org-archive-done-tasks)
    (defun pdp-org-autoarchive ()
      (when (derived-mode-p 'org-mode)
        (save-excursion
          (goto-char 0)
          (if (string-equal (car
                             (cdr
                              (car
                               (org-collect-keywords '("AUTOARCHIVE")))))
                            "t")
              (progn
                (pdp-org-archive-done-tasks))))))
    (add-hook 'before-save-hook #'pdp-org-autoarchive)
    (setq org-agenda-sorting-strategy '(time-up priority-down category-up))
    (setq org-agenda-skip-deadline-if-done t)
    (setq org-agenda-skip-scheduled-if-done t)
    (setq org-agenda-window-setup 'current-window)
    (setq org-deadline-warning-days 0)

    (setq org-use-property-inheritance t)

    (setq org-export-section-number-format '((("1" "."))
                                             . " - "))

    (setq org-capture-templates
          '(
            ("t" "Todo" entry (file (lambda () (concat org-directory "inbox.org")))
             "* TODO %?\n%u\n%i\n")
          ))

    (add-hook 'org-mode-hook #'turn-off-auto-fill)
    (setq org-log-done 'time)
    (setq org-refile-use-outline-path nil)
    (setq org-agenda-start-on-weekday nil)

    (setq org-todo-keywords
          '((sequence "TODO(t)" "|" "DONE(d)" "CANCELLED(c)")))
    (setq org-use-fast-todo-selection t)
    ;;; Bind C-z to org-toggle-narrow-to-subtree
    ;;; https://taonaw.com/2022-05-24/
    (global-set-key "\C-z" 'org-toggle-narrow-to-subtree)
    (add-hook 'auto-save-hook 'org-save-all-org-buffers)
    ; I only want one priority
    (setq org-default-priority 66
          org-priority-highest 66
          org-priority-lowest 66)
    ;;; Org ligatures
    (add-hook 'org-mode-hook (lambda ()
                               "Beautify Org Checkbox Symbol"
                               (push '("#+TITLE:" . "") prettify-symbols-alist)
                               (push '("#+title:" . "") prettify-symbols-alist)
                               (push '("SCHEDULED:" . "⧗") prettify-symbols-alist)
                               (push '("DEADLINE:" . "⌘") prettify-symbols-alist)
                               (push '("COMPLETED:" . "✓") prettify-symbols-alist)
                               (push '("CLOSED:" . "✓") prettify-symbols-alist)
                               (push '(":PROPERTIES:" . "") prettify-symbols-alist)
                               (push '(":END:" . "↤") prettify-symbols-alist)
                               (push '("#+begin_src" . "✎") prettify-symbols-alist)
                               (push '("#+end_src" . "↵") prettify-symbols-alist)
                               (push '("#+begin_quote" . "✎") prettify-symbols-alist)
                               (push '("#+end_quote" . "↵") prettify-symbols-alist)
                               (prettify-symbols-mode)))

    ;;; Change some color stuff
    (custom-set-faces
     '(org-todo ((t (:background "#4F894C" :distant-foreground "#4F894C" :foreground "#eef" :weight bold))))
     ; I don't want my org headers to be big
     '(outline-1 ((t (:height 1.0))))
     '(outline-2 ((t (:height 1.0))))
     '(outline-3 ((t (:height 1.0))))
     )

    (setq org-ellipsis "↴")
    ;;; Quick function to open my inbox.org file
    (defun pdp-open-inbox ()
      (interactive)
      (find-file (expand-file-name (concat org-directory "/inbox.org")))
      (revert-buffer))

    ;;; Let's use SUPER for handy shit.
    (global-set-key (kbd "s-n") (lambda () (interactive) (org-capture nil "t")))
    (global-set-key (kbd "s-a") (lambda () (interactive) (org-agenda nil "p")))
    (global-set-key (kbd "s-i") 'pdp-open-inbox)
  )
)

(after! markdown
  (add-hook 'markdown-mode-hook #'turn-off-auto-fill)
  )

(use-package! org-roam
  :after org
  :ensure t
  :custom
  (org-roam-directory (file-truename (concat org-directory "roam")))
  :config
  ;;; Let each machine have it's own DB cache
  ;;; Borrowed from https://www.reddit.com/r/orgmode/comments/kocvjb/can_i_sync_orgroam_across_devices_if_so_what_is/
  (setq org-roam-db-location (expand-file-name (concat "org-roam." (system-name) ".db") org-roam-directory))

  ;;; CMD-o to open org roam
  (global-set-key (kbd "s-o") 'org-roam-node-find)

  (org-roam-db-autosync-enable)
  ;;; Capture templates
  (setq org-roam-capture-templates
        '(
          ("d" "default" plain "%?"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}")
           :unnarrowed t)
          ("p" "person" plain "%?"
           :target (file+head "people/%<%Y%m%d%H%M%S>-${slug}.org"
                                        ; I name these files after the persons work alias, then put their full name under "ROAM_ALIASES"
                              ":PROPERTIES:\n:ROAM_ALIASES: \"${fullname}\"\n:END:\n#+title: ${title}\n\n- tags :: [[id:dfd98009-3b6a-4f32-8235-00131e66918c][People]]")
           :unnarrowed t)
          ("l" "plan" plain "%?"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                                        ; I name these files after the persons work alias, then put their full name under "ROAM_ALIASES"
                              "#+title: ${title}\n\n- tags :: [[id:95f115f1-c0c8-4cd3-8328-f85df945d469][.plan]]\n\n* To Try On\n\n* Daily\n** Monday\n** Tuesday\n** Wednesday\n** Thursday\n** Friday\n* Bright Spots")
           :unnarrowed t)
          )
        )
  )

;;; Stolen from here:
;;; https://github.com/sunnyhasija/Academic-Doom-Emacs-Config/blob/master/config.el
;;; I like these bindings so I don't have to go through the 'r' subtree
(after! org-roam
  ;;; Helper for appending at the end of org files
  ;;; Common action for me in org-roam is to open a file, go to the end of the file
  ;;; and then add '* <current date>'
  (defun pdp-org-roam-insert ()
    (interactive)
    (goto-char (point-max))
    (insert (format-time-string "* %m/%d/%Y")))

  (map! :leader
        :prefix "n"
        :desc "org-roam-buffer-toggle" "l" #'org-roam-buffer-toggle
        :desc "pdp-open-inbox" "b" #'pdp-open-inbox
        :desc "org-roam-find-file" "f" #'org-roam-node-find
        :desc "org-roam-insert" "i" #'org-roam-node-insert
        :desc "org-roam-capture" "c" #'org-roam-capture
        :desc "end-of-file-insert" "p" #'pdp-org-roam-insert
        (:prefix-map ("d" . "Dailies/Journal")
                     "d" #'org-roam-dailies-capture-today
                     "o" #'org-roam-dailies-goto-today
                     "g" #'org-roam-dailies-goto-date
                     "m" #'org-roam-dailies-capture-tomorrow)
        )

  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry "* %<%I:%M %p>\n%?" :target
          (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n")))
        )
  ;;; Helpers to find the .plan file for the current week.
  (defun pdp-org-roam-plan-find (&optional title-or-alias)
    (interactive current-prefix-arg)
    (let ((node (org-roam-node-from-title-or-alias title-or-alias)))
      (if node
          (org-roam-node-visit node)
        (let ((node (org-roam-node-read title-or-alias)))
          (org-roam-capture- :node node))
        )))
  (defun pdp-org-roam-new-plan ()
    (interactive)
    (pdp-org-roam-plan-find (concat (org-read-date nil nil "++1" nil (org-read-date nil t "-sun")) ".plan"))
  )
)

;;; Allow me to get sloppy with holding down control for these options I use a lot
(global-set-key "\C-c\C-np" 'pdp-org-roam-insert)
(global-set-key "\C-c\C-n\C-p" 'pdp-org-roam-insert)
(global-set-key "\C-c\C-nl" 'org-roam-buffer-toggle)
(global-set-key "\C-c\C-n\C-l" 'org-roam-buffer-toggle)
(global-set-key "\C-c\C-nb" 'pdp-open-inbox)
(global-set-key "\C-c\C-n\C-b" 'pdp-open-inbox)
(global-set-key "\C-c\C-nf" 'org-roam-node-find)
(global-set-key "\C-c\C-n\C-f" 'org-roam-node-find)
(global-set-key "\C-c\C-ni" 'org-roam-node-insert)
(global-set-key "\C-c\C-n\C-i" 'org-roam-node-insert)

(use-package! avy
  :config
  ;;; Avy search stuff
  ;;;
  ;;; I like avy-goto-char-timer
  (global-set-key (kbd "C-\\") 'avy-goto-char-timer)
  (global-set-key (kbd "M-\\") 'avy-goto-char-timer)
  ;; Show candidate on all windows
  ;; 'windows' in the emacs sense, aka the different views in an OS window
  (setq avy-all-windows t)
  )

;;; ORG-SUPER-AGENDA config
(use-package! org-super-agenda
  :after org-agenda
  :config
  (progn
    (org-super-agenda-mode)
    (setq org-agenda-custom-commands
          '(("p" "Phil View"
             (
              (alltodo ""
                       (
                        (org-super-agenda-groups
                         '(
                           (:name "Priority"
                            :priority>="C"
                            )
                           (:discard (:anything t)
                                     )))))
              (agenda ""
                      (
                       (org-agenda-span 'week)
                       (org-agenda-start-day (org-today))
                       ))
              ))))
    (setq org-agenda-prefix-format '(
                                     (agenda  . "%-2T")
                                     (todo  . "%-2T")
                                     ))
    )
  )

(use-package! easy-hugo
  :config
  (setq easy-hugo-basedir "~/code/pdp80-blog/")
  (setq easy-hugo-url "https://pdp.dev")
  (setq easy-hugo-postdir "content/posts")
  (global-set-key (kbd "s-b") 'easy-hugo)
  )

(use-package! cargo-mode
  :config
  (add-hook 'rustic-mode-hook 'cargo-minor-mode))

;;; Show persp in the modeline
;;; https://github.com/hlissner/doom-emacs/issues/314
(after! doom-modeline
  (setq doom-modeline-persp-name t)
  (setq doom-modeline-height 30))

;;; When using emacsclient, don't create a new workspace
;;; every time.
(after! persp-mode
  (setq persp-emacsclient-init-frame-behaviour-override "main"))

;;; f-you so-long-mode, change the threshold to be obscenely long
(after! so-long
  (setq so-long-threshold 10000))

;; Configure lsp-mode for rust
;; From: https://robert.kra.hn/posts/rust-emacs-setup/
(use-package! lsp-mode
  :ensure
  :commands lsp
  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  ;; enable / disable the hints as you prefer:
  (lsp-inlay-hint-enable t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints nil)
  (lsp-rust-analyzer-display-reborrow-hints nil)
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package! lsp-ui
  :ensure
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil)
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  )

(use-package! dap-mode
  :config
  ; First time you set this up, do ~M-x dap-cpptools-setup~
  (setq dap-auto-configure-mode t)
  (with-eval-after-load 'lsp-rust
    (require 'dap-cpptools))
  )

(use-package! projectile
  :config
  (global-set-key (kbd "s-p") 'projectile-switch-project)
  )

(use-package! marginalia
    :config
    ; I had to add this so that when I open a Projectile project
    ; and it then opens the file selector I'd get annotations.
    ; Otherwise it was plain.
    (add-to-list 'marginalia-prompt-categories '("Find file" . file))
    )

; persist history across sessions.
(use-package! savehist
  :init
  (savehist-mode))

(use-package! orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package! graphviz-dot-mode)

;;; org-roam UI stuff
(use-package! websocket
    :after org-roam)

(use-package! org-roam-ui
    :after org-roam
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

(use-package! treemacs
  :config
  (global-set-key (kbd "s-t") 'treemacs)
  )
