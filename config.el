;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

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
;; brew tap homebrew/cask-fonts && brew install --cask font-roboto-mono-nerd-font
(setq doom-font (font-spec :family "Hack Nerd Font" :size 15)
      doom-big-font (font-spec :family "Hack Nerd Font" :size 16)
      doom-variable-pitch-font (font-spec :family "Iosevka Term" :size 16)
      )

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-moonlight)

;; Enable magit-delta (coloring for diffs in magit)
;; See https://github.com/dandavison/magit-delta
(use-package! magit-delta
  :after magit
  :config
  (magit-delta-mode))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Dropbox/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)

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
      (org-ql-select (list (concat org-directory "inbox.org") (concat org-directory "work.org"))
        '(and
              (todo "TODO")
              (not (ts :from -10)) ; older than 10 days
              (not (priority >= "C")) ; has no priority
              (not (scheduled)) ; isn't scheduled
              (not (deadline)) ; doesn't have a deadline
              (not (tags "email")) ; doesn't have an email tag
        )
        :action '(org-archive-subtree) ; archive it
        )

      ; Archive stuff that's DONE or CANCELLED
      ; I originally had this as an 'or' in the above query, but it didn't work.
      ; not sure why, oh well
      (org-ql-select (list (concat org-directory "inbox.org") (concat org-directory "work.org"))
        '(todo "DONE" "CANCELLED")
        :action '(org-archive-subtree) ; archive it
        )

      (org-save-all-org-buffers)
      )
    (global-set-key "\C-cnh" 'pdp-org-archive-done-tasks)
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
            ("d" "Todo Today" entry (file (lambda () (concat org-directory "inbox.org")))
             "* TODO %?\nSCHEDULED: %t\n%u")
            ("l" "Link" entry (file (lambda () (concat org-directory "inbox.org")))
             "* TODO %?\n%u\n%a")
            ("e" "Email" entry (file (lambda () (concat org-directory "inbox.org")))
             "* TODO %:fromname: %a :email:\n%u\n%i\n" :immediate-finish t)
          ))

    ;; C-c x to do generic TODO without interactive template selection
    (define-key global-map (kbd "C-c x")
      (lambda () (interactive) (org-capture nil "t")))
    ;; C-c z to open the "g"ood agenda view
    (define-key global-map (kbd "C-c z")
      (lambda () (interactive) (org-agenda nil "p")))

    (setq org-log-done 'time)
    (setq org-refile-use-outline-path nil)
    (setq org-agenda-start-on-weekday nil)
    (setq org-refile-targets '(
                               ("work.org" . (:level . 1))
                               ))
    (setq org-todo-keywords
          '((sequence "TODO(t)" "|" "DONE(d)" "CANCELLED(c)")))
    (setq org-use-fast-todo-selection t)
    )
  )

;;; org-modern stuff
(use-package org-modern
  :config
  (add-hook 'org-mode-hook #'org-modern-mode)
  ; Turn this off for the time being, it makes things ugly
  (add-hook 'org-agenda-finalize-hook #'org-modern-agenda)
  (custom-set-faces
   '(org-modern-date-active ((t :inherit org-modern-priority :background "#615545" :foreground "#EED47E")))
   '(org-modern-time-active ((t :inherit org-modern-priority :background "#EED47E" :foreground "black")))
   '(org-modern-date-inactive ((t :inherit org-modern-priority :background "#615545" :foreground "#EED47E")))
   '(org-modern-time-inactive ((t :inherit org-modern-priority :background "#EED47E" :foreground "black")))
   '(org-modern-done ((t :inherit org-modern-priority :background "#000000" :foreground "#ef6787")))
   '(org-modern-todo ((t :inherit org-modern-priority :background "#000000" :foreground "#8ee6d6")))
   )

  ;;; I didn't like this. Turns "#+TITLE" in "TITLE"...
  (setq org-modern-keyword nil)
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
                           (prettify-symbols-mode)))
  (setq org-modern-checkbox '((88 . "✓") (45 . "□–") (32 . "□")))
  (setq org-ellipsis "↴")
  )

;;; Save all org buffers periodically (I think every 30 seconds)
(after! org
  (add-hook 'auto-save-hook 'org-save-all-org-buffers)
  ; I only want one priority
  (setq org-default-priority 66
        org-priority-highest 66
        org-priority-lowest 66)
  )

(setq org-roam-directory (concat org-directory "roam"))
;;; Let each machine have it's own DB cache
;;; Borrowed from https://www.reddit.com/r/orgmode/comments/kocvjb/can_i_sync_orgroam_across_devices_if_so_what_is/
(setq org-roam-db-location (expand-file-name (concat "org-roam." (system-name) ".db") org-roam-directory))

;;; Helper for appending at the end of org files
;;; Common action for me in org-roam is to open a file, go to the end of the file
;;; and then add '* <current date>'
 (defun pdp-org-roam-insert ()
   (interactive)
   (goto-char (point-max))
   (insert (format-time-string "* %m/%d/%Y")))

(use-package org-roam
      :ensure t
      :custom
      (org-roam-directory (file-truename org-roam-directory))
      :config
      (org-roam-db-autosync-enable)
      (setq org-roam-completion-system 'ivy)
      )

;;; Capture templates
(setq org-roam-capture-templates
  '(
    ("d" "default" plain "%?"
     :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                        "#+title: ${title}")
     :unnarrowed t)
    ("p" "person" plain "%?"
     :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                        ; I name these files after the persons work alias, then put their full name under "ROAM_ALIASES"
                        ":PROPERTIES:\n:ROAM_ALIASES: \"${fullname}\"\n:END:\n#+title: ${title}\n\n- tags :: [[id:dfd98009-3b6a-4f32-8235-00131e66918c][People]]")
     :unnarrowed t)
     ("l" "plan" plain "%?"
     :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                        ; I name these files after the persons work alias, then put their full name under "ROAM_ALIASES"
                        "#+title: ${title}\n\n- tags :: [[id:95f115f1-c0c8-4cd3-8328-f85df945d469][.plan]]\n\n* To Try On\n\n* Start of the week\n\n* Midweek checkin\n\n* End of week\n\n* Org-clock for the last week")
     :unnarrowed t)
    )
  )

;;; Quick function to open my inbox.org file
(defun pdp-open-inbox ()
  (interactive)
  (find-file (expand-file-name (concat org-directory "/inbox.org")))
  (revert-buffer)
  )

;;; Stolen from here:
;;; https://github.com/sunnyhasija/Academic-Doom-Emacs-Config/blob/master/config.el
;;; I like these bindings so I don't have to go through the 'r' subtree
(after! org-roam
  (map! :leader
        :prefix "n"
        :desc "org-roam-buffer-toggle" "l" #'org-roam-buffer-toggle
        :desc "pdp-open-inbox" "b" #'pdp-open-inbox
        :desc "org-roam-find-file" "f" #'org-roam-node-find
        :desc "org-roam-insert" "i" #'org-roam-node-insert
        :desc "org-roam-capture" "c" #'org-roam-capture
        :desc "end-of-file-insert" "p" #'pdp-org-roam-insert
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

;;; Avy search stuff
;;;
;;; I like avy-goto-char-timer
(global-set-key (kbd "C-<return>") 'avy-goto-char-timer)
(global-set-key (kbd "M-<return>") 'avy-goto-char-timer)
;; Show candidate on all windows
;; 'windows' in the emacs sense, aka the different views in an OS window
(setq avy-all-windows t)

;;; ORG-SUPER-AGENDA config
(use-package! org-super-agenda
  :after org-agenda
  :config
  (progn
    (org-super-agenda-mode)
    (setq org-agenda-custom-commands
      '(("g" "Good View"
         (
          (agenda ""
                  ((org-agenda-overriding-header "Timeline")
                   (org-agenda-span 'day)
                   (org-agenda-start-day (org-today))
                   (org-super-agenda-groups
                    '((:auto-outline-path t)))
                   ))
          (alltodo "" ((org-agenda-overriding-header "Other")
                       (org-super-agenda-groups
                        '(
                          (:name "Priority"
                           :priority>="C"
                           :order 1)
                          (:name "Overdue"
                           :deadline past
                           :order 10)
                          (:name "Emails to Write"
                           :tag "email"
                           :order 21)
                          (:discard (:anything t)
                          )))))
         ))
        ; Example
        ("p" "Phil's View"
         (
          (org-ql-block '(and (todo "TODO")
                              (or (scheduled :to today) (deadline :to today))
                              )
                        ((org-ql-block-header "Overdue + Today")
                        (org-super-agenda-groups  '((:auto-planning t)))
                        ))
          (org-ql-block '(and (todo "TODO")
                              (or (scheduled :on +1) (deadline :on +1))
                              )
                        ((org-ql-block-header "Tomorrow")
                        (org-super-agenda-groups  '((:auto-planning t)))
                        ))
          (org-ql-block '(and (todo "TODO")
                              (priority >= "C")
                              )
                        ((org-ql-block-header "Priority")
                         (org-super-agenda-groups '((:auto-parent t)))
                         ))
          (org-ql-block '(and (todo "TODO")
                              (tags "email")
                              )
                        ((org-ql-block-header "Emails to Write")
                         (org-super-agenda-groups '((:auto-parent t)))
                         ))
          (org-ql-block '(and (todo "TODO")
                              (not (ts :from -7))
                              (not (priority >= "C")) ; has no priority
                              (not (scheduled)) ; isn't scheduled
                              (not (deadline)) ; doesn't have a deadline
                              (not (tags "email")) ; doesn't have an email tag
                              )
                        ((org-ql-block-header "Expiring")
                        (org-super-agenda-groups '((:auto-parent t)))
                        ))
          (org-ql-block '(and (todo "TODO")
                              (ts :from -7)
                              (not (priority >= "C")) ; has no priority
                              (not (scheduled)) ; isn't scheduled
                              (not (deadline)) ; doesn't have a deadline
                              (not (tags "email")) ; doesn't have an email tag
                              )
                        ((org-ql-block-header "Stuff")
                         (org-super-agenda-groups '((:auto-parent t)))
                         ))
          )))
        )
      )
  )

;;; Never wrap my shit
(add-hook 'org-mode-hook #'turn-off-auto-fill)
(add-hook 'org-journal-mode-hook #'turn-off-auto-fill)
(add-hook 'markdown-mode-hook #'turn-off-auto-fill)

;;; Bind C-z to org-toggle-narrow-to-subtree
;;; https://taonaw.com/2022-05-24/
(global-set-key "\C-z" 'org-toggle-narrow-to-subtree)

;;; HUGO
(setq easy-hugo-basedir "~/code/pdp80-blog/")
(setq easy-hugo-url "https://pdp.dev")
(setq easy-hugo-postdir "content/posts")
;;; END HUGO

(use-package! cargo-mode
  :config
  (add-hook 'rustic-mode-hook 'cargo-minor-mode))

;;; Show persp in the modeline
;;; https://github.com/hlissner/doom-emacs/issues/314
(after! doom-modeline
  (setq doom-modeline-persp-name t))

;;; f-you so-long-mode, change the threshold to be obscenely long
(after! so-long
  (setq so-long-threshold 10000))

;; Load my mu4e settings
;; There's nothing super private in there,
;; but it has addresses and prefs and stuff.
(load (concat doom-user-dir "modules/mu4e.el"))

;;; Load org-msg settings
;;; Includes CSS styling and some preferences
(load! (concat doom-user-dir "modules/org-msg.el"))

;;; elfeed
;;; Some stuff borrowed from here
;;; https://cestlaz.github.io/posts/using-emacs-29-elfeed/
(setq elfeed-db-directory "~/Dropbox/elfeeddb")

(defun elfeed-mark-all-as-read ()
  (interactive)
  (mark-whole-buffer)
  (elfeed-search-untag-all-unread))

;;functions to support syncing .elfeed between machines
;;makes sure elfeed reads index from disk before launching
(defun bjm/elfeed-load-db-and-open ()
  "Wrapper to load the elfeed db from disk before opening"
  (interactive)
  (elfeed-db-load)
  (elfeed)
  (elfeed-search-update--force))

;;write to disk when quiting
(defun bjm/elfeed-save-db-and-bury ()
  "Wrapper to save the elfeed db to disk before burying buffer"
  (interactive)
  (elfeed-db-save)
  (quit-window))

(use-package elfeed
  :ensure t
  :bind (:map elfeed-search-mode-map
         ("q" . bjm/elfeed-save-db-and-bury)
         ("Q" . bjm/elfeed-save-db-and-bury)
         ("v" . elfeed-search-browse-url)
         ("u" . elfeed-update)
         ("r" . elfeed-mark-all-as-read)
         ("b" . ar/elfeed-search-browse-background-url))
  :config
  (elfeed-search-set-filter "+unread")
  (defun ar/elfeed-search-browse-background-url ()
    "Open current `elfeed' entry (or region entries) in browser without losing focus."
    (interactive)
    (let ((entries (elfeed-search-selected)))
      (mapc (lambda (entry)
              (start-process (concat "open " (elfeed-entry-link entry))
                             nil "open" "--background" (elfeed-entry-link entry))
              (elfeed-untag entry 'unread)
              (elfeed-search-update-entry entry))
            entries)
      (unless (or elfeed-search-remain-on-entry (use-region-p))
        (forward-line))))

  (defun rakso/custom-elfeed-sort (a b)
  (let* ((a-tags (format "%s" (elfeed-entry-tags a)))
         (b-tags (format "%s" (elfeed-entry-tags b)))
         (a-title (elfeed-feed-title (elfeed-entry-feed a)))
         (b-title (elfeed-feed-title (elfeed-entry-feed b))))
    (if (string= a-tags b-tags)
        (if (string= a-title b-title)
            (< (elfeed-entry-date b) (elfeed-entry-date a))
          (string< b-title a-title))
      (string< a-tags b-tags))))

  (setf elfeed-search-sort-function #'rakso/custom-elfeed-sort)
)

(use-package elfeed-org
  :ensure t
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list (concat org-directory "feeds.org"))))

;; Configure lsp-mode for rust
;; From: https://robert.kra.hn/posts/rust-emacs-setup/
(use-package lsp-mode
  :ensure
  :commands lsp
  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  ;; enable / disable the hints as you prefer:
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints nil)
  (lsp-rust-analyzer-display-reborrow-hints nil)
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package lsp-ui
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

;;; dap mode stuff
(setq dap-auto-configure-mode t)
(with-eval-after-load 'lsp-rust
  (require 'dap-cpptools))
; First time you set this up, do ~M-x dap-cpptools-setup~
;;; END dap mode stuff
