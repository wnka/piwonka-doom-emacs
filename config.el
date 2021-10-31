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
;;; Influenced by:
;;; https://github.com/pkazmier/doom-nebula-theme
;;(setq doom-font (font-spec :family "Iosevka Term" :size 16)
;;      doom-big-font (font-spec :family "Iosevka Term" :size 22)
;;      doom-variable-pitch-font (font-spec :family "Iosevka Term" :size 16))

;; Get fonts from here:
;; https://github.com/ryanoasis/nerd-fonts
(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 15)
      doom-big-font (font-spec :family "JetBrainsMono Nerd Font" :size 18)
      doom-variable-pitch-font (font-spec :family "Iosevka Term" :size 16))

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
(global-set-key "\C-x\C-m" 'counsel-M-x)
(global-set-key "\C-xm" 'counsel-M-x)

;; Make jumping around lists/parens easy
(global-set-key "\M-n" 'forward-list)
(global-set-key "\M-p" 'backward-list)

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
    (global-set-key "\C-cnr" 'org-refile)
    (defun my-org-archive-done-tasks ()
      (interactive)
      (org-map-entries 'org-archive-subtree "/DONE" 'file))
    (global-set-key "\C-cnh" 'my-org-archive-done-tasks)
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
             "* TODO %?\n%i\n")
            ("d" "Todo Today" entry (file (lambda () (concat org-directory "inbox.org")))
             "* TODO %?\nSCHEDULED: %t")
            ("c" "Todo with Clipboard" entry (file (lambda () (concat org-directory "inbox.org")))
             "* TODO %?\n%c" :empty-lines 1)
            ("l" "link" entry (file (lambda () (concat org-directory "inbox.org")))
             "* TODO %?\n%a")
          ))

    ;; C-c x to do generic TODO without interactive template selection
    (define-key global-map (kbd "C-c x")
      (lambda () (interactive) (org-capture nil "t")))
    ;; C-c z to open the "g"ood agenda view
    (define-key global-map (kbd "C-c z")
      (lambda () (interactive) (org-agenda nil "g")))

    (setq org-log-done 'time)
    (setq org-refile-use-outline-path nil)
    (setq org-agenda-start-on-weekday nil)
    (setq org-refile-targets '(
                               ("work.org" . (:level . 1))
                               ("personal.org" . (:level . 1))
                               ))
    (setq org-todo-keywords
          '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)" "CANCELLED(c)")))
    (setq org-use-fast-todo-selection t)
    )
  )

;;; ORG-HABIT config
(after! org
  (add-to-list 'org-modules 'org-habit t)
  (setq org-habit-show-habits t))

;;; Save all org buffers periodically (I think every 30 seconds)
(after! org
  (add-hook 'auto-save-hook 'org-save-all-org-buffers)
  )

;;; ORG-JOURNAL config
(after! org
  (add-to-list 'org-modules 'org-journal t)
  (progn
    (setq org-journal-dir (concat org-directory "journal"))
    (setq org-journal-file-format "%Y%m%d.org")
    (map! :leader
      (:prefix ("j" . "journal") ;; org-journal bindings
        :desc "Create new journal entry" "j" #'org-journal-new-entry
        :desc "Open previous entry" "p" #'org-journal-open-previous-entry
        :desc "Open next entry" "n" #'org-journal-open-next-entry
        :desc "Search journal" "s" #'org-journal-search-forever))
    (setq org-journal-date-prefix "#+TITLE: "
          org-journal-time-prefix "* ")
    ))

(setq org-roam-directory "~/Dropbox/org/roam")
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
      (org-roam-directory (file-truename "~/Dropbox/org/roam/"))
      :config
      (org-roam-setup)
      ;; If using org-roam-protocol
      (require 'org-roam-protocol))

;;; Stolen from here:
;;; https://github.com/sunnyhasija/Academic-Doom-Emacs-Config/blob/master/config.el
;;; I like these bindings so I don't have to go through the 'r' subtree
(after! org-roam
  (map! :leader
        :prefix "n"
        :desc "org-roam-buffer-toggle" "l" #'org-roam-buffer-toggle
        :desc "org-roam-switch-to-buffer" "b" #'org-roam-switch-to-buffer
        :desc "org-roam-find-file" "f" #'org-roam-node-find
        :desc "org-roam-show-graph" "g" #'org-roam-show-graph
        :desc "org-roam-insert" "i" #'org-roam-node-insert
        :desc "rifle" "e" #'helm-org-rifle
        :desc "org-roam-capture" "c" #'org-roam-capture
        :desc "end-of-file-insert" "p" #'pdp-org-roam-insert
        :desc "Roam TODOs" "t" #'ora-roam-todo))
;;; Allow me to get sloppy with holding down control for these options I use a lot
(global-set-key "\C-c\C-np" 'pdp-org-roam-insert)
(global-set-key "\C-c\C-n\C-p" 'pdp-org-roam-insert)
(global-set-key "\C-c\C-nf" 'org-roam-node-find)
(global-set-key "\C-c\C-n\C-f" 'org-roam-node-find)
(global-set-key "\C-c\C-ni" 'org-roam-node-insert)
(global-set-key "\C-c\C-n\C-i" 'org-roam-node-insert)

;;; Avy search stuff
;;;
(progn
  (map! :leader
        (:prefix ("a" . "avy") ;; avy bindings
         :desc "Goto Char Timer" "s" #'avy-goto-char-timer
         :desc "Goto word-1" "a" #'avy-goto-word-1
         :desc "Next" "n" #'avy-next
         :desc "Prev" "p" #'avy-prev
         )))
(global-set-key "\C-\\" 'avy-goto-char-timer)

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
                  ((org-agenda-overriding-header "TODAY")
                   (org-agenda-span 'day)
                   (org-agenda-start-day (org-today))
                   (org-super-agenda-groups
                    '((:auto-outline-path t)))
                   ))
          (todo ""
                ((org-agenda-overriding-header "NEXT")
                 (org-agenda-skip-function
                  '(or
                    (org-agenda-skip-entry-if 'nottodo '("NEXT"))))
                 ))
          (todo ""
                ((org-agenda-overriding-header "TO FILE")
                 (org-agenda-files (mapcar #'(lambda (orgfile) (concat org-directory orgfile))
                                           (list
                                             "inbox.org"
                                             )))))
;;          (todo ""
;;                ((org-agenda-overriding-header "DEADLINE/SCHEDULE")
;;                 (org-super-agenda-groups '(
;;                                            (:discard (:habit t))
;;                                            (:name "Due Soon"
;;                                             :scheduled future
;;                                             )
;;                                            (:name "Overdue"
;;                                             :scheduled past
;;                                             )
;;                                            (:discard (:anything t)))
;;                 ))
          ))))
    )
  )

;;; Never wrap my shit
(add-hook 'org-mode-hook #'turn-off-auto-fill)
(add-hook 'org-journal-mode-hook #'turn-off-auto-fill)
(add-hook 'markdown-mode-hook #'turn-off-auto-fill)
(setq plantuml-output-type "png")

;;; HUGO
(setq easy-hugo-basedir "~/code/pdp80-blog/")
(setq easy-hugo-url "https://pdp.dev")
(setq easy-hugo-postdir "content/posts")
;;; END HUGO

(use-package! cargo-mode
  :config
  (add-hook 'rustic-mode-hook 'cargo-minor-mode))
