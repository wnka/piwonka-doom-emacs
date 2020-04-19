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
(setq doom-font (font-spec :family "JetBrains Mono" :size 14))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;(setq doom-theme 'doom-laserwave)
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/ws/orgmode/src/PiwonkaOrgMode/")

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
    (setq wnka/org-path "~/Dropbox/org/")
    (setq wnka/org-notes-path "~/ws/orgmode/src/PiwonkaOrgMode/notes/")
    (setq org-agenda-files (mapcar #'(lambda (orgfile) (concat wnka/org-path orgfile))
                                   (list
                                    "inbox.org"
                                    "personal.org"
                                    "work.org"
                                    "web.org"
                                    )))

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
            ("t" "Todo" entry (file+headline (lambda () (concat wnka/org-path "inbox.org")) "Inbox")
             "* TODO %?\n  %i\n")
            ("d" "Todo Today" entry (file+headline (lambda () (concat wnka/org-path "inbox.org")) "Inbox")
             "* TODO %?\n  SCHEDULED: %t")
            ("c" "Todo with Clipboard" entry (file+headline (lambda () (concat wnka/org-path "inbox.org")) "Inbox")
             "* TODO %?\n  %c" :empty-lines 1)
            )
          )

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

;;; ORG-JOURNAL config
(after! org
  (add-to-list 'org-modules 'org-journal t)
  (progn
    (setq org-journal-dir "~/ws/orgmode/src/PiwonkaOrgMode/journal/")
    (setq org-journal-file-format "%Y%m%d.org")
    (map! :leader
      (:prefix ("j" . "journal") ;; org-journal bindings
        :desc "Create new journal entry" "j" #'org-journal-new-entry
        :desc "Open previous entry" "p" #'org-journal-open-previous-entry
        :desc "Open next entry" "n" #'org-journal-open-next-entry
        :desc "Search journal" "s" #'org-journal-search-forever))
    ))

;;; ORG-SUPER-AGENDA config
(use-package! org-super-agenda
  :after org-agenda
  :config
  (progn
    (org-super-agenda-mode)
    (setq org-agenda-custom-commands
      '(("g" "Good View"
         ((agenda ""
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
                 (org-super-agenda-groups
                  '((:auto-parent t)))))
          (todo ""
                 ((org-agenda-overriding-header "TO FILE")
                  (org-agenda-files (mapcar #'(lambda (orgfile) (concat wnka/org-path orgfile))
                                            (list
                                             "inbox.org"
                                             "web.org"
                                             )))))
          ))))
    )
  )

;;; Never wrap my shit
(add-hook 'org-mode-hook #'turn-off-auto-fill)
(add-hook 'org-journal-mode-hook #'turn-off-auto-fill)
(add-hook 'markdown-mode-hook #'turn-off-auto-fill)
