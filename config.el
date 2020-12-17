;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; NATIVE SHIT - for gccemacs (which I'm currently not using because it was a pain and broke a lot)
;; (setq comp-speed 2)
;; (if (and (fboundp 'native-comp-available-p)
;;            (native-comp-available-p))
;;       (progn
;;         (message "Native comp is available")
;;         (add-to-list 'exec-path (expand-file-name "/usr/local/opt/gccemacs/bin"))
;;         (setenv "LIBRARY_PATH" (concat (getenv "LIBRARY_PATH")
;;                                        (when (getenv "LIBRARY_PATH")
;;                                          ":")
;;                                        (car (file-expand-wildcards
;;                                              (expand-file-name "/usr/local/opt/gcc/lib/gcc/*")))))
;;         ;; Only set after LIBRARY_PATH can find gcc libraries.
;;         (setq comp-deferred-compilation t))
;;   (message "Native comp is *not* available"))
;; END NATIVE SHIT

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
;(setq doom-font (font-spec :family "JetBrains Mono" :size 14))
(setq doom-font (font-spec :family "Roboto Mono" :size 14))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;(setq doom-theme 'doom-laserwave)
;(if (display-graphic-p)
;    (setq doom-theme 'twilight-anti-bright)
;  (setq doom-theme 'doom-one))
;(setq doom-theme 'doom-Iosvkem)
(setq doom-theme 'zaiste)

;; Enable magit-delta (coloring for diffs in magit)
;; See https://github.com/dandavison/magit-delta
(use-package! magit-delta
  :after magit
  :config
  (setq
    magit-delta-default-dark-theme "OneHalfDark"
    magit-delta-default-light-theme "OneHalfLight")
  (magit-delta-mode))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/ws/orgmode/src/PiwonkaOrgMode/")

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
    (setq wnka/org-path "~/Dropbox/org/")
    (setq wnka/org-notes-path "~/ws/orgmode/src/PiwonkaOrgMode/notes/")
    (setq org-agenda-files (mapcar #'(lambda (orgfile) (concat wnka/org-path orgfile))
                                   (list
                                    "habits.org"
                                    "inbox.org"
                                    "personal.org"
                                    "work.org"
                                    "web.org"
                                    "birthdays.org"
                                    "1on1.org"
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
            ("t" "Todo" entry (file (lambda () (concat wnka/org-path "inbox.org")))
             "* TODO %?\n%i\n")
            ("d" "Todo Today" entry (file (lambda () (concat wnka/org-path "inbox.org")))
             "* TODO %?\nSCHEDULED: %t")
            ("c" "Todo with Clipboard" entry (file (lambda () (concat wnka/org-path "inbox.org")))
             "* TODO %?\n%c" :empty-lines 1)
            ("w" "Web with Clipboard" entry (file (lambda () (concat wnka/org-path "web.org")))
             "* TODO %?\n%c" :empty-lines 1)
            )
          )

    (setq org-log-done 'time)
    (setq org-refile-use-outline-path nil)
    (setq org-agenda-start-on-weekday nil)
    (setq org-refile-targets '(
                               ("work.org" . (:level . 1))
                               ("1on1.org" . (:level . 2))
                               ("personal.org" . (:level . 1))
                               ))
    (setq org-todo-keywords
          '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)" "CANCELLED(c)")))
    (setq org-use-fast-todo-selection t)

    ;; Make it so global clock in shows recent tasks
    (defun pdp80/org-clock-in ()
      (interactive)
      (org-clock-in '(4)))

    (global-set-key "\C-cI" 'pdp80/org-clock-in)
    (global-set-key "\C-cO" 'org-clock-out)
    (global-set-key "\C-cG" 'org-clock-goto)
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
    (setq org-journal-dir "~/ws/orgmode/src/PiwonkaOrgMode/journal/")
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

;;; Stolen from here:
;;; https://github.com/sunnyhasija/Academic-Doom-Emacs-Config/blob/master/config.el
;;; I like these bindings so I don't have to go through the 'r' subtree
(after! org-roam
  (map! :leader
        :prefix "n"
        :desc "org-roam" "l" #'org-roam
        :desc "org-roam-switch-to-buffer" "b" #'org-roam-switch-to-buffer
        :desc "org-roam-find-file" "f" #'org-roam-find-file
        :desc "org-roam-show-graph" "g" #'org-roam-show-graph
        :desc "org-roam-insert" "i" #'org-roam-insert
        :desc "rifle" "e" #'helm-org-rifle
        :desc "org-roam-capture" "c" #'org-roam-capture))

(use-package company-org-roam
  :after org-roam
  :config
  (push 'company-org-roam company-backends))

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
                 (org-super-agenda-groups
                  '((:auto-parent t)))))
          (todo ""
                 ((org-agenda-overriding-header "TO FILE")
                  (org-agenda-files (mapcar #'(lambda (orgfile) (concat wnka/org-path orgfile))
                                            (list
                                             "inbox.org"
                                             "web.org"
                                             "phone.org"
                                             )))))
          ))))
    )
  )

;;; Never wrap my shit
(add-hook 'org-mode-hook #'turn-off-auto-fill)
(add-hook 'org-journal-mode-hook #'turn-off-auto-fill)
(add-hook 'markdown-mode-hook #'turn-off-auto-fill)
(setq plantuml-output-type "png")

;;; Use bufler for buffer management
(global-set-key "\C-x\C-b" 'bufler-switch-buffer)

;;; HUGO
(setq easy-hugo-basedir "~/Documents/hugo/pdp80/")
(setq easy-hugo-url "https://pdp.dev")
(setq easy-hugo-postdir "content/posts")
