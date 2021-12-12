;;; plang-mode-el -- Major mode for editing plang files

;; Author: Phil Piwonka <me@pdp.dev>
;; Created: 12 Dec 2021
;; Keywords: PLang major-mode

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:
;;
;; This mode is derived from an example used in a tutorial about Emacs
;; mode creation. The tutorial can be found here:
;; http://two-wugs.net/emacs/mode-tutorial.html
;;
;; P-lang: https://github.com/p-org/P

;;; Code:
(defvar plang-mode-hook nil)
(defvar plang-mode-map
  (let ((plang-mode-map (make-keymap)))
    (define-key plang-mode-map "\C-j" 'newline-and-indent)
    plang-mode-map)
  "Keymap for PLANG major mode")

(add-to-list 'auto-mode-alist '("\\.p\\'" . plang-mode))

(defconst plang-font-lock-keywords-1
  (list
   ; To generate these, do:
   ; (regexp-opt '("any" "bool" "compose" "data" "enum" "event" "float" "int" "machine" "map" "module" "seq" "spec" "test" "this" "type" "union") 'words)
   '("\\<\\(any\\|bool\\|compose\\|data\\|e\\(?:num\\|vent\\)\\|float\\|int\\|m\\(?:a\\(?:chine\\|p\\)\\|odule\\)\\|s\\(?:eq\\|pec\\)\\|t\\(?:est\\|his\\|ype\\)\\|union\\)\\>" . font-lock-builtin-face)
   '("\\('\\w*'\\)" . font-lock-variable-name-face))
  "Minimal highlighting expressions for PLANG mode.")

(defconst plang-font-lock-keywords-2
  (append plang-font-lock-keywords-1
                  (list '("\\<\\(cold\\|fun\\|hot\\|observes\\|receives\\|s\\(?:ends\\|ta\\(?:rt\\|te\\)\\)\\|test\\|var\\)\\>" . font-lock-keyword-face)
                   '("\\<\\(TRUE\\|FALSE\\)\\>" . font-lock-constant-face)))
  "Additional Keywords to highlight in PLANG mode.")

(defconst plang-font-lock-keywords-3
  (append plang-font-lock-keywords-2
                  (list
                   '("\\<\\(\\$\\|d\\(?:efer\\|o\\)\\|e\\(?:lse\\|ntry\\|xit\\)\\|goto\\|i\\(?:f\\|gnore\\)\\|on\\|push\\|sizeof\\|w\\(?:hile\\|ith\\)\\)\\>" . font-lock-constant-face)))
  "Balls-out highlighting in PLANG mode.")

(defconst plang-font-lock-keywords-4
  (append plang-font-lock-keywords-3
                  (list
                   '("\\<\\(a\\(?:nnounce\\|ssert\\)\\|case\\|default\\|e\\*\\|goto\\|in\\|new\\|p\\(?:op\\|rint\\)\\|r\\(?:aise\\|e\\(?:ceive\\|turn\\)\\)\\|send\\)\\>"
                     . font-lock-constant-face)))
  "Balls-out highlighting in PLANG mode.")

(defvar plang-font-lock-keywords plang-font-lock-keywords-4
  "Default highlighting expressions for PLANG mode.")

(defun plang-indent-line ()
  "Indent current line as PLANG code."
  (interactive)
  (beginning-of-line)
  (if (bobp)
          (indent-line-to 0)               ; First line is always non-indented
        (let ((not-indented t) cur-indent)
          (if (looking-at "^[ \t]*}") ; If the line we are looking at is the end of a block, then decrease the indentation
                  (progn
                        (save-excursion
                          (forward-line -1)
                          (setq cur-indent (- (current-indentation) 4)))
                        (if (< cur-indent 0) ; We can't indent past the left margin
                                (setq cur-indent 0)))
                (save-excursion
                  (while not-indented ; Iterate backwards until we find an indentation hint
                        (forward-line -1)
                        (if (looking-at "^.*}") ; This hint indicates that we need to indent at the level of the { token
                                (progn
                                  (setq cur-indent (current-indentation))
                                  (setq not-indented nil))
                          (if (looking-at "^.*{") ; This hint indicates that we need to indent an extra level
                                  (progn
                                        (setq cur-indent (+ (current-indentation) 4)) ; Do the actual indenting
                                        (setq not-indented nil))
                                (if (bobp)
                                        (setq not-indented nil)))))))
          (if cur-indent
                  (indent-line-to cur-indent)
                (indent-line-to 0))))) ; If we didn't see an indentation hint, then allow no indentation

(defvar plang-mode-syntax-table
  (let ((plang-mode-syntax-table (make-syntax-table)))

        ; This is added so entity names with underscores can be more easily parsed
        (modify-syntax-entry ?_ "w" plang-mode-syntax-table)

        ; Comment styles are same as C++
        (modify-syntax-entry ?/ ". 124b" plang-mode-syntax-table)
        (modify-syntax-entry ?* ". 23" plang-mode-syntax-table)
        (modify-syntax-entry ?\n "> b" plang-mode-syntax-table)
        plang-mode-syntax-table)
  "Syntax table for plang-mode")

(defun plang-mode ()
  (interactive)
  (kill-all-local-variables)
  (use-local-map plang-mode-map)
  (set-syntax-table plang-mode-syntax-table)
  ;; Set up font-lock
  (set (make-local-variable 'font-lock-defaults) '(plang-font-lock-keywords))
  ;; Register our indentation function
  (set (make-local-variable 'indent-line-function) 'plang-indent-line)
  (setq major-mode 'plang-mode)
  (setq mode-name "PLANG")
  (run-hooks 'plang-mode-hook))

(provide 'plang-mode)

;;; plang-mode.el ends here
