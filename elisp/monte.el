;; monte.el -- support for editing Monte code -*- lexical-binding: t -*-
;; copyright 2015 Allen Short, available under MIT license (see LICENSE)
(provide 'monte)
;;;###autoload
(add-to-list 'auto-mode-alist (cons "\\.mt\\'" 'monte-mode))

(defgroup monte nil
  "The Monte development environment"
  :group 'languages
  :version "0.1"
  :link '(emacs-commentary-link "monte"))

(defcustom monte-indent-offset 4
  "Default indentation offset for Monte."
  :group 'monte
  :type 'integer
  :safe 'integerp)

(require 'flycheck)
(require 'monte-indent)

(defvar monte-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap forward-sentence] 'monte-nav-forward-block)
    (define-key map [remap backward-sentence] 'monte-nav-backward-block)
    (define-key map "\177" 'monte-indent-dedent-line-backspace)
    (define-key map (kbd "<backtab>") 'monte-indent-dedent-line)
    map)
  "Keymap for monte-mode.")

(defvar monte-font-lock-keywords
  `(,(rx symbol-start
         (or "as" "bind" "break" "catch" "continue" "def" "else" "escape"
             "exit" "extends" "exports" "finally" "fn" "for" "guards" "if"
             "implements" "import" "in" "interface" "match" "meta" "method"
             "object" "pass" "pragma" "return" "switch" "to" "try" "var"
             "via" "when" "while")
         symbol-end)
    (,(rx symbol-start "def" (1+ space) (group (1+ (or word ?_))) (0+ space) ?\()
     (1 font-lock-function-name-face))
    (,(rx symbol-start "object" (1+ space) (group (1+ (or word ?_))))
     (1 font-lock-function-name-face))
    (,(rx symbol-start (or "def" "var") (1+ space) (group (1+ (or word ?_))) (0+ space) ?: ?=)
     (1 font-lock-variable-name-face))
    ))

(defvar monte-mode-syntax-table
  (let ((table (make-syntax-table)))
    (mapc (lambda (c) (modify-syntax-entry c "." table)) "$%+-.:;<=>?@^|")
    (modify-syntax-entry ?+ "." table)
    (modify-syntax-entry ?* ". 23b" table)
    (modify-syntax-entry ?/ ". 14b" table)
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?' "\"" table)
    (modify-syntax-entry ?` "\"" table)
    (modify-syntax-entry ?\\ "\\" table)
    table)
  "Monte syntax table.")

;;;###autoload
(define-derived-mode monte-mode prog-mode "Monte"
  "Major mode for editing Montefiles.

\\{monte-mode-map}"
  (set (make-local-variable 'indent-tabs-mode) nil)
  (set (make-local-variable 'comment-start) "# ")
  (set (make-local-variable 'comment-start-skip) "#+\\s-*")
  (set (make-local-variable 'font-lock-defaults) '(monte-font-lock-keywords nil nil nil nil))
  (set (make-local-variable 'indent-line-function) 'monte-indent-line-function)
  (setq-local electric-indent-inhibit t))


(flycheck-define-checker monte-lint
  "Syntax and scope checking for Monte."
  :command ("monte" "lint" source-inplace)
  :modes (monte-mode)
  :error-patterns
  ((warning line-start (file-name) ":" line "." column
           "-" (+ digit) "." (+ digit)
           ": " (message (: "Unused name " (one-or-more not-newline))))
   (error line-start (file-name) ":" line "." column
           "-" (+ digit) "." (+ digit)
           ": " (message))))

(add-to-list 'flycheck-checkers 'monte-lint)
