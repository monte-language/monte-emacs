;; Derived from python.el by Fabián E. Gallina.
;; Modifications by Allen Short.
;; This file is licensed under the GPLv3; see <http://www.gnu.org/licenses/>.
(provide 'monte-indent)
(eval-when-compile
  (defun monte-syntax--context-compiler-macro (form type &optional syntax-ppss)
    (pcase type
      (`'comment
       `(let ((ppss (or ,syntax-ppss (syntax-ppss))))
          (and (nth 4 ppss) (nth 8 ppss))))
      (`'string
       `(let ((ppss (or ,syntax-ppss (syntax-ppss))))
          (and (nth 3 ppss) (nth 8 ppss))))
      (`'paren
       `(nth 1 (or ,syntax-ppss (syntax-ppss))))
      (_ form))))

(defsubst monte-syntax-count-quotes (quote-char &optional point limit)
  "Count number of quotes around point (max is 3).
QUOTE-CHAR is the quote char to count.  Optional argument POINT is
the point where scan starts (defaults to current point), and LIMIT
is used to limit the scan."
  (let ((i 0))
    (while (and (< i 3)
                (or (not limit) (< (+ point i) limit))
                (eq (char-after (+ point i)) quote-char))
      (setq i (1+ i)))
    i))

(defsubst monte-syntax-comment-or-string-p (&optional ppss)
  "Return non-nil if PPSS is inside 'comment or 'string."
  (nth 8 (or ppss (syntax-ppss))))

(defsubst monte-syntax-closing-paren-p ()
  "Return non-nil if char after point is a closing paren."
  (= (syntax-class (syntax-after (point)))
     (syntax-class (string-to-syntax ")"))))

(eval-when-compile
  (let* ((string-literal
          (rx ?\" (*? (or (seq ?\\ (any "\"'btnfr\\\n"))
                          (not (any "\\\"")))) ?\"))
         (noun
          (rx-to-string `(or (seq (any letter ?_) (* (any word ?_)))
                             (seq "::" (regexp ,string-literal))))))
    (defconst monte-rx-constituents
      `((block-start          . ,(rx-to-string
                                  `(or (: (opt "return" (+ space)) symbol-start
                                          (or "object" "if" "else if" "else"
                                              "try" "catch" "escape" "finally"
                                              "for" "interface" "match"
                                              "method" "to" "while" "when")
                                          symbol-end)
                                       (: (or "def" "bind" "def bind")
                                          (+ space) (regexp ,noun) (* space) (or ?\( "as" "implements")))))
        (dedenter            . ,(rx symbol-start
                                    (or "else if" "else" "catch" "finally")
                                    symbol-end))
        (block-ender         . ,(rx symbol-start
                                    (or
                                     "break" "continue" "pass" "return")
                                    symbol-end))
        (defun                . ,(rx symbol-start (or "def" "object") symbol-end))
        (symbol-name          . ,noun)
        (open-paren           . ,(rx (or "{" "[" "(")))
        (close-paren          . ,(rx (or "}" "]" ")")))
        (simple-operator      . ,(rx (any ?+ ?- ?/ ?& ?^ ?~ ?| ?* ?< ?> ?= ?% ?!)))
        ;; FIXME: rx should support (not simple-operator).
        (not-simple-operator  . ,(rx
                                  (not
                                   (any ?+ ?- ?/ ?& ?^ ?~ ?| ?* ?< ?> ?= ?% ?!))))
        ;; FIXME: Use regexp-opt.
        (operator             . ,(rx (or "+" "-" "/" "&" "^" "~" "|" "*" "<" ">"
                                         "=" "%" "**" "//" "<<" ">>" "<=" "!="
                                         "==" ">=" "&!")))
        ;; FIXME: Use regexp-opt.
        (assignment-operator  . ,(rx (or "=" "+=" "-=" "*=" "/=" "//=" "%=" "**="
                                         ">>=" "<<=" "&=" "^=" "|=" (and word "="))))
        (string-delimiter . ,(rx (and
                                  ;; Match even number of backslashes.
                                  (or (not (any ?\\ ?\' ?\")) point
                                      ;; Quotes might be preceded by a escaped quote.
                                      (and (or (not (any ?\\)) point) ?\\
                                           (* ?\\ ?\\) (any ?\' ?\")))
                                  (* ?\\ ?\\)
                                  ;; Match single quotes of any kind.
                                  (group (or  "\"" "'" "`"))))))
      "Additional Monte specific sexps for `monte-rx'")

    (defmacro monte-rx (&rest regexps)
      "Monte mode specialized rx macro.
This variant of `rx' supports common Monte named REGEXPS."
      (let ((rx-constituents (append monte-rx-constituents rx-constituents)))
        (cond ((null regexps)
               (error "No regexp"))
              ((cdr regexps)
               (rx-to-string `(and ,@regexps) t))
              (t
               (rx-to-string (car regexps) t)))))))

(defun monte-indent-context ()
  "Get information about the current indentation context.
Context is returned in a cons with the form (STATUS . START).

STATUS can be one of the following:

keyword
-------

:after-comment
 - Point is after a comment line.
 - START is the position of the \"#\" character.
:inside-string
 - Point is inside string.
 - START is the position of the first quote that starts it.
:no-indent
 - No possible indentation case matches.
 - START is always zero.

:inside-paren
 - Fallback case when point is inside paren.
 - START is the first non space char position *after* the open paren.
:inside-paren-at-closing-nested-paren
 - Point is on a line that contains a nested paren closer.
 - START is the position of the open paren it closes.
:inside-paren-at-closing-paren
 - Point is on a line that contains a paren closer.
 - START is the position of the open paren.
:inside-paren-newline-start
 - Point is inside a paren with items starting in their own line.
 - START is the position of the open paren.
:inside-paren-newline-start-from-block
 - Point is inside a paren with items starting in their own line
   from a block start.
 - START is the position of the open paren.

:after-backslash
 - Fallback case when point is after backslash.
 - START is the char after the position of the backslash.
:after-backslash-assignment-continuation
 - Point is after a backslashed assignment.
 - START is the char after the position of the backslash.
:after-backslash-block-continuation
 - Point is after a backslashed block continuation.
 - START is the char after the position of the backslash.
:after-backslash-dotted-continuation
 - Point is after a backslashed dotted continuation.  Previous
   line must contain a dot to align with.
 - START is the char after the position of the backslash.
:after-backslash-first-line
 - First line following a backslashed continuation.
 - START is the char after the position of the backslash.

:after-block-end
 - Point is after a line containing a block ender.
 - START is the position where the ender starts.
:after-block-start
 - Point is after a line starting a block.
 - START is the position where the block starts.
:after-line
 - Point is after a simple line.
 - START is the position where the previous line starts.
:at-dedenter-block-start
 - Point is on a line starting a dedenter block.
 - START is the position where the dedenter block starts."
  (save-restriction
    (widen)
    (let ((ppss (save-excursion
                  (beginning-of-line)
                  (syntax-ppss))))
      (cond
       ;; Beginning of buffer.
       ((= (line-number-at-pos) 1)
        (cons :no-indent 0))
       ;; Inside a string.
       ((let ((start (monte-syntax-context 'string ppss)))
          (when start
            (cons :inside-string start))))
       ;; Inside a paren.
       ((let* ((start (monte-syntax-context 'paren ppss))
               (starts-in-newline
                (when start
                  (save-excursion
                    (goto-char start)
                    (forward-char)
                    (not
                     (= (line-number-at-pos)
                        (progn
                          (monte-util-forward-comment)
                          (line-number-at-pos))))))))
          (when start
            (cond
             ;; Current line only holds the closing paren.
             ((save-excursion
                (skip-syntax-forward " ")
                (when (and (monte-syntax-closing-paren-p)
                           (progn
                             (forward-char 1)
                             (not (monte-syntax-context 'paren))))
                  (cons :inside-paren-at-closing-paren start))))
             ;; Current line only holds a closing paren for nested.
             ((save-excursion
                (back-to-indentation)
                (monte-syntax-closing-paren-p))
              (cons :inside-paren-at-closing-nested-paren start))
             ;; This line starts from a opening block in its own line.
             ((save-excursion
                (goto-char start)
                (when (and
                       starts-in-newline
                       (save-excursion
                         (back-to-indentation)
                         (looking-at (monte-rx block-start))))
                  (cons
                   :inside-paren-newline-start-from-block start))))
             (starts-in-newline
              (cons :inside-paren-newline-start start))
             ;; General case.
             (t (cons :inside-paren
                      (save-excursion
                        (goto-char (1+ start))
                        (skip-syntax-forward "(" 1)
                        (skip-syntax-forward " ")
                        (point))))))))
       ;; After backslash.
       ((let ((start (when (not (monte-syntax-comment-or-string-p ppss))
                       (monte-info-line-ends-backslash-p
                        (1- (line-number-at-pos))))))
          (when start
            (cond
             ;; Continuation of dotted expression.
             ((save-excursion
                (back-to-indentation)
                (when (eq (char-after) ?\.)
                  ;; Move point back until it's not inside a paren.
                  (while (prog2
                             (forward-line -1)
                             (and (not (bobp))
                                  (monte-syntax-context 'paren))))
                  (goto-char (line-end-position))
                  (while (and (search-backward
                               "." (line-beginning-position) t)
                              (monte-syntax-context-type)))
                  ;; Ensure previous statement has dot to align with.
                  (when (and (eq (char-after) ?\.)
                             (not (monte-syntax-context-type)))
                    (cons :after-backslash-dotted-continuation (point))))))
             ;; Continuation of block definition.
             ((let ((block-continuation-start
                     (monte-info-block-continuation-line-p)))
                (when block-continuation-start
                  (save-excursion
                    (goto-char block-continuation-start)
                    (re-search-forward
                     (monte-rx block-start (* space))
                     (line-end-position) t)
                    (cons :after-backslash-block-continuation (point))))))
             ;; Continuation of assignment.
             ((let ((assignment-continuation-start
                     (monte-info-assignment-continuation-line-p)))
                (when assignment-continuation-start
                  (save-excursion
                    (goto-char assignment-continuation-start)
                    (cons :after-backslash-assignment-continuation (point))))))
             ;; First line after backslash continuation start.
             ((save-excursion
                (goto-char start)
                (when (or (= (line-number-at-pos) 1)
                          (not (monte-info-beginning-of-backslash
                                (1- (line-number-at-pos)))))
                  (cons :after-backslash-first-line start))))
             ;; General case.
             (t (cons :after-backslash start))))))
       ;; After beginning of block.
       ((let ((start (save-excursion
                       (back-to-indentation)
                       (monte-util-forward-comment -1)
                       (when (equal (char-before) ?:)
                         (monte-nav-beginning-of-block)))))
          (when start
            (cons :after-block-start start))))
       ;; At dedenter statement.
       ((let ((start (monte-info-dedenter-statement-p)))
          (when start
            (cons :at-dedenter-block-start start))))
       ;; After normal line, comment or ender (default case).
       ((save-excursion
          (back-to-indentation)
          (skip-chars-backward " \t\n")
          (monte-nav-beginning-of-statement)
          (cons
           (cond ((monte-info-current-line-comment-p)
                  :after-comment)
                 ((save-excursion
                    (goto-char (line-end-position))
                    (monte-util-forward-comment -1)
                    (monte-nav-beginning-of-statement)
                    (looking-at (monte-rx block-ender)))
                  :after-block-end)
                 (t :after-line))
           (point))))))))

(defun monte-indent--calculate-indentation ()
  "Internal implementation of `monte-indent-calculate-indentation'.
May return an integer for the maximum possible indentation at
current context or a list of integers.  The latter case is only
happening for :at-dedenter-block-start context since the
possibilities can be narrowed to specific indentation points."
  (save-restriction
    (widen)
    (save-excursion
      (pcase (monte-indent-context)
        (`(:no-indent . ,_) 0)
        (`(,(or :after-line
                :after-comment
                :inside-string
                :after-backslash
                :inside-paren-at-closing-paren
                :inside-paren-at-closing-nested-paren) . ,start)
         ;; Copy previous indentation.
         (goto-char start)
         (current-indentation))
        (`(,(or :after-block-start
                :after-backslash-first-line
                :inside-paren-newline-start) . ,start)
         ;; Add one indentation level.
         (goto-char start)
         (+ (current-indentation) monte-indent-offset))
        (`(,(or :inside-paren
                :after-backslash-block-continuation
                :after-backslash-assignment-continuation
                :after-backslash-dotted-continuation) . ,start)
         ;; Use the column given by the context.
         (goto-char start)
         (current-column))
        (`(:after-block-end . ,start)
         ;; Subtract one indentation level.
         (goto-char start)
         (- (current-indentation) monte-indent-offset))
        (`(:at-dedenter-block-start . ,_)
         ;; List all possible indentation levels from opening blocks.
         (let ((opening-block-start-points
                (monte-info-dedenter-opening-block-positions)))
           (if (not opening-block-start-points)
               0  ; if not found default to first column
             (mapcar (lambda (pos)
                       (save-excursion
                         (goto-char pos)
                         (current-indentation)))
                     opening-block-start-points))))
        (`(,(or :inside-paren-newline-start-from-block) . ,start)
         ;; Add two indentation levels to make the suite stand out.
         (goto-char start)
         (+ (current-indentation) (* monte-indent-offset 2)))))))

(defun monte-indent--calculate-levels (indentation)
  "Calculate levels list given INDENTATION.
Argument INDENTATION can either be an integer or a list of
integers.  Levels are returned in ascending order, and in the
case INDENTATION is a list, this order is enforced."
  (if (listp indentation)
      (sort (copy-sequence indentation) #'<)
    (let* ((remainder (% indentation monte-indent-offset))
           (steps (/ (- indentation remainder) monte-indent-offset))
           (levels (mapcar (lambda (step)
                             (* monte-indent-offset step))
                           (number-sequence steps 0 -1))))
      (reverse
       (if (not (zerop remainder))
           (cons indentation levels)
         levels)))))

(defun monte-indent--previous-level (levels indentation)
  "Return previous level from LEVELS relative to INDENTATION."
  (let* ((levels (sort (copy-sequence levels) #'>))
         (default (car levels)))
    (catch 'return
      (dolist (level levels)
        (when (funcall #'< level indentation)
          (throw 'return level)))
      default)))

(defun monte-indent-calculate-indentation (&optional previous)
  "Calculate indentation.
Get indentation of PREVIOUS level when argument is non-nil.
Return the max level of the cycle when indentation reaches the
minimum."
  (let* ((indentation (monte-indent--calculate-indentation))
         (levels (monte-indent--calculate-levels indentation)))
    (if previous
        (monte-indent--previous-level levels (current-indentation))
      (apply #'max levels))))

(defun monte-indent-line (&optional previous)
  "Internal implementation of `monte-indent-line-function'.
Use the PREVIOUS level when argument is non-nil, otherwise indent
to the maximum available level.  When indentation is the minimum
possible and PREVIOUS is non-nil, cycle back to the maximum
level."
  (let ((follow-indentation-p
         ;; Check if point is within indentation.
         (and (<= (line-beginning-position) (point))
              (>= (+ (line-beginning-position)
                     (current-indentation))
                  (point)))))
    (save-excursion
      (indent-line-to
       (monte-indent-calculate-indentation previous))
      (monte-info-dedenter-opening-block-message))
    (when follow-indentation-p
      (back-to-indentation))))

(defun monte-indent-calculate-levels ()
  "Return possible indentation levels."
  (monte-indent--calculate-levels
   (monte-indent--calculate-indentation)))

(defun monte-indent-line-function ()
  "`indent-line-function' for Python mode.
When the variable `last-command' is equal to one of the symbols
inside `monte-indent-trigger-commands' it cycles possible
indentation levels from right to left."
  (monte-indent-line
   (and (eq this-command 'indent-for-tab-command)
        (eq last-command this-command))))

(defun monte-indent-dedent-line ()
  "De-indent current line."
  (interactive "*")
  (when (and (not (bolp))
           (not (monte-syntax-comment-or-string-p))
           (= (current-indentation) (current-column)))
      (monte-indent-line t)
      t))

(defun monte-indent-dedent-line-backspace (arg)
  "De-indent current line.
Argument ARG is passed to `backward-delete-char-untabify' when
point is not in between the indentation."
  (interactive "*p")
  (unless (monte-indent-dedent-line)
    (backward-delete-char-untabify arg)))

(put 'monte-indent-dedent-line-backspace 'delete-selection 'supersede)

(defun monte-indent-region (start end)
  "Indent a Monte region automagically.

Called from a program, START and END specify the region to indent."
  (let ((deactivate-mark nil))
    (save-excursion
      (goto-char end)
      (setq end (point-marker))
      (goto-char start)
      (or (bolp) (forward-line 1))
      (while (< (point) end)
        (or (and (bolp) (eolp))
            (when (and
                   ;; Skip if previous line is empty or a comment.
                   (save-excursion
                     (let ((line-is-comment-p
                            (monte-info-current-line-comment-p)))
                       (forward-line -1)
                       (not
                        (or (and (monte-info-current-line-comment-p)
                                 ;; Unless this line is a comment too.
                                 (not line-is-comment-p))
                            (monte-info-current-line-empty-p)))))
                   ;; Don't mess with strings, unless it's the
                   ;; enclosing set of quotes.
                   (or (not (monte-syntax-context 'string))
                       (eq
                        (syntax-after
                         (+ (1- (point))
                            (current-indentation)
                            (monte-syntax-count-quotes (char-after) (point))))
                        (string-to-syntax "|")))
                   ;; Skip if current line is a block start, a
                   ;; dedenter or block ender.
                   (save-excursion
                     (back-to-indentation)
                     (not (looking-at
                           (monte-rx
                            (or block-start dedenter block-ender))))))
              (monte-indent-line)))
        (forward-line 1))
      (move-marker end nil))))

(defun monte-indent-shift-left (start end &optional count)
  "Shift lines contained in region START END by COUNT columns to the left.
COUNT defaults to `monte-indent-offset'.  If region isn't
active, the current line is shifted.  The shifted region includes
the lines in which START and END lie.  An error is signaled if
any lines in the region are indented less than COUNT columns."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end) current-prefix-arg)
     (list (line-beginning-position) (line-end-position) current-prefix-arg)))
  (if count
      (setq count (prefix-numeric-value count))
    (setq count monte-indent-offset))
  (when (> count 0)
    (let ((deactivate-mark nil))
      (save-excursion
        (goto-char start)
        (while (< (point) end)
          (if (and (< (current-indentation) count)
                   (not (looking-at "[ \t]*$")))
              (error "Can't shift all lines enough"))
          (forward-line))
        (indent-rigidly start end (- count))))))

(add-to-list 'debug-ignored-errors "^Can't shift all lines enough")

(defun monte-indent-shift-right (start end &optional count)
  "Shift lines contained in region START END by COUNT columns to the right.
COUNT defaults to `monte-indent-offset'.  If region isn't
active, the current line is shifted.  The shifted region includes
the lines in which START and END lie."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end) current-prefix-arg)
     (list (line-beginning-position) (line-end-position) current-prefix-arg)))
  (let ((deactivate-mark nil))
    (setq count (if count (prefix-numeric-value count)
                  monte-indent-offset))
    (indent-rigidly start end count)))

(defun monte-indent-post-self-insert-function ()
  "Adjust indentation after insertion of some characters.
This function is intended to be added to `post-self-insert-hook.'
If a line renders a paren alone, after adding a char before it,
the line will be re-indented automatically if needed."
  (when (and electric-indent-mode
             (eq (char-before) last-command-event))
    (cond
     ;; Electric indent inside parens
     ((and
       (not (bolp))
       (let ((paren-start (monte-syntax-context 'paren)))
         ;; Check that point is inside parens.
         (when paren-start
           (not
            ;; Filter the case where input is happening in the same
            ;; line where the open paren is.
            (= (line-number-at-pos)
               (line-number-at-pos paren-start)))))
       ;; When content has been added before the closing paren or a
       ;; comma has been inserted, it's ok to do the trick.
       (or
        (memq (char-after) '(?\) ?\] ?\}))
        (eq (char-before) ?,)))
      (save-excursion
        (goto-char (line-beginning-position))
        (let ((indentation (monte-indent-calculate-indentation)))
          (when (< (current-indentation) indentation)
            (indent-line-to indentation)))))
     ;; Electric colon
     ((and (eq ?: last-command-event)
           (memq ?: electric-indent-chars)
           (not current-prefix-arg)
           ;; Trigger electric colon only at end of line
           (eolp)
           ;; Avoid re-indenting on extra colon
           (not (equal ?: (char-before (1- (point)))))
           (not (monte-syntax-comment-or-string-p)))
      ;; Just re-indent dedenters
      (let ((dedenter-pos (monte-info-dedenter-statement-p))
            (current-pos (point)))
        (when dedenter-pos
          (save-excursion
            (goto-char dedenter-pos)
            (monte-indent-line)
            (unless (= (line-number-at-pos dedenter-pos)
                       (line-number-at-pos current-pos))
              ;; Reindent region if this is a multiline statement
              (monte-indent-region dedenter-pos current-pos)))))))))

(defun monte-syntax-context (type &optional syntax-ppss)
  "Return non-nil if point is on TYPE using SYNTAX-PPSS.
TYPE can be `comment', `string' or `paren'.  It returns the start
character address of the specified TYPE."
  (declare (compiler-macro monte-syntax--context-compiler-macro))
  (let ((ppss (or syntax-ppss (syntax-ppss))))
    (pcase type
      (`comment (and (nth 4 ppss) (nth 8 ppss)))
      (`string (and (nth 3 ppss) (nth 8 ppss)))
      (`paren (nth 1 ppss))
      (_ nil))))

(defun monte-syntax-context-type (&optional syntax-ppss)
  "Return the context type using SYNTAX-PPSS.
The type returned can be `comment', `string' or `paren'."
  (let ((ppss (or syntax-ppss (syntax-ppss))))
    (cond
     ((nth 8 ppss) (if (nth 4 ppss) 'comment 'string))
     ((nth 1 ppss) 'paren))))

;;; Navigation

(defvar monte-nav-beginning-of-defun-regexp
  (monte-rx line-start (* space) defun (+ space) (group symbol-name))
  "Regexp matching class or function definition.
The name of the defun should be grouped so it can be retrieved
via `match-string'.")

(defun monte-nav--beginning-of-defun (&optional arg)
  "Internal implementation of `monte-nav-beginning-of-defun'.
With positive ARG search backwards, else search forwards."
  (when (or (null arg) (= arg 0)) (setq arg 1))
  (let* ((re-search-fn (if (> arg 0)
                           #'re-search-backward
                         #'re-search-forward))
         (line-beg-pos (line-beginning-position))
         (line-content-start (+ line-beg-pos (current-indentation)))
         (pos (point-marker))
         (beg-indentation
          (and (> arg 0)
               (save-excursion
                 (while (and
                         (not (monte-info-looking-at-beginning-of-defun))
                         (monte-nav-backward-block)))
                 (or (and (monte-info-looking-at-beginning-of-defun)
                          (+ (current-indentation) monte-indent-offset))
                     0))))
         (found
          (progn
            (when (and (< arg 0)
                       (monte-info-looking-at-beginning-of-defun))
              (end-of-line 1))
            (while (and (funcall re-search-fn
                                 monte-nav-beginning-of-defun-regexp nil t)
                        (or (monte-syntax-context-type)
                            ;; Handle nested defuns when moving
                            ;; backwards by checking indentation.
                            (and (> arg 0)
                                 (not (= (current-indentation) 0))
                                 (>= (current-indentation) beg-indentation)))))
            (and (monte-info-looking-at-beginning-of-defun)
                 (or (not (= (line-number-at-pos pos)
                             (line-number-at-pos)))
                     (and (>= (point) line-beg-pos)
                          (<= (point) line-content-start)
                          (> pos line-content-start)))))))
    (if found
        (or (beginning-of-line 1) t)
      (and (goto-char pos) nil))))

(defun monte-nav-beginning-of-defun (&optional arg)
  "Move point to `beginning-of-defun'.
With positive ARG search backwards else search forward.
ARG nil or 0 defaults to 1.  When searching backwards,
nested defuns are handled with care depending on current
point position.  Return non-nil if point is moved to
`beginning-of-defun'."
  (when (or (null arg) (= arg 0)) (setq arg 1))
  (let ((found))
    (while (and (not (= arg 0))
                (let ((keep-searching-p
                       (monte-nav--beginning-of-defun arg)))
                  (when (and keep-searching-p (null found))
                    (setq found t))
                  keep-searching-p))
      (setq arg (if (> arg 0) (1- arg) (1+ arg))))
    found))

(defun monte-nav-end-of-defun ()
  "Move point to the end of def or class.
Returns nil if point is not in a def or class."
  (interactive)
  (let ((beg-defun-indent)
        (beg-pos (point)))
    (when (or (monte-info-looking-at-beginning-of-defun)
              (monte-nav-beginning-of-defun 1)
              (monte-nav-beginning-of-defun -1))
      (setq beg-defun-indent (current-indentation))
      (while (progn
               (monte-nav-end-of-statement)
               (monte-util-forward-comment 1)
               (and (> (current-indentation) beg-defun-indent)
                    (not (eobp)))))
      (monte-util-forward-comment -1)
      (forward-line 1)
      ;; Ensure point moves forward.
      (and (> beg-pos (point)) (goto-char beg-pos)))))

(defun monte-nav--syntactically (fn poscompfn &optional contextfn)
  "Move point using FN avoiding places with specific context.
FN must take no arguments.  POSCOMPFN is a two arguments function
used to compare current and previous point after it is moved
using FN, this is normally a less-than or greater-than
comparison.  Optional argument CONTEXTFN defaults to
`monte-syntax-context-type' and is used for checking current
point context, it must return a non-nil value if this point must
be skipped."
  (let ((contextfn (or contextfn 'monte-syntax-context-type))
        (start-pos (point-marker))
        (prev-pos))
    (catch 'found
      (while t
        (let* ((newpos
                (and (funcall fn) (point-marker)))
               (context (funcall contextfn)))
          (cond ((and (not context) newpos
                      (or (and (not prev-pos) newpos)
                          (and prev-pos newpos
                               (funcall poscompfn newpos prev-pos))))
                 (throw 'found (point-marker)))
                ((and newpos context)
                 (setq prev-pos (point)))
                (t (when (not newpos) (goto-char start-pos))
                   (throw 'found nil))))))))

(defun monte-nav--forward-defun (arg)
  "Internal implementation of monte-nav-{backward,forward}-defun.
Uses ARG to define which function to call, and how many times
repeat it."
  (let ((found))
    (while (and (> arg 0)
                (setq found
                      (monte-nav--syntactically
                       (lambda ()
                         (re-search-forward
                          monte-nav-beginning-of-defun-regexp nil t))
                       '>)))
      (setq arg (1- arg)))
    (while (and (< arg 0)
                (setq found
                      (monte-nav--syntactically
                       (lambda ()
                         (re-search-backward
                          monte-nav-beginning-of-defun-regexp nil t))
                       '<)))
      (setq arg (1+ arg)))
    found))

(defun monte-nav-backward-defun (&optional arg)
  "Navigate to closer defun backward ARG times.
Unlikely `monte-nav-beginning-of-defun' this doesn't care about
nested definitions."
  (interactive "^p")
  (monte-nav--forward-defun (- (or arg 1))))

(defun monte-nav-forward-defun (&optional arg)
  "Navigate to closer defun forward ARG times.
Unlikely `monte-nav-beginning-of-defun' this doesn't care about
nested definitions."
  (interactive "^p")
  (monte-nav--forward-defun (or arg 1)))

(defun monte-nav-beginning-of-statement ()
  "Move to start of current statement."
  (interactive "^")
  (back-to-indentation)
  (let* ((ppss (syntax-ppss))
         (context-point
          (or
           (monte-syntax-context 'paren ppss)
           (monte-syntax-context 'string ppss))))
    (cond ((bobp))
          (context-point
           (goto-char context-point)
           (monte-nav-beginning-of-statement))
          ((save-excursion
             (forward-line -1)
             (monte-info-line-ends-backslash-p))
           (forward-line -1)
           (monte-nav-beginning-of-statement))))
  (point-marker))

(defun monte-nav-end-of-statement (&optional noend)
  "Move to end of current statement.
Optional argument NOEND is internal and makes the logic to not
jump to the end of line when moving forward searching for the end
of the statement."
  (interactive "^")
  (let (string-start bs-pos)
    (while (and (or noend (goto-char (line-end-position)))
                (not (eobp))
                (cond ((setq string-start (monte-syntax-context 'string))
                       (goto-char string-start)
                       (if (monte-syntax-context 'paren)
                           ;; Ended up inside a paren, roll again.
                           (monte-nav-end-of-statement t)
                         ;; This is not inside a paren, move to the
                         ;; end of this string.
                         (goto-char (+ (point)
                                       (monte-syntax-count-quotes
                                        (char-after (point)) (point))))
                         (or (re-search-forward (rx (syntax string-delimiter)) nil t)
                             (goto-char (point-max)))))
                      ((monte-syntax-context 'paren)
                       ;; The statement won't end before we've escaped
                       ;; at least one level of parenthesis.
                       (condition-case err
                           (goto-char (scan-lists (point) 1 -1))
                         (scan-error (goto-char (nth 3 err)))))
                      ((setq bs-pos (monte-info-line-ends-backslash-p))
                       (goto-char bs-pos)
                       (forward-line 1))))))
  (point-marker))

(defun monte-nav-backward-statement (&optional arg)
  "Move backward to previous statement.
With ARG, repeat.  See `monte-nav-forward-statement'."
  (interactive "^p")
  (or arg (setq arg 1))
  (monte-nav-forward-statement (- arg)))

(defun monte-nav-forward-statement (&optional arg)
  "Move forward to next statement.
With ARG, repeat.  With negative argument, move ARG times
backward to previous statement."
  (interactive "^p")
  (or arg (setq arg 1))
  (while (> arg 0)
    (monte-nav-end-of-statement)
    (monte-util-forward-comment)
    (monte-nav-beginning-of-statement)
    (setq arg (1- arg)))
  (while (< arg 0)
    (monte-nav-beginning-of-statement)
    (monte-util-forward-comment -1)
    (monte-nav-beginning-of-statement)
    (setq arg (1+ arg))))

(defun monte-nav-beginning-of-block ()
  "Move to start of current block."
  (interactive "^")
  (let ((starting-pos (point)))
    (if (progn
          (monte-nav-beginning-of-statement)
          (looking-at (monte-rx block-start)))
        (point-marker)
      ;; Go to first line beginning a statement
      (while (and (not (bobp))
                  (or (and (monte-nav-beginning-of-statement) nil)
                      (monte-info-current-line-comment-p)
                      (monte-info-current-line-empty-p)))
        (forward-line -1))
      (let ((block-matching-indent
             (- (current-indentation) monte-indent-offset)))
        (while
            (and (monte-nav-backward-block)
                 (> (current-indentation) block-matching-indent)))
        (if (and (looking-at (monte-rx block-start))
                 (= (current-indentation) block-matching-indent))
            (point-marker)
          (and (goto-char starting-pos) nil))))))

(defun monte-nav-end-of-block ()
  "Move to end of current block."
  (interactive "^")
  (when (monte-nav-beginning-of-block)
    (let ((block-indentation (current-indentation)))
      (monte-nav-end-of-statement)
      (while (and (forward-line 1)
                  (not (eobp))
                  (or (and (> (current-indentation) block-indentation)
                           (or (monte-nav-end-of-statement) t))
                      (monte-info-current-line-comment-p)
                      (monte-info-current-line-empty-p))))
      (monte-util-forward-comment -1)
      (point-marker))))

(defun monte-nav-backward-block (&optional arg)
  "Move backward to previous block of code.
With ARG, repeat.  See `monte-nav-forward-block'."
  (interactive "^p")
  (or arg (setq arg 1))
  (monte-nav-forward-block (- arg)))

(defun monte-nav-forward-block (&optional arg)
  "Move forward to next block of code.
With ARG, repeat.  With negative argument, move ARG times
backward to previous block."
  (interactive "^p")
  (or arg (setq arg 1))
  (let ((block-start-regexp
         (monte-rx line-start (* whitespace) block-start))
        (starting-pos (point)))
    (while (> arg 0)
      (monte-nav-end-of-statement)
      (while (and
              (re-search-forward block-start-regexp nil t)
              (monte-syntax-context-type)))
      (setq arg (1- arg)))
    (while (< arg 0)
      (monte-nav-beginning-of-statement)
      (while (and
              (re-search-backward block-start-regexp nil t)
              (monte-syntax-context-type)))
      (setq arg (1+ arg)))
    (monte-nav-beginning-of-statement)
    (if (not (looking-at (monte-rx block-start)))
        (and (goto-char starting-pos) nil)
      (and (not (= (point) starting-pos)) (point-marker)))))

(defun monte-nav--lisp-forward-sexp (&optional arg)
  "Standard version `forward-sexp'.
It ignores completely the value of `forward-sexp-function' by
setting it to nil before calling `forward-sexp'.  With positive
ARG move forward only one sexp, else move backwards."
  (let ((forward-sexp-function)
        (arg (if (or (not arg) (> arg 0)) 1 -1)))
    (forward-sexp arg)))

(defun monte-nav--lisp-forward-sexp-safe (&optional arg)
  "Safe version of standard `forward-sexp'.
When at end of sexp (i.e. looking at a opening/closing paren)
skips it instead of throwing an error.  With positive ARG move
forward only one sexp, else move backwards."
  (let* ((arg (if (or (not arg) (> arg 0)) 1 -1))
         (paren-regexp
          (if (> arg 0) (monte-rx close-paren) (monte-rx open-paren)))
         (search-fn
          (if (> arg 0) #'re-search-forward #'re-search-backward)))
    (condition-case nil
        (monte-nav--lisp-forward-sexp arg)
      (error
       (while (and (funcall search-fn paren-regexp nil t)
                   (monte-syntax-context 'paren)))))))

(defun monte-nav--forward-sexp (&optional dir safe)
  "Move to forward sexp.
With positive optional argument DIR direction move forward, else
backwards.  When optional argument SAFE is non-nil do not throw
errors when at end of sexp, skip it instead."
  (setq dir (or dir 1))
  (unless (= dir 0)
    (let* ((forward-p (if (> dir 0)
                          (and (setq dir 1) t)
                        (and (setq dir -1) nil)))
           (context-type (monte-syntax-context-type)))
      (cond
       ((memq context-type '(string comment))
        ;; Inside of a string, get out of it.
        (let ((forward-sexp-function))
          (forward-sexp dir)))
       ((or (eq context-type 'paren)
            (and forward-p (looking-at (monte-rx open-paren)))
            (and (not forward-p)
                 (eq (syntax-class (syntax-after (1- (point))))
                     (car (string-to-syntax ")")))))
        ;; Inside a paren or looking at it, lisp knows what to do.
        (if safe
            (monte-nav--lisp-forward-sexp-safe dir)
          (monte-nav--lisp-forward-sexp dir)))
       (t
        ;; This part handles the lispy feel of
        ;; `monte-nav-forward-sexp'.  Knowing everything about the
        ;; current context and the context of the next sexp tries to
        ;; follow the lisp sexp motion commands in a symmetric manner.
        (let* ((context
                (cond
                 ((monte-info-beginning-of-block-p) 'block-start)
                 ((monte-info-end-of-block-p) 'block-end)
                 ((monte-info-beginning-of-statement-p) 'statement-start)
                 ((monte-info-end-of-statement-p) 'statement-end)))
               (next-sexp-pos
                (save-excursion
                  (if safe
                      (monte-nav--lisp-forward-sexp-safe dir)
                    (monte-nav--lisp-forward-sexp dir))
                  (point)))
               (next-sexp-context
                (save-excursion
                  (goto-char next-sexp-pos)
                  (cond
                   ((monte-info-beginning-of-block-p) 'block-start)
                   ((monte-info-end-of-block-p) 'block-end)
                   ((monte-info-beginning-of-statement-p) 'statement-start)
                   ((monte-info-end-of-statement-p) 'statement-end)
                   ((monte-info-statement-starts-block-p) 'starts-block)
                   ((monte-info-statement-ends-block-p) 'ends-block)))))
          (if forward-p
              (cond ((and (not (eobp))
                          (monte-info-current-line-empty-p))
                     (monte-util-forward-comment dir)
                     (monte-nav--forward-sexp dir))
                    ((eq context 'block-start)
                     (monte-nav-end-of-block))
                    ((eq context 'statement-start)
                     (monte-nav-end-of-statement))
                    ((and (memq context '(statement-end block-end))
                          (eq next-sexp-context 'ends-block))
                     (goto-char next-sexp-pos)
                     (monte-nav-end-of-block))
                    ((and (memq context '(statement-end block-end))
                          (eq next-sexp-context 'starts-block))
                     (goto-char next-sexp-pos)
                     (monte-nav-end-of-block))
                    ((memq context '(statement-end block-end))
                     (goto-char next-sexp-pos)
                     (monte-nav-end-of-statement))
                    (t (goto-char next-sexp-pos)))
            (cond ((and (not (bobp))
                        (monte-info-current-line-empty-p))
                   (monte-util-forward-comment dir)
                   (monte-nav--forward-sexp dir))
                  ((eq context 'block-end)
                   (monte-nav-beginning-of-block))
                  ((eq context 'statement-end)
                   (monte-nav-beginning-of-statement))
                  ((and (memq context '(statement-start block-start))
                        (eq next-sexp-context 'starts-block))
                   (goto-char next-sexp-pos)
                   (monte-nav-beginning-of-block))
                  ((and (memq context '(statement-start block-start))
                        (eq next-sexp-context 'ends-block))
                   (goto-char next-sexp-pos)
                   (monte-nav-beginning-of-block))
                  ((memq context '(statement-start block-start))
                   (goto-char next-sexp-pos)
                   (monte-nav-beginning-of-statement))
                  (t (goto-char next-sexp-pos))))))))))

(defun monte-nav-forward-sexp (&optional arg)
  "Move forward across expressions.
With ARG, do it that many times.  Negative arg -N means move
backward N times."
  (interactive "^p")
  (or arg (setq arg 1))
  (while (> arg 0)
    (monte-nav--forward-sexp 1)
    (setq arg (1- arg)))
  (while (< arg 0)
    (monte-nav--forward-sexp -1)
    (setq arg (1+ arg))))

(defun monte-nav-backward-sexp (&optional arg)
  "Move backward across expressions.
With ARG, do it that many times.  Negative arg -N means move
forward N times."
  (interactive "^p")
  (or arg (setq arg 1))
  (monte-nav-forward-sexp (- arg)))

(defun monte-nav-forward-sexp-safe (&optional arg)
  "Move forward safely across expressions.
With ARG, do it that many times.  Negative arg -N means move
backward N times."
  (interactive "^p")
  (or arg (setq arg 1))
  (while (> arg 0)
    (monte-nav--forward-sexp 1 t)
    (setq arg (1- arg)))
  (while (< arg 0)
    (monte-nav--forward-sexp -1 t)
    (setq arg (1+ arg))))

(defun monte-nav-backward-sexp-safe (&optional arg)
  "Move backward safely across expressions.
With ARG, do it that many times.  Negative arg -N means move
forward N times."
  (interactive "^p")
  (or arg (setq arg 1))
  (monte-nav-forward-sexp-safe (- arg)))

(defun monte-nav--up-list (&optional dir)
  "Internal implementation of `monte-nav-up-list'.
DIR is always 1 or -1 and comes sanitized from
`monte-nav-up-list' calls."
  (let ((context (monte-syntax-context-type))
        (forward-p (> dir 0)))
    (cond
     ((memq context '(string comment)))
     ((eq context 'paren)
      (let ((forward-sexp-function))
        (up-list dir)))
     ((and forward-p (monte-info-end-of-block-p))
      (let ((parent-end-pos
             (save-excursion
               (let ((indentation (and
                                   (monte-nav-beginning-of-block)
                                   (current-indentation))))
                 (while (and indentation
                             (> indentation 0)
                             (>= (current-indentation) indentation)
                             (monte-nav-backward-block)))
                 (monte-nav-end-of-block)))))
        (and (> (or parent-end-pos (point)) (point))
             (goto-char parent-end-pos))))
     (forward-p (monte-nav-end-of-block))
     ((and (not forward-p)
           (> (current-indentation) 0)
           (monte-info-beginning-of-block-p))
      (let ((prev-block-pos
             (save-excursion
               (let ((indentation (current-indentation)))
                 (while (and (monte-nav-backward-block)
                             (>= (current-indentation) indentation))))
               (point))))
        (and (> (point) prev-block-pos)
             (goto-char prev-block-pos))))
     ((not forward-p) (monte-nav-beginning-of-block)))))

(defun monte-nav-up-list (&optional arg)
  "Move forward out of one level of parentheses (or blocks).
With ARG, do this that many times.
A negative argument means move backward but still to a less deep spot.
This command assumes point is not in a string or comment."
  (interactive "^p")
  (or arg (setq arg 1))
  (while (> arg 0)
    (monte-nav--up-list 1)
    (setq arg (1- arg)))
  (while (< arg 0)
    (monte-nav--up-list -1)
    (setq arg (1+ arg))))

(defun monte-nav-backward-up-list (&optional arg)
  "Move backward out of one level of parentheses (or blocks).
With ARG, do this that many times.
A negative argument means move forward but still to a less deep spot.
This command assumes point is not in a string or comment."
  (interactive "^p")
  (or arg (setq arg 1))
  (monte-nav-up-list (- arg)))

(defun monte-info-current-defun (&optional include-type)
  "Return name of surrounding function with Python compatible dotty syntax.
Optional argument INCLUDE-TYPE indicates to include the type of the defun.
This function can be used as the value of `add-log-current-defun-function'
since it returns nil if point is not inside a defun."
  (save-restriction
    (widen)
    (save-excursion
      (end-of-line 1)
      (let ((names)
            (starting-indentation (current-indentation))
            (starting-pos (point))
            (first-run t)
            (last-indent)
            (type))
        (catch 'exit
          (while (monte-nav-beginning-of-defun 1)
            (when (save-match-data
                    (and
                     (or (not last-indent)
                         (< (current-indentation) last-indent))
                     (or
                      (and first-run
                           (save-excursion
                             ;; If this is the first run, we may add
                             ;; the current defun at point.
                             (setq first-run nil)
                             (goto-char starting-pos)
                             (monte-nav-beginning-of-statement)
                             (beginning-of-line 1)
                             (looking-at-p
                              monte-nav-beginning-of-defun-regexp)))
                      (< starting-pos
                         (save-excursion
                           (let ((min-indent
                                  (+ (current-indentation)
                                     monte-indent-offset)))
                             (if (< starting-indentation  min-indent)
                                 ;; If the starting indentation is not
                                 ;; within the min defun indent make the
                                 ;; check fail.
                                 starting-pos
                               ;; Else go to the end of defun and add
                               ;; up the current indentation to the
                               ;; ending position.
                               (monte-nav-end-of-defun)
                               (+ (point)
                                  (if (>= (current-indentation) min-indent)
                                      (1+ (current-indentation))
                                    0)))))))))
              (save-match-data (setq last-indent (current-indentation)))
              (if (or (not include-type) type)
                  (setq names (cons (match-string-no-properties 1) names))
                (let ((match (split-string (match-string-no-properties 0))))
                  (setq type (car match))
                  (setq names (cons (cadr match) names)))))
            ;; Stop searching ASAP.
            (and (= (current-indentation) 0) (throw 'exit t))))
        (and names
             (concat (and type (format "%s " type))
                     (mapconcat 'identity names ".")))))))

(defun monte-info-statement-starts-block-p ()
  "Return non-nil if current statement opens a block."
  (save-excursion
    (monte-nav-beginning-of-statement)
    (looking-at (monte-rx block-start))))

(defun monte-info-statement-ends-block-p ()
  "Return non-nil if point is at end of block."
  (let ((end-of-block-pos (save-excursion
                            (monte-nav-end-of-block)))
        (end-of-statement-pos (save-excursion
                                (monte-nav-end-of-statement))))
    (and end-of-block-pos end-of-statement-pos
         (= end-of-block-pos end-of-statement-pos))))

(defun monte-info-beginning-of-statement-p ()
  "Return non-nil if point is at beginning of statement."
  (= (point) (save-excursion
               (monte-nav-beginning-of-statement)
               (point))))

(defun monte-info-end-of-statement-p ()
  "Return non-nil if point is at end of statement."
  (= (point) (save-excursion
               (monte-nav-end-of-statement)
               (point))))

(defun monte-info-beginning-of-block-p ()
  "Return non-nil if point is at beginning of block."
  (and (monte-info-beginning-of-statement-p)
       (monte-info-statement-starts-block-p)))

(defun monte-info-end-of-block-p ()
  "Return non-nil if point is at end of block."
  (and (monte-info-end-of-statement-p)
       (monte-info-statement-ends-block-p)))

(define-obsolete-function-alias
  'monte-info-closing-block
  'monte-info-dedenter-opening-block-position "24.4")

(defun monte-info-dedenter-opening-block-position ()
  "Return the point of the closest block the current line closes.
Returns nil if point is not on a dedenter statement or no opening
block can be detected.  The latter case meaning current file is
likely an invalid python file."
  (let ((positions (monte-info-dedenter-opening-block-positions))
        (indentation (current-indentation))
        (position))
    (while (and (not position)
                positions)
      (save-excursion
        (goto-char (car positions))
        (if (<= (current-indentation) indentation)
            (setq position (car positions))
          (setq positions (cdr positions)))))
    position))

(defun monte-info-dedenter-opening-block-positions ()
  "Return points of blocks the current line may close sorted by closer.
Returns nil if point is not on a dedenter statement or no opening
block can be detected.  The latter case meaning current file is
likely an invalid python file."
  (save-excursion
    (let ((dedenter-pos (monte-info-dedenter-statement-p)))
      (when dedenter-pos
        (goto-char dedenter-pos)
        (let* ((pairs '(("elif" "elif" "if")
                        ("else" "if" "elif" "except" "for" "while")
                        ("except" "except" "try")
                        ("finally" "else" "except" "try")))
               (dedenter (match-string-no-properties 0))
               (possible-opening-blocks (cdr (assoc-string dedenter pairs)))
               (collected-indentations)
               (opening-blocks))
          (catch 'exit
            (while (monte-nav--syntactically
                    (lambda ()
                      (re-search-backward (monte-rx block-start) nil t))
                    #'<)
              (let ((indentation (current-indentation)))
                (when (and (not (memq indentation collected-indentations))
                           (or (not collected-indentations)
                               (< indentation (apply #'min collected-indentations))))
                  (setq collected-indentations
                        (cons indentation collected-indentations))
                  (when (member (match-string-no-properties 0)
                                possible-opening-blocks)
                    (setq opening-blocks (cons (point) opening-blocks))))
                (when (zerop indentation)
                  (throw 'exit nil)))))
          ;; sort by closer
          (nreverse opening-blocks))))))

(define-obsolete-function-alias
  'monte-info-closing-block-message
  'monte-info-dedenter-opening-block-message "24.4")

(defun monte-info-dedenter-opening-block-message  ()
  "Message the first line of the block the current statement closes."
  (let ((point (monte-info-dedenter-opening-block-position)))
    (when point
      (save-restriction
        (widen)
        (message "Closes %s" (save-excursion
                               (goto-char point)
                               (buffer-substring
                                (point) (line-end-position))))))))

(defun monte-info-dedenter-statement-p ()
  "Return point if current statement is a dedenter.
Sets `match-data' to the keyword that starts the dedenter
statement."
  (save-excursion
    (monte-nav-beginning-of-statement)
    (when (and (not (monte-syntax-context-type))
               (looking-at (monte-rx dedenter)))
      (point))))

(defun monte-info-line-ends-backslash-p (&optional line-number)
  "Return non-nil if current line ends with backslash.
With optional argument LINE-NUMBER, check that line instead."
  (save-excursion
    (save-restriction
      (widen)
      (when line-number
        (monte-util-goto-line line-number))
      (while (and (not (eobp))
                  (goto-char (line-end-position))
                  (monte-syntax-context 'paren)
                  (not (equal (char-before (point)) ?\\)))
        (forward-line 1))
      (when (equal (char-before) ?\\)
        (point-marker)))))

(defun monte-info-beginning-of-backslash (&optional line-number)
  "Return the point where the backslashed line start.
Optional argument LINE-NUMBER forces the line number to check against."
  (save-excursion
    (save-restriction
      (widen)
      (when line-number
        (monte-util-goto-line line-number))
      (when (monte-info-line-ends-backslash-p)
        (while (save-excursion
                 (goto-char (line-beginning-position))
                 (monte-syntax-context 'paren))
          (forward-line -1))
        (back-to-indentation)
        (point-marker)))))

(defun monte-info-continuation-line-p ()
  "Check if current line is continuation of another.
When current line is continuation of another return the point
where the continued line ends."
  (save-excursion
    (save-restriction
      (widen)
      (let* ((context-type (progn
                             (back-to-indentation)
                             (monte-syntax-context-type)))
             (line-start (line-number-at-pos))
             (context-start (when context-type
                              (monte-syntax-context context-type))))
        (cond ((equal context-type 'paren)
               ;; Lines inside a paren are always a continuation line
               ;; (except the first one).
               (monte-util-forward-comment -1)
               (point-marker))
              ((member context-type '(string comment))
               ;; move forward an roll again
               (goto-char context-start)
               (monte-util-forward-comment)
               (monte-info-continuation-line-p))
              (t
               ;; Not within a paren, string or comment, the only way
               ;; we are dealing with a continuation line is that
               ;; previous line contains a backslash, and this can
               ;; only be the previous line from current
               (back-to-indentation)
               (monte-util-forward-comment -1)
               (when (and (equal (1- line-start) (line-number-at-pos))
                          (monte-info-line-ends-backslash-p))
                 (point-marker))))))))

(defun monte-info-block-continuation-line-p ()
  "Return non-nil if current line is a continuation of a block."
  (save-excursion
    (when (monte-info-continuation-line-p)
      (forward-line -1)
      (back-to-indentation)
      (when (looking-at (monte-rx block-start))
        (point-marker)))))

(defun monte-info-assignment-continuation-line-p ()
  "Check if current line is a continuation of an assignment.
When current line is continuation of another with an assignment
return the point of the first non-blank character after the
operator."
  (save-excursion
    (when (monte-info-continuation-line-p)
      (forward-line -1)
      (back-to-indentation)
      (when (and (not (looking-at (monte-rx block-start)))
                 (and (re-search-forward (monte-rx not-simple-operator
                                                    assignment-operator
                                                    not-simple-operator)
                                         (line-end-position) t)
                      (not (monte-syntax-context-type))))
        (skip-syntax-forward "\s")
        (point-marker)))))

(defun monte-info-looking-at-beginning-of-defun (&optional syntax-ppss)
  "Check if point is at `beginning-of-defun' using SYNTAX-PPSS."
  (and (not (monte-syntax-context-type (or syntax-ppss (syntax-ppss))))
       (save-excursion
         (beginning-of-line 1)
         (looking-at monte-nav-beginning-of-defun-regexp))))

(defun monte-info-current-line-comment-p ()
  "Return non-nil if current line is a comment line."
  (char-equal
   (or (char-after (+ (line-beginning-position) (current-indentation))) ?_)
   ?#))

(defun monte-info-current-line-empty-p ()
  "Return non-nil if current line is empty, ignoring whitespace."
  (save-excursion
    (beginning-of-line 1)
    (looking-at
     (monte-rx line-start (* whitespace)
                (group (* not-newline))
                (* whitespace) line-end))
    (string-equal "" (match-string-no-properties 1))))

;;; Utility functions

(defun monte-util-goto-line (line-number)
  "Move point to LINE-NUMBER."
  (goto-char (point-min))
  (forward-line (1- line-number)))

(defun monte-util-forward-comment (&optional direction)
  "Monte mode specific version of `forward-comment'.
Optional argument DIRECTION defines the direction to move to."
  (let ((comment-start (monte-syntax-context 'comment))
        (factor (if (< (or direction 0) 0)
                    -99999
                  99999)))
    (when comment-start
      (goto-char comment-start))
    (forward-comment factor)))

(defun monte-util-popn (lst n)
  "Return LST first N elements.
N should be an integer, when negative its opposite is used.
When N is bigger than the length of LST, the list is
returned as is."
  (let* ((n (min (abs n)))
         (len (length lst))
         (acc))
    (if (> n len)
        lst
      (while (< 0 n)
        (setq acc (cons (car lst) acc)
              lst (cdr lst)
              n (1- n)))
      (reverse acc))))

(defun monte-util-strip-string (string)
  "Strip STRING whitespace and newlines from end and beginning."
  (replace-regexp-in-string
   (rx (or (: string-start (* (any whitespace ?\r ?\n)))
           (: (* (any whitespace ?\r ?\n)) string-end)))
   ""
   string))

(defun monte-util-valid-regexp-p (regexp)
  "Return non-nil if REGEXP is valid."
  (ignore-errors (string-match regexp "") t))
