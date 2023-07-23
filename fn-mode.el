;;; fn-mode.el --- Major mode for editing fn source code
(provide 'fn-mode)

;;; Important notes about using this software:
;; - This is a very aggressive indenter. The default settings will make it so
;;   space, backspace, tab, and shift+tab only indent to legal levels (space and
;;   backspace behave normally when not editing leading whitespace). Space and
;;   backspace only affect the current line, so they can sometimes cause lines
;;   AFTER the indented line to have illegal offsets. Tab and shift+tab will
;;   look ahead and make the necessary changes to ensure existing groups are
;;   preserved. This is intended behavior so that groups can be rearranged with
;;   space/backspace even if this momentarily breaks indentation on subsequent
;;   line.
;; - Tab works like python-mode tab, in that repeated presses cycle through
;;   available offsets. The first press usually does nothing unless the line has
;;   an illegal offset to start. Shift+tab cycles in the other direction.
;; - Triple quote doc strings are not properly handled. They still mostly work
;;   (unless you include unescaped single quotes in them), but fn-mode thinks
;;   they're actually three normal strings.
;; - It is recommended to set `electric-indent-mode' to t (at least locally)
;; - fn is in a very unstable state. I will provide no guarantees about the
;;   correctness of the tools here until things settle down. This will take a
;;   while.
;; - I gotta be honest with you guys, I did not test all the options.

;;; Other notes that might be of interest
;; - I keep changing what keywords are in the language, so the list that gets
;;   highlighted here should not be taken to be authoritative
;; - I use evil-mode, and want to add better integration (for instance, making
;;   the 'x' key in normal mode indent-aware)
;; - I hope to add structural editing features in the future. There are some
;;   comments throughout that show what this might look like.
;; - Ideally I'd be doing this with tree-sitter, but fn is not a friendly
;;   language to parse with tree-sitter (significant whitespace can't be caught
;;   with a CFG, so I'll need a handwritten tokenizer at the least).
;; - The way nvim indents most languages with tree-sitter queries is useless for
;;   fn (as far as I can tell). In fn, we may have multiple distinct offsets
;;   corresponding to the same indentation level, and
;;   decreases in indentation that continue in the same group.
;; - There are generally many legal offsets for a line in fn, so there's a
;;   little bit of extra code that will copy the previous line's indentation on
;;   new lines. This can be configured with
;;   `fn-mode-blank-lines-before-toplevel'.

;;; Notes of varying degrees of importance that are only of interest to me
;; - method names in a methods form don't get highlighted properly
;; - the code that computes indentation offsets is complicated and finicky. It's
;;   complicated enough that it should definitely have a test suite before I
;;   make any modifications to it. Like, it will probably save work to just
;;   write the test suite first. I really hope I read this comment and don't
;;   waste a ton of time trying to hack on this without a test suite :'(
;; - the main to-do items are:
;;   - test suite
;;   - triple-quote strings
;;   - revisit the stuff I do using `last-command' and `this-command' and see
;;     if there's a better/more robust/idiomatic way to do what I'm doing. I
;;     the overall idea is ok but I need to select which commands I'm
;;     interested in more carefully.
;;   - smart delimiters
;;   - better evil integration
;; - this commenting style has been very helpful for me, both to remember things
;;   and to blow off a little bit of steam. It might look a little
;;   unprofessional, but I do not have the resources to worry about
;;   professionalism on this project. The one downside might be making sure the
;;   many comments are up to date.
;; - Some types of symbols in fn (those leading with tildes or colons) have
;;   slightly diferent grouping behaviors. Type annotations also have some
;;   interesting grouping semantics. These are not considered at all right now,
;;   but once the language is in a more stable state, we may want to reexamine
;;   how those are treated.
;;
;;
;; - THIS INDENTER IS MORE THAN GOOD ENOUGH IN ITS CURRENT STATE. the fn
;;   compiler should take priority over the tools until it works. I'm not going
;;   to work on this emacs extension again (except small modifications) until
;;   then.

;;; Custom Variables

(defcustom fn-mode-hook '()
  "List of functions to call after `fn-mode' is enabled."
  :group 'fn-mode
  :type 'hook
  :options nil)

(defcustom fn-mode-indentation-wraps t
  "If t, indentation commands wrap around the max/min offsets."
  :group 'fn-mode
  :type 'boolean
  :options '(t nil))

(defcustom fn-mode-backspace-dedents t
  "If t, backspace behavior is indentation-aware."
  :group 'fn-mode
  :type 'boolean
  :options '(t nil))

(defcustom fn-mode-space-indents t
  "If t, space insertion becomes indentation-aware."
  :group 'fn-mode
  :type 'boolean
  :options '(t nil))

(defcustom fn-mode-blank-lines-before-toplevel 1
  "The number of blank (and commentless) lines after which the
indenter starts guessing that we're back at the toplevel.

For example, with the default value of 1, every time we insert a
new line after a blank one, the indenter will guess an initial
indentation level of 0.

This setting only applies to newly inserted lines and will not
trigger reindentation of legal code that happens to contain blank
lines. It will do nothing if `fn-mode-indent-new-lines' is nil.
"
  :group 'fn-mode
  :type 'integer
  :options '(1 2 3 4))

;;; TODO: revise and implement the features below

;;; IMPLNOTE: the commented-out options below would be used to add paredit-style
;;; structural editing to fn-mode. I am honestly not really sure how much we
;;; would benefit from this in fn, and it would require the introduction of a
;;; bunch more commands/keyboard shortcuts. I still want to do something with
;;; delimiter behavior, but I'll come back to that later after getting some
;;; higher-priority items done.

;; (defcustom fn-mode-smart-delimiters t
;;   "Prevent insertion of unmatched delimiters (parens, brackets,
;; braces).

;; This option does not affect quotes. See `fn-mode-smart-quotes'
;; for similar behavior for quotes. This option also does not do
;; anything when the cursor is within a string.

;; When t, rebinds opening delimiter keys so they also insert the
;; corresponding closing delimiter after the cursor. Also changes
;; the behavior of the closing delimiter keys so they may be used
;; for navigation.

;; When the cursor is between paired delimiters, for instance
;; parentheses, then typing any closing delimiter will jump to the
;; next closing delimiter that currently encloses the point. If
;; there is an opening delimiter but no matching closing delimiter,
;; then the correct closing delimiter is inserted. If the point is
;; not within any paired delimiters, nothing happens.
;; "
;;   :group 'fn-mode
;;   :type 'boolean
;;   :options '(t nil))

;; (defcustom fn-mode-smart-quotes t
;;   "Prevent insertion of unmatched quotes.

;; If t, the quote character will not insert unmatched quote
;; characters. Instead, it inserts quotes in pairs when pressed
;; outside of a string, and jumps to the closing quote when pressed
;; inside of a (properly-terminated) string. The jumping behavior is
;; nullified if there is a backslash before the cursor, so escaped
;; quotes can still be typed easily. When typed within an
;; unterminated string, inserts a quote character.

;; See also `fn-mode-smart-delimiters'.
;; "
;;   :group 'fn-mode
;;   :type 'boolean
;;   :options '(t nil))


;;; A couple house-keeping duties

(add-to-list 'auto-mode-alist '("\\.fn\\'" . fn-mode))

;; A defunct CL implementation used .fn as the extension for some of its build
;; artifacts. As a result, .fn is one of a small group of extensions that Emacs
;; completion ignores by default. What a garbage editor.
(setq completion-ignored-extensions
      (remove ".fn" completion-ignored-extensions))


;;; Syntax table and font-lock-defaults

;; Printable characters which (at the time of writing) cannot legally appear
;; anywhere in Fn source code.
(setq fn-mode--reserved-characters
  '(?` ?! ?@ ?$ ?^ ?& ?\\ ?\; ?\" ?,))

(setq fn-mode--syntax-table
  (let ((s (make-syntax-table)))
    ;; this takes care of non-printable characters
    (cl-loop for ch from 0 to 31
      do (modify-syntax-entry ch "@" s))
    ;; set all letters and numbers to word constituents
    (cl-loop for ch from 48 to 57
      do (modify-syntax-entry ch "w" s))
    (cl-loop for ch from 65 to 90
      do (modify-syntax-entry ch "w" s))
    (cl-loop for ch from 97 to 122
      do (modify-syntax-entry ch "w" s))
    ;; whitespace and comments
    (modify-syntax-entry ?\n ">" s)
    (modify-syntax-entry ?\s " " s)
    (modify-syntax-entry ?\# "<" s)
    ;; reserved characters
    (dolist (ch fn-mode--reserved-characters)
      (modify-syntax-entry ch "." s))
    ;; additional symbol constituents
    (dolist (c '(?~ ?% ?- ?_ ?= ?+ ?| ?: ?< ?. ?> ?? ?/))
      (modify-syntax-entry c "_" s))
    ;; delimiters
    (modify-syntax-entry ?\( "()" s)
    (modify-syntax-entry ?\) ")(" s)
    (modify-syntax-entry ?\[ "(]" s)
    (modify-syntax-entry ?\{ "(}" s)
    (modify-syntax-entry ?\] ")[" s)
    (modify-syntax-entry ?\} "){" s)
    (modify-syntax-entry ?\' "\"" s)
   s))

(let* ((symb (rx (+ (or (syntax symbol) (syntax word)))))
       (fn-intro (rx symbol-start
                     (or "fn" "function" "macro")
                     symbol-end
                     (+ space)
                     (? "(")))
       (type-intro (rx symbol-start
                       (or "extend" "structure" "variant")
                       symbol-end
                       (+ space)
                       (? "(")))
       (keyword (regexp-opt
                 '("branch" "def" "do" "extend" "fn" "if" "implement" "import"
                   "let" "macro" "match" "method" "namespace" "protocol" "self"
                   "structure" "variant")
                 'symbols)))
  (setq fn--font-lock-defaults
        `((,keyword 
           0 font-lock-keyword-face)
          (,(regexp-opt (mapcar 'string fn-mode--reserved-characters))
           0 font-lock-warning-face)
          (,(rx symbol-start (or "yes" "no" "else") symbol-end)
           0 font-lock-constant-face)
          (,(rx ":" (regexp symb))
           0 font-lock-function-name-face)
          (,(rx (regexp fn-intro) (* space) (group (regexp symb)))
           1 font-lock-function-name-face)
          (,(rx (regexp type-intro) (* space) (group (regexp symb)))
           1 font-lock-type-face)
          (,(rx "[" (group (+ (not "]"))) "]")
           1 font-lock-type-face))))

;;; Indentation
(defun fn-mode--point-backslashed-p ()
  "Whether char at (point) is preceded by an odd number of backslashes."
  (let ((p (point)))
    (save-excursion
      (skip-chars-backward "\\\\")
      (= (mod (- p (point)) 2) 1))))

;; FIXME: triple quote strings
(defun fn-mode--end-cur-string ()
  "Move to the next unescaped quote character."
  (skip-chars-forward "^'")
  (cl-loop while (fn-mode--point-backslashed-p)
           do (progn (forward-char)
                     (skip-chars-forward "^'"))))

(defun fn-mode--end-of-form ()
  "Move to the last character of the form under the point. If
the point is not currently inside a form, move to the end of the
next form."
  (let ((p (syntax-ppss)))
    (if (nth 3 p)
        (fn-mode--end-cur-string)
      (progn
        ;; IMPLNOTE: using forward-sexp here feels kinda dirty... but it's
        ;; exactly what we need. Probably not worth implementing any more
        ;; parsing than this.
        (forward-sexp)
        (backward-char)))))

(defun fn-mode--line-blank-p ()
  "nil unless the current line is just spaces (not even comments)."
  ;; Fn only accepts the one true whitespace character: the ASCII space, so we
  ;; don't even need to check for tabs here.
  (and (cl-every (lambda (c)
                   (eq (char-syntax c) ?\s))
                 (buffer-substring-no-properties
                  (line-beginning-position)
                  (line-end-position)))
       t))

(defun fn-mode--line-blank-or-comment-p ()
  "nil unless the current line is just spaces and comments."
  (save-excursion
    (beginning-of-line)
    (looking-at-p " *\\(#.*\\)?$")))

(defun fn-mode--next-line-with (pred &optional backward max-depth)
  "Move the point one line at a time until (PRED) is true. Returns
the value of (PRED) if a line was found and NIL if the
beginning/end of the buffer was reached. Always advances at least
one line. If MAX-DEPTH is a positive integer, the search will
stop early after MAX-DEPTH lines have been checked. If BACKWARD
is true, the search will proceed backwards instead of forwards."
  (let* ((dir (if backward -1 1))
         (start-line (line-number-at-pos))
         (stop-on-line (if (and max-depth (> max-depth 0))
                           (+ (* dir max-depth) start-line)
                         (if backward 1 (line-number-at-pos (point-max))))))
    (cl-loop initially (forward-line dir)
             for test = (funcall pred) then (funcall pred)
             ;; this is important: we call (line-number-at-pos) again to
             ;; initialize line in case the previous (forward-line) call failed
             for line = (line-number-at-pos) then (+ dir line)
             while (and (not (= line stop-on-line))
                        (not test))
             do (forward-line dir)
             finally (return test))))

(defun fn-mode--to-parent-line ()
  "Move the cursor to the beginning of the group containing the
current line."
  (when (fn-mode--line-blank-p)
    (fn-mode--to-nonblank-line t nil))
  (let* ((start (current-indentation))
         (res (fn-mode--next-line-with
               (lambda ()
                 (and (< (current-indentation) start)
                      (not (fn-mode--line-blank-p))))
               t
               nil)))
    (back-to-indentation)
    res))

(defun fn-mode--child-info-helper (op-pos by-2 arg1)
  "Finds the ARG1-END-LINE and BY-2-START-LINE quantities returned by
`fn-mode--line-group-info'"
  (let* ((end-arg1 (line-number-at-pos op-pos)))
    (if arg1
        (save-excursion
          (goto-char op-pos)
          (forward-line)
          ;; find the end of the arg1 arguments
          (cl-loop
           for offset = (current-indentation) then (current-indentation)
           while (and (not (eobp))
                      ;; IMPLNOTE: assuming correctly formatted code, it would
                      ;; also be fine to do (>= offset arg1) here. The behavior
                      ;; might be subtly different when code is improperly
                      ;; formatted
                      (or (> offset by-2)
                          (fn-mode--line-blank-p)))
           do (when (and (= offset arg1)
                         (not (fn-mode--line-blank-p)))
                ;; note: this will also catch comments. that's good actually
                (setq end-arg1 (line-number-at-pos)))
           do (forward-line))
          ;; find the first by-2 argument
          (cl-loop
           for offset = (current-indentation) then (current-indentation)
           while (and (not (eobp))
                      (or (> offset by-2)
                          (fn-mode--line-blank-p)))
           do (forward-line))
          (beginning-of-line)
          (list end-arg1 (max (line-number-at-pos) (+ end-arg1 1))))
      (list end-arg1 end-arg1))))

(defun fn-mode--line-group-info ()
  "Give information about the group starting on the current line.

Returns a list of six items, (OP-POS BY-2 ARG1 ARG1-STOP-LINE
BY2-START-LINE):

- OP-POS is the position in the buffer of the operator
- BY-2 is the column two spaces past that of the operator
- ARG1 is the column of the first argument only if it represents
  a legal indentation column distinct from BY-2. In all other
  cases it is nil.
- ARG1-STOP-LINE is the largest line number containing a child of
  this line indented to the ARG1 level. If ARG1 is nil, this will
  be the current line
- BY2-START-LINE is the smallest line number containing a child
  of this line indented to the BY-2 level. If there are no such
  args, this will be one more than the current line.

If the current line does not start a new group, the behavior of
this function is undefined.
"
  (let (op-pos stop-lines)
    (save-excursion
      (back-to-indentation)
      (setq op-pos (point))
      (let ((by-2 (+ (current-indentation) 2))
            (arg1 (fn-mode--arg1-offset)))
        (setq arg1 (and arg1 (not (= by-2 arg1)) arg1))
        (setq stop-lines
              (fn-mode--child-info-helper op-pos by-2 arg1))
        (cons op-pos
              (cons by-2
                    (cons arg1 stop-lines)))))))

(defun fn-mode--to-nonblank-line (&optional backward max-depth)
  "Move the point up to MAX-DEPTH lines, stopping at the first
nonblank line encountered. If MAX-DEPTH is nil goes to the
end/beginning of the document."
  (fn-mode--next-line-with (lambda () (not (fn-mode--line-blank-p)))
                           backward
                           max-depth))

(defun fn-mode--prev-line-offset (max-depth)
  "Determine the indentation offset (in characters) of the last
non-blank line up to MAX-DEPTH. Unlike other fn-mode commands,
this will consider comment lines non-blank. Returns 0 if every
line is blank."
  (if (> (line-number-at-pos) 1)
      (save-excursion
        (fn-mode--to-nonblank-line t max-depth)
        ;; check if the last line is still blank
        (if (fn-mode--line-blank-p)
            0
          (current-indentation)))
    ;; return 0 on the first line
    0))

(defun fn-mode--arg1-offset ()
  "Return the indentation offset of the first argument of the
current line group."
  ;; FIXME: fn-mode--end-of-form is a little sus
  (save-excursion
    (let ((start-line (line-number-at-pos)))
      (back-to-indentation)
      (fn-mode--end-of-form)
      (unless (> (line-number-at-pos) start-line)
        (forward-char)
        (skip-chars-forward " ")
        (and (not (eolp))
             (current-column))))))

(defun fn-mode--offsets-helper (rel-to-line)
  (when (not rel-to-line) (setq rel-to-line (+ (line-number-at-pos) 1)))
  (let* ((info (fn-mode--line-group-info))
         (by-2 (cadr info))
         (arg1 (caddr info))
         (arg1-end (cadddr info))
         (by-2-start (cadddr (cdr info)))
         (from-this-group
          ;; don't allow indenting past comments. (They are still taken into
          ;; account for picking subgroup indentation levels, however).
          (unless (fn-mode--line-blank-or-comment-p)
            (cons by-2
                  (when (and arg1 (<= rel-to-line by-2-start))
                    (list arg1))))))
    (append (save-excursion
              (back-to-indentation)
              (if (fn-mode--to-parent-line)
                  (fn-mode--offsets-helper rel-to-line)
                (list 0)))
            from-this-group)))

(defun fn-mode--offsets-after ()
  "Get a list of all legal indentation offsets which could legally
follow the current line."
  (fn-mode--offsets-helper (+ (line-number-at-pos) 1)))

(defun fn-mode--all-offsets ()
  "Get a list of all legal indentation offsets for the current line
in ascending order. Returns nil if the beginning of the line is
within a multiline string."
  (cond
   ((and (= (line-number-at-pos))
         (= (line-number-at-pos) (point-min)))
    (list 0))
   ;; multiline string
   ((nth 3 (save-excursion
             (parse-partial-sexp (point-min) (line-beginning-position))))
    ;; This is handled by the function that calls us :)
    nil)
   (t 
    (let* ((other (save-excursion
                    (fn-mode--to-nonblank-line t)
                    (fn-mode--offsets-after)))
           (start (save-excursion
                    (current-indentation)))
           (next-offset
            (if (fn-mode--line-blank-p)
                ;; if a line is blank, it has to be indented as much as the next
                ;; nonblank line to avoid unintentional regrouping
                (save-excursion
                  (fn-mode--to-nonblank-line)
                  (current-indentation))
              ;; if a line is nonblank, we can skip over its group (i.e. the
              ;; lines indented further than it)
              (save-excursion
                (fn-mode--next-line-with
                 (lambda ()
                   (and (<= (current-indentation) start)
                        (not (fn-mode--line-blank-p)))))
                (current-indentation)))))
      (append (cl-remove-if-not (lambda (n)
                                  (>= n next-offset))
                                other))))))

;; FIXME: every place that calls this only uses one of the values, so maybe
;; there should be three functions
(defun fn-mode--compute-offsets (&optional wrap)
  "Compute the available indentation offsets for the current
line (unless it is inside a multiline string). This returns a
list containing the information (INITIAL INC DEC), where

- INITIAL is the column to which this line should be indented the
  first time the indentation command is invoked. This will leave
  any legal indentation as-is, but will correct over-indented
  lines (by removing spaces) or oddly-indented lines (by adding
  spaces).
- INC is the next legal column obtained by increasing the current
  indentation, subject to specified wrapping behavior.
- DEC is the next legal column obtained by decreasing the current
  indentation, subject to specified wrapping behavior.

The behaviors of INC and DEC are influenced by wrap. If wrap is
non-nil and the current indentation is already at the maximum or
minimum, then INC and DEC will wrap around to the minimum/maximum
indentation levels respectively. Otherwise, they will be clamped
to the maximum indentation level and zero respectively.

When the start of the current line is within a multiline string,
this will return nil.
"
  (let* ((all (fn-mode--all-offsets))
         (cur (current-indentation))
         (inc (cl-find-if (lambda (x) (> x cur)) all))
         (dec (cl-find-if (lambda (x) (< x cur)) (reverse all))))
    (if all
        (list (if (member cur all)
                  cur
                (or inc dec 0))
              (or inc
                  (if wrap
                      (car (last all))
                    (car all)))
              (or dec
                  (if wrap
                      (car all)
                    (car (last all)))))
      ;; at present, this branch is only evaluated within a multi-line string.
      nil)))

(defun fn-mode--shift-whole-group (shift-by)
  "Add SHIFT-BY spaces for each line in the rest of the
group. Removes spaces if negative."
  ;; FIXME: behavior on comments
  (if (fn-mode--line-blank-p)
      (indent-line-to (+ (current-indentation) shift-by))
    (let ((start (current-indentation)))
      (indent-line-to (+ (current-indentation) shift-by))
      (save-excursion
        (forward-line)
        (cl-loop for cur = (current-indentation) then (current-indentation)
                 while (and (not (eobp))
                            (or (fn-mode--line-blank-p)
                                (> cur start)))
                 do (unless (fn-mode--line-blank-p)
                      (indent-line-to (+ cur shift-by)))
                 do (forward-line))))))

;; TODO: put more thought into the lists below

;; Command names that might trigger repeated indentation
(setq fn-mode--manual-indent-commands
  '(fn-mode-indent-line
    indent-for-tab-command
    indent-according-to-mode
    fn-mode-increase-indent
    fn-mode-decrease-indent
    ;; this should maybe only be here if fn-mode-backspace-dedents is t
    fn-mode-backspace
    ;; see above comment
    fn-mode-space-insert-function)) 

;; This is a list of known commands that insert newlines and might trigger
;; indentation. When `fn-mode-indent-line' is called on a blank line from within
;; one of these commands, the new line wil automatically be indented to a depth
;; matching the last, subject to `fn-mode-blank-line-indent'.
(defvar fn-mode--newline-indent-commands
  '(newline newline-and-indent evil-open-above evil-open-below))

;; TODO: modify these functions to work on regions

(defun fn-mode-increase-indent ()
  "Increase the indentation of the current line."
  (interactive)
  (let* ((cur (current-indentation))
         (choices (fn-mode--compute-offsets))
         (new (if choices
                  (nth 1 choices)
                (+ cur 4))))
    (fn-mode--shift-whole-group (- new cur))))

(defun fn-mode-decrease-indent ()
  "Decrease the indentation of the current line."
  (interactive)
  (let* ((cur (current-indentation))
         (choices (fn-mode--compute-offsets))
         (new (if choices
                  (nth 2 choices)
                (max (- cur 4) 0))))
    (fn-mode--shift-whole-group (- new cur))))

(defun fn-mode-indent-line-function ()
  "Indent the current line as Fn code. This may have several
behaviors depending on the command that invoked it, and whether
that command was repeated or not."
  (cond
   ;; IMPLNOTE: I have serious questions about the way I have implemented this
   ;; desired indentation behavior. I arrived at this solution after reading the
   ;; python-mode source code that implements indentation cycling on repeated
   ;; TAB presses, but I'm not confident in which commands I should use in the
   ;; lists here
 
   ;; repeated invocations of indent commands should increase indentation
   ((and (member this-command fn-mode--manual-indent-commands)
         (member last-command fn-mode--manual-indent-commands))
    (fn-mode-increase-indent))
   ;; indent newly inserted blank lines to match the previous non-blank line
   ;; IMPLNOTE: might want to consider indenting one level deeper when the
   ;; newline is inserted in front of new characters
   ((and (or (and (member this-command fn-mode--newline-indent-commands)
                  (eolp))
             ;; catches self-inserted newlines. FIXME: investigate ways to do
             ;; this in case there's a more standard way to do it
             (= last-command-event ?\n))
         (= (current-column) 0))
    (indent-line-to
     (fn-mode--prev-line-offset fn-mode-blank-lines-before-toplevel)))
   ;; invoking the indent command right after inserting a newline should
   ;; increase indentation where legal
   ((member last-command fn-mode--newline-indent-commands)
    (fn-mode-increase-indent))
   (t (let ((offsets (fn-mode--compute-offsets)))
        (indent-line-to (nth 0 offsets))))))

;;; Intelligent space and backspace
(defun fn-mode--backward-delete-char ()
  ;; TODO: the main thing that needs to be done here is to make it so
  ;; backspace-type commands skip paired delimiters when the structural editing
  ;; features (coming Soon) are enabled.
  (delete-char -1))

(defun fn-mode-backspace ()
  "Delete the previous character with customizations for `fn-mode'."
  (interactive)
  (if (and fn-mode-backspace-dedents
           (<= (current-column) (current-indentation)))
      (let* ((offsets (save-excursion
                        (fn-mode--to-nonblank-line t)
                        (fn-mode--offsets-after)))
             (next-dedent
              (cl-find-if (lambda (n)
                            (< n (current-indentation)))
                          (reverse offsets))))
        (if next-dedent
            (indent-line-to next-dedent)
          (fn-mode--backward-delete-char)))
    (fn-mode--backward-delete-char)))

(defun fn-mode-space-insert-function ()
  "Insert a space with indentation awareness."
  (interactive)
  (if (and fn-mode-space-indents
           (<= (current-column) (current-indentation)))
      ;; TODO: dedent behavior according to offsets. Decide if backspace dedent
      ;; should be able to break up a group
      (let* ((offsets (save-excursion
                        (fn-mode--to-nonblank-line t)
                        (fn-mode--offsets-after)))
             (next-indent
              (cl-find-if (lambda (n)
                            (> n (current-indentation)))
                          offsets)))
        (when next-indent
          (indent-line-to next-indent)))
    (insert ?\s)))

;;; Smart delimiters and quotes

;; TODO

;;; Keymap and major mode

(defvar fn-mode-map (make-sparse-keymap))

(define-key fn-mode-map (kbd "<backspace>") 'fn-mode-backspace)
(define-key fn-mode-map (kbd "SPC") 'fn-mode-space-insert-function)
(define-key fn-mode-map (kbd "<backtab>") 'fn-mode-decrease-indent)

(define-derived-mode fn-mode prog-mode "Fn"
  "Major mode for editing Fn code.

\\{fn-mode-map}"
  :abbrev-table nil
  ;; FIXME: I forgot to run the hook here
  (run-hooks 'fn-mode-hook)
  (set-syntax-table fn-mode--syntax-table)
  (setq-local comment-start "#")
  (setq-local comment-padding 1)
  (setq-local comment-end "")
  (setq-local indent-line-function 'fn-mode-indent-line-function)
  (setq-local font-lock-defaults (list fn--font-lock-defaults)))

