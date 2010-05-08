;;; Miscelaneous additions to auctex (version M2.00 for emacs)
;;; Jan Manuch (jmanuch@sfu.ca) based on
;;; Miscelaneous additions to auctex (version 1.01)
;;; Copyright (C) 2001, 2002 Stefan D. Bruda (bruda@ubishops.ca)

;(add-hook 'latex-mode-hook
;					(function (lambda () (require 'latex-paren)))) 

(load "~/.xemacs/rev-texmathp")

(defun latex-surround-by (empty beg end back-over-end)
  "The main function implementing electric special characters
($, ^,_, etc.; see for example `latex-subscript'). If region is
active, insert BEG before it and END after. Otherwise, insert EMPTY
and place point BACK-OVER-END characters before the end of EMPTY."
  (if mark-active
      (let ((text (buffer-substring (mark) (point))))
        (delete-region (mark) (point))
        (insert beg)
        (insert text)
        (insert end))
    (progn
      (insert empty)
      (backward-char back-over-end))))

(defun latex-mathematicize ()
  "Electric \"$\".
If region is not active, inserts a \"$$\" character.  Otherwise,
surrounds the region by \"$\" characters."
  (interactive)
  (if mark-active
      (let ((endpoint (point)) (text (buffer-substring (mark) (point))))
        (TeX-insert-dollar)
        (goto-char (mark))
        (TeX-insert-dollar)
        (goto-char endpoint)
        (forward-char 2))
    (if (texmathp)
        (if (and (>= (- (point) 1) (point-min))
                 (equal "\\" (buffer-substring (- (point) 1) (point))))
;						(TeX-insert-dollar)
            (insert "\$\{\}")
          (if (string-equal (car texmathp-why) "\$")
              (error "Already in math mode. Cannot insert dollar. Use to C-$ to force insertion.")
            (TeX-insert-dollar)))
      (progn
        (TeX-insert-dollar)
        (TeX-insert-dollar)
        (backward-char 1)))))

(defun latex-parentheses-left ()
  "Ordinary parenthesis.
If region is not active, inserts \"()\" and places cursor between
parentheses.  Otherwise, inserts \"(\" before region and \")\"
after."
  (interactive)
  (if mark-active
      (latex-surround-by "" "(" ")" 1)
    (if (and (>= (- (point) 5) (point-min))
             (equal "\\left" (buffer-substring (- (point) 5) (point))))
        (progn
          (insert "(\\right)")
          (backward-char 7))
      (progn 
        (insert "()")
        (backward-char 1)))))


(defun latex-parentheses-right ()
  "Give warning if the parenthesis would not match."
  (interactive)
  (insert ")")
  (let* ((after-inserted (point)))
    (if (not (my-backward-sexp))
        (progn
          (message "No matching left '('! Use C-M-) to force insert.")
          (delete-backward-char 1))
      (let* ((left-match (point)))
        (goto-char after-inserted)
        (delete-backward-char 1)
        (goto-char left-match)
        (if (my-forward-sexp)
            (progn
              (message "Left '(' already has a match! Use C-M-) to force insert.")
              (goto-char (- after-inserted 1)))
          (progn
            (goto-char (- after-inserted 1))
            (insert ")")))))))

(defun latex-brackets ()
  "Square brackets.
If region is not active, inserts \"[]\" and places cursor between
parentheses.  Otherwise, inserts \"[\" before region and \"]\"
after."
  (interactive)
  (latex-surround-by "[]" "[" "]" 1))

(defun latex-brackets-right ()
  "Give warning if the brackets would not match."
  (interactive)
  (insert "]")
  (let* ((after-inserted (point)))
    (if (not (my-backward-sexp))
        (progn
          (message "No matching left '['! Use M-] to force insert.")
          (delete-backward-char 1))
      (let* ((left-match (point)))
        (goto-char after-inserted)
        (delete-backward-char 1)
        (goto-char left-match)
        (if (my-forward-sexp)
            (progn
              (message "Left '[' already has a match! Use M-] to force insert.")
              (goto-char (- after-inserted 1)))
          (progn
            (goto-char (- after-inserted 1))
            (insert "]")))))))

(defun latex-vertical ()
  "Electric vertical line.
If region is not active, inserts \"||\" and places cursor between
lines.  Otherwise, inserts \"|\" before region and \"|\"
after. Basically, just a call to `latex-surround-by' with appropriate
arguments."
  (interactive)
  (if (texmathp)
      (latex-surround-by "||" "|" "|" 1)
    (insert "|")))

(defun latex-subscript ()
  "Electric subscript.
If region is not active, inserts \"_{}\" and places cursor between
braces.  Otherwise, inserts \"_{\" before region and \"}\"
after. Basically, just a call to `latex-surround-by' with appropriate
arguments."
  (interactive)
  (if (texmathp)
      (latex-surround-by "_{}" "_{" "}" 1)
    (insert "_")))

(defun latex-superscript ()
  "Electric superscript.
If region is not active, inserts \"^{}\" and places cursor between
braces.  Otherwise, inserts \"^{\" before region and \"}\"
after. Basically, just a call to `latex-surround-by' with appropriate
arguments."
  (interactive)
  (latex-surround-by "^{}" "^{" "}" 1))

(defun latex-left-curly-bracket ()
  "Electric left-curly-bracket.
   If math mode and the previous symbol is \\ add \\\}. Otherwise add also \}"
  (interactive)
  (if mark-active
      (latex-surround-by "" "{" "}" 1)
    (if (and (>= (- (point) 1) (point-min))
             (equal "\\" (buffer-substring (- (point) 1) (point)))
             (texmathp))
        (progn
          (insert "\{\\\}")
          (backward-char 2))
      (if (and (>= (- (point) 5) (point-min))
               (equal "\\frac" (buffer-substring (- (point) 5) (point))))
          (progn
            (insert "\{\}\{\}")
            (backward-char 3))
        (progn 
          (insert "\{\}")
          (backward-char 1))))))

(defun latex-right-curly-bracket ()
  "Give warning if the braces would not match."
  (interactive)
  (insert "}")
  (let* ((after-inserted (point)))
    (if (not (my-backward-sexp))
        (progn
          (message "No matching left '{'! Use C-} to force insert.")
          (delete-backward-char 1))
      (let* ((left-match (point)))
        (goto-char after-inserted)
        (delete-backward-char 1)
        (goto-char left-match)
        (if (my-forward-sexp)
            (progn
              (message "Left '{' already has a match! Use C-} to force insert.")
              (goto-char (- after-inserted 1)))
          (progn
            (goto-char (- after-inserted 1))
            (insert "}")))))))

(defun latex-backslash ()
  "If in math mode and a region is selected, enclode region with \{..\}"
  (interactive)
  (if (and mark-active (texmathp))
      (latex-surround-by "" "\\{" "\\}" 1)
    (insert "\\")))


;; LaTeX sectioning search. 

(defun latex-next-section ()
  "Moves cursor to the nearest sectioning command below point."
  (interactive)
  (unless (search-forward-regexp 
           "\\(\\\\\\(sub\\)*section\\)\\|\\(\\\\chapter\\)" nil t)
    (error "No more sectioning commands below.")))

(defun latex-previous-section ()
  "Moves cursor to the nearest sectioning command above point."
  (interactive)
  (unless (search-backward-regexp 
           "\\(\\\\\\(sub\\)*section\\)\\|\\(\\\\chapter\\)" nil t)
    (error "No more sectioning commands above.")))

;; smart dots: 3 dots changed to \dots

(defun latex-dots () 
  "Electric \"...\". 
Typing a period after a sequence of two periods converts the whole
sequence to the LaTeX command \\ldots." 
  (interactive) 
  (cond 
   ((and (boundp 'pending-delete-mode) pending-delete-mode mark-active)
    (delete-region (point) (mark)) 
    (insert ".")) 
   ((and (>= (- (point) 2) (point-min)) 
         (equal ".." (buffer-substring (- (point) 2) (point)))) 
    (delete-region (- (point) 2) (point)) 
    (insert "\\dots")) 
   (t (insert "."))))


(defun my-forward-sexp (&optional arg)
  "Move forward across one balanced expression (sexp).
With argument, do it that many times.  Negative arg -N means
move backward across N balanced expressions. Return t (nil) if the match was (not) found."
  (interactive)
  (or arg (setq arg 1))
  (let ((syntax-table (syntax-table)))
    ;; for more info about syntax tables, see: http://www.emacswiki.org/emacs/EmacsSyntaxTable
    (progn
      (setq my-table (make-syntax-table syntax-table))
      (set-syntax-table my-table)
      (modify-syntax-entry ?\\ "w")
      ;; XEmacs: evil hack! The other half of the evil hack below.
      ;;   - what is the hack exactly? can it be eliminated for emacs?
      (if (and (> arg 0) (looking-at "#s("))
          (goto-char (+ (point) 2)))
      (let* ((matched-position (condition-case nil (scan-sexps (point) arg) (error nil))))
        ;; for more info about handling errors see
        ;;   http://www.gnu.org/software/emacs/elisp/html_node/Handling-Errors.html
        (if matched-position
            (progn
              (goto-char (or matched-position (buffer-end arg)))
              (if (< arg 0) (backward-prefix-chars))
              ;; XEmacs: evil hack! Skip back over #s so that structures are read
              ;; properly.  the current cheesified syntax tables just aren't up to
              ;; this.
              (if (and (< arg 0)
                       (eq (char-after (point)) ?\()
                       (>= (- (point) (point-min)) 2)
                       (eq (char-after (- (point) 1)) ?s)
                       (eq (char-after (- (point) 2)) ?#))
                  (goto-char (- (point) 2)))
              (set-syntax-table syntax-table)
              t)
          (progn
            (message "Match not found")
            (set-syntax-table syntax-table)
            nil))))))

(defun my-backward-sexp (&optional arg)
  "Move backward across one balanced expression (sexp).
With argument, do it that many times.  Negative arg -N means
move forward across N balanced expressions. Return t (nil) if the match was (not) found."
  (interactive)
  (my-forward-sexp (- (or arg 1))))

(defun delete-pair-backward (&optional arg)
  "delete-character and the corresponding matching character to the left"
  (interactive)
  (or arg (setq arg 1))
  (let* ((curpoint (point)))
    (if (my-backward-sexp)
        (progn
          (forward-char 1)
          (delete-backward-char arg)
          (goto-char (- curpoint arg))
          (delete-backward-char arg))
      (progn
        (message "No matching parenthesis!")
        (sleep-for 1)
        (delete-backward-char arg)))))


(defun delete-pair-forward (&optional arg)
  "delete-character and the corresponding matching character to the right"
  (or arg (setq arg 1))
  (interactive)
  (let* ((curpoint (point)))
    (backward-char 1)
    (if (my-forward-sexp)
        (progn
          (delete-backward-char arg)
          (goto-char curpoint)
          (delete-backward-char arg))
      (progn
        (message "No matching parenthesis!")
        (sleep-for 1)
        (forward-char 1)
        (delete-backward-char arg)))))

;; Making backspace electric: 

(defun latex-smart-delete (arg &optional killflag)
  "Electric backward delete. 
Works just like `delete-backward-char', except that 

 - \"_{-!-}\" is converted to \"_-!-\"
 - \"^{-!-}\" is converted to \"^-!-\"
 - \"$...$-!-\" is converted to \"...-!-\"
 - \"$-!-...$\" is converted to \"-!-...\"

where \"-!-\" specifies the position of the point.
and ... specifies arbitrary text."
  (interactive "p")
  (let* ((safe-sub/super (and (>= (- (point) 2) (point-min)) 
                              (<= (+ (point) 1) (point-max))))
         (safe-dots (>= (- (point) 5) (point-min)))
         (safe-char (>= (- (point) 1) (point-min)))
         (safe-next-char (<= (+ (point) 1) (point-max)))
         (curpoint (point)))
    (cond 
     ((and mark-active (boundp 'pending-delete-mode) pending-delete-mode)
      (delete-region (point) (mark)))
; if in pending-delete mode and if region is 
; active, just delete the region; otherwise... 

;; deal with \$ in the input
     ((and safe-sub/super (equal (buffer-substring (- (point) 2) (point))
                                 "\\\$"))
      (delete-region (- (point) 2) (point)))

;; deal with $ in the input
     ((and safe-char (equal (buffer-substring (- (point) 1) (point)) "$"))
      (if (texmathp)
          (progn
; (message "Seems like math mode: %s . %s (%s)"
; (car texmathp-why) (cdr texmathp-why)
; (buffer-substring (cdr texmathp-why)
; (+ (cdr texmathp-why) 1)))
            (backward-char 1)
            (rev-texmathp)
            (if (equal (car texmathp-why) (car rev-texmathp-why))
                (delete-region (cdr rev-texmathp-why) 
                               (+ (cdr rev-texmathp-why) 1))
              (progn
; (message "Beg. and end. math modes do not match: %s  %s"
; (car texmathp-why) (car rev-texmathp-why))
                (if (equal (car texmathp-why) "$")
                    (forward-char 1))
                (delete-backward-char 1)))
            (forward-char 1)
            (delete-backward-char 1))
        (progn
          (backward-char 1)
          (if (texmathp)
              (progn
; (message "Seems like math mode after back: %s . %s"
; (car texmathp-why) (cdr texmathp-why))
                (delete-region (cdr texmathp-why) 
                               (+ (cdr texmathp-why) 1))
                (forward-char 1)
                (delete-backward-char 1))
            (message "Can't delete!"))
          )
        ))

;; \dots to ...
     ((and safe-dots
           (equal (buffer-substring (- (point) 5) (point)) "\\dots"))
      (delete-region (- (point) 5) (point))
      (insert "..."))                                   
		 
;; LaTeX quote to character
; ((and safe-quote
; (or (equal (buffer-substring (- (point) 2) (point)) "``")
; (equal (buffer-substring (- (point) 2) (point)) "''")))
; (delete-region (- (point) 2) (point))
; (insert "\""))
                                  
;; sub/superscript to character
     ((and safe-sub/super
           (or (equal (buffer-substring (- (point) 2) 
                                        (+ (point) 1)) "_{}")
               (equal (buffer-substring (- (point) 2) 
                                        (+ (point) 1)) "^{}")))
      (delete-region (- (point) 1) (+ (point) 1)))

;; if deleting "]" delete corresponding "[" 
     ((and safe-char (equal (buffer-substring (- (point) 1) (point)) "]"))
      (delete-pair-backward))
;; if deleting "[" delete corresponding "]" 
     ((and safe-char (equal (buffer-substring (- (point) 1) (point)) "["))
      (delete-pair-forward))
;; if deleting ")" delete corresponding "(" 
     ((and safe-char (equal (buffer-substring (- (point) 1) (point)) ")"))
      (delete-pair-backward))
;; if deleting "(" delete corresponding ")" 
     ((and safe-char (equal (buffer-substring (- (point) 1) (point)) "("))
      (delete-pair-forward))
;; if deleting "\}" delete corresponding "\{" 
     ((and safe-char (equal (buffer-substring (- (point) 2) (point)) "\\}"))
      (delete-pair-backward 2))
     ((and safe-char safe-next-char
           (equal (buffer-substring (- (point) 1) (point)) "\\")
           (equal (buffer-substring (+ (point) 1) (point)) "}"))
      (progn
        (forward-char 1)
        (delete-pair-backward 2)))
;; if deleting "\{" delete corresponding "\}" 
     ((and safe-char (equal (buffer-substring (- (point) 2) (point)) "\\{"))
      (delete-pair-forward 2))
     ((and safe-char safe-next-char
           (equal (buffer-substring (- (point) 1) (point)) "\\")
           (equal (buffer-substring (+ (point) 1) (point)) "{"))
      (progn
        (forward-char 1)
        (delete-pair-forward 2)))
;; if deleting "}" delete corresponding "{" 
     ((and safe-char (equal (buffer-substring (- (point) 1) (point)) "}"))
      (delete-pair-backward))
;; if deleting "{" delete corresponding "}" 
     ((and safe-char (equal (buffer-substring (- (point) 1) (point)) "{"))
      (delete-pair-forward))
; otherwise just delete the character
     (t (delete-backward-char arg killflag)))))


(defun latex-compile ()
  "Keep only one window open and compile using LATeX"
  (interactive)
  (progn
    (delete-other-windows)
    (TeX-command-master 0)))

(defun latex-bibtex ()
  "Run bibtex on the master file"
  (interactive)
  (TeX-run-BibTeX (TeX-master-file TeX-default-extension)
                  "bibtex"
                  `TeX-master-file))

(defun my-latex-bindings ()
  "Create new bindings in LaTeX mode. 
See `latex-mathematicize', `latex-subscript',
`latex-superscript', `latex-smart-delete', `latex-next-section',
`latex-previous-section'. etc"
  (interactive)
  (define-key LaTeX-mode-map (kbd "<f9>") 'latex-bibtex)
  (define-key LaTeX-mode-map (kbd "<f10>") 'latex-compile)
  (define-key LaTeX-mode-map (kbd "C-$") 'TeX-insert-dollar)
  (define-key LaTeX-mode-map [(control \")] (lambda() (interactive)
                                              (insert "\"")))
  (define-key LaTeX-mode-map (kbd "$") 'latex-mathematicize)
  (define-key LaTeX-mode-map (kbd "|") 'latex-vertical)
  (define-key LaTeX-mode-map (kbd "_") 'latex-subscript)
  (define-key LaTeX-mode-map (kbd "^") 'latex-superscript)
  (define-key LaTeX-mode-map (kbd "\\") 'latex-backslash)
  (define-key LaTeX-mode-map (kbd "(") 'latex-parentheses-left)
  (define-key LaTeX-mode-map (kbd ")") 'latex-parentheses-right)
  (define-key LaTeX-mode-map [(control \()]
    (lambda() "Just insert left parenthesis"
      (interactive) (insert "(")))
  (define-key LaTeX-mode-map (kbd "C-M-)")
    (lambda() "Just insert right parenthesis"
      (interactive) (insert ")")))
  (define-key LaTeX-mode-map (kbd "[") 'latex-brackets)
  (define-key LaTeX-mode-map (kbd "]") 'latex-brackets-right)
  (define-key LaTeX-mode-map [(meta \[)]
    (lambda() "Just insert left bracket"
      (interactive) (insert "[")))
  (define-key LaTeX-mode-map [(meta \])]
    (lambda() "Just insert right bracket"
      (interactive) (insert "]")))
  (define-key LaTeX-mode-map (kbd "{") 'latex-left-curly-bracket)
  (define-key LaTeX-mode-map (kbd "}") 'latex-right-curly-bracket)
  (define-key LaTeX-mode-map [(control \{)]
    (lambda() "Just insert left bracket"
      (interactive) (insert "{")))
  (define-key LaTeX-mode-map [(control \})]
    (lambda() "Just insert right bracket"
      (interactive) (insert "}")))
  (define-key LaTeX-mode-map (kbd ".") 'latex-dots)
  (define-key LaTeX-mode-map [backspace] 'latex-smart-delete)
  (define-key LaTeX-mode-map [(control \`)] 
    (lambda () "Moves point after the next closing brace."
      (interactive) 
      (if (equal (buffer-substring (point) (+ (point) 1)) "}")
          (forward-char 1)
        (search-forward "}" nil t))))
  (define-key LaTeX-mode-map  [(control down)] 'latex-next-section)
  (define-key LaTeX-mode-map  [(control up)] 'latex-previous-section)
;  (define-key LaTeX-mode-map [(meta p)]
;    (lambda () (interactive) (message "Dollar-pos: %i" 
;                                 (progn (rev-texmathp) (cdr rev-texmathp-why)))))
  )

(add-hook 'LaTeX-mode-hook 'my-latex-bindings)

