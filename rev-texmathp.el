; the copy of texmathp.el 
;   - should do the reverse search to find the end of math environment

(defun rev-texmathp ()
  "Determine if point is inside (La)TeX math mode.
Returns t or nil.  Additional info is placed into `rev-texmathp-why'.
The functions assumes that you have (almost) syntactically correct (La)TeX in
the buffer.
See the variable `texmathp-tex-commands' about which commands are checked."
  (interactive)
  (let* ((pos (point)) math-on sw-match
	 (bound (save-excursion
		  (if (re-search-forward "[\n\t][ \t]*[\n\r]"
					  nil 1 texmathp-search-n-paragraphs)
		      (match-end 0)
		    (point-max))))
	 (mac-match (rev-texmathp-match-macro bound))
	 (env-match (rev-texmathp-match-environment
		     (if (and mac-match (< (cdr mac-match) bound))
			 (cdr mac-match)
		       bound)))
	 (match (cons nil bound)))

    ;; Select the nearer match
    (and env-match (setq match env-match))
    (and mac-match (< (cdr mac-match) (cdr match)) (setq match mac-match))
    (setq math-on (memq (nth 1 (assoc (car match) texmathp-tex-commands1))
			'(env-on arg-on)))

    ;; Check for switches
    (and (not math-on)
	 (setq sw-match (rev-texmathp-match-switch bound))
	 (< (cdr sw-match) (cdr match))
	 (eq (nth 1 (assoc (car sw-match) texmathp-tex-commands1)) 'sw-on)
	 (setq match sw-match math-on t))

    ;; Check for togglers
    (if (not math-on)
	(save-excursion
	  (goto-char (cdr match))
	  (while (re-search-backward texmathp-toggle-regexp pos t)
	    (if (setq math-on (not math-on))
		(setq sw-match (cons (match-string 2) (match-beginning 2)))
	      (setq sw-match nil)))
	  (and math-on sw-match (setq match sw-match))))

    ;; Store info, show as message when interactive, and return
    (setq rev-texmathp-why match)
    (and (interactive-p)
	 (message "math-mode is %s: %s ends at buffer position %d"
		  (if math-on "on" "off")
		  (or (car match) "new paragraph")
		  (cdr match)))
    (and math-on t)))

(defun rev-texmathp-match-environment (bound)
  "Find out if point is inside any of the math environments.
Limit searched to BOUND.  The return value is like (\"equation\" . (point))."
  (catch 'exit
    (save-excursion
      (and (null texmathp-environments) (throw 'exit nil))
      (let (end-list env)
	(while (re-search-forward "\\\\\\(begin\\|end\\){\\([^}]+\\)}"
				   bound t)
	  (setq env (buffer-substring-no-properties
		     (match-beginning 2) (match-end 2)))
	  (cond ((string= (match-string 1) "end")
		 (setq end-list (cons env end-list)))
		((equal env (car end-list))
		 (setq end-list (cdr end-list)))
		((member env texmathp-environments)
		 (throw 'exit (cons env (point))))))
	nil))))

(defun rev-texmathp-match-macro (bound)
  "Find out if point is within the arguments of any of the Math macros.
Limit searches to BOUND.  The return value is like (\"\\macro\" . (point))."
  (catch 'exit
    (and (null texmathp-macros) (throw 'exit nil))
    (let (pos cmd (syntax-table (syntax-table)))
      (unwind-protect
	  (save-restriction
	    (save-excursion
	      (set-syntax-table texmathp-syntax-table)
	      (narrow-to-region (max 1 bound) (point))
	      ;; Move forward out of the current parenthesis
	      (while (condition-case nil (progn (up-list 1) t) (error nil))
		;; Move forward over any touching sexps (in fact also non-touching)
		(while
		    (and
		     (cond
		      ((memq (preceding-char) '(?\] ?\})))
		      ((and
			texmathp-allow-detached-args
			(re-search-forward
			"[]}][ \t]*[\n\r]?\\([ \t]*%[^\n\r]*[\n\r]\\)*[ \t]*\\="
			bound t))
		       (goto-char (1+ (match-beginning 0))) t))
		     (if (eq (preceding-char) ?\})
			 ;; Jump back over {}
			 (condition-case nil
			     (progn (forward-sexp) t)
			   (error nil))
		       ;; Jump back over []. Modify syntax temporarily for this.
		       (unwind-protect
			   (progn
			     (modify-syntax-entry ?\{ ".")
			     (modify-syntax-entry ?\} ".")
			     (modify-syntax-entry ?\[ "(]")
			     (modify-syntax-entry ?\] ")[")
			     (condition-case nil
				 (progn (forward-sexp) t)
			       (error nil)))
			 (modify-syntax-entry ?\{ "(}")
			 (modify-syntax-entry ?\} "){")
			 (modify-syntax-entry ?\[ ".")
			 (modify-syntax-entry ?\] ".")
			 nil))))
		(setq pos (point))
		(and (memq (following-char) '(?\[ ?\{))
		     (re-search-forward "\\\\[*a-zA-Z]+\\=" nil t)
		     (setq cmd (buffer-substring-no-properties
				(match-beginning 0) (match-end 0)))
		     (member cmd texmathp-macros)
		     (throw 'exit (cons cmd (point))))
		(goto-char pos))
	      (throw 'exit nil)))
	(set-syntax-table syntax-table)))))

(defun rev-texmathp-match-switch (bound)
  "Search forward for any of the math switches.
Limit searched to BOUND."
  ;; The return value is like ("\\(" . (point)).
  (save-excursion
    (if (re-search-forward texmathp-onoff-regexp bound t)
	(cons (buffer-substring-no-properties (match-beginning 1) (match-end 1))
	      (match-beginning 1))
      nil)))
