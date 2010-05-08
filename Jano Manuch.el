(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/")
(tooltip-mode t)
(tool-bar-mode t)
(menu-bar-mode t)
;; for some reason emacs will remove tool-bar after creating new
;; frame, so we need to the following line to avoid that:
(setq after-make-frame-functions (function (lambda (f) (tool-bar-mode 1))))
(global-set-key "\C-z" 'undo)

;; AUCTEX
(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)
(require 'tex-mik)
(require 'toolbar-x)
(autoload 'reftex-mode    "reftex" "RefTeX Minor Mode" t)
(autoload 'turn-on-reftex "reftex" "RefTeX Minor Mode" t)
(setq reftex-plug-into-AUCTeX t)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(defvar TeX-view-program-list
  '(("Yap" ("yap -1" (mode-io-correlate " -s %n%b") " %o"))
    ("dvips and start" "dvips %d -o && start \"\" %f")
    ("start" "start \"\" %o")))

;; the following command provides some functionality of tex-addos, it
;; automatically adds {} after _, but it does not delete stuff nicely,
;; etc.., so I rather let tex-addos do all
;(setq TeX-electric-sub-and-superscript t)
(load "~/.emacs.d/tex-addos")


;; -----------------------------------------------------------------------------
;; The following stuff is taken from
;; http://scottmcpeak.com/elisp/scott.emacs.el
;; (the main purpose is to add shift-arrow selection as in xemacs)
;; -----------------------------------------------------------------------------

(setq version-xemacs nil)
(setq version-emacs t)

;; ---------------- portable mark functions ---------------
(defun is-mark-active ()
  "True if the selection is displayed."
  (cond
    (version-emacs
      mark-active)           ; (mark) causes error if inactive under regular emacs
    (version-xemacs
      (not (not (mark))))    ; but nil with xemacs, and mark-active doesn't exist
    (t t)
  ))

(defun turn-off-mark ()
  "Turn off the mark selection."
  (cond
    (version-emacs
      (setq mark-active nil))
    (version-xemacs
      (zmacs-deactivate-region))
    (t t)
  ))

(defun turn-on-mark ()
  "Turn on the mark selection, with mark equal to cursor."
  (cond
    (version-emacs
      (set-mark (point)))      ; push-mark doesn't work for this!!!
    (version-xemacs
      (push-mark (point))
      (zmacs-activate-region))
    (t t)
  ))

(defun keep-region-selected ()
  "Allow the selected region to remain selected after this command."
  (cond
    (version-emacs t)
    (version-xemacs (setq zmacs-region-stays t))
    (t t)
  ))

;; ---- cursor movement ----
;; policy controlling whether cursor movement is destructive
(defun destructive-cursor-movement ()
  "Policy; returns t if we want cursor movement to potentially change
  the file"
  (interactive)

  (not                               ; any of these conditions turn off destructive:
    (or buffer-read-only               ; if it's read-only, we just get beeps
        (eq major-mode 'custom-mode)   ; M-x customize also doesn't like it
        (eq major-mode 'makefile-mode) ; and Makefiles are very sensitive to such changes
        (not (buffer-modified-p))      ; if we have made no changes, don't start
    )))


;; ----------------- shift-move to select text --------------------------
;; passing a cursor-movement fn returns a fn that will turn on the region
;; highlighting if it is off, and then exec the cursor-movement
(defmacro make-extend-region-fn (name func)
  "Define a function called 'name' which behaves like 'func'
  except it extends the selected region."
  `(defun ,name ()
     ,(concat "Like " (prin1-to-string func) ", but extends the region.")
     (interactive)
     (if (not (is-mark-active))
         (turn-on-mark))
     ,func
     ))

(defun left-backward-char ()
  "move the cursor to the left, stopping at BOL"
  (interactive)
  (if (= (current-column) 0)
     t                   ; nop if at BOL
     (if (destructive-cursor-movement)
         (move-to-column                 ; forceful move
           (- (current-column) 1)          ; one char left
           t)                              ; force
         (backward-char)                 ; nonforceful
           ; if we did same thing as above, can't move left over tabs
     )
  )
)

(make-extend-region-fn move-select-left   (left-backward-char))
(make-extend-region-fn move-select-right  (forward-char))
(make-extend-region-fn move-select-up     (previous-line 1))
(make-extend-region-fn move-select-down   (next-line 1))
(make-extend-region-fn move-select-home   (beginning-of-line))
(make-extend-region-fn move-select-end    (end-of-line))
(make-extend-region-fn move-select-pgup   (my-page-up))
(make-extend-region-fn move-select-pgdn   (my-page-down))
(make-extend-region-fn move-select-top    (beginning-of-buffer))
(make-extend-region-fn move-select-bottom (end-of-buffer))

                                        ; -------------------- keybindings menu ---------------------
;; the code here is processes a keybindings menu, like the one that
;; appears at the end of this file, and installs the bindings it
;; finds in that menu
(defun install-keybindings-menu (menu)
  "Install a menu of keybindings.
  
  'menu' is a list of lists.  Each contained list begins with one
  of several symbols; which symbol it is determines what follows.
  
  The list of symbols, and what is expected to follow, is:

  (menu <menu title> <menu description> items...)
    A (sub) menu.  The 'items' is another keybindings menu.

  (item <item title> <action> bindings...)
    A menu item.  'action' is the function to execute when the menu
    item is selected, or key is pressed; if it is missing, the
    menu item is ignored.  'bindings' is a list of key binding
    vectors.
    
  (separator)
    Visual separator."

  (interactive)
  (if (eq menu ())
    t    ; done
    (progn
      (install-keybindings-item (car menu))   ; process 1 item
      (install-keybindings-menu (cdr menu))   ; process rest
    )))

(defun install-keybindings-item (list)
  "Helper for `install-keybindings-menu'."
  (let ((tag (car list)))
    (cond ((eq tag 'menu)
             (install-keybindings-menu (nthcdr 3 list)))   ; process items after description
          ((eq tag 'item)
             (if (>= (length list) 4)                      ; see if there are any bindings here
               (bind-some-keys (nth 2 list) (nthcdr 3 list))   ; yes, process them
             ))
          ((eq tag 'separator)
             t)      ; nop for now
          (t
             (error "Invalid menu tag: %s" tag))
    )))

(defun is-bindable-key (binding)
  "Return t if 'binding' is a key we can bind; this just checks for
  a few special cases that are known to be problems -- it is not the
  case that every key this fn accepts can be bound."
  ; the only one at this time is [(control shift s)] on xemacs
  (if (and version-xemacs 
           (equal (car bindings) [(control shift s)]))
    nil    ; this is the one we can't bind
    t      ; assume the rest are ok
  ))

(defun bind-some-keys (action bindings)
  "Call `global-set-key', binding every key that appears in the
  'bindings' list to 'action'."
  (if (eq bindings ())
      t   ; done
      (progn
        (if (is-bindable-key (car bindings))     ; check for problematic keys
          (global-set-key (car bindings) action)   ; normal behavior
        )
        
        ; recursively process rest of bindings
        (bind-some-keys action (cdr bindings))
      )))
;(bind-some-keys 'save-buffer '([f2] [(control o)]))

(setq my-bindings-menu '(
  (menu "Selecting" "Selects text while moving"
       (item "Left" move-select-left                [(shift left)])
       (item "Right" move-select-right              [(shift right)])
       (item "Up" move-select-up                    [(shift up)])
       (item "Down" move-select-down                [(shift down)])
       (item "Beginning of line" move-select-home   [(shift home)])
       (item "End of line" move-select-end          [(shift end)])
       (item "Page up" move-select-pgup             [(shift prior)])
       (item "Page down" move-select-pgdn           [(shift next)])
       (item "Top of file" move-select-top          [(shift control prior)])
       (item "End of file" move-select-bottom       [(shift control next)])
       )))

;; cause the above menu to take effect
(install-keybindings-menu my-bindings-menu)

