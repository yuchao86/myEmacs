(define-key global-map "\C-hr" 'display-random-help)

(provide 'randomhelp)
(require 'apropos)
(setq commands-buffer "commands")
(setq commands-filename "~/.emacs-commands")

(defun display-random-help ()  
  (interactive)
  "Displays help documentation for a random symbol"

  (if (not (file-readable-p commands-filename))
       (rebuild-command-list))

  (find-file commands-filename) 

  (setq line-count (count-lines 1 (point-max)))
  (random t)
  (setq random-line-num (random line-count))
  (goto-line random-line-num)
  (setq random-command (buffer-substring (point) (line-end-position)))

  (kill-buffer (current-buffer))


;;; limit to just this one, not a regexp
  (setq random-regexp (concat "^" random-command "$"))
  (message "picked %s at %d" random-command  random-line-num)
  (describe-symbols random-regexp)
  )

(defun rebuild-command-list ()
  (interactive)
  (message "Rebuilding command list in %s" commands-filename)
  (set-buffer (get-buffer-create commands-buffer))
  (set-visited-file-name commands-filename) 
  (apropos-mini "")
  (save-buffer))

(defun apropos-mini (apropos-regexp &optional do-all)
  (setq apropos-accumulator
	(apropos-internal apropos-regexp
			  (and (lambda (symbol)
				 (or (fboundp symbol)
				     (boundp symbol)
				     (facep symbol)
				     (symbol-plist symbol))))))
  (let ((p apropos-accumulator) symbol )
    (mini-print p)
    ))

(defun mini-print (pr) 
  (dolist (tmp pr)
    (princ tmp (get-buffer (current-buffer)))
    (princ "\n" (get-buffer (current-buffer)))
    ))

;;; er .. from the lisp manual, hacked so it returns a string
  (defun describe-symbols (pattern)
       "Describe the Emacs Lisp symbols matching PATTERN.
     All symbols that have PATTERN in their name are described
     in the `*Help*' buffer."
       (interactive "sDescribe symbols matching: ")
       (let ((describe-func
              (function
               (lambda (s)
                 ;; Print description of symbol.
                 (if (fboundp s)             ; It is a function.
                     (princ
                      (format "%s\t%s\n%s\n\n" s
                        (if (commandp s)
                            (let ((keys (where-is-internal s)))
                              (if keys
                                  (concat
                                   "Keys: "
                                   (mapconcat 'key-description
                                              keys " "))
                                "Keys: none"))
                          "Function")
                        (or (documentation s)
                            "not documented"))))
     
                 (if (boundp s)              ; It is a variable.
                     (princ
                      (format "%s\t%s\n%s\n\n" s
                        (if (user-variable-p s)
                            "Option " "Variable")
                        (or (documentation-property
                              s 'variable-documentation)
                            "not documented")))))))
             sym-list)
     
         ;; Build a list of symbols that match pattern.
         (mapatoms (function
                    (lambda (sym)
                      (if (string-match pattern (symbol-name sym))
                          (setq sym-list (cons sym sym-list))))))
     
         ;; Display the data.
         (with-output-to-temp-buffer "*Help*"
           (mapcar describe-func (sort sym-list 'string<))
           )
           ;(print-help-return-message)
         ))

     
 
     
  