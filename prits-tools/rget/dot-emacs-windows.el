;; NOTE: Load from ~/.emacs with e.g. (load "z:/dot-emacs-windows.txt)
;; NOTE: ~/.emacs (interpreted by emacs), is *actually* $HOME/AppData/Roaming/.emacs!
;; NOTE: Start emacs:
;; "c:\Program Files (x86)\Emacs\i686\bin\emacs.exe" --chdir z:\ z:\rget_fetch_files.pro

(setq idlwave-block-indent 3)           ; Indentation settings
(setq idlwave-main-block-indent 2)
(setq idlwave-end-offset -3)
(setq idlwave-continuation-indent 1)
(setq idlwave-begin-line-comment "^;[^;]")  ; Leave ";" but not ";;"
                                            ; anchored at start of line.
(setq idlwave-surround-by-blank t)      ; Turn on padding ops =,<,>
(setq idlwave-pad-keyword nil)          ; Remove spaces for keyword '='
(setq idlwave-expand-generic-end t)     ; convert END to ENDIF etc...
(setq idlwave-reserved-word-upcase t)   ; Make reserved words upper case
                                        ; (with abbrevs only)
(setq idlwave-abbrev-change-case t)     ; Force case of expansions
(setq idlwave-hang-indent-regexp ": ")  ; Change from "- " for auto-fill
(setq idlwave-show-block nil)           ; Turn off blinking to begin
(setq idlwave-abbrev-move t)            ; Allow abbrevs to move point
(setq idlwave-query-class '((method-default . nil) ; No query for method
                            (keyword-default . nil); or keyword completion
                            ("INIT" . t)           ; except for these
                            ("CLEANUP" . t)
                            ("SETPROPERTY" .t)
                            ("GETPROPERTY" .t)))

(setq mouse-buffer-menu-mode-mult 1000)

;; Some setting can only be done from a mode hook.  Here is an example:
(add-hook 'idlwave-mode-hook
  (lambda ()
    (message "Running idlwave-mode-hook")
    (setq case-fold-search nil)          ; Make searches case sensitive
    ;; Run other functions here
    (font-lock-mode 1)                   ; Turn on font-lock mode
    (idlwave-auto-fill-mode 0)           ; Turn off auto filling
    (setq idlwave-help-browser-function 'browse-url-w3)

    ;; Pad with 1 space (if -n is used then make the
    ;; padding a minimum of n spaces.)  The defaults use -1
    ;; instead of 1.
    (idlwave-action-and-binding "=" '(idlwave-expand-equal 1 1))
    (idlwave-action-and-binding "<" '(idlwave-surround 1 1))
    (idlwave-action-and-binding ">" '(idlwave-surround 1 1 '(?-)))
    (idlwave-action-and-binding "&" '(idlwave-surround 1 1))
    (idlwave-action-and-binding "+" '(idlwave-surround 1 1))
    (idlwave-action-and-binding "-" '(idlwave-surround 1 1))

    ;; Only pad after comma and with exactly 1 space
    (idlwave-action-and-binding "," '(idlwave-surround nil 1))

    ;; Pad only after '->', remove any space before the arrow
    (idlwave-action-and-binding "->"  '(idlwave-surround 0 -1 nil 2))

    ;; Set some personal bindings
    (local-set-key [f5] 'idlwave-shell-break-here)
    (local-set-key [f6] 'idlwave-shell-clear-current-bp)

    ;; Create a newline, indenting the original and new line.
    ;; A similar function that does _not_ reindent the original
    ;; line is on "\C-j" (The default for emacs programming modes).
    (local-set-key "\r" 'idlwave-newline)

    (local-set-key "\C-c\C-c" 'idlwave-shell-save-and-run)
    
    ))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(idlwave-shell-automatic-start t)
 '(idlwave-shell-explicit-file-name "z:\\startidl.bat")
 '(idlwave-shell-use-dedicated-frame t)
 '(idlwave-shell-use-dedicated-window t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
