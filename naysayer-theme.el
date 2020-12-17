(deftheme naysayer
  "joni blow boi theme.")

(custom-theme-set-faces
 'naysayer
 '(default ((t (:foreground "#E1CCA6" :background "#06181b"))))
 '(custom-group-tag-face ((t (:underline t :foreground "lightblue"))) t)
 '(custom-variable-tag-face ((t (:underline t :foreground "lightblue"))) t)
 '(font-lock-builtin-face ((t (:foreground "lightgreen"))))
 '(font-lock-comment-face ((t (:foreground "#3fCf1f"))))
 '(font-lock-function-name-face ((((class color) (background dark)) (:foreground "white")))) 
 '(font-lock-keyword-face ((t (:foreground "white" ))))
 '(font-lock-string-face ((t (:foreground "#0fdfaf"))))
 '(font-lock-variable-name-face ((((class color) (background dark)) (:foreground "#c8d4ec"))))  
 '(font-lock-warning-face ((t (:foreground "#504038"))))
 '(cursor ((t (:background "#ff0068"))))
 '(vertical-border ((t (:background "#000" :foreground "#000"))))
 '(highlight ((t (:foreground "navyblue" :background "darkseagreen2"))))
 '(fringe ((t (:background "#011c22"))))
 '(mode-line ((t (:foreground "#04242c" :background "#d3b58d")))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'naysayer)

;;; naysayer-theme.el ends here
