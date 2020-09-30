;;; aftereight-theme.el --- Aftereight theme for Emacs.

;; Copyright (C) 2019-2019 Stephen Dickinson

;; Author: Stephen Dickinson <stephencottontail@me.com>
;; URL: https://github.com/stephencottontail/aftereight-theme.git
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(deftheme aftereight
  "Aftereight theme for Emacs.")

(let* ((class '((class color) (min-colors 89)))

       (active     "#007acc")
       (fg         "#b6a997") ;;b6a997 <> d5c4a1
       (bg         "#141414")
       (strings    "#62d0c8")
       (constants  "#89E2A1")
       (builtins   "#FFFBD9")
       (keywords   "#FFFBD9")
       (comments   "#30f035")
       (hl         "#d0d0ff")
       )

  (custom-theme-set-faces
   'aftereight

   `(default ((,class (:background ,bg :foreground ,fg))))

   ;; global font lock
   `(font-lock-keyword-face       ((,class (:foreground ,keywords))))
   ;;`(font-lock-constant-face      ((,class (:foreground ,constants))))
   ;;`(font-lock-type-face          ((,class (:foreground ,fg))))
   `(font-lock-builtin-face       ((,class (:foreground ,builtins))))
   `(font-lock-string-face        ((,class (:foreground ,strings))))
   `(font-lock-doc-face           ((,class (:foreground ,comments))))
   `(font-lock-comment-face       ((,class (:foreground ,comments))))
   
   `(font-lock-function-name-face ((,class (:foreground "white"))))
   `(font-lock-variable-name-face ((,class (:foreground "#CCE8D3"))))
   `(cursor ((,class (:foreground "#000" :background "#ff0060"))))
   ;; Magit
   `(magit-diff-context           ((,class (:foreground ,fg))))
   `(magit-diff-context-highlight ((,class (:foreground ,fg))))

   ;; org
   `(org-verbatim                 ((,class (:foreground ,constants))))

   ;; clojure font lock
   `(clojure-keyword-face         ((,class (:background "gray90"))))

   ;; mode line
   `(mode-line                    ((,class (:inverse-video t))))
   `(mode-line-inactive           ((,class (:underline nil))))

   ;; split border
   `(vertical-border ((,class (:foreground ,fg))))
   
   ;; hl-line
   `(hl-line                      ((,class (:background ,hl))))))

(provide-theme 'aftereight)

;;;###autoload
(when (and (boundp 'custom-theme-load-path)
           load-file-name)
  ;; add theme folder to `custom-theme-load-path' when installing over MELPA
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

;;; aftereight-theme.el ends here
