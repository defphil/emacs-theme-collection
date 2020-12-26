;;; hhteal-theme.el --- Happy Hacking Teal theme for Emacs.

;; Copyright (C) 2019-2019 Stephen Dickinson

;; Author: Filip Miletic <filip.miletic@me.com>
;; URL: https://github.com/stephencottontail/hhteal-theme.git
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

;;; Commentary:
;; Port of legendary vim theme.
;; Minimal config.

;;; Code:

(deftheme hhteal
  "Hhteal theme for Emacs.")

(let* ((class '((class color) (min-colors 89)))

       (fg         "#a4bbaa") ;; 88bb88
       (bg         "#081618") ;; 031616
       (strings    "#2d9594")
       (constants  "#a060c0")
       (type       "#32f020")
       (keyword    "burlywood3")
       (comments   "#2aa1ae") ;;30f035
       (hl         "#afbfff"))

  (custom-theme-set-faces
   'hhteal

   `(default ((,class (:background ,bg :foreground ,fg))))

   ;; global font lock
   `(fringe ((,class (:background "#001111" :foreground "burlywood3"))))
   `(font-lock-keyword-face       ((,class (:foreground ,keyword))))
   `(font-lock-constant-face      ((,class (:foreground "cyan3"))))
   `(font-lock-type-face          ((,class (:foreground "cyan2"))))
   `(font-lock-builtin-face       ((,class (:foreground "gray40"))))
   `(font-lock-string-face        ((,class (:foreground ,strings))))
   `(font-lock-doc-face           ((,class (:foreground ,type))))
   `(font-lock-comment-face       ((,class (:foreground ,type))))
   `(font-lock-function-name-face ((,class (:foreground "gray100"))))
   `(font-lock-variable-name-face ((,class (:foreground ,fg))))
   `(cursor ((,class (:foreground "#000" :background "#ff0090"))))
   ;; Magit
   `(magit-diff-context           ((,class (:foreground ,fg))))
   `(magit-diff-context-highlight ((,class (:foreground ,strings))))
   '(vertical-border ((t (:background "#000" :foreground "#000"))))
   ;; org
   `(org-verbatim                 ((,class (:foreground ,constants))))

   ;; clojure font lock
   `(clojure-keyword-face         ((,class (:background "gray90"))))

   ;; mode line
   `(mode-line                    ((,class (:inherit default :foreground ,keyword :background "#032628"))))
   `(mode-line-inactive           ((,class (:background "#001111" :foreground "#666"))))
   '(dired-directory ((t (:foreground "LightSkyBlue"))))
   '(dired-flagged ((t (:weight bold :foreground "Pink"))))
   '(dired-header ((t (:foreground "PaleGreen"))))
   '(dired-ignored ((t (:foreground "grey70"))))
   '(dired-mark ((t (:foreground "Aquamarine"))))
   '(dired-marked ((t (:weight bold :foreground "DarkOrange"))))
   '(dired-perm-write ((t (:foreground "chocolate1"))))
   '(dired-symlink ((t (:foreground "Cyan1"))))
   '(dired-warning ((t (:foreground "Pink" :weight bold))))   
   ;; hl-line
   `(hl-line                      ((,class (:background ,hl))))))

(provide-theme 'hhteal)

;;;###autoload
(when (and (boundp 'custom-theme-load-path)
           load-file-name)
  ;; add theme folder to `custom-theme-load-path' when installing over MELPA
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

;;; hhteal-theme.el ends here
