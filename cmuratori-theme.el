;;; cmuratori-theme.el --- Cmuratori theme for Emacs.

;; Copyright (C) 2019-2019 Stephen Dickinson

;; Author: Stephen Dickinson <stephencottontail@me.com>
;; URL: https://github.com/stephencottontail/cmuratori-theme.git
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

(deftheme cmuratori
  "Cmuratori theme for Emacs.")

(let* ((class '((class color) (min-colors 89)))

       (fg         "#B6A997") ;;b6a997 <> d5c4a1 <> D2B58D
       (bg         "#101010")
       (strings    "#44b9b1")
       (constants  "#a060c0")
       (type       "#c090c0")
       (comments   "#6faeb0") ;;30f035
       (hl         "#afbfff"))

  (custom-theme-set-faces
   'cmuratori

   `(default ((,class (:background ,bg :foreground "burlywood3"))))

   ;; global font lock
   `(fringe ((,class (:background "#111111" :foreground "burlywood3"))))
   `(font-lock-keyword-face       ((,class (:foreground "DarkGoldenrod3"))))
   `(font-lock-constant-face      ((,class (:foreground "olive drab"))))
   `(font-lock-type-face          ((,class (:foreground "burlywood3"))))
   `(font-lock-builtin-face       ((,class (:foreground "#DAB98F"))))
   `(font-lock-string-face        ((,class (:foreground "olive drab"))))
   `(font-lock-doc-face           ((,class (:foreground ,comments))))
   `(font-lock-comment-face       ((,class (:foreground ,comments))))
   `(highlight-numbers-number     ((,class (:foreground "LightSteelBlue"))))
   `(font-lock-function-name-face ((,class (:foreground "seashell2"))))
   `(font-lock-variable-name-face ((,class (:foreground "burlywood3"))))
   `(cursor ((,class (:foreground "#000" :background "#ff0080"))))
   ;; Magit
   `(magit-diff-context           ((,class (:foreground ,fg))))
   `(magit-diff-context-highlight ((,class (:foreground ,fg))))
   '(vertical-border ((t (:background "#000" :foreground "#000"))))
   ;; org
   `(org-verbatim                 ((,class (:foreground ,constants))))

   ;; clojure font lock
   `(clojure-keyword-face         ((,class (:background "gray90"))))

   ;; mode line
   `(mode-line                    ((,class (:inherit default :foreground "ivory3" :background "#222222"))))
   `(mode-line-inactive           ((,class (:underline nil :foreground "#555"))))
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

(provide-theme 'cmuratori)

;;;###autoload
(when (and (boundp 'custom-theme-load-path)
           load-file-name)
  ;; add theme folder to `custom-theme-load-path' when installing over MELPA
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

;;; cmuratori-theme.el ends here
