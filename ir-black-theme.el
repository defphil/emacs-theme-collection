;;; ir-black-theme.el --- Port of ir-black theme

;; Copyright (C) 2012  Jon-Michael Deldin

;; Author: Jon-Michael Deldin <dev@jmdeldin.com>
;; Keywords: faces
;; Package-Version: 20130303.755
;; Compatibility: 24
;; Version: 1.0.1

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

;; This is an Emacs 24 port of Todd Werth's IR Black theme available at URL
;; `http://blog.toddwerth.com/entries/8'. It still needs font-locking for
;; operators, numbers, and regular expressions, and it could definitely use
;; some cleaning up. Improvements are welcome!
;;
;; To use this theme, download it to ~/.emacs.d/themes. In your `.emacs' or
;; `init.el', add this line:
;;
;;    (add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
;;
;; Once you have reloaded your configuration (`eval-buffer'), do `M-x
;; load-theme' and select "ir-black".

;;; Credits:

;; Thanks to Bozhidar Batsov for pointers and the autoloader from his
;; solarized theme at URL
;; `https://github.com/bbatsov/solarized-emacs/blob/master/solarized-theme.el'.

;;; Code:

(deftheme ir-black "Port of ir-black theme")

(let ((*background-color*   "#101010")
      (*brown*              "#E64")
      (*comments*           "#3fdf1f")
      (*constant*           "#99CC99")
      (*current-line*       "#222222")
      (*cursor-underscore*  "#FFFAAA")
      (*keywords*           "#aaaaaa") ;;96CBFE
      (*light-purple*       "#FFCCFF")
      (*line-number*        "#3D3D3D")
      (*method-declaration* "#FFD2A7")
      (*mode-line-bg*       "#222222")
      (*mode-line-fg*       "#CCCCCC")
      (*normal*             "#ffffff") ;;959bf4
      (*number*             "#FF73FD")
      (*operators*          "#FFFFB6")
      (*red*                "#FF6C60")
      (*red-light*          "#FFB6B0")
      (*regexp*             "#E9C")
      (*regexp-alternate*   "#FF0")
      (*regexp-alternate-2* "#EFCC29")
      (*search-selection*   "#2F2F00")
      (*string*             "#b8FF60")
      (*sstring*            "#5555ff")
      (*string-inner*       "#50c0c0")
      (*variable*           "#C6C5FE")
      (*visual-selection*   "#262D81"))

  (custom-theme-set-faces
   'ir-black

   `(bold ((t (:bold t))))
   `(button ((t (:foreground, *keywords* :underline t))))
   `(default ((t (:background, *background-color* :foreground, *keywords*))))
   `(escape-glyph ((t (:foreground, *string-inner*))))
   `(header-line ((t (:background, *mode-line-bg* :foreground, *normal*)))) ;; info header
   `(highlight ((t (:background, *current-line*))))
   `(highlight-face ((t (:background, *current-line*))))
   `(hl-line ((t (:background, *current-line* :underline t))))
   `(info-xref ((t (:foreground, *keywords* :underline t))))
   `(region ((t (:background, *visual-selection* :foreground, "white"))))
   `(underline ((nil (:underline t))))

   ;; font-lock
   `(font-lock-builtin-face ((t (:foreground, *operators*))))
   `(font-lock-comment-delimiter-face ((t (:foreground, *comments*))))
   `(font-lock-comment-face ((t (:foreground, *comments*))))
   `(font-lock-constant-face ((t (:foreground "#959bf4"))))
   `(font-lock-doc-face ((t (:foreground, *comments*))))
   `(font-lock-doc-string-face ((t (:foreground, *comments*))))
   `(font-lock-function-name-face ((t (:foreground, *constant*))))
   `(font-lock-keyword-face ((t (:foreground, *normal*))))
   `(font-lock-negation-char-face ((t (:foreground, *red*))))
   `(highlight-numbers-number ((t (:foreground, *number*))))
   `(font-lock-preprocessor-face ((t (:foreground, *keywords*))))
   `(font-lock-reference-face ((t (:foreground, *constant*))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground, *regexp*))))
   `(font-lock-regexp-grouping-construct ((t (:foreground, *regexp*))))
   `(font-lock-string-face ((t (:foreground, *string*))))
   `(font-lock-type-face ((t (:foreground, *string-inner*))))
   `(font-lock-variable-name-face ((t (:foreground, *variable*))))
   `(font-lock-warning-face ((t (:foreground, *red*))))

   ;; GUI
   `(fringe ((t (:background, *background-color*))))
   `(linum ((t (:background, *line-number*))))
   `(minibuffer-prompt ((t (:foreground, *variable*))))
   `(mode-line ((t (:background, *mode-line-bg* :foreground, *mode-line-fg*))))
   `(mode-line-inactive ((t (:background, *mode-line-bg* :foreground, *background-color*))))
   `(mode-line-buffer-id ((t (:foreground, *string-inner*))))
   `(cursor ((t (:background "#ff0068")))) ;; or green
   `(text-cursor ((t (:background, *background-color*))))
   `(vertical-border ((t (:foreground, *background-color*)))) ;; between splits

   ;; show-paren
   `(show-paren-mismatch ((t (:background, *red* :foreground, *normal* :weight bold))))
   `(show-paren-match ((t (:background, *regexp-alternate-2* :foreground, *current-line* :weight bold))))

   ;; search
   `(isearch ((t (:background, *regexp-alternate* :foreground, *search-selection*))))
   `(isearch-fail ((t (:background, *red*))))
   `(lazy-highlight ((t (:background, *operators* :foreground, *search-selection*))))

   ;; magit
   `(magit-diff-add ((t (:foreground, *string*))))
   `(magit-diff-del ((t (:foreground, *red*))))

   ;; org-mode
   `(org-date ((t (:foreground, *light-purple* :underline t))))
   `(org-level-1 ((t (:foreground, *string*))))
   `(org-special-keyword ((t (:foreground, *variable*))))
   `(org-link ((t (:foreground, *keywords* :underline t))))
   `(org-checkbox ((t (:foreground, *keywords* :background, *background-color* :bold t))))
   `(org-clock-overlay ((t (:foreground, *mode-line-bg* :background, *string*))))

   ;; starter kit

    '(company-tooltip ((t (:foreground "black" :background "grey"))))
 '(company-tooltip-common ((t (:foreground "black" :background "lightgrey"))))
 '(company-tooltip-common-selection ((t (:foreground "white" :background "steelblue"))))
 '(company-tooltip-selection ((t (:foreground "black" :background "steelblue"))))
 '(company-preview-common ((t (:background nil :foreground "lightgrey" :underline t))))
 '(company-scrollbar-fg ((t (:background "orange"))))
 '(company-scrollbar-bg ((t (:background "gray40"))))
 '(company-template-field ((t (:background nil :foreground "yellow"))))
 '(company-tooltip-annotation ((t (:background nil :foreground "dimgray"))))
   
   `(esk-paren-face ((t (:foreground, *string-inner*))))))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

;; Local Variables:
;; no-byte-compile: t
;; End:

(provide-theme 'ir-black)
;;; ir-black-theme.el ends here