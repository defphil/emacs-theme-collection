;;; syohex-theme.el --- Syohex Emacs theme

;; Copyright (C) 2020 by Shohei YOSHIDA

;; Author: Shohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-syohex-theme
;; Version: 0.01

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
;;
;; My own theme. This is based on reverse video theme(`emacs --reverse-video`).
;;

;;; Code:

(deftheme syohex
  "Syohex theme.")

(custom-theme-set-faces
 'syohex
 '(button ((t (:underline t :foreground "cyan1"))))
 '(default ((t (:background "black" :foreground "white"))))
 '(cursor ((t (:foreground "white" :background "#ff0068"))))
 '(region ((t (:background "blue3"))))
 '(font-lock-builtin-face ((t (:foreground "LightSteelBlue"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "LightSalmon"))))
 '(font-lock-comment-face ((t (:foreground "LightSalmon"))))
 '(font-lock-constant-face ((t (:foreground "Aquamarine"))))
 '(font-lock-doc-face ((t (:foreground "LightSalmon"))))
 '(font-lock-function-name-face ((t (:foreground "LightSkyBlue"))))
 '(font-lock-keyword-face ((t (:foreground "Cyan1"))))
 '(font-lock-negation-char-face ((t (nil))))
 '(font-lock-preprocessor-face ((t (:foreground "LightSteelBlue"))))
 '(font-lock-regexp-grouping-backslash ((t (:foreground "#ff1493":weight bold))))
 '(font-lock-regexp-grouping-construct ((t (:foreground "#ff8c00" :weight bold))))
 '(font-lock-string-face ((t (:foreground "LightSalmon"))))
 '(font-lock-type-face ((t (:foreground "PaleGreen"))))
 '(font-lock-variable-name-face ((t (:foreground "LightGoldenrod"))))
 '(font-lock-warning-face ((t (:weight bold :foreground "Pink"))))
 '(minibuffer-prompt ((t (:foreground "cyan"))))

 '(completions-annotations ((t (:underline t))))
 '(completions-common-part ((t (:foreground "white" :background "black"))))
 '(completions-first-difference ((t (:weight bold))))
 '(dired-directory ((t (:foreground "LightSkyBlue"))))
 '(dired-flagged ((t (:weight bold :foreground "Pink"))))
 '(dired-header ((t (:foreground "PaleGreen"))))
 '(dired-ignored ((t (:foreground "grey70"))))
 '(dired-mark ((t (:foreground "Aquamarine"))))
 '(dired-marked ((t (:weight bold :foreground "DarkOrange"))))
 '(dired-perm-write ((t (:foreground "chocolate1"))))
 '(dired-symlink ((t (:foreground "Cyan1"))))
 '(dired-warning ((t (:foreground "Pink" :weight bold))))
 '(error ((t (:foreground "pink" :weight bold))))
 '(escape-glyph ((t (:foreground "cyan"))))
 '(file-name-shadow ((t (:foreground "grey70"))))
 '(fringe ((t (:background "grey10"))))
 '(glyphless-char ((t (:height 0.6))))
 '(header-line ((t (:box (:line-width -1 :style released-button)
                         :background "grey20" :foreground "grey90" :box nil))))
 '(help-argument-name ((t (nil))))
 '(highlight ((t (:background "darkolivegreen"))))
 '(isearch ((t (:background "palevioletred2" :foreground "brown4"))))
 '(isearch-fail ((t (:background "red4"))))
 '(italic ((t (:underline t))))
 '(lazy-highlight ((t (:background "paleturquoise4"))))
 '(link ((t (:foreground "cyan1" :underline t))))
 '(link-visited ((t (:underline t :foreground "violet"))))
 '(match ((t (:background "RoyalBlue3"))))
 '(menu ((t (nil))))

 '(mode-line ((t (:background "#333333" :foreground "#cccccd"))))
 '(mode-line-buffer-id ((t (:weight bold :foreground "orange"))))
 '(mode-line-emphasis ((t (:weight bold))))
 '(mode-line-highlight ((t (:box (:line-width 2 :color "grey40"
                                              :style released-button)))))
 '(mode-line-inactive ((t (:background "grey30" :foreground "grey80"
                                       :box (:line-width -1 :color "grey40" :style nil)
                                       :weight light))))

 '(mouse ((t (nil))))
 '(next-error ((t (:background "blue3"))))
 '(nobreak-space ((t (:foreground "cyan" :underline t))))
 '(query-replace ((t (:foreground "brown4" :background "palevioletred2"))))
 '(scroll-bar ((t (nil))))
 '(secondary-selection ((t (:background "SkyBlue4"))))
 '(shadow ((t (:foreground "grey70"))))
 '(success ((t (:foreground "Green1" :weight bold))))
 '(tool-bar ((t (:background "grey75" :foreground "black"
                             :box (:line-width 1 :style released-button)))))
 '(tooltip ((t (:background "lightyellow" :foreground "black"))))
 '(trailing-whitespace ((t (:background "red1"))))
 '(underline ((t (:underline t))))
 '(vertical-border ((t (nil))))
 '(warning ((t (:foreground "DarkOrange" :weight bold))))
 '(widget-button ((t (:weight bold))))
 '(widget-button-pressed ((t (:foreground "red1"))))
 '(widget-documentation ((t (:foreground "lime green"))))
 '(widget-field ((t (:background "dim gray"))))
 '(widget-inactive ((t (:foreground "grey70"))))

 '(which-func ((t (:foreground "chartreuse1" :weight bold))))
 '(comint-highlight-input ((t (:foreground "grey80" :weight semi-bold))))

 '(flycheck-info ((t (:underline (:style wave :color "green")))))
 '(flycheck-error ((t (:foreground "yellow" :background "red" :weight bold))))
 '(flycheck-warning ((t (:foreground nil :background nil :underline "darkorange"
                                     :weight bold))))
 '(flycheck-error-list-highlight ((t (:background "grey15"))))

 '(diff-added ((t (:background nil :foreground "green" :weight normal))))
 '(diff-removed ((t (:background nil :foreground "firebrick1" :weight normal))))
 '(diff-file-header ((t (:background nil :weight extra-bold))))
 '(diff-refine-added ((t (:background nil :underline "green"))))
 '(diff-refine-removed ((t (:background nil :underline "red"))))
 '(diff-refine-changed ((t (:background nil))))
 '(diff-header ((t (:background nil :weight extra-bold))))
 '(diff-hunk-header ((t (:foreground "turquoise" :weight extra-bold :underline t))))

 '(helm-gtags-file ((t (:foreground "aquamarine1"))))
 '(helm-gtags-lineno ((t (:foreground "IndianRed1" :underline nil))))

 '(cperl-array-face ((t (:background nil :foreground "yellow" :weight normal))))
 '(cperl-hash-face ((t (:foreground "DarkOliveGreen3" :background nil :weight normal))))

 '(show-paren-match ((t (:foreground nil :underline "#ffff00" :weight extra-bold))))

 '(org-block ((t (:foreground "green"))))
 '(org-tag ((t (:foreground "GreenYellow"))))
 '(org-checkbox ((t (:foreground "LawnGreen" :weight bold))))
 '(org-warning ((t (:foreground "hotpink" :weight bold))))
 '(org-level-1 ((t (:foreground "hotpink" :weight bold))))
 '(org-level-2 ((t (:foreground "yellow" :weight semi-bold))))
 '(org-level-3 ((t (:foreground "LightGreen" :weight semi-bold))))
 '(org-level-4 ((t (:foreground "grey80"))))
 '(org-mode-line-clock ((t :foreground "#cccccd" :weight semi-bold)))
 '(org-priority ((t :foreground "orange" :weight normal)))

 '(compilation-error ((t (:underline nil))))
 '(compilation-line-number ((t (:underline t))))
 '(compilation-mode-line-exit ((t (:foreground "SpringGreen1" :weight semi-bold))))
 '(compilation-mode-line-fail ((t (:foreground "IndianRed1" :weight semi-bold))))

 '(eshell-prompt ((t (:foreground "yellow" :weight bold))))

 '(eldoc-highlight-function-argument ((t (:foreground "green" :underline t :weight bold))))
 '(anzu2-mode-line ((t (:foreground "yellow" :weight bold))))

 '(git-gutter2-deleted ((t (:background "red"))))
 '(git-gutter2-modified ((t (:background "magenta"))))

 '(git-rebase-hash ((t (:foreground "yellow"))))

 '(helm-source-header ((t (:background "RoyalBlue4" :weight semi-bold :family nil :height 1.0))))
 '(helm-ff-file ((t (:foreground "white" :background nil))))
 '(helm-ff-directory ((t (:foreground "cyan" :background nil :underline t))))
 '(helm-grep-lineno ((t (:foreground "IndianRed1"))))
 '(helm-moccur-buffer ((t (:foreground "aquamarine1" :underline nil))))

 '(popup-scroll-bar-foreground-face ((t (:background "DarkOrange"))))

 '(company-tooltip ((t (:foreground "black" :background "lightgrey"))))
 '(company-tooltip-common ((t (:foreground "black" :background "lightgrey"))))
 '(company-tooltip-common-selection ((t (:foreground "white" :background "steelblue"))))
 '(company-tooltip-selection ((t (:foreground "black" :background "steelblue"))))
 '(company-preview-common ((t (:background nil :foreground "lightgrey" :underline t))))
 '(company-scrollbar-fg ((t (:background "orange"))))
 '(company-scrollbar-bg ((t (:background "gray40"))))
 '(company-template-field ((t (:background nil :foreground "yellow"))))
 '(company-tooltip-annotation ((t (:background nil :foreground "dimgray"))))

 '(flyspell-duplicate ((t (:foreground "white" :background "orange" :underline nil weight bold))))
 '(flyspell-incorrect ((t (:foreground "white" :background "red" :underline nil :weight bold))))

 '(vc-edited-state ((t (:foreground "tomato" :weight bold))))
 '(vc-locally-added-state ((t :foreground "GreenYellow" :weight bold))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'syohex)

;;; syohex-theme.el ends here
