;;; 4coder-theme.el --- A low contrast color theme for Emacs.

;; Copyright (C) 2011-2018 Bozhidar Batsov

;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: http://github.com/bbatsov/4coder-emacs
;; Package-Version: 20190809.1324
;; Version: 2.7-snapshot

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

;; A port of the popular Vim theme 4coder for Emacs 24+, built on top
;; of the new built-in theme support in Emacs 24.

;;; Credits:

;; Jani Nurminen created the original theme for vim on which this port
;; is based.

;;; Code:

(deftheme 4coder "The 4coder color theme")

(defgroup 4coder-theme nil
  "4coder theme."
  :group 'faces
  :prefix "4coder-theme-"
  :link '(url-link :tag "GitHub" "http://github.com/bbatsov/4coder-emacs")
  :tag "4coder theme")

;;;###autoload
(defcustom 4coder-override-colors-alist '()
  "Place to override default theme colors.

You can override a subset of the theme's default colors by
defining them in this alist."
  :group '4coder-theme
  :type '(alist
          :key-type (string :tag "Name")
          :value-type (string :tag " Hex")))

(defcustom 4coder-use-variable-pitch nil
  "Use variable pitch face for some headings and titles."
  :type 'boolean
  :group '4coder-theme
  :package-version '(4coder . "2.6"))

(defcustom 4coder-height-minus-1 0.8
  "Font size -1."
  :type 'number
  :group '4coder-theme
  :package-version '(4coder . "2.6"))

(defcustom 4coder-height-plus-1 1.1
  "Font size +1."
  :type 'number
  :group '4coder-theme
  :package-version '(4coder . "2.6"))

(defcustom 4coder-height-plus-2 1.15
  "Font size +2."
  :type 'number
  :group '4coder-theme
  :package-version '(4coder . "2.6"))

(defcustom 4coder-height-plus-3 1.2
  "Font size +3."
  :type 'number
  :group '4coder-theme
  :package-version '(4coder . "2.6"))

(defcustom 4coder-height-plus-4 1.3
  "Font size +4."
  :type 'number
  :group '4coder-theme
  :package-version '(4coder . "2.6"))

(defcustom 4coder-scale-org-headlines nil
  "Whether `org-mode' headlines should be scaled."
  :type 'boolean
  :group '4coder-theme
  :package-version '(4coder . "2.6"))

(defcustom 4coder-scale-outline-headlines nil
  "Whether `outline-mode' headlines should be scaled."
  :type 'boolean
  :group '4coder-theme
  :package-version '(4coder . "2.6"))

;;; Color Palette

(defvar 4coder-default-colors-alist
  '(("4coder-fg-1"     . "#777790")
    ("4coder-fg-05"    . "#aaaed0")
    ("4coder-fg"       . "#90c080")
    ("4coder-fg+1"     . "#eeefff")
    ("4coder-fg+2"     . "#FFFfff")
    ("4coder-bg-2"     . "#050505")
    ("4coder-bg-1"     . "#121212")
    ("4coder-bg-08"    . "#181818")
    ("4coder-bg-05"    . "#383838")
    ("4coder-bg"       . "#060606")
    ("4coder-bg+05"    . "#02345f")
    ("4coder-bg+1"     . "#03446f")
    ("4coder-bg+2"     . "#045480")
    ("4coder-bg+3"     . "#045480")
    ("4coder-red-6"    . "#6C3333")
    ("4coder-red-5"    . "#7C4343")
    ("4coder-red-4"    . "#8C5353")
    ("4coder-red-3"    . "#9C6363")
    ("4coder-red-2"    . "#AC7373")
    ("4coder-red-1"    . "#BC8383")
    ("4coder-red"      . "#CC9393")
    ("4coder-red+1"    . "#DCA3A3")
    ("4coder-red+2"    . "#ECB3B3")
    ("4coder-orange"   . "#DFAF8F")
    ("4coder-yellow-2" . "#D0BF8F")
    ("4coder-yellow-1" . "#E0CF9F")
    ("4coder-yellow"   . "#d08f20")
    ("4coder-green-5"  . "#2F4F2F")
    ("4coder-green-4"  . "#3F5F3F")
    ("4coder-green-3"  . "#4F6F4F")
    ("4coder-green-2"  . "#5F7F5F")
    ("4coder-green-1"  . "#6F8F6F")
    ("4coder-green"    . "#50ff30")
    ("4coder-green+1"  . "#8FB28F")
    ("4coder-green+2"  . "#9FC59F")
    ("4coder-green+3"  . "#AFD8AF")
    ("4coder-green+4"  . "#BFEBBF")
    ("4coder-cyan"     . "#93E0E3")
    ("4coder-blue+3"   . "#BDE0F3")
    ("4coder-blue+2"   . "#ACE0E3")
    ("4coder-blue+1"   . "#94BFF3")
    ("4coder-blue"     . "#00a0ff")
    ("4coder-blue-1"   . "#7CB8BB")
    ("4coder-blue-2"   . "#6CA0A3")
    ("4coder-blue-3"   . "#5C888B")
    ("4coder-blue-4"   . "#4C7073")
    ("4coder-blue-5"   . "#366060")
    ("4coder-magenta"  . "#cC5Ca3"))
  "List of 4coder colors.
Each element has the form (NAME . HEX).

`+N' suffixes indicate a color is lighter.
`-N' suffixes indicate a color is darker.")

(defmacro 4coder-with-color-variables (&rest body)
  "`let' bind all colors defined in `4coder-colors-alist' around BODY.
Also bind `class' to ((class color) (min-colors 89))."
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
         ,@(mapcar (lambda (cons)
                     (list (intern (car cons)) (cdr cons)))
                   (append 4coder-default-colors-alist
                           4coder-override-colors-alist))
         (z-variable-pitch (if 4coder-use-variable-pitch
                               'variable-pitch 'default)))
     ,@body))

;;; Theme Faces
(4coder-with-color-variables
  (custom-theme-set-faces
   '4coder
;;;; Built-in
;;;;; basic coloring
   '(button ((t (:underline t))))
   `(link ((t (:foreground ,4coder-yellow :underline t :weight bold))))
   `(link-visited ((t (:foreground ,4coder-yellow-2 :underline t :weight normal))))
   `(default ((t (:foreground ,4coder-fg :background ,4coder-bg))))
   `(cursor ((t (:foreground ,4coder-fg :background "#ff0055"))))
   `(widget-field ((t (:foreground ,4coder-fg :background ,4coder-bg+3))))
   `(escape-glyph ((t (:foreground ,4coder-yellow :weight bold))))
   `(fringe ((t (:foreground ,4coder-fg :background ,4coder-bg-1))))
   `(header-line ((t (:foreground ,4coder-yellow
                                  :background ,4coder-bg-1
                                  :box (:line-width -1 :style released-button)))))
   `(highlight ((t (:background ,4coder-bg-05))))
   `(success ((t (:foreground ,4coder-green :weight bold))))
   `(warning ((t (:foreground ,4coder-orange :weight bold))))
   `(tooltip ((t (:foreground ,4coder-fg :background ,4coder-bg+1))))
;;;;; compilation
   `(compilation-column-face ((t (:foreground ,4coder-yellow))))
   `(compilation-enter-directory-face ((t (:foreground ,4coder-green))))
   `(compilation-error-face ((t (:foreground ,4coder-red-1 :weight bold :underline t))))
   `(compilation-face ((t (:foreground ,4coder-fg))))
   `(compilation-info-face ((t (:foreground ,4coder-blue))))
   `(compilation-info ((t (:foreground ,4coder-green+4 :underline t))))
   `(compilation-leave-directory-face ((t (:foreground ,4coder-green))))
   `(compilation-line-face ((t (:foreground ,4coder-yellow))))
   `(compilation-line-number ((t (:foreground ,4coder-yellow))))
   `(compilation-message-face ((t (:foreground ,4coder-blue))))
   `(compilation-warning-face ((t (:foreground ,4coder-orange :weight bold :underline t))))
   `(compilation-mode-line-exit ((t (:foreground ,4coder-green+2 :weight bold))))
   `(compilation-mode-line-fail ((t (:foreground ,4coder-red :weight bold))))
   `(compilation-mode-line-run ((t (:foreground ,4coder-yellow :weight bold))))
;;;;; completions
   `(completions-annotations ((t (:foreground ,4coder-fg-1))))
;;;;; customize
   `(custom-variable-tag ((t (:foreground ,4coder-blue :weight bold))))
   `(custom-group-tag ((t (:foreground ,4coder-blue :weight bold :height 1.2))))
   `(custom-state ((t (:foreground ,4coder-green+4))))
;;;;; display-fill-column-indicator
     `(fill-column-indicator ((,class :foreground ,4coder-bg-05 :weight semilight)))
;;;;; eww
   '(eww-invalid-certificate ((t (:inherit error))))
   '(eww-valid-certificate   ((t (:inherit success))))
;;;;; grep
   `(grep-context-face ((t (:foreground ,4coder-fg))))
   `(grep-error-face ((t (:foreground ,4coder-red-1 :weight bold :underline t))))
   `(grep-hit-face ((t (:foreground ,4coder-blue))))
   `(grep-match-face ((t (:foreground ,4coder-orange :weight bold))))
   `(match ((t (:background ,4coder-bg-1 :foreground ,4coder-orange :weight bold))))
;;;;; hi-lock
   `(hi-blue    ((t (:background ,4coder-cyan    :foreground ,4coder-bg-1))))
   `(hi-green   ((t (:background ,4coder-green+4 :foreground ,4coder-bg-1))))
   `(hi-pink    ((t (:background ,4coder-magenta :foreground ,4coder-bg-1))))
   `(hi-yellow  ((t (:background ,4coder-yellow  :foreground ,4coder-bg-1))))
   `(hi-blue-b  ((t (:foreground ,4coder-blue    :weight     bold))))
   `(hi-green-b ((t (:foreground ,4coder-green+2 :weight     bold))))
   `(hi-red-b   ((t (:foreground ,4coder-red     :weight     bold))))
;;;;; info
   `(Info-quoted ((t (:inherit font-lock-constant-face))))
;;;;; isearch
   `(isearch ((t (:foreground ,4coder-yellow-2 :weight bold :background ,4coder-bg+2))))
   `(isearch-fail ((t (:foreground ,4coder-fg :background ,4coder-red-4))))
   `(lazy-highlight ((t (:foreground ,4coder-yellow-2 :weight bold :background ,4coder-bg-05))))

   `(menu ((t (:foreground ,4coder-fg :background ,4coder-bg))))
   `(minibuffer-prompt ((t (:foreground ,4coder-yellow))))
   `(mode-line
     ((,class (:foreground ,4coder-green+1
                           :background ,4coder-bg-1
              ))
      (t :inverse-video t)))
   `(mode-line-buffer-id ((t (:foreground ,4coder-yellow :weight bold))))
   `(mode-line-inactive
     ((t (:foreground ,4coder-fg-1
                      :background ,4coder-bg-2
              ))))
   `(region ((,class (:background "blue"))
             (t :inverse-video t)))
   `(secondary-selection ((t (:background ,4coder-bg+2))))
   `(trailing-whitespace ((t (:background ,4coder-red))))
   `(vertical-border ((t (:foreground ,4coder-fg))))
;;;;; font lock
   `(font-lock-builtin-face ((t (:foreground ,4coder-fg))))
   `(font-lock-comment-face ((t (:foreground ,4coder-blue))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,4coder-blue))))
   `(font-lock-constant-face ((t (:foreground ,4coder-green))))
   `(font-lock-doc-face ((t (:foreground ,4coder-blue))))
   `(font-lock-function-name-face ((t (:foreground ,4coder-fg))))
   `(font-lock-keyword-face ((t (:foreground ,4coder-yellow))))
   `(font-lock-negation-char-face ((t (:foreground ,4coder-yellow :weight bold))))
   `(font-lock-preprocessor-face ((t (:foreground ,4coder-fg))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,4coder-yellow :weight bold))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,4coder-green :weight bold))))
   `(font-lock-string-face ((t (:foreground ,4coder-green))))
   `(font-lock-type-face ((t (:foreground ,4coder-yellow))))
   `(font-lock-variable-name-face ((t (:foreground ,4coder-fg))))
   `(font-lock-warning-face ((t (:foreground "#ee0020" :weight bold))))

   `(c-annotation-face ((t (:inherit font-lock-constant-face))))
;;;;; line numbers (Emacs 26.1 and above)
   `(line-number ((t (:foreground ,4coder-bg+3 :background ,4coder-bg-05))))
   `(line-number-current-line ((t (:inherit line-number :foreground ,4coder-yellow-2))))
;;;;; man
   '(Man-overstrike ((t (:inherit font-lock-keyword-face))))
   '(Man-underline  ((t (:inherit (font-lock-string-face underline)))))
;;;;; newsticker
   `(newsticker-date-face ((t (:foreground ,4coder-fg))))
   `(newsticker-default-face ((t (:foreground ,4coder-fg))))
   `(newsticker-enclosure-face ((t (:foreground ,4coder-green+3))))
   `(newsticker-extra-face ((t (:foreground ,4coder-bg+2 :height 0.8))))
   `(newsticker-feed-face ((t (:foreground ,4coder-fg))))
   `(newsticker-immortal-item-face ((t (:foreground ,4coder-green))))
   `(newsticker-new-item-face ((t (:foreground ,4coder-blue))))
   `(newsticker-obsolete-item-face ((t (:foreground ,4coder-red))))
   `(newsticker-old-item-face ((t (:foreground ,4coder-bg+3))))
   `(newsticker-statistics-face ((t (:foreground ,4coder-fg))))
   `(newsticker-treeview-face ((t (:foreground ,4coder-fg))))
   `(newsticker-treeview-immortal-face ((t (:foreground ,4coder-green))))
   `(newsticker-treeview-listwindow-face ((t (:foreground ,4coder-fg))))
   `(newsticker-treeview-new-face ((t (:foreground ,4coder-blue :weight bold))))
   `(newsticker-treeview-obsolete-face ((t (:foreground ,4coder-red))))
   `(newsticker-treeview-old-face ((t (:foreground ,4coder-bg+3))))
   `(newsticker-treeview-selection-face ((t (:background ,4coder-bg-1 :foreground ,4coder-yellow))))
;;;;; woman
   '(woman-bold   ((t (:inherit font-lock-keyword-face))))
   '(woman-italic ((t (:inherit (font-lock-string-face italic)))))
;;;; Third-party
;;;;; ace-jump
   `(ace-jump-face-background
     ((t (:foreground ,4coder-fg-1 :background ,4coder-bg :inverse-video nil))))
   `(ace-jump-face-foreground
     ((t (:foreground ,4coder-green+2 :background ,4coder-bg :inverse-video nil))))
;;;;; ace-window
   `(aw-background-face
     ((t (:foreground ,4coder-fg-1 :background ,4coder-bg :inverse-video nil))))
   `(aw-leading-char-face ((t (:inherit aw-mode-line-face))))
;;;;; android mode
   `(android-mode-debug-face ((t (:foreground ,4coder-green+1))))
   `(android-mode-error-face ((t (:foreground ,4coder-orange :weight bold))))
   `(android-mode-info-face ((t (:foreground ,4coder-fg))))
   `(android-mode-verbose-face ((t (:foreground ,4coder-green))))
   `(android-mode-warning-face ((t (:foreground ,4coder-yellow))))
;;;;; anzu
   `(anzu-mode-line ((t (:foreground ,4coder-cyan :weight bold))))
   `(anzu-mode-line-no-match ((t (:foreground ,4coder-red :weight bold))))
   `(anzu-match-1 ((t (:foreground ,4coder-bg :background ,4coder-green))))
   `(anzu-match-2 ((t (:foreground ,4coder-bg :background ,4coder-orange))))
   `(anzu-match-3 ((t (:foreground ,4coder-bg :background ,4coder-blue))))
   `(anzu-replace-to ((t (:inherit anzu-replace-highlight :foreground ,4coder-yellow))))
;;;;; auctex
   `(font-latex-bold-face ((t (:inherit bold))))
   `(font-latex-warning-face ((t (:foreground nil :inherit font-lock-warning-face))))
   `(font-latex-sectioning-5-face ((t (:foreground ,4coder-red :weight bold ))))
   `(font-latex-sedate-face ((t (:foreground ,4coder-yellow))))
   `(font-latex-italic-face ((t (:foreground ,4coder-cyan :slant italic))))
   `(font-latex-string-face ((t (:inherit ,font-lock-string-face))))
   `(font-latex-math-face ((t (:foreground ,4coder-orange))))
   `(font-latex-script-char-face ((t (:foreground ,4coder-orange))))
;;;;; agda-mode
   `(agda2-highlight-keyword-face ((t (:foreground ,4coder-yellow :weight bold))))
   `(agda2-highlight-string-face ((t (:foreground ,4coder-red))))
   `(agda2-highlight-symbol-face ((t (:foreground ,4coder-orange))))
   `(agda2-highlight-primitive-type-face ((t (:foreground ,4coder-blue-1))))
   `(agda2-highlight-inductive-constructor-face ((t (:foreground ,4coder-fg))))
   `(agda2-highlight-coinductive-constructor-face ((t (:foreground ,4coder-fg))))
   `(agda2-highlight-datatype-face ((t (:foreground ,4coder-blue))))
   `(agda2-highlight-function-face ((t (:foreground ,4coder-blue))))
   `(agda2-highlight-module-face ((t (:foreground ,4coder-blue-1))))
   `(agda2-highlight-error-face ((t (:foreground ,4coder-bg :background ,4coder-magenta))))
   `(agda2-highlight-unsolved-meta-face ((t (:foreground ,4coder-bg :background ,4coder-magenta))))
   `(agda2-highlight-unsolved-constraint-face ((t (:foreground ,4coder-bg :background ,4coder-magenta))))
   `(agda2-highlight-termination-problem-face ((t (:foreground ,4coder-bg :background ,4coder-magenta))))
   `(agda2-highlight-incomplete-pattern-face ((t (:foreground ,4coder-bg :background ,4coder-magenta))))
   `(agda2-highlight-typechecks-face ((t (:background ,4coder-red-4))))
;;;;; auto-complete
   `(ac-candidate-face ((t (:background ,4coder-bg+3 :foreground ,4coder-bg-2))))
   `(ac-selection-face ((t (:background ,4coder-blue-4 :foreground ,4coder-fg))))
   `(popup-tip-face ((t (:background ,4coder-yellow-2 :foreground ,4coder-bg-2))))
   `(popup-menu-mouse-face ((t (:background ,4coder-yellow-2 :foreground ,4coder-bg-2))))
   `(popup-summary-face ((t (:background ,4coder-bg+3 :foreground ,4coder-bg-2))))
   `(popup-scroll-bar-foreground-face ((t (:background ,4coder-blue-5))))
   `(popup-scroll-bar-background-face ((t (:background ,4coder-bg-1))))
   `(popup-isearch-match ((t (:background ,4coder-bg :foreground ,4coder-fg))))
;;;;; avy
   `(avy-background-face
     ((t (:foreground ,4coder-fg-1 :background ,4coder-bg :inverse-video nil))))
   `(avy-lead-face-0
     ((t (:foreground ,4coder-green+3 :background ,4coder-bg :inverse-video nil :weight bold))))
   `(avy-lead-face-1
     ((t (:foreground ,4coder-yellow :background ,4coder-bg :inverse-video nil :weight bold))))
   `(avy-lead-face-2
     ((t (:foreground ,4coder-red+1 :background ,4coder-bg :inverse-video nil :weight bold))))
   `(avy-lead-face
     ((t (:foreground ,4coder-cyan :background ,4coder-bg :inverse-video nil :weight bold))))
;;;;; company-mode
   `(company-tooltip ((t (:foreground ,4coder-fg :background ,4coder-bg+1))))
   `(company-tooltip-annotation ((t (:foreground ,4coder-orange :background ,4coder-bg+1))))
   `(company-tooltip-annotation-selection ((t (:foreground ,4coder-orange :background ,4coder-bg-1))))
   `(company-tooltip-selection ((t (:foreground ,4coder-fg :background ,4coder-bg-1))))
   `(company-tooltip-mouse ((t (:background ,4coder-bg-1))))
   `(company-tooltip-common ((t (:foreground ,4coder-green+2))))
   `(company-tooltip-common-selection ((t (:foreground ,4coder-green+2))))
   `(company-scrollbar-fg ((t (:background ,4coder-bg-1))))
   `(company-scrollbar-bg ((t (:background ,4coder-bg+2))))
   `(company-preview ((t (:background ,4coder-green+2))))
   `(company-preview-common ((t (:foreground ,4coder-green+2 :background ,4coder-bg-1))))
;;;;; bm
   `(bm-face ((t (:background ,4coder-yellow-1 :foreground ,4coder-bg))))
   `(bm-fringe-face ((t (:background ,4coder-yellow-1 :foreground ,4coder-bg))))
   `(bm-fringe-persistent-face ((t (:background ,4coder-green-2 :foreground ,4coder-bg))))
   `(bm-persistent-face ((t (:background ,4coder-green-2 :foreground ,4coder-bg))))
;;;;; calfw
   `(cfw:face-annotation ((t (:foreground ,4coder-red :inherit cfw:face-day-title))))
   `(cfw:face-day-title ((t nil)))
   `(cfw:face-default-content ((t (:foreground ,4coder-green))))
   `(cfw:face-default-day ((t (:weight bold))))
   `(cfw:face-disable ((t (:foreground ,4coder-fg-1))))
   `(cfw:face-grid ((t (:inherit shadow))))
   `(cfw:face-header ((t (:inherit font-lock-keyword-face))))
   `(cfw:face-holiday ((t (:inherit cfw:face-sunday))))
   `(cfw:face-periods ((t (:foreground ,4coder-cyan))))
   `(cfw:face-saturday ((t (:foreground ,4coder-blue :weight bold))))
   `(cfw:face-select ((t (:background ,4coder-blue-5))))
   `(cfw:face-sunday ((t (:foreground ,4coder-red :weight bold))))
   `(cfw:face-title ((t (:height 2.0 :inherit (variable-pitch font-lock-keyword-face)))))
   `(cfw:face-today ((t (:foreground ,4coder-cyan :weight bold))))
   `(cfw:face-today-title ((t (:inherit highlight bold))))
   `(cfw:face-toolbar ((t (:background ,4coder-blue-5))))
   `(cfw:face-toolbar-button-off ((t (:underline nil :inherit link))))
   `(cfw:face-toolbar-button-on ((t (:underline nil :inherit link-visited))))
;;;;; centaur-tabs
   `(centaur-tabs-default ((t (:background ,4coder-bg :foreground ,4coder-fg :box nil))))
   `(centaur-tabs-selected ((t (:background ,4coder-bg :foreground ,4coder-fg+2 :box nil))))
   `(centaur-tabs-unselected ((t (:background ,4coder-bg-1 :foreground ,4coder-fg-05 :box nil))))
   `(centaur-tabs-selected-modified ((t (:background ,4coder-bg :foreground ,4coder-orange :box nil))))
   `(centaur-tabs-unselected-modified ((t (:background ,4coder-bg-1 :foreground ,4coder-orange :box nil))))
   `(centaur-tabs-active-bar-face ((t (:background ,4coder-yellow :box nil))))
   `(centaur-tabs-modified-marker-selected ((t (:inherit 'centaur-tabs-selected-modified :foreground ,4coder-yellow :box nil))))
   `(centaur-tabs-modified-marker-unselected ((t (:inherit 'centaur-tabs-unselected-modified :foreground ,4coder-yellow :box nil))))
;;;;; cider
   `(cider-result-overlay-face ((t (:background unspecified))))
   `(cider-enlightened-face ((t (:box (:color ,4coder-orange :line-width -1)))))
   `(cider-enlightened-local-face ((t (:weight bold :foreground ,4coder-green+1))))
   `(cider-deprecated-face ((t (:background ,4coder-yellow-2))))
   `(cider-instrumented-face ((t (:box (:color ,4coder-red :line-width -1)))))
   `(cider-traced-face ((t (:box (:color ,4coder-cyan :line-width -1)))))
   `(cider-test-failure-face ((t (:background ,4coder-red-4))))
   `(cider-test-error-face ((t (:background ,4coder-magenta))))
   `(cider-test-success-face ((t (:background ,4coder-green-2))))
   `(cider-fringe-good-face ((t (:foreground ,4coder-green+4))))
;;;;; circe
   `(circe-highlight-nick-face ((t (:foreground ,4coder-cyan))))
   `(circe-my-message-face ((t (:foreground ,4coder-fg))))
   `(circe-fool-face ((t (:foreground ,4coder-red+1))))
   `(circe-topic-diff-removed-face ((t (:foreground ,4coder-red :weight bold))))
   `(circe-originator-face ((t (:foreground ,4coder-fg))))
   `(circe-server-face ((t (:foreground ,4coder-green))))
   `(circe-topic-diff-new-face ((t (:foreground ,4coder-orange :weight bold))))
   `(circe-prompt-face ((t (:foreground ,4coder-orange :background ,4coder-bg :weight bold))))
;;;;; context-coloring
   `(context-coloring-level-0-face ((t :foreground ,4coder-fg)))
   `(context-coloring-level-1-face ((t :foreground ,4coder-cyan)))
   `(context-coloring-level-2-face ((t :foreground ,4coder-green+4)))
   `(context-coloring-level-3-face ((t :foreground ,4coder-yellow)))
   `(context-coloring-level-4-face ((t :foreground ,4coder-orange)))
   `(context-coloring-level-5-face ((t :foreground ,4coder-magenta)))
   `(context-coloring-level-6-face ((t :foreground ,4coder-blue+1)))
   `(context-coloring-level-7-face ((t :foreground ,4coder-green+2)))
   `(context-coloring-level-8-face ((t :foreground ,4coder-yellow-2)))
   `(context-coloring-level-9-face ((t :foreground ,4coder-red+1)))
;;;;; coq
   `(coq-solve-tactics-face ((t (:foreground nil :inherit font-lock-constant-face))))
;;;;; ctable
   `(ctbl:face-cell-select ((t (:background ,4coder-blue :foreground ,4coder-bg))))
   `(ctbl:face-continue-bar ((t (:background ,4coder-bg-05 :foreground ,4coder-bg))))
   `(ctbl:face-row-select ((t (:background ,4coder-cyan :foreground ,4coder-bg))))
;;;;; debbugs
   `(debbugs-gnu-done ((t (:foreground ,4coder-fg-1))))
   `(debbugs-gnu-handled ((t (:foreground ,4coder-green))))
   `(debbugs-gnu-new ((t (:foreground ,4coder-red))))
   `(debbugs-gnu-pending ((t (:foreground ,4coder-blue))))
   `(debbugs-gnu-stale ((t (:foreground ,4coder-orange))))
   `(debbugs-gnu-tagged ((t (:foreground ,4coder-red))))
;;;;; diff
   ;; Please read (info "(magit)Theming Faces") before changing this.
   `(diff-added          ((t (:background "#335533" :foreground ,4coder-green))))
   `(diff-changed        ((t (:background "#555511" :foreground ,4coder-yellow-1))))
   `(diff-removed        ((t (:background "#553333" :foreground ,4coder-red-2))))
   `(diff-refine-added   ((t (:background "#338833" :foreground ,4coder-green+4))))
   `(diff-refine-changed ((t (:background "#888811" :foreground ,4coder-yellow))))
   `(diff-refine-removed ((t (:background "#883333" :foreground ,4coder-red))))
   `(diff-header ((,class (:background ,4coder-bg+2))
                  (t (:background ,4coder-fg :foreground ,4coder-bg))))
   `(diff-file-header
     ((,class (:background ,4coder-bg+2 :foreground ,4coder-fg :weight bold))
      (t (:background ,4coder-fg :foreground ,4coder-bg :weight bold))))
;;;;; diff-hl
   `(diff-hl-change ((,class (:foreground ,4coder-blue :background ,4coder-blue-2))))
   `(diff-hl-delete ((,class (:foreground ,4coder-red+1 :background ,4coder-red-1))))
   `(diff-hl-insert ((,class (:foreground ,4coder-green+1 :background ,4coder-green-2))))
;;;;; dim-autoload
   `(dim-autoload-cookie-line ((t :foreground ,4coder-bg+1)))
;;;;; dired+
   `(diredp-display-msg ((t (:foreground ,4coder-blue))))
   `(diredp-compressed-file-suffix ((t (:foreground ,4coder-orange))))
   `(diredp-date-time ((t (:foreground ,4coder-magenta))))
   `(diredp-deletion ((t (:foreground ,4coder-yellow))))
   `(diredp-deletion-file-name ((t (:foreground ,4coder-red))))
   `(diredp-dir-heading ((t (:foreground ,4coder-blue :background ,4coder-bg-1))))
   `(diredp-dir-priv ((t (:foreground ,4coder-cyan))))
   `(diredp-exec-priv ((t (:foreground ,4coder-red))))
   `(diredp-executable-tag ((t (:foreground ,4coder-green+1))))
   `(diredp-file-name ((t (:foreground ,4coder-blue))))
   `(diredp-file-suffix ((t (:foreground ,4coder-green))))
   `(diredp-flag-mark ((t (:foreground ,4coder-yellow))))
   `(diredp-flag-mark-line ((t (:foreground ,4coder-orange))))
   `(diredp-ignored-file-name ((t (:foreground ,4coder-red))))
   `(diredp-link-priv ((t (:foreground ,4coder-yellow))))
   `(diredp-mode-line-flagged ((t (:foreground ,4coder-yellow))))
   `(diredp-mode-line-marked ((t (:foreground ,4coder-orange))))
   `(diredp-no-priv ((t (:foreground ,4coder-fg))))
   `(diredp-number ((t (:foreground ,4coder-green+1))))
   `(diredp-other-priv ((t (:foreground ,4coder-yellow-1))))
   `(diredp-rare-priv ((t (:foreground ,4coder-red-1))))
   `(diredp-read-priv ((t (:foreground ,4coder-green-2))))
   `(diredp-symlink ((t (:foreground ,4coder-yellow))))
   `(diredp-write-priv ((t (:foreground ,4coder-magenta))))
;;;;; dired-async
   `(dired-async-failures ((t (:foreground ,4coder-red :weight bold))))
   `(dired-async-message ((t (:foreground ,4coder-yellow :weight bold))))
   `(dired-async-mode-message ((t (:foreground ,4coder-yellow))))
;;;;; diredfl
   `(diredfl-compressed-file-suffix ((t (:foreground ,4coder-orange))))
   `(diredfl-date-time ((t (:foreground ,4coder-magenta))))
   `(diredfl-deletion ((t (:foreground ,4coder-yellow))))
   `(diredfl-deletion-file-name ((t (:foreground ,4coder-red))))
   `(diredfl-dir-heading ((t (:foreground ,4coder-blue :background ,4coder-bg-1))))
   `(diredfl-dir-priv ((t (:foreground ,4coder-cyan))))
   `(diredfl-exec-priv ((t (:foreground ,4coder-red))))
   `(diredfl-executable-tag ((t (:foreground ,4coder-green+1))))
   `(diredfl-file-name ((t (:foreground ,4coder-blue))))
   `(diredfl-file-suffix ((t (:foreground ,4coder-green))))
   `(diredfl-flag-mark ((t (:foreground ,4coder-yellow))))
   `(diredfl-flag-mark-line ((t (:foreground ,4coder-orange))))
   `(diredfl-ignored-file-name ((t (:foreground ,4coder-red))))
   `(diredfl-link-priv ((t (:foreground ,4coder-yellow))))
   `(diredfl-no-priv ((t (:foreground ,4coder-fg))))
   `(diredfl-number ((t (:foreground ,4coder-green+1))))
   `(diredfl-other-priv ((t (:foreground ,4coder-yellow-1))))
   `(diredfl-rare-priv ((t (:foreground ,4coder-red-1))))
   `(diredfl-read-priv ((t (:foreground ,4coder-green-1))))
   `(diredfl-symlink ((t (:foreground ,4coder-yellow))))
   `(diredfl-write-priv ((t (:foreground ,4coder-magenta))))
;;;;; doom-modeline
   `(doom-modeline-bar  ((t (:background ,4coder-yellow))))
   `(doom-modeline-inactive-bar  ((t (:background nil))))
;;;;; ediff
   `(ediff-current-diff-A ((t (:foreground ,4coder-fg :background ,4coder-red-4))))
   `(ediff-current-diff-Ancestor ((t (:foreground ,4coder-fg :background ,4coder-red-4))))
   `(ediff-current-diff-B ((t (:foreground ,4coder-fg :background ,4coder-green-2))))
   `(ediff-current-diff-C ((t (:foreground ,4coder-fg :background ,4coder-blue-5))))
   `(ediff-even-diff-A ((t (:background ,4coder-bg+1))))
   `(ediff-even-diff-Ancestor ((t (:background ,4coder-bg+1))))
   `(ediff-even-diff-B ((t (:background ,4coder-bg+1))))
   `(ediff-even-diff-C ((t (:background ,4coder-bg+1))))
   `(ediff-fine-diff-A ((t (:foreground ,4coder-fg :background ,4coder-red-2 :weight bold))))
   `(ediff-fine-diff-Ancestor ((t (:foreground ,4coder-fg :background ,4coder-red-2 weight bold))))
   `(ediff-fine-diff-B ((t (:foreground ,4coder-fg :background ,4coder-green :weight bold))))
   `(ediff-fine-diff-C ((t (:foreground ,4coder-fg :background ,4coder-blue-3 :weight bold ))))
   `(ediff-odd-diff-A ((t (:background ,4coder-bg+2))))
   `(ediff-odd-diff-Ancestor ((t (:background ,4coder-bg+2))))
   `(ediff-odd-diff-B ((t (:background ,4coder-bg+2))))
   `(ediff-odd-diff-C ((t (:background ,4coder-bg+2))))
;;;;; egg
   `(egg-text-base ((t (:foreground ,4coder-fg))))
   `(egg-help-header-1 ((t (:foreground ,4coder-yellow))))
   `(egg-help-header-2 ((t (:foreground ,4coder-green+3))))
   `(egg-branch ((t (:foreground ,4coder-yellow))))
   `(egg-branch-mono ((t (:foreground ,4coder-yellow))))
   `(egg-term ((t (:foreground ,4coder-yellow))))
   `(egg-diff-add ((t (:foreground ,4coder-green+4))))
   `(egg-diff-del ((t (:foreground ,4coder-red+1))))
   `(egg-diff-file-header ((t (:foreground ,4coder-yellow-2))))
   `(egg-section-title ((t (:foreground ,4coder-yellow))))
   `(egg-stash-mono ((t (:foreground ,4coder-green+4))))
;;;;; elfeed
   `(elfeed-log-error-level-face ((t (:foreground ,4coder-red))))
   `(elfeed-log-info-level-face ((t (:foreground ,4coder-blue))))
   `(elfeed-log-warn-level-face ((t (:foreground ,4coder-yellow))))
   `(elfeed-search-date-face ((t (:foreground ,4coder-yellow-1 :underline t
                                              :weight bold))))
   `(elfeed-search-tag-face ((t (:foreground ,4coder-green))))
   `(elfeed-search-feed-face ((t (:foreground ,4coder-cyan))))
;;;;; emacs-w3m
   `(w3m-anchor ((t (:foreground ,4coder-yellow :underline t
                                 :weight bold))))
   `(w3m-arrived-anchor ((t (:foreground ,4coder-yellow-2
                                         :underline t :weight normal))))
   `(w3m-form ((t (:foreground ,4coder-red-1 :underline t))))
   `(w3m-header-line-location-title ((t (:foreground ,4coder-yellow
                                                     :underline t :weight bold))))
   '(w3m-history-current-url ((t (:inherit match))))
   `(w3m-lnum ((t (:foreground ,4coder-green+2 :background ,4coder-bg))))
   `(w3m-lnum-match ((t (:background ,4coder-bg-1
                                     :foreground ,4coder-orange
                                     :weight bold))))
   `(w3m-lnum-minibuffer-prompt ((t (:foreground ,4coder-yellow))))
;;;;; erc
   `(erc-action-face ((t (:inherit erc-default-face))))
   `(erc-bold-face ((t (:weight bold))))
   `(erc-current-nick-face ((t (:foreground ,4coder-blue :weight bold))))
   `(erc-dangerous-host-face ((t (:inherit font-lock-warning-face))))
   `(erc-default-face ((t (:foreground ,4coder-fg))))
   `(erc-direct-msg-face ((t (:inherit erc-default-face))))
   `(erc-error-face ((t (:inherit font-lock-warning-face))))
   `(erc-fool-face ((t (:inherit erc-default-face))))
   `(erc-highlight-face ((t (:inherit hover-highlight))))
   `(erc-input-face ((t (:foreground ,4coder-yellow))))
   `(erc-keyword-face ((t (:foreground ,4coder-blue :weight bold))))
   `(erc-nick-default-face ((t (:foreground ,4coder-yellow :weight bold))))
   `(erc-my-nick-face ((t (:foreground ,4coder-red :weight bold))))
   `(erc-nick-msg-face ((t (:inherit erc-default-face))))
   `(erc-notice-face ((t (:foreground ,4coder-green))))
   `(erc-pal-face ((t (:foreground ,4coder-orange :weight bold))))
   `(erc-prompt-face ((t (:foreground ,4coder-orange :background ,4coder-bg :weight bold))))
   `(erc-timestamp-face ((t (:foreground ,4coder-green+4))))
   `(erc-underline-face ((t (:underline t))))
;;;;; eros
   `(eros-result-overlay-face ((t (:background unspecified))))
;;;;; ert
   `(ert-test-result-expected ((t (:foreground ,4coder-green+4 :background ,4coder-bg))))
   `(ert-test-result-unexpected ((t (:foreground ,4coder-red :background ,4coder-bg))))
;;;;; eshell
   `(eshell-prompt ((t (:foreground ,4coder-yellow :weight bold))))
   `(eshell-ls-archive ((t (:foreground ,4coder-red-1 :weight bold))))
   `(eshell-ls-backup ((t (:inherit font-lock-comment-face))))
   `(eshell-ls-clutter ((t (:inherit font-lock-comment-face))))
   `(eshell-ls-directory ((t (:foreground ,4coder-blue+1 :weight bold))))
   `(eshell-ls-executable ((t (:foreground ,4coder-red+1 :weight bold))))
   `(eshell-ls-unreadable ((t (:foreground ,4coder-fg))))
   `(eshell-ls-missing ((t (:inherit font-lock-warning-face))))
   `(eshell-ls-product ((t (:inherit font-lock-doc-face))))
   `(eshell-ls-special ((t (:foreground ,4coder-yellow :weight bold))))
   `(eshell-ls-symlink ((t (:foreground ,4coder-cyan :weight bold))))
;;;;; flx
   `(flx-highlight-face ((t (:foreground ,4coder-green+2 :weight bold))))
;;;;; flycheck
   `(flycheck-error
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,4coder-red-1) :inherit unspecified))
      (t (:foreground ,4coder-red-1 :weight bold :underline t))))
   `(flycheck-warning
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,4coder-yellow) :inherit unspecified))
      (t (:foreground ,4coder-yellow :weight bold :underline t))))
   `(flycheck-info
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,4coder-cyan) :inherit unspecified))
      (t (:foreground ,4coder-cyan :weight bold :underline t))))
   `(flycheck-fringe-error ((t (:foreground ,4coder-red-1 :weight bold))))
   `(flycheck-fringe-warning ((t (:foreground ,4coder-yellow :weight bold))))
   `(flycheck-fringe-info ((t (:foreground ,4coder-cyan :weight bold))))
;;;;; flymake
   `(flymake-errline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,4coder-red)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,4coder-red-1 :weight bold :underline t))))
   `(flymake-warnline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,4coder-orange)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,4coder-orange :weight bold :underline t))))
   `(flymake-infoline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,4coder-green)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,4coder-green-2 :weight bold :underline t))))
;;;;; flyspell
   `(flyspell-duplicate
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,4coder-orange) :inherit unspecified))
      (t (:foreground ,4coder-orange :weight bold :underline t))))
   `(flyspell-incorrect
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,4coder-red) :inherit unspecified))
      (t (:foreground ,4coder-red-1 :weight bold :underline t))))
;;;;; full-ack
   `(ack-separator ((t (:foreground ,4coder-fg))))
   `(ack-file ((t (:foreground ,4coder-blue))))
   `(ack-line ((t (:foreground ,4coder-yellow))))
   `(ack-match ((t (:foreground ,4coder-orange :background ,4coder-bg-1 :weight bold))))
;;;;; git-annex
   '(git-annex-dired-annexed-available ((t (:inherit success :weight normal))))
   '(git-annex-dired-annexed-unavailable ((t (:inherit error :weight normal))))
;;;;; git-commit
   `(git-commit-comment-action  ((,class (:foreground ,4coder-green+1 :weight bold))))
   `(git-commit-comment-branch  ((,class (:foreground ,4coder-blue+1  :weight bold)))) ; obsolete
   `(git-commit-comment-branch-local  ((,class (:foreground ,4coder-blue+1  :weight bold))))
   `(git-commit-comment-branch-remote ((,class (:foreground ,4coder-green  :weight bold))))
   `(git-commit-comment-heading ((,class (:foreground ,4coder-yellow  :weight bold))))
;;;;; git-gutter
   `(git-gutter:added ((t (:foreground ,4coder-green :weight bold :inverse-video t))))
   `(git-gutter:deleted ((t (:foreground ,4coder-red :weight bold :inverse-video t))))
   `(git-gutter:modified ((t (:foreground ,4coder-magenta :weight bold :inverse-video t))))
   `(git-gutter:unchanged ((t (:foreground ,4coder-fg :weight bold :inverse-video t))))
;;;;; git-gutter-fr
   `(git-gutter-fr:added ((t (:foreground ,4coder-green  :weight bold))))
   `(git-gutter-fr:deleted ((t (:foreground ,4coder-red :weight bold))))
   `(git-gutter-fr:modified ((t (:foreground ,4coder-magenta :weight bold))))
;;;;; git-rebase
   `(git-rebase-hash ((t (:foreground, 4coder-orange))))
;;;;; gnus
   `(gnus-group-mail-1 ((t (:weight bold :inherit gnus-group-mail-1-empty))))
   `(gnus-group-mail-1-empty ((t (:inherit gnus-group-news-1-empty))))
   `(gnus-group-mail-2 ((t (:weight bold :inherit gnus-group-mail-2-empty))))
   `(gnus-group-mail-2-empty ((t (:inherit gnus-group-news-2-empty))))
   `(gnus-group-mail-3 ((t (:weight bold :inherit gnus-group-mail-3-empty))))
   `(gnus-group-mail-3-empty ((t (:inherit gnus-group-news-3-empty))))
   `(gnus-group-mail-4 ((t (:weight bold :inherit gnus-group-mail-4-empty))))
   `(gnus-group-mail-4-empty ((t (:inherit gnus-group-news-4-empty))))
   `(gnus-group-mail-5 ((t (:weight bold :inherit gnus-group-mail-5-empty))))
   `(gnus-group-mail-5-empty ((t (:inherit gnus-group-news-5-empty))))
   `(gnus-group-mail-6 ((t (:weight bold :inherit gnus-group-mail-6-empty))))
   `(gnus-group-mail-6-empty ((t (:inherit gnus-group-news-6-empty))))
   `(gnus-group-mail-low ((t (:weight bold :inherit gnus-group-mail-low-empty))))
   `(gnus-group-mail-low-empty ((t (:inherit gnus-group-news-low-empty))))
   `(gnus-group-news-1 ((t (:weight bold :inherit gnus-group-news-1-empty))))
   `(gnus-group-news-2 ((t (:weight bold :inherit gnus-group-news-2-empty))))
   `(gnus-group-news-3 ((t (:weight bold :inherit gnus-group-news-3-empty))))
   `(gnus-group-news-4 ((t (:weight bold :inherit gnus-group-news-4-empty))))
   `(gnus-group-news-5 ((t (:weight bold :inherit gnus-group-news-5-empty))))
   `(gnus-group-news-6 ((t (:weight bold :inherit gnus-group-news-6-empty))))
   `(gnus-group-news-low ((t (:weight bold :inherit gnus-group-news-low-empty))))
   `(gnus-header-content ((t (:inherit message-header-other))))
   `(gnus-header-from ((t (:inherit message-header-to))))
   `(gnus-header-name ((t (:inherit message-header-name))))
   `(gnus-header-newsgroups ((t (:inherit message-header-other))))
   `(gnus-header-subject ((t (:inherit message-header-subject))))
   `(gnus-server-opened ((t (:foreground ,4coder-green+2 :weight bold))))
   `(gnus-server-denied ((t (:foreground ,4coder-red+1 :weight bold))))
   `(gnus-server-closed ((t (:foreground ,4coder-blue :slant italic))))
   `(gnus-server-offline ((t (:foreground ,4coder-yellow :weight bold))))
   `(gnus-server-agent ((t (:foreground ,4coder-blue :weight bold))))
   `(gnus-summary-cancelled ((t (:foreground ,4coder-orange))))
   `(gnus-summary-high-ancient ((t (:foreground ,4coder-blue))))
   `(gnus-summary-high-read ((t (:foreground ,4coder-green :weight bold))))
   `(gnus-summary-high-ticked ((t (:foreground ,4coder-orange :weight bold))))
   `(gnus-summary-high-unread ((t (:foreground ,4coder-fg :weight bold))))
   `(gnus-summary-low-ancient ((t (:foreground ,4coder-blue))))
   `(gnus-summary-low-read ((t (:foreground ,4coder-green))))
   `(gnus-summary-low-ticked ((t (:foreground ,4coder-orange :weight bold))))
   `(gnus-summary-low-unread ((t (:foreground ,4coder-fg))))
   `(gnus-summary-normal-ancient ((t (:foreground ,4coder-blue))))
   `(gnus-summary-normal-read ((t (:foreground ,4coder-green))))
   `(gnus-summary-normal-ticked ((t (:foreground ,4coder-orange :weight bold))))
   `(gnus-summary-normal-unread ((t (:foreground ,4coder-fg))))
   `(gnus-summary-selected ((t (:foreground ,4coder-yellow :weight bold))))
   `(gnus-cite-1 ((t (:foreground ,4coder-blue))))
   `(gnus-cite-10 ((t (:foreground ,4coder-yellow-1))))
   `(gnus-cite-11 ((t (:foreground ,4coder-yellow))))
   `(gnus-cite-2 ((t (:foreground ,4coder-blue-1))))
   `(gnus-cite-3 ((t (:foreground ,4coder-blue-2))))
   `(gnus-cite-4 ((t (:foreground ,4coder-green+2))))
   `(gnus-cite-5 ((t (:foreground ,4coder-green+1))))
   `(gnus-cite-6 ((t (:foreground ,4coder-green))))
   `(gnus-cite-7 ((t (:foreground ,4coder-red))))
   `(gnus-cite-8 ((t (:foreground ,4coder-red-1))))
   `(gnus-cite-9 ((t (:foreground ,4coder-red-2))))
   `(gnus-group-news-1-empty ((t (:foreground ,4coder-yellow))))
   `(gnus-group-news-2-empty ((t (:foreground ,4coder-green+3))))
   `(gnus-group-news-3-empty ((t (:foreground ,4coder-green+1))))
   `(gnus-group-news-4-empty ((t (:foreground ,4coder-blue-2))))
   `(gnus-group-news-5-empty ((t (:foreground ,4coder-blue-3))))
   `(gnus-group-news-6-empty ((t (:foreground ,4coder-bg+2))))
   `(gnus-group-news-low-empty ((t (:foreground ,4coder-bg+2))))
   `(gnus-signature ((t (:foreground ,4coder-yellow))))
   `(gnus-x ((t (:background ,4coder-fg :foreground ,4coder-bg))))
   `(mm-uu-extract ((t (:background ,4coder-bg-05 :foreground ,4coder-green+1))))
;;;;; go-guru
   `(go-guru-hl-identifier-face ((t (:foreground ,4coder-bg-1 :background ,4coder-green+1))))
;;;;; guide-key
   `(guide-key/highlight-command-face ((t (:foreground ,4coder-blue))))
   `(guide-key/key-face ((t (:foreground ,4coder-green))))
   `(guide-key/prefix-command-face ((t (:foreground ,4coder-green+1))))
;;;;; hackernews
   '(hackernews-comment-count ((t (:inherit link-visited :underline nil))))
   '(hackernews-link          ((t (:inherit link         :underline nil))))
;;;;; helm
   `(helm-header
     ((t (:foreground ,4coder-green
                      :background ,4coder-bg
                      :underline nil
                      :box nil))))
   `(helm-source-header
     ((t (:foreground ,4coder-yellow
                      :background ,4coder-bg-1
                      :underline nil
                      :weight bold
                      :box (:line-width -1 :style released-button)))))
   `(helm-selection ((t (:background ,4coder-bg+1 :underline nil))))
   `(helm-selection-line ((t (:background ,4coder-bg+1))))
   `(helm-visible-mark ((t (:foreground ,4coder-bg :background ,4coder-yellow-2))))
   `(helm-candidate-number ((t (:foreground ,4coder-green+4 :background ,4coder-bg-1))))
   `(helm-separator ((t (:foreground ,4coder-red :background ,4coder-bg))))
   `(helm-time-zone-current ((t (:foreground ,4coder-green+2 :background ,4coder-bg))))
   `(helm-time-zone-home ((t (:foreground ,4coder-red :background ,4coder-bg))))
   `(helm-bookmark-addressbook ((t (:foreground ,4coder-orange :background ,4coder-bg))))
   `(helm-bookmark-directory ((t (:foreground nil :background nil :inherit helm-ff-directory))))
   `(helm-bookmark-file ((t (:foreground nil :background nil :inherit helm-ff-file))))
   `(helm-bookmark-gnus ((t (:foreground ,4coder-magenta :background ,4coder-bg))))
   `(helm-bookmark-info ((t (:foreground ,4coder-green+2 :background ,4coder-bg))))
   `(helm-bookmark-man ((t (:foreground ,4coder-yellow :background ,4coder-bg))))
   `(helm-bookmark-w3m ((t (:foreground ,4coder-magenta :background ,4coder-bg))))
   `(helm-buffer-not-saved ((t (:foreground ,4coder-red :background ,4coder-bg))))
   `(helm-buffer-process ((t (:foreground ,4coder-cyan :background ,4coder-bg))))
   `(helm-buffer-saved-out ((t (:foreground ,4coder-fg :background ,4coder-bg))))
   `(helm-buffer-size ((t (:foreground ,4coder-fg-1 :background ,4coder-bg))))
   `(helm-ff-directory ((t (:foreground ,4coder-cyan :background ,4coder-bg :weight bold))))
   `(helm-ff-file ((t (:foreground ,4coder-fg :background ,4coder-bg :weight normal))))
   `(helm-ff-executable ((t (:foreground ,4coder-green+2 :background ,4coder-bg :weight normal))))
   `(helm-ff-invalid-symlink ((t (:foreground ,4coder-red :background ,4coder-bg :weight bold))))
   `(helm-ff-symlink ((t (:foreground ,4coder-yellow :background ,4coder-bg :weight bold))))
   `(helm-ff-prefix ((t (:foreground ,4coder-bg :background ,4coder-yellow :weight normal))))
   `(helm-grep-cmd-line ((t (:foreground ,4coder-cyan :background ,4coder-bg))))
   `(helm-grep-file ((t (:foreground ,4coder-fg :background ,4coder-bg))))
   `(helm-grep-finish ((t (:foreground ,4coder-green+2 :background ,4coder-bg))))
   `(helm-grep-lineno ((t (:foreground ,4coder-fg-1 :background ,4coder-bg))))
   `(helm-grep-match ((t (:foreground nil :background nil :inherit helm-match))))
   `(helm-grep-running ((t (:foreground ,4coder-red :background ,4coder-bg))))
   `(helm-match ((t (:foreground ,4coder-orange :background ,4coder-bg-1 :weight bold))))
   `(helm-moccur-buffer ((t (:foreground ,4coder-cyan :background ,4coder-bg))))
   `(helm-mu-contacts-address-face ((t (:foreground ,4coder-fg-1 :background ,4coder-bg))))
   `(helm-mu-contacts-name-face ((t (:foreground ,4coder-fg :background ,4coder-bg))))
;;;;; helm-lxc
   `(helm-lxc-face-frozen ((t (:foreground ,4coder-blue :background ,4coder-bg))))
   `(helm-lxc-face-running ((t (:foreground ,4coder-green :background ,4coder-bg))))
   `(helm-lxc-face-stopped ((t (:foreground ,4coder-red :background ,4coder-bg))))
;;;;; helm-swoop
   `(helm-swoop-target-line-face ((t (:foreground ,4coder-fg :background ,4coder-bg+1))))
   `(helm-swoop-target-word-face ((t (:foreground ,4coder-yellow :background ,4coder-bg+2 :weight bold))))
;;;;; hl-line-mode
   `(hl-line-face ((,class (:background ,4coder-bg-05))
                   (t :weight bold)))
   `(hl-line ((,class (:background ,4coder-bg-05)) ; old emacsen
              (t :weight bold)))
;;;;; hl-sexp
   `(hl-sexp-face ((,class (:background ,4coder-bg+1))
                   (t :weight bold)))
;;;;; hydra
   `(hydra-face-red ((t (:foreground ,4coder-red-1 :background ,4coder-bg))))
   `(hydra-face-amaranth ((t (:foreground ,4coder-red-3 :background ,4coder-bg))))
   `(hydra-face-blue ((t (:foreground ,4coder-blue :background ,4coder-bg))))
   `(hydra-face-pink ((t (:foreground ,4coder-magenta :background ,4coder-bg))))
   `(hydra-face-teal ((t (:foreground ,4coder-cyan :background ,4coder-bg))))
;;;;; info+
   `(info-command-ref-item ((t (:background ,4coder-bg-1 :foreground ,4coder-orange))))
   `(info-constant-ref-item ((t (:background ,4coder-bg-1 :foreground ,4coder-magenta))))
   `(info-double-quoted-name ((t (:inherit font-lock-comment-face))))
   `(info-file ((t (:background ,4coder-bg-1 :foreground ,4coder-yellow))))
   `(info-function-ref-item ((t (:background ,4coder-bg-1 :inherit font-lock-function-name-face))))
   `(info-macro-ref-item ((t (:background ,4coder-bg-1 :foreground ,4coder-yellow))))
   `(info-menu ((t (:foreground ,4coder-yellow))))
   `(info-quoted-name ((t (:inherit font-lock-constant-face))))
   `(info-reference-item ((t (:background ,4coder-bg-1))))
   `(info-single-quote ((t (:inherit font-lock-keyword-face))))
   `(info-special-form-ref-item ((t (:background ,4coder-bg-1 :foreground ,4coder-yellow))))
   `(info-string ((t (:inherit font-lock-string-face))))
   `(info-syntax-class-item ((t (:background ,4coder-bg-1 :foreground ,4coder-blue+1))))
   `(info-user-option-ref-item ((t (:background ,4coder-bg-1 :foreground ,4coder-red))))
   `(info-variable-ref-item ((t (:background ,4coder-bg-1 :foreground ,4coder-orange))))
;;;;; irfc
   `(irfc-head-name-face ((t (:foreground ,4coder-red :weight bold))))
   `(irfc-head-number-face ((t (:foreground ,4coder-red :weight bold))))
   `(irfc-reference-face ((t (:foreground ,4coder-blue-1 :weight bold))))
   `(irfc-requirement-keyword-face ((t (:inherit font-lock-keyword-face))))
   `(irfc-rfc-link-face ((t (:inherit link))))
   `(irfc-rfc-number-face ((t (:foreground ,4coder-cyan :weight bold))))
   `(irfc-std-number-face ((t (:foreground ,4coder-green+4 :weight bold))))
   `(irfc-table-item-face ((t (:foreground ,4coder-green+3))))
   `(irfc-title-face ((t (:foreground ,4coder-yellow
                                      :underline t :weight bold))))
;;;;; ivy
   `(ivy-confirm-face ((t (:foreground "#000000" :background "blue"))))
   `(ivy-current-match ((t (:foreground ,4coder-yellow :weight bold :background "blue"))))
   `(ivy-cursor ((t (:foreground ,4coder-bg+05 :background "blue"))))
   `(ivy-match-required-face ((t (:foreground ,4coder-blue-2 :background ,4coder-bg))))
   `(ivy-minibuffer-match-face-1 ((t (:background ,4coder-blue+1 :foreground ,4coder-bg))))
   `(ivy-minibuffer-match-face-2 ((t (:background ,4coder-red :foreground ,4coder-bg))))
   `(ivy-minibuffer-match-face-3 ((t (:background ,4coder-blue :foreground ,4coder-bg))))
   `(ivy-minibuffer-match-face-4 ((t (:background ,4coder-yellow :foreground ,4coder-bg))))
   `(ivy-remote ((t (:foreground ,4coder-blue :background ,4coder-bg))))
   `(ivy-subdir ((t (:foreground ,4coder-yellow :background ,4coder-bg))))
;;;;; ido-mode
   `(ido-first-match ((t (:foreground ,4coder-yellow :weight bold))))
   `(ido-only-match ((t (:foreground ,4coder-orange :weight bold))))
   `(ido-subdir ((t (:foreground ,4coder-yellow))))
   `(ido-indicator ((t (:foreground ,4coder-yellow :background ,4coder-red-4))))
;;;;; iedit-mode
   `(iedit-occurrence ((t (:background ,4coder-bg+2 :weight bold))))
;;;;; jabber-mode
   `(jabber-roster-user-away ((t (:foreground ,4coder-green+2))))
   `(jabber-roster-user-online ((t (:foreground ,4coder-blue-1))))
   `(jabber-roster-user-dnd ((t (:foreground ,4coder-red+1))))
   `(jabber-roster-user-xa ((t (:foreground ,4coder-magenta))))
   `(jabber-roster-user-chatty ((t (:foreground ,4coder-orange))))
   `(jabber-roster-user-error ((t (:foreground ,4coder-red+1))))
   `(jabber-rare-time-face ((t (:foreground ,4coder-green+1))))
   `(jabber-chat-prompt-local ((t (:foreground ,4coder-blue-1))))
   `(jabber-chat-prompt-foreign ((t (:foreground ,4coder-red+1))))
   `(jabber-chat-prompt-system ((t (:foreground ,4coder-green+3))))
   `(jabber-activity-face((t (:foreground ,4coder-red+1))))
   `(jabber-activity-personal-face ((t (:foreground ,4coder-blue+1))))
   `(jabber-title-small ((t (:height 1.1 :weight bold))))
   `(jabber-title-medium ((t (:height 1.2 :weight bold))))
   `(jabber-title-large ((t (:height 1.3 :weight bold))))
;;;;; js2-mode
   `(js2-warning ((t (:underline ,4coder-orange))))
   `(js2-error ((t (:foreground ,4coder-red :weight bold))))
   `(js2-jsdoc-tag ((t (:foreground ,4coder-green-2))))
   `(js2-jsdoc-type ((t (:foreground ,4coder-green+2))))
   `(js2-jsdoc-value ((t (:foreground ,4coder-green+3))))
   `(js2-function-param ((t (:foreground, 4coder-orange))))
   `(js2-external-variable ((t (:foreground ,4coder-orange))))
;;;;; additional js2 mode attributes for better syntax highlighting
   `(js2-instance-member ((t (:foreground ,4coder-green-2))))
   `(js2-jsdoc-html-tag-delimiter ((t (:foreground ,4coder-orange))))
   `(js2-jsdoc-html-tag-name ((t (:foreground ,4coder-red-1))))
   `(js2-object-property ((t (:foreground ,4coder-blue+1))))
   `(js2-magic-paren ((t (:foreground ,4coder-blue-5))))
   `(js2-private-function-call ((t (:foreground ,4coder-cyan))))
   `(js2-function-call ((t (:foreground ,4coder-cyan))))
   `(js2-private-member ((t (:foreground ,4coder-blue-1))))
   `(js2-keywords ((t (:foreground ,4coder-magenta))))
;;;;; ledger-mode
   `(ledger-font-payee-uncleared-face ((t (:foreground ,4coder-red-1 :weight bold))))
   `(ledger-font-payee-cleared-face ((t (:foreground ,4coder-fg :weight normal))))
   `(ledger-font-payee-pending-face ((t (:foreground ,4coder-red :weight normal))))
   `(ledger-font-xact-highlight-face ((t (:background ,4coder-bg+1))))
   `(ledger-font-auto-xact-face ((t (:foreground ,4coder-yellow-1 :weight normal))))
   `(ledger-font-periodic-xact-face ((t (:foreground ,4coder-green :weight normal))))
   `(ledger-font-pending-face ((t (:foreground ,4coder-orange weight: normal))))
   `(ledger-font-other-face ((t (:foreground ,4coder-fg))))
   `(ledger-font-posting-date-face ((t (:foreground ,4coder-orange :weight normal))))
   `(ledger-font-posting-account-face ((t (:foreground ,4coder-blue-1))))
   `(ledger-font-posting-account-cleared-face ((t (:foreground ,4coder-fg))))
   `(ledger-font-posting-account-pending-face ((t (:foreground ,4coder-orange))))
   `(ledger-font-posting-amount-face ((t (:foreground ,4coder-orange))))
   `(ledger-occur-narrowed-face ((t (:foreground ,4coder-fg-1 :invisible t))))
   `(ledger-occur-xact-face ((t (:background ,4coder-bg+1))))
   `(ledger-font-comment-face ((t (:foreground ,4coder-green))))
   `(ledger-font-reconciler-uncleared-face ((t (:foreground ,4coder-red-1 :weight bold))))
   `(ledger-font-reconciler-cleared-face ((t (:foreground ,4coder-fg :weight normal))))
   `(ledger-font-reconciler-pending-face ((t (:foreground ,4coder-orange :weight normal))))
   `(ledger-font-report-clickable-face ((t (:foreground ,4coder-orange :weight normal))))
;;;;; linum-mode
   `(linum ((t (:foreground ,4coder-green+2 :background ,4coder-bg))))
;;;;; lispy
   `(lispy-command-name-face ((t (:background ,4coder-bg-05 :inherit font-lock-function-name-face))))
   `(lispy-cursor-face ((t (:foreground ,4coder-bg :background ,4coder-fg))))
   `(lispy-face-hint ((t (:inherit highlight :foreground ,4coder-yellow))))
;;;;; ruler-mode
   `(ruler-mode-column-number ((t (:inherit 'ruler-mode-default :foreground ,4coder-fg))))
   `(ruler-mode-fill-column ((t (:inherit 'ruler-mode-default :foreground ,4coder-yellow))))
   `(ruler-mode-goal-column ((t (:inherit 'ruler-mode-fill-column))))
   `(ruler-mode-comment-column ((t (:inherit 'ruler-mode-fill-column))))
   `(ruler-mode-tab-stop ((t (:inherit 'ruler-mode-fill-column))))
   `(ruler-mode-current-column ((t (:foreground ,4coder-yellow :box t))))
   `(ruler-mode-default ((t (:foreground ,4coder-green+2 :background ,4coder-bg))))

;;;;; lui
   `(lui-time-stamp-face ((t (:foreground ,4coder-blue-1))))
   `(lui-hilight-face ((t (:foreground ,4coder-green+2 :background ,4coder-bg))))
   `(lui-button-face ((t (:inherit hover-highlight))))
;;;;; macrostep
   `(macrostep-gensym-1
     ((t (:foreground ,4coder-green+2 :background ,4coder-bg-1))))
   `(macrostep-gensym-2
     ((t (:foreground ,4coder-red+1 :background ,4coder-bg-1))))
   `(macrostep-gensym-3
     ((t (:foreground ,4coder-blue+1 :background ,4coder-bg-1))))
   `(macrostep-gensym-4
     ((t (:foreground ,4coder-magenta :background ,4coder-bg-1))))
   `(macrostep-gensym-5
     ((t (:foreground ,4coder-yellow :background ,4coder-bg-1))))
   `(macrostep-expansion-highlight-face
     ((t (:inherit highlight))))
   `(macrostep-macro-face
     ((t (:underline t))))
;;;;; magit
;;;;;; headings and diffs
   ;; Please read (info "(magit)Theming Faces") before changing this.
   `(magit-section-highlight           ((t (:background ,4coder-bg+05))))
   `(magit-section-heading             ((t (:foreground ,4coder-yellow :weight bold))))
   `(magit-section-heading-selection   ((t (:foreground ,4coder-orange :weight bold))))
   `(magit-diff-file-heading           ((t (:weight bold))))
   `(magit-diff-file-heading-highlight ((t (:background ,4coder-bg+05 :weight bold))))
   `(magit-diff-file-heading-selection ((t (:background ,4coder-bg+05 :weight bold
                                                        :foreground ,4coder-orange))))
   `(magit-diff-added                  ((t (:background ,4coder-green-2))))
   `(magit-diff-added-highlight        ((t (:background ,4coder-green))))
   `(magit-diff-removed                ((t (:background ,4coder-red-4))))
   `(magit-diff-removed-highlight      ((t (:background ,4coder-red-3))))
   `(magit-diff-hunk-heading           ((t (:background ,4coder-bg+1))))
   `(magit-diff-hunk-heading-highlight ((t (:background ,4coder-bg+2))))
   `(magit-diff-hunk-heading-selection ((t (:background ,4coder-bg+2
                                                        :foreground ,4coder-orange))))
   `(magit-diff-lines-heading          ((t (:background ,4coder-orange
                                                        :foreground ,4coder-bg+2))))
   `(magit-diff-context-highlight      ((t (:background ,4coder-bg+05
                                                        :foreground "grey70"))))
   `(magit-diffstat-added              ((t (:foreground ,4coder-green+4))))
   `(magit-diffstat-removed            ((t (:foreground ,4coder-red))))
;;;;;; popup
   `(magit-popup-heading             ((t (:foreground ,4coder-yellow  :weight bold))))
   `(magit-popup-key                 ((t (:foreground ,4coder-green-2 :weight bold))))
   `(magit-popup-argument            ((t (:foreground ,4coder-green   :weight bold))))
   `(magit-popup-disabled-argument   ((t (:foreground ,4coder-fg-1    :weight normal))))
   `(magit-popup-option-value        ((t (:foreground ,4coder-blue-2  :weight bold))))
;;;;;; process
   `(magit-process-ok    ((t (:foreground ,4coder-green  :weight bold))))
   `(magit-process-ng    ((t (:foreground ,4coder-red    :weight bold))))
;;;;;; log
   `(magit-log-author    ((t (:foreground ,4coder-orange))))
   `(magit-log-date      ((t (:foreground ,4coder-fg-1))))
   `(magit-log-graph     ((t (:foreground ,4coder-fg+1))))
;;;;;; sequence
   `(magit-sequence-pick ((t (:foreground ,4coder-yellow-2))))
   `(magit-sequence-stop ((t (:foreground ,4coder-green))))
   `(magit-sequence-part ((t (:foreground ,4coder-yellow))))
   `(magit-sequence-head ((t (:foreground ,4coder-blue))))
   `(magit-sequence-drop ((t (:foreground ,4coder-red))))
   `(magit-sequence-done ((t (:foreground ,4coder-fg-1))))
   `(magit-sequence-onto ((t (:foreground ,4coder-fg-1))))
;;;;;; bisect
   `(magit-bisect-good ((t (:foreground ,4coder-green))))
   `(magit-bisect-skip ((t (:foreground ,4coder-yellow))))
   `(magit-bisect-bad  ((t (:foreground ,4coder-red))))
;;;;;; blame
   `(magit-blame-heading ((t (:background ,4coder-bg-1 :foreground ,4coder-blue-2))))
   `(magit-blame-hash    ((t (:background ,4coder-bg-1 :foreground ,4coder-blue-2))))
   `(magit-blame-name    ((t (:background ,4coder-bg-1 :foreground ,4coder-orange))))
   `(magit-blame-date    ((t (:background ,4coder-bg-1 :foreground ,4coder-orange))))
   `(magit-blame-summary ((t (:background ,4coder-bg-1 :foreground ,4coder-blue-2
                                          :weight bold))))
;;;;;; references etc
   `(magit-dimmed         ((t (:foreground ,4coder-bg+3))))
   `(magit-hash           ((t (:foreground ,4coder-bg+3))))
   `(magit-tag            ((t (:foreground ,4coder-orange :weight bold))))
   `(magit-branch-remote  ((t (:foreground ,4coder-green  :weight bold))))
   `(magit-branch-local   ((t (:foreground ,4coder-blue   :weight bold))))
   `(magit-branch-current ((t (:foreground ,4coder-blue   :weight bold :box t))))
   `(magit-head           ((t (:foreground ,4coder-blue   :weight bold))))
   `(magit-refname        ((t (:background ,4coder-bg+2 :foreground ,4coder-fg :weight bold))))
   `(magit-refname-stash  ((t (:background ,4coder-bg+2 :foreground ,4coder-fg :weight bold))))
   `(magit-refname-wip    ((t (:background ,4coder-bg+2 :foreground ,4coder-fg :weight bold))))
   `(magit-signature-good      ((t (:foreground ,4coder-green))))
   `(magit-signature-bad       ((t (:foreground ,4coder-red))))
   `(magit-signature-untrusted ((t (:foreground ,4coder-yellow))))
   `(magit-signature-expired   ((t (:foreground ,4coder-orange))))
   `(magit-signature-revoked   ((t (:foreground ,4coder-magenta))))
   '(magit-signature-error     ((t (:inherit    magit-signature-bad))))
   `(magit-cherry-unmatched    ((t (:foreground ,4coder-cyan))))
   `(magit-cherry-equivalent   ((t (:foreground ,4coder-magenta))))
   `(magit-reflog-commit       ((t (:foreground ,4coder-green))))
   `(magit-reflog-amend        ((t (:foreground ,4coder-magenta))))
   `(magit-reflog-merge        ((t (:foreground ,4coder-green))))
   `(magit-reflog-checkout     ((t (:foreground ,4coder-blue))))
   `(magit-reflog-reset        ((t (:foreground ,4coder-red))))
   `(magit-reflog-rebase       ((t (:foreground ,4coder-magenta))))
   `(magit-reflog-cherry-pick  ((t (:foreground ,4coder-green))))
   `(magit-reflog-remote       ((t (:foreground ,4coder-cyan))))
   `(magit-reflog-other        ((t (:foreground ,4coder-cyan))))
;;;;; markup-faces
   `(markup-anchor-face ((t (:foreground ,4coder-blue+1))))
   `(markup-code-face ((t (:inherit font-lock-constant-face))))
   `(markup-command-face ((t (:foreground ,4coder-yellow))))
   `(markup-emphasis-face ((t (:inherit bold))))
   `(markup-internal-reference-face ((t (:foreground ,4coder-yellow-2 :underline t))))
   `(markup-list-face ((t (:foreground ,4coder-fg+1))))
   `(markup-meta-face ((t (:foreground ,4coder-yellow))))
   `(markup-meta-hide-face ((t (:foreground ,4coder-yellow))))
   `(markup-secondary-text-face ((t (:foreground ,4coder-yellow-1))))
   `(markup-title-0-face ((t (:inherit font-lock-function-name-face :weight bold))))
   `(markup-title-1-face ((t (:inherit font-lock-function-name-face :weight bold))))
   `(markup-title-2-face ((t (:inherit font-lock-function-name-face :weight bold))))
   `(markup-title-3-face ((t (:inherit font-lock-function-name-face :weight bold))))
   `(markup-title-4-face ((t (:inherit font-lock-function-name-face :weight bold))))
   `(markup-typewriter-face ((t (:inherit font-lock-constant-face))))
   `(markup-verbatim-face ((t (:inherit font-lock-constant-face))))
   `(markup-value-face ((t (:foreground ,4coder-yellow))))
;;;;; message-mode
   `(message-cited-text ((t (:inherit font-lock-comment-face))))
   `(message-header-name ((t (:foreground ,4coder-green+1))))
   `(message-header-other ((t (:foreground ,4coder-green))))
   `(message-header-to ((t (:foreground ,4coder-yellow :weight bold))))
   `(message-header-cc ((t (:foreground ,4coder-yellow :weight bold))))
   `(message-header-newsgroups ((t (:foreground ,4coder-yellow :weight bold))))
   `(message-header-subject ((t (:foreground ,4coder-orange :weight bold))))
   `(message-header-xheader ((t (:foreground ,4coder-green))))
   `(message-mml ((t (:foreground ,4coder-yellow :weight bold))))
   `(message-separator ((t (:inherit font-lock-comment-face))))
;;;;; mew
   `(mew-face-header-subject ((t (:foreground ,4coder-orange))))
   `(mew-face-header-from ((t (:foreground ,4coder-yellow))))
   `(mew-face-header-date ((t (:foreground ,4coder-green))))
   `(mew-face-header-to ((t (:foreground ,4coder-red))))
   `(mew-face-header-key ((t (:foreground ,4coder-green))))
   `(mew-face-header-private ((t (:foreground ,4coder-green))))
   `(mew-face-header-important ((t (:foreground ,4coder-blue))))
   `(mew-face-header-marginal ((t (:foreground ,4coder-fg :weight bold))))
   `(mew-face-header-warning ((t (:foreground ,4coder-red))))
   `(mew-face-header-xmew ((t (:foreground ,4coder-green))))
   `(mew-face-header-xmew-bad ((t (:foreground ,4coder-red))))
   `(mew-face-body-url ((t (:foreground ,4coder-orange))))
   `(mew-face-body-comment ((t (:foreground ,4coder-fg :slant italic))))
   `(mew-face-body-cite1 ((t (:foreground ,4coder-green))))
   `(mew-face-body-cite2 ((t (:foreground ,4coder-blue))))
   `(mew-face-body-cite3 ((t (:foreground ,4coder-orange))))
   `(mew-face-body-cite4 ((t (:foreground ,4coder-yellow))))
   `(mew-face-body-cite5 ((t (:foreground ,4coder-red))))
   `(mew-face-mark-review ((t (:foreground ,4coder-blue))))
   `(mew-face-mark-escape ((t (:foreground ,4coder-green))))
   `(mew-face-mark-delete ((t (:foreground ,4coder-red))))
   `(mew-face-mark-unlink ((t (:foreground ,4coder-yellow))))
   `(mew-face-mark-refile ((t (:foreground ,4coder-green))))
   `(mew-face-mark-unread ((t (:foreground ,4coder-red-2))))
   `(mew-face-eof-message ((t (:foreground ,4coder-green))))
   `(mew-face-eof-part ((t (:foreground ,4coder-yellow))))
;;;;; mic-paren
   `(paren-face-match ((t (:foreground ,4coder-cyan :background ,4coder-bg :weight bold))))
   `(paren-face-mismatch ((t (:foreground ,4coder-bg :background ,4coder-magenta :weight bold))))
   `(paren-face-no-match ((t (:foreground ,4coder-bg :background ,4coder-red :weight bold))))
;;;;; mingus
   `(mingus-directory-face ((t (:foreground ,4coder-blue))))
   `(mingus-pausing-face ((t (:foreground ,4coder-magenta))))
   `(mingus-playing-face ((t (:foreground ,4coder-cyan))))
   `(mingus-playlist-face ((t (:foreground ,4coder-cyan ))))
   `(mingus-mark-face ((t (:bold t :foreground ,4coder-magenta))))
   `(mingus-song-file-face ((t (:foreground ,4coder-yellow))))
   `(mingus-artist-face ((t (:foreground ,4coder-cyan))))
   `(mingus-album-face ((t (:underline t :foreground ,4coder-red+1))))
   `(mingus-album-stale-face ((t (:foreground ,4coder-red+1))))
   `(mingus-stopped-face ((t (:foreground ,4coder-red))))
;;;;; nav
   `(nav-face-heading ((t (:foreground ,4coder-yellow))))
   `(nav-face-button-num ((t (:foreground ,4coder-cyan))))
   `(nav-face-dir ((t (:foreground ,4coder-green))))
   `(nav-face-hdir ((t (:foreground ,4coder-red))))
   `(nav-face-file ((t (:foreground ,4coder-fg))))
   `(nav-face-hfile ((t (:foreground ,4coder-red-4))))
;;;;; merlin
   `(merlin-type-face ((t (:inherit highlight))))
   `(merlin-compilation-warning-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,4coder-orange)))
      (t
       (:underline ,4coder-orange))))
   `(merlin-compilation-error-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,4coder-red)))
      (t
       (:underline ,4coder-red))))
;;;;; mu4e
   `(mu4e-cited-1-face ((t (:foreground ,4coder-blue    :slant italic))))
   `(mu4e-cited-2-face ((t (:foreground ,4coder-green+2 :slant italic))))
   `(mu4e-cited-3-face ((t (:foreground ,4coder-blue-2  :slant italic))))
   `(mu4e-cited-4-face ((t (:foreground ,4coder-green   :slant italic))))
   `(mu4e-cited-5-face ((t (:foreground ,4coder-blue-4  :slant italic))))
   `(mu4e-cited-6-face ((t (:foreground ,4coder-green-2 :slant italic))))
   `(mu4e-cited-7-face ((t (:foreground ,4coder-blue    :slant italic))))
   `(mu4e-replied-face ((t (:foreground ,4coder-bg+3))))
   `(mu4e-trashed-face ((t (:foreground ,4coder-bg+3 :strike-through t))))
;;;;; mumamo
   `(mumamo-background-chunk-major ((t (:background nil))))
   `(mumamo-background-chunk-submode1 ((t (:background ,4coder-bg-1))))
   `(mumamo-background-chunk-submode2 ((t (:background ,4coder-bg+2))))
   `(mumamo-background-chunk-submode3 ((t (:background ,4coder-bg+3))))
   `(mumamo-background-chunk-submode4 ((t (:background ,4coder-bg+1))))
;;;;; neotree
   `(neo-banner-face ((t (:foreground ,4coder-blue+1 :weight bold))))
   `(neo-header-face ((t (:foreground ,4coder-fg))))
   `(neo-root-dir-face ((t (:foreground ,4coder-blue+1 :weight bold))))
   `(neo-dir-link-face ((t (:foreground ,4coder-blue))))
   `(neo-file-link-face ((t (:foreground ,4coder-fg))))
   `(neo-expand-btn-face ((t (:foreground ,4coder-blue))))
   `(neo-vc-default-face ((t (:foreground ,4coder-fg+1))))
   `(neo-vc-user-face ((t (:foreground ,4coder-red :slant italic))))
   `(neo-vc-up-to-date-face ((t (:foreground ,4coder-fg))))
   `(neo-vc-edited-face ((t (:foreground ,4coder-magenta))))
   `(neo-vc-needs-merge-face ((t (:foreground ,4coder-red+1))))
   `(neo-vc-unlocked-changes-face ((t (:foreground ,4coder-red :background ,4coder-blue-5))))
   `(neo-vc-added-face ((t (:foreground ,4coder-green+1))))
   `(neo-vc-conflict-face ((t (:foreground ,4coder-red+1))))
   `(neo-vc-missing-face ((t (:foreground ,4coder-red+1))))
   `(neo-vc-ignored-face ((t (:foreground ,4coder-fg-1))))
;;;;; org-mode
   `(org-agenda-date-today
     ((t (:foreground ,4coder-fg+1 :slant italic :weight bold))) t)
   `(org-agenda-structure
     ((t (:inherit font-lock-comment-face))))
   `(org-archived ((t (:foreground ,4coder-fg :weight bold))))
   `(org-checkbox ((t (:background ,4coder-bg+2 :foreground ,4coder-fg+1
                                   :box (:line-width 1 :style released-button)))))
   `(org-date ((t (:foreground ,4coder-blue :underline t))))
   `(org-deadline-announce ((t (:foreground ,4coder-red-1))))
   `(org-done ((t (:weight bold :weight bold :foreground ,4coder-green+3))))
   `(org-formula ((t (:foreground ,4coder-yellow-2))))
   `(org-headline-done ((t (:foreground ,4coder-green+3))))
   `(org-hide ((t (:foreground ,4coder-bg))))
   `(org-level-1 ((t (:inherit ,z-variable-pitch :foreground ,4coder-orange
                               ,@(when 4coder-scale-org-headlines
                                   (list :height 4coder-height-plus-4))))))
   `(org-level-2 ((t (:inherit ,z-variable-pitch :foreground ,4coder-green+4
                               ,@(when 4coder-scale-org-headlines
                                   (list :height 4coder-height-plus-3))))))
   `(org-level-3 ((t (:inherit ,z-variable-pitch :foreground ,4coder-blue-1
                               ,@(when 4coder-scale-org-headlines
                                   (list :height 4coder-height-plus-2))))))
   `(org-level-4 ((t (:inherit ,z-variable-pitch :foreground ,4coder-yellow-2
                               ,@(when 4coder-scale-org-headlines
                                   (list :height 4coder-height-plus-1))))))
   `(org-level-5 ((t (:inherit ,z-variable-pitch :foreground ,4coder-cyan))))
   `(org-level-6 ((t (:inherit ,z-variable-pitch :foreground ,4coder-green+2))))
   `(org-level-7 ((t (:inherit ,z-variable-pitch :foreground ,4coder-red-4))))
   `(org-level-8 ((t (:inherit ,z-variable-pitch :foreground ,4coder-blue-4))))
   `(org-link ((t (:foreground ,4coder-yellow-2 :underline t))))
   `(org-scheduled ((t (:foreground ,4coder-green+4))))
   `(org-scheduled-previously ((t (:foreground ,4coder-red))))
   `(org-scheduled-today ((t (:foreground ,4coder-blue+1))))
   `(org-sexp-date ((t (:foreground ,4coder-blue+1 :underline t))))
   `(org-special-keyword ((t (:inherit font-lock-comment-face))))
   `(org-table ((t (:foreground ,4coder-green+2))))
   `(org-tag ((t (:weight bold :weight bold))))
   `(org-time-grid ((t (:foreground ,4coder-orange))))
   `(org-todo ((t (:weight bold :foreground ,4coder-red :weight bold))))
   `(org-upcoming-deadline ((t (:inherit font-lock-keyword-face))))
   `(org-warning ((t (:weight bold :foreground ,4coder-red :weight bold :underline nil))))
   `(org-column ((t (:background ,4coder-bg-1))))
   `(org-column-title ((t (:background ,4coder-bg-1 :underline t :weight bold))))
   `(org-mode-line-clock ((t (:foreground ,4coder-fg :background ,4coder-bg-1))))
   `(org-mode-line-clock-overrun ((t (:foreground ,4coder-bg :background ,4coder-red-1))))
   `(org-ellipsis ((t (:foreground ,4coder-yellow-1 :underline t))))
   `(org-footnote ((t (:foreground ,4coder-cyan :underline t))))
   `(org-document-title ((t (:inherit ,z-variable-pitch :foreground ,4coder-blue
                                      :weight bold
                                      ,@(when 4coder-scale-org-headlines
                                          (list :height 4coder-height-plus-4))))))
   `(org-document-info ((t (:foreground ,4coder-blue))))
   `(org-habit-ready-face ((t :background ,4coder-green)))
   `(org-habit-alert-face ((t :background ,4coder-yellow-1 :foreground ,4coder-bg)))
   `(org-habit-clear-face ((t :background ,4coder-blue-3)))
   `(org-habit-overdue-face ((t :background ,4coder-red-3)))
   `(org-habit-clear-future-face ((t :background ,4coder-blue-4)))
   `(org-habit-ready-future-face ((t :background ,4coder-green-2)))
   `(org-habit-alert-future-face ((t :background ,4coder-yellow-2 :foreground ,4coder-bg)))
   `(org-habit-overdue-future-face ((t :background ,4coder-red-4)))
;;;;; org-ref
   `(org-ref-ref-face ((t :underline t)))
   `(org-ref-label-face ((t :underline t)))
   `(org-ref-cite-face ((t :underline t)))
   `(org-ref-glossary-face ((t :underline t)))
   `(org-ref-acronym-face ((t :underline t)))
;;;;; outline
   `(outline-1 ((t (:inherit ,z-variable-pitch :foreground ,4coder-orange
                             ,@(when 4coder-scale-outline-headlines
                                 (list :height 4coder-height-plus-4))))))
   `(outline-2 ((t (:inherit ,z-variable-pitch :foreground ,4coder-green+4
                             ,@(when 4coder-scale-outline-headlines
                                 (list :height 4coder-height-plus-3))))))
   `(outline-3 ((t (:inherit ,z-variable-pitch :foreground ,4coder-blue-1
                             ,@(when 4coder-scale-outline-headlines
                                 (list :height 4coder-height-plus-2))))))
   `(outline-4 ((t (:inherit ,z-variable-pitch :foreground ,4coder-yellow-2
                             ,@(when 4coder-scale-outline-headlines
                                 (list :height 4coder-height-plus-1))))))
   `(outline-5 ((t (:inherit ,z-variable-pitch :foreground ,4coder-cyan))))
   `(outline-6 ((t (:inherit ,z-variable-pitch :foreground ,4coder-green+2))))
   `(outline-7 ((t (:inherit ,z-variable-pitch :foreground ,4coder-red-4))))
   `(outline-8 ((t (:inherit ,z-variable-pitch :foreground ,4coder-blue-4))))
;;;;; p4
   `(p4-depot-added-face ((t :inherit diff-added)))
   `(p4-depot-branch-op-face ((t :inherit diff-changed)))
   `(p4-depot-deleted-face ((t :inherit diff-removed)))
   `(p4-depot-unmapped-face ((t :inherit diff-changed)))
   `(p4-diff-change-face ((t :inherit diff-changed)))
   `(p4-diff-del-face ((t :inherit diff-removed)))
   `(p4-diff-file-face ((t :inherit diff-file-header)))
   `(p4-diff-head-face ((t :inherit diff-header)))
   `(p4-diff-ins-face ((t :inherit diff-added)))
;;;;; c/perl
   `(cperl-nonoverridable-face ((t (:foreground ,4coder-magenta))))
   `(cperl-array-face ((t (:foreground ,4coder-yellow, :background ,4coder-bg))))
   `(cperl-hash-face ((t (:foreground ,4coder-yellow-1, :background ,4coder-bg))))
;;;;; paren-face
   `(parenthesis ((t (:foreground ,4coder-fg-1))))
;;;;; perspective
   `(persp-selected-face ((t (:foreground ,4coder-yellow-2 :inherit mode-line))))
;;;;; powerline
   `(powerline-active1 ((t (:background ,4coder-bg-05 :inherit mode-line))))
   `(powerline-active2 ((t (:background ,4coder-bg+2 :inherit mode-line))))
   `(powerline-inactive1 ((t (:background ,4coder-bg+1 :inherit mode-line-inactive))))
   `(powerline-inactive2 ((t (:background ,4coder-bg+3 :inherit mode-line-inactive))))
;;;;; proofgeneral
   `(proof-active-area-face ((t (:underline t))))
   `(proof-boring-face ((t (:foreground ,4coder-fg :background ,4coder-bg+2))))
   `(proof-command-mouse-highlight-face ((t (:inherit proof-mouse-highlight-face))))
   `(proof-debug-message-face ((t (:inherit proof-boring-face))))
   `(proof-declaration-name-face ((t (:inherit font-lock-keyword-face :foreground nil))))
   `(proof-eager-annotation-face ((t (:foreground ,4coder-bg :background ,4coder-orange))))
   `(proof-error-face ((t (:foreground ,4coder-fg :background ,4coder-red-4))))
   `(proof-highlight-dependency-face ((t (:foreground ,4coder-bg :background ,4coder-yellow-1))))
   `(proof-highlight-dependent-face ((t (:foreground ,4coder-bg :background ,4coder-orange))))
   `(proof-locked-face ((t (:background ,4coder-blue-5))))
   `(proof-mouse-highlight-face ((t (:foreground ,4coder-bg :background ,4coder-orange))))
   `(proof-queue-face ((t (:background ,4coder-red-4))))
   `(proof-region-mouse-highlight-face ((t (:inherit proof-mouse-highlight-face))))
   `(proof-script-highlight-error-face ((t (:background ,4coder-red-2))))
   `(proof-tacticals-name-face ((t (:inherit font-lock-constant-face :foreground nil :background ,4coder-bg))))
   `(proof-tactics-name-face ((t (:inherit font-lock-constant-face :foreground nil :background ,4coder-bg))))
   `(proof-warning-face ((t (:foreground ,4coder-bg :background ,4coder-yellow-1))))
;;;;; racket-mode
   `(racket-keyword-argument-face ((t (:inherit font-lock-constant-face))))
   `(racket-selfeval-face ((t (:inherit font-lock-type-face))))
;;;;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,4coder-fg))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,4coder-green+4))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,4coder-yellow-2))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,4coder-cyan))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,4coder-green+2))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,4coder-blue+1))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,4coder-yellow-1))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,4coder-green+1))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,4coder-blue-2))))
   `(rainbow-delimiters-depth-10-face ((t (:foreground ,4coder-orange))))
   `(rainbow-delimiters-depth-11-face ((t (:foreground ,4coder-green))))
   `(rainbow-delimiters-depth-12-face ((t (:foreground ,4coder-blue-5))))
;;;;; rcirc
   `(rcirc-my-nick ((t (:foreground ,4coder-blue))))
   `(rcirc-other-nick ((t (:foreground ,4coder-orange))))
   `(rcirc-bright-nick ((t (:foreground ,4coder-blue+1))))
   `(rcirc-dim-nick ((t (:foreground ,4coder-blue-2))))
   `(rcirc-server ((t (:foreground ,4coder-green))))
   `(rcirc-server-prefix ((t (:foreground ,4coder-green+1))))
   `(rcirc-timestamp ((t (:foreground ,4coder-green+2))))
   `(rcirc-nick-in-message ((t (:foreground ,4coder-yellow))))
   `(rcirc-nick-in-message-full-line ((t (:weight bold))))
   `(rcirc-prompt ((t (:foreground ,4coder-yellow :weight bold))))
   `(rcirc-track-nick ((t (:inverse-video t))))
   `(rcirc-track-keyword ((t (:weight bold))))
   `(rcirc-url ((t (:weight bold))))
   `(rcirc-keyword ((t (:foreground ,4coder-yellow :weight bold))))
;;;;; re-builder
   `(reb-match-0 ((t (:foreground ,4coder-bg :background ,4coder-magenta))))
   `(reb-match-1 ((t (:foreground ,4coder-bg :background ,4coder-blue))))
   `(reb-match-2 ((t (:foreground ,4coder-bg :background ,4coder-orange))))
   `(reb-match-3 ((t (:foreground ,4coder-bg :background ,4coder-red))))
;;;;; realgud
   `(realgud-overlay-arrow1 ((t (:foreground ,4coder-green))))
   `(realgud-overlay-arrow2 ((t (:foreground ,4coder-yellow))))
   `(realgud-overlay-arrow3 ((t (:foreground ,4coder-orange))))
   `(realgud-bp-enabled-face ((t (:inherit error))))
   `(realgud-bp-disabled-face ((t (:inherit secondary-selection))))
   `(realgud-bp-line-enabled-face ((t (:box (:color ,4coder-red :style nil)))))
   `(realgud-bp-line-disabled-face ((t (:box (:color "grey70" :style nil)))))
   `(realgud-line-number ((t (:foreground ,4coder-yellow))))
   `(realgud-backtrace-number ((t (:foreground ,4coder-yellow, :weight bold))))
;;;;; regex-tool
   `(regex-tool-matched-face ((t (:background ,4coder-blue-4 :weight bold))))
;;;;; rpm-mode
   `(rpm-spec-dir-face ((t (:foreground ,4coder-green))))
   `(rpm-spec-doc-face ((t (:foreground ,4coder-green))))
   `(rpm-spec-ghost-face ((t (:foreground ,4coder-red))))
   `(rpm-spec-macro-face ((t (:foreground ,4coder-yellow))))
   `(rpm-spec-obsolete-tag-face ((t (:foreground ,4coder-red))))
   `(rpm-spec-package-face ((t (:foreground ,4coder-red))))
   `(rpm-spec-section-face ((t (:foreground ,4coder-yellow))))
   `(rpm-spec-tag-face ((t (:foreground ,4coder-blue))))
   `(rpm-spec-var-face ((t (:foreground ,4coder-red))))
;;;;; rst-mode
   `(rst-level-1-face ((t (:foreground ,4coder-orange))))
   `(rst-level-2-face ((t (:foreground ,4coder-green+1))))
   `(rst-level-3-face ((t (:foreground ,4coder-blue-1))))
   `(rst-level-4-face ((t (:foreground ,4coder-yellow-2))))
   `(rst-level-5-face ((t (:foreground ,4coder-cyan))))
   `(rst-level-6-face ((t (:foreground ,4coder-green-2))))
;;;;; sh-mode
   `(sh-heredoc     ((t (:foreground ,4coder-yellow :weight bold))))
   `(sh-quoted-exec ((t (:foreground ,4coder-red))))
;;;;; show-paren
   `(show-paren-mismatch ((t (:foreground ,4coder-red+1 :background ,4coder-bg+3 :weight bold))))
   `(show-paren-match ((t (:foreground ,4coder-fg :background ,4coder-bg+3 :weight bold))))
;;;;; smart-mode-line
   ;; use (setq sml/theme nil) to enable 4coder for sml
   `(sml/global ((,class (:foreground ,4coder-fg :weight bold))))
   `(sml/modes ((,class (:foreground ,4coder-yellow :weight bold))))
   `(sml/minor-modes ((,class (:foreground ,4coder-fg-1 :weight bold))))
   `(sml/filename ((,class (:foreground ,4coder-yellow :weight bold))))
   `(sml/line-number ((,class (:foreground ,4coder-blue :weight bold))))
   `(sml/col-number ((,class (:foreground ,4coder-blue+1 :weight bold))))
   `(sml/position-percentage ((,class (:foreground ,4coder-blue-1 :weight bold))))
   `(sml/prefix ((,class (:foreground ,4coder-orange))))
   `(sml/git ((,class (:foreground ,4coder-green+3))))
   `(sml/process ((,class (:weight bold))))
   `(sml/sudo ((,class  (:foreground ,4coder-orange :weight bold))))
   `(sml/read-only ((,class (:foreground ,4coder-red-2))))
   `(sml/outside-modified ((,class (:foreground ,4coder-orange))))
   `(sml/modified ((,class (:foreground ,4coder-red))))
   `(sml/vc-edited ((,class (:foreground ,4coder-green+2))))
   `(sml/charging ((,class (:foreground ,4coder-green+4))))
   `(sml/discharging ((,class (:foreground ,4coder-red+1))))
;;;;; smartparens
   `(sp-show-pair-mismatch-face ((t (:foreground ,4coder-red+1 :background ,4coder-bg+3 :weight bold))))
   `(sp-show-pair-match-face ((t (:background ,4coder-bg+3 :weight bold))))
;;;;; sml-mode-line
   '(sml-modeline-end-face ((t :inherit default :width condensed)))
;;;;; SLIME
   `(slime-repl-output-face ((t (:foreground ,4coder-red))))
   `(slime-repl-inputed-output-face ((t (:foreground ,4coder-green))))
   `(slime-error-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,4coder-red)))
      (t
       (:underline ,4coder-red))))
   `(slime-warning-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,4coder-orange)))
      (t
       (:underline ,4coder-orange))))
   `(slime-style-warning-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,4coder-yellow)))
      (t
       (:underline ,4coder-yellow))))
   `(slime-note-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,4coder-green)))
      (t
       (:underline ,4coder-green))))
   `(slime-highlight-face ((t (:inherit highlight))))
;;;;; solaire
   `(solaire-default-face ((t (:inherit default :background ,4coder-bg-08))))
   `(solaire-minibuffer-face ((t (:inherit default :background ,4coder-bg-08))))
   `(solaire-hl-line-face ((t (:inherit hl-line :background ,4coder-bg))))
   `(solaire-org-hide-face ((t (:inherit org-hide :background ,4coder-bg-08))))
;;;;; speedbar
   `(speedbar-button-face ((t (:foreground ,4coder-green+2))))
   `(speedbar-directory-face ((t (:foreground ,4coder-cyan))))
   `(speedbar-file-face ((t (:foreground ,4coder-fg))))
   `(speedbar-highlight-face ((t (:foreground ,4coder-bg :background ,4coder-green+2))))
   `(speedbar-selected-face ((t (:foreground ,4coder-red))))
   `(speedbar-separator-face ((t (:foreground ,4coder-bg :background ,4coder-blue-1))))
   `(speedbar-tag-face ((t (:foreground ,4coder-yellow))))
;;;;; swiper
   `(swiper-line-face ((t (:background "blue"))))
;;;;; sx
   `(sx-custom-button
     ((t (:background ,4coder-fg :foreground ,4coder-bg-1
          :box (:line-width 3 :style released-button) :height 0.9))))
   `(sx-question-list-answers
     ((t (:foreground ,4coder-green+3
          :height 1.0 :inherit sx-question-list-parent))))
   `(sx-question-mode-accepted
     ((t (:foreground ,4coder-green+3
          :height 1.3 :inherit sx-question-mode-title))))
   '(sx-question-mode-content-face ((t (:inherit highlight))))
   `(sx-question-mode-kbd-tag
     ((t (:box (:color ,4coder-bg-1 :line-width 3 :style released-button)
          :height 0.9 :weight semi-bold))))
;;;;; tabbar
   `(tabbar-button ((t (:foreground ,4coder-fg
                                    :background ,4coder-bg))))
   `(tabbar-selected ((t (:foreground ,4coder-fg
                                      :background ,4coder-bg
                                      :box (:line-width -1 :style pressed-button)))))
   `(tabbar-unselected ((t (:foreground ,4coder-fg
                                        :background ,4coder-bg+1
                                        :box (:line-width -1 :style released-button)))))
;;;;; term
   `(term-color-black ((t (:foreground ,4coder-bg
                                       :background ,4coder-bg-1))))
   `(term-color-red ((t (:foreground ,4coder-red-2
                                     :background ,4coder-red-4))))
   `(term-color-green ((t (:foreground ,4coder-green
                                       :background ,4coder-green+2))))
   `(term-color-yellow ((t (:foreground ,4coder-orange
                                        :background ,4coder-yellow))))
   `(term-color-blue ((t (:foreground ,4coder-blue-1
                                      :background ,4coder-blue-4))))
   `(term-color-magenta ((t (:foreground ,4coder-magenta
                                         :background ,4coder-red))))
   `(term-color-cyan ((t (:foreground ,4coder-cyan
                                      :background ,4coder-blue))))
   `(term-color-white ((t (:foreground ,4coder-fg
                                       :background ,4coder-fg-1))))
   '(term-default-fg-color ((t (:inherit term-color-white))))
   '(term-default-bg-color ((t (:inherit term-color-black))))
;;;;; undo-tree
   `(undo-tree-visualizer-active-branch-face ((t (:foreground ,4coder-fg+1 :weight bold))))
   `(undo-tree-visualizer-current-face ((t (:foreground ,4coder-red-1 :weight bold))))
   `(undo-tree-visualizer-default-face ((t (:foreground ,4coder-fg))))
   `(undo-tree-visualizer-register-face ((t (:foreground ,4coder-yellow))))
   `(undo-tree-visualizer-unmodified-face ((t (:foreground ,4coder-cyan))))
;;;;; visual-regexp
   `(vr/group-0 ((t (:foreground ,4coder-bg :background ,4coder-green :weight bold))))
   `(vr/group-1 ((t (:foreground ,4coder-bg :background ,4coder-orange :weight bold))))
   `(vr/group-2 ((t (:foreground ,4coder-bg :background ,4coder-blue :weight bold))))
   `(vr/match-0 ((t (:inherit isearch))))
   `(vr/match-1 ((t (:foreground ,4coder-yellow-2 :background ,4coder-bg-1 :weight bold))))
   `(vr/match-separator-face ((t (:foreground ,4coder-red :weight bold))))
;;;;; volatile-highlights
   `(vhl/default-face ((t (:background ,4coder-bg-05))))
;;;;; web-mode
   `(web-mode-builtin-face ((t (:inherit ,font-lock-builtin-face))))
   `(web-mode-comment-face ((t (:inherit ,font-lock-comment-face))))
   `(web-mode-constant-face ((t (:inherit ,font-lock-constant-face))))
   `(web-mode-css-at-rule-face ((t (:foreground ,4coder-orange ))))
   `(web-mode-css-prop-face ((t (:foreground ,4coder-orange))))
   `(web-mode-css-pseudo-class-face ((t (:foreground ,4coder-green+3 :weight bold))))
   `(web-mode-css-rule-face ((t (:foreground ,4coder-blue))))
   `(web-mode-doctype-face ((t (:inherit ,font-lock-comment-face))))
   `(web-mode-folded-face ((t (:underline t))))
   `(web-mode-function-name-face ((t (:foreground ,4coder-blue))))
   `(web-mode-html-attr-name-face ((t (:foreground ,4coder-orange))))
   `(web-mode-html-attr-value-face ((t (:inherit ,font-lock-string-face))))
   `(web-mode-html-tag-face ((t (:foreground ,4coder-cyan))))
   `(web-mode-keyword-face ((t (:inherit ,font-lock-keyword-face))))
   `(web-mode-preprocessor-face ((t (:inherit ,font-lock-preprocessor-face))))
   `(web-mode-string-face ((t (:inherit ,font-lock-string-face))))
   `(web-mode-type-face ((t (:inherit ,font-lock-type-face))))
   `(web-mode-variable-name-face ((t (:inherit ,font-lock-variable-name-face))))
   `(web-mode-server-background-face ((t (:background ,4coder-bg))))
   `(web-mode-server-comment-face ((t (:inherit web-mode-comment-face))))
   `(web-mode-server-string-face ((t (:inherit web-mode-string-face))))
   `(web-mode-symbol-face ((t (:inherit font-lock-constant-face))))
   `(web-mode-warning-face ((t (:inherit font-lock-warning-face))))
   `(web-mode-whitespaces-face ((t (:background ,4coder-red))))
;;;;; whitespace-mode
   `(whitespace-space ((t (:background ,4coder-bg+1 :foreground ,4coder-bg+1))))
   `(whitespace-hspace ((t (:background ,4coder-bg+1 :foreground ,4coder-bg+1))))
   `(whitespace-tab ((t (:background ,4coder-red-1))))
   `(whitespace-newline ((t (:foreground ,4coder-bg+1))))
   `(whitespace-trailing ((t (:background ,4coder-red))))
   `(whitespace-line ((t (:background ,4coder-bg :foreground ,4coder-magenta))))
   `(whitespace-space-before-tab ((t (:background ,4coder-orange :foreground ,4coder-orange))))
   `(whitespace-indentation ((t (:background ,4coder-yellow :foreground ,4coder-red))))
   `(whitespace-empty ((t (:background ,4coder-yellow))))
   `(whitespace-space-after-tab ((t (:background ,4coder-yellow :foreground ,4coder-red))))
;;;;; wanderlust
   `(wl-highlight-folder-few-face ((t (:foreground ,4coder-red-2))))
   `(wl-highlight-folder-many-face ((t (:foreground ,4coder-red-1))))
   `(wl-highlight-folder-path-face ((t (:foreground ,4coder-orange))))
   `(wl-highlight-folder-unread-face ((t (:foreground ,4coder-blue))))
   `(wl-highlight-folder-zero-face ((t (:foreground ,4coder-fg))))
   `(wl-highlight-folder-unknown-face ((t (:foreground ,4coder-blue))))
   `(wl-highlight-message-citation-header ((t (:foreground ,4coder-red-1))))
   `(wl-highlight-message-cited-text-1 ((t (:foreground ,4coder-red))))
   `(wl-highlight-message-cited-text-2 ((t (:foreground ,4coder-green+2))))
   `(wl-highlight-message-cited-text-3 ((t (:foreground ,4coder-blue))))
   `(wl-highlight-message-cited-text-4 ((t (:foreground ,4coder-blue+1))))
   `(wl-highlight-message-header-contents-face ((t (:foreground ,4coder-green))))
   `(wl-highlight-message-headers-face ((t (:foreground ,4coder-red+1))))
   `(wl-highlight-message-important-header-contents ((t (:foreground ,4coder-green+2))))
   `(wl-highlight-message-header-contents ((t (:foreground ,4coder-green+1))))
   `(wl-highlight-message-important-header-contents2 ((t (:foreground ,4coder-green+2))))
   `(wl-highlight-message-signature ((t (:foreground ,4coder-green))))
   `(wl-highlight-message-unimportant-header-contents ((t (:foreground ,4coder-fg))))
   `(wl-highlight-summary-answered-face ((t (:foreground ,4coder-blue))))
   `(wl-highlight-summary-disposed-face ((t (:foreground ,4coder-fg
                                                         :slant italic))))
   `(wl-highlight-summary-new-face ((t (:foreground ,4coder-blue))))
   `(wl-highlight-summary-normal-face ((t (:foreground ,4coder-fg))))
   `(wl-highlight-summary-thread-top-face ((t (:foreground ,4coder-yellow))))
   `(wl-highlight-thread-indent-face ((t (:foreground ,4coder-magenta))))
   `(wl-highlight-summary-refiled-face ((t (:foreground ,4coder-fg))))
   `(wl-highlight-summary-displaying-face ((t (:underline t :weight bold))))
;;;;; which-func-mode
   `(which-func ((t (:foreground ,4coder-green+4))))
;;;;; xcscope
   `(cscope-file-face ((t (:foreground ,4coder-yellow :weight bold))))
   `(cscope-function-face ((t (:foreground ,4coder-cyan :weight bold))))
   `(cscope-line-number-face ((t (:foreground ,4coder-red :weight bold))))
   `(cscope-mouse-face ((t (:foreground ,4coder-bg :background ,4coder-blue+1))))
   `(cscope-separator-face ((t (:foreground ,4coder-red :weight bold
                                            :underline t :overline t))))
;;;;; yascroll
   `(yascroll:thumb-text-area ((t (:background ,4coder-bg-1))))
   `(yascroll:thumb-fringe ((t (:background ,4coder-bg-1 :foreground ,4coder-bg-1))))
   ))

;;; Theme Variables
(4coder-with-color-variables
  (custom-theme-set-variables
   '4coder
;;;;; ansi-color
   `(ansi-color-names-vector [,4coder-bg ,4coder-red ,4coder-green ,4coder-yellow
                                          ,4coder-blue ,4coder-magenta ,4coder-cyan ,4coder-fg])
;;;;; company-quickhelp
   `(company-quickhelp-color-background ,4coder-bg+1)
   `(company-quickhelp-color-foreground ,4coder-fg)
;;;;; fill-column-indicator
   `(fci-rule-color ,4coder-bg-05)
;;;;; nrepl-client
   `(nrepl-message-colors
     '(,4coder-red ,4coder-orange ,4coder-yellow ,4coder-green ,4coder-green+4
       ,4coder-cyan ,4coder-blue+1 ,4coder-magenta))
;;;;; pdf-tools
   `(pdf-view-midnight-colors '(,4coder-fg . ,4coder-bg-05))
;;;;; vc-annotate
   `(vc-annotate-color-map
     '(( 20. . ,4coder-red-1)
       ( 40. . ,4coder-red)
       ( 60. . ,4coder-orange)
       ( 80. . ,4coder-yellow-2)
       (100. . ,4coder-yellow-1)
       (120. . ,4coder-yellow)
       (140. . ,4coder-green-2)
       (160. . ,4coder-green)
       (180. . ,4coder-green+1)
       (200. . ,4coder-green+2)
       (220. . ,4coder-green+3)
       (240. . ,4coder-green+4)
       (260. . ,4coder-cyan)
       (280. . ,4coder-blue-2)
       (300. . ,4coder-blue-1)
       (320. . ,4coder-blue)
       (340. . ,4coder-blue+1)
       (360. . ,4coder-magenta)))
   `(vc-annotate-very-old-color ,4coder-magenta)
   `(vc-annotate-background ,4coder-bg-1)
   ))

;;; Rainbow Support

(declare-function rainbow-mode 'rainbow-mode)
(declare-function rainbow-colorize-by-assoc 'rainbow-mode)

(defvar 4coder-add-font-lock-keywords nil
  "Whether to add font-lock keywords for 4coder color names.

In buffers visiting library `4coder-theme.el' the 4coder
specific keywords are always added, provided that library has
been loaded (because that is where the code that does it is
definded).  If you visit this file and only enable the theme,
then you have to turn `rainbow-mode' off and on again for the
4coder-specific font-lock keywords to be used.

In all other Emacs-Lisp buffers this variable controls whether
this should be done.  This requires library `rainbow-mode'.")

(defvar 4coder-colors-font-lock-keywords nil)

(defun 4coder--rainbow-turn-on ()
  "Maybe also add font-lock keywords for 4coder colors."
  (when (and (derived-mode-p 'emacs-lisp-mode)
             (or 4coder-add-font-lock-keywords
                 (and (buffer-file-name)
                      (equal (file-name-nondirectory (buffer-file-name))
                             "4coder-theme.el"))))
    (unless 4coder-colors-font-lock-keywords
      (setq 4coder-colors-font-lock-keywords
            `((,(regexp-opt (mapcar 'car 4coder-default-colors-alist) 'words)
               (0 (rainbow-colorize-by-assoc 4coder-default-colors-alist))))))
    (font-lock-add-keywords nil 4coder-colors-font-lock-keywords 'end)))

(defun 4coder--rainbow-turn-off ()
  "Also remove font-lock keywords for 4coder colors."
  (font-lock-remove-keywords nil 4coder-colors-font-lock-keywords))

(when (fboundp 'advice-add)
  (advice-add 'rainbow-turn-on :after  #'4coder--rainbow-turn-on)
  (advice-add 'rainbow-turn-off :after #'4coder--rainbow-turn-off))

;;; Footer

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme '4coder)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (require 'rainbow-mode nil t) (rainbow-mode 1))
;; End:
;;; 4coder-theme.el ends here
