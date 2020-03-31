
;;; naysayer-theme.el --- The naysayer color theme

;; Author: Nick Aversano <nickav@users.noreply.github.com>
;; Version: 0.3
;; Package-Version: 20191207.1936
;; Filename: naysayer-theme.el
;; Package-Requires: ((emacs "24"))
;; URL: https://github.com/nickav/naysayer-theme.el
;; License: GPL-3+

;;; Commentary:

;; Dark green blue color scheme with tan colors. Inspired by Jonathan Blow's compiler livestreams.

;;; Code:

(unless (>= emacs-major-version 24)
  (error "The naysayer theme requires Emacs 24 or later!"))

(deftheme naysayer "The naysayer color theme")

(let ((background "#052329")
      (gutters    "#082628")
      (gutter-fg  "#082628")
      (gutters-active "#082628")
      (builtin      "#ffffff")
      (selection  "#0010ff")
      (text       "#D3BE97")
      (comments   "#3fdf1f")
      (punctuation "#80ffb0")
      (keywords "#ffffff")
      (variables "#d4d4ff")
      (functions "#d4d4d4")
      (methods    "#d4d4d4")
      (strings    "#0fdfaf")
      (constants "#80f0e0")
      (macros "#80ffb0")
      (numbers "#8fe1c8")
      (white     "#ffffff")
      (error "#ff0000")
      (warning "#ffbb00")
      (highlight-line "#0b3335")
      (line-fg "#126367"))

  (custom-theme-set-faces
   'naysayer

   ;; Default colors
   ;; *****************************************************************************

   `(default                          ((t (:foreground ,text :background ,background, :weight normal))))
   `(region                           ((t (:foreground nil :background ,selection))))
   `(cursor                           ((t (:background "#ff0068"                        ))))
   `(fringe                           ((t (:background ,background   :foreground ,white))))
   `(linum                            ((t (:background ,background :foreground ,gutter-fg))))
   `(highlight ((t (:foreground nil :background ,selection))))

   ;; Font lock faces
   ;; *****************************************************************************

   `(font-lock-keyword-face           ((t (:foreground ,keywords, :weight normal))))
   `(font-lock-type-face              ((t (:foreground "lightgreen"))))
   `(font-lock-constant-face          ((t (:foreground ,constants))))
   `(font-lock-variable-name-face     ((t (:foreground "lightblue"))))
   `(font-lock-builtin-face           ((t (:foreground "lightgreen"))))
   `(font-lock-string-face            ((t (:foreground ,strings))))
   `(font-lock-comment-face           ((t (:foreground ,comments))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,comments))))
   `(font-lock-function-name-face     ((t (:foreground ,functions))))
   `(font-lock-doc-string-face        ((t (:foreground ,strings))))
   `(font-lock-preprocessor-face      ((t (:foreground ,macros))))
   `(font-lock-warning-face           ((t (:foreground ,warning))))

   ;; Plugins
   ;; *****************************************************************************
   `(trailing-whitespace ((t (:foreground nil :background ,warning))))
   `(whitespace-trailing ((t (:background nil :foreground ,warning :inverse-video t))))

   `(linum ((t (:foreground ,line-fg :background ,background))))
   `(linum-relative-current-face ((t (:foreground ,white :background ,background))))
   `(line-number ((t (:foreground ,line-fg :background ,background))))
   `(line-number-current-line ((t (:foreground ,white :background ,background))))

   ;; hl-line-mode
   `(hl-line ((t (:background ,highlight-line))))
   `(hl-line-face ((t (:background ,highlight-line))))

   ;; ;; rainbow-delimiters
   ;; `(rainbow-delimiters-depth-1-face ((t (:foreground ,naysayer-theme-violet))))
   ;; `(rainbow-delimiters-depth-2-face ((t (:foreground ,naysayer-theme-blue))))
   ;; `(rainbow-delimiters-depth-3-face ((t (:foreground ,naysayer-theme-green))))
   ;; `(rainbow-delimiters-depth-4-face ((t (:foreground ,naysayer-theme-yellow))))
   ;; `(rainbow-delimiters-depth-5-face ((t (:foreground ,naysayer-theme-orange))))
   ;; `(rainbow-delimiters-depth-6-face ((t (:foreground ,naysayer-theme-red))))
   ;; `(rainbow-delimiters-depth-7-face ((t (:foreground ,naysayer-theme-violet))))
   ;; `(rainbow-delimiters-depth-8-face ((t (:foreground ,naysayer-theme-blue))))
   ;; `(rainbow-delimiters-depth-9-face ((t (:foreground ,naysayer-theme-green))))
   ;; `(rainbow-delimiters-depth-10-face ((t (:foreground ,naysayer-theme-yellow))))
   ;; `(rainbow-delimiters-depth-11-face ((t (:foreground ,naysayer-theme-orange))))
   ;; `(rainbow-delimiters-depth-12-face ((t (:foreground ,naysayer-theme-red))))

   ;; mode-line and powerline
   `(mode-line-buffer-id ((t (:foreground ,background :distant-foreground ,text :text ,text :weight bold))))
   `(mode-line ((t (:foreground ,background
                    :background "#d8b488"
                                   ))))
   `(powerline-active1 ((t (:background ,text :foreground ,background))))
   `(powerline-active2 ((t (:background ,text :foreground ,background))))

   `(mode-line-inactive ((t (:inverse-video unspecified
                                            :underline unspecified
                                            :foreground ,text
                                            :background ,highlight-line
                                            :box nil))))
   `(powerline-inactive1 ((t (:background ,background :foreground ,text))))
   `(powerline-inactive2 ((t (:background ,background :foreground ,text))))

   ;; js2-mode
   `(js2-function-call ((t (:inherit (font-lock-function-name-face)))))
   `(js2-function-param ((t (:foreground ,text))))
   `(js2-jsdoc-tag ((t (:foreground ,keywords))))
   `(js2-jsdoc-type ((t (:foreground ,constants))))
   `(js2-jsdoc-value((t (:foreground ,text))))
   `(js2-object-property ((t (:foreground ,text))))
   `(js2-external-variable ((t (:foreground ,constants))))
   `(js2-error ((t (:foreground ,error))))
   `(js2-warning ((t (:foreground ,warning))))

   ;; highlight numbers
   `(highlight-numbers-number ((t (:foreground ,numbers))))
  )

  (custom-theme-set-variables
    'naysayer
    '(linum-format " %5i ")
  )
)

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

;; *****************************************************************************

(provide-theme 'naysayer)

;; Local Variables:
;; no-byte-compile: t
;; End:

(provide 'naysayer-theme)

;;; naysayer-theme.el ends here
