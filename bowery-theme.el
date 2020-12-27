;;; bowery-theme.el --- Gruber Darker color theme for Emacs 24.

;; Copyright (C) 2013-2016 Alexey Kutepov a.k.a rexim
;; Copyright (C) 2009-2010 Jason R. Blevins

;; Author: Alexey Kutepov <reximkut@gmail.com>
;; URL: http://github.com/rexim/bowery-theme
;; Package-Version: 20200227.2238
;; Package-Commit: 7f95ce96079eb22b9214435ed25c5af98f60b482
;; Version: 0.6

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:
;;
;; Gruber Darker color theme for Emacs by Jason Blevins. A darker
;; variant of the Gruber Dark theme for BBEdit by John Gruber. Adapted
;; for deftheme and extended by Alexey Kutepov a.k.a. rexim.


(deftheme bowery
  "Gruber Darker color theme for Emacs 24")

;; Please, install rainbow-mode.
;; Colors with +x are lighter. Colors with -x are darker.
(let ((bowery-fg        "#e4e4ef")
      (bowery-fg+1      "#f4f4ff")
      (bowery-fg+2      "#f5f5f5")
      (bowery-white     "#ffffff")
      (bowery-black     "#000000")
      (bowery-bg-1      "#101010")
      (bowery-bg        "#141718")
      (bowery-bg+1      "#282828")
      (bowery-bg+2      "#453d41")
      (bowery-bg+3      "#484848")
      (bowery-bg+4      "#52494e")
      (bowery-red-1     "#c73c3f")
      (bowery-red       "#f43841")
      (bowery-red+1     "#ff4f58")
      (bowery-green     "#63e936")
      (bowery-yellow    "#FAC03B")
      (bowery-brown     "#cc8c3c")
      (bowery-blue      "#83c8ed")
      (bowery-quartz    "#95a99f")
      (bowery-niagara-2 "#303540")
      (bowery-niagara-1 "#565f73")
      (bowery-niagara   "#96a6d8")
      (bowery-wisteria  "#9e95c7")
      )
  (custom-theme-set-variables
   'bowery
   '(frame-brackground-mode (quote dark)))

  (custom-theme-set-faces
   'bowery

   ;; Agda2
   `(agda2-highlight-datatype-face ((t (:foreground ,bowery-quartz))))
   `(agda2-highlight-primitive-type-face ((t (:foreground ,bowery-quartz))))
   `(agda2-highlight-function-face ((t (:foreground ,bowery-niagara))))
   `(agda2-highlight-keyword-face ((t ,(list :foreground bowery-yellow
                                             :bold t))))
   `(agda2-highlight-inductive-constructor-face ((t (:foreground ,bowery-green))))
   `(agda2-highlight-number-face ((t (:foreground ,bowery-wisteria))))

   ;; AUCTeX
   `(font-latex-bold-face ((t (:foreground ,bowery-quartz :bold t))))
   `(font-latex-italic-face ((t (:foreground ,bowery-quartz :italic t))))
   `(font-latex-math-face ((t (:foreground ,bowery-green))))
   `(font-latex-sectioning-5-face ((t ,(list :foreground bowery-niagara
                                             :bold t))))
   `(font-latex-slide-title-face ((t (:foreground ,bowery-niagara))))
   `(font-latex-string-face ((t (:foreground ,bowery-blue))))
   `(font-latex-warning-face ((t (:foreground ,bowery-red))))

   ;; Basic Coloring (or Uncategorized)
   '(vertical-border ((t (:background "#000" :foreground "#000"))))
   `(border ((t ,(list :background bowery-bg-1
                       :foreground bowery-bg+2))))
   `(cursor ((t (:background "#ff0080"))))
   `(default ((t ,(list :foreground bowery-fg
                        :background bowery-bg))))
   `(fringe ((t ,(list :background bowery-bg-1
                       :foreground bowery-bg+2))))
   `(link ((t (:foreground ,bowery-niagara :underline t))))
   `(link-visited ((t (:foreground ,bowery-wisteria :underline t))))
   `(match ((t (:background ,bowery-bg+4))))
   `(shadow ((t (:foreground ,bowery-bg+4))))
   `(minibuffer-prompt ((t (:foreground ,bowery-niagara))))
   `(region ((t (:background ,bowery-bg+3 :foreground nil))))
   `(secondary-selection ((t ,(list :background bowery-bg+3
                                    :foreground nil))))
   `(trailing-whitespace ((t ,(list :foreground bowery-black
                                    :background bowery-red))))
   `(tooltip ((t ,(list :background bowery-bg+4
                        :foreground bowery-white))))

   ;; Calendar
   `(holiday-face ((t (:foreground ,bowery-red))))

   ;; Compilation
   `(compilation-info ((t ,(list :foreground bowery-green
                                 :inherit 'unspecified))))
   `(compilation-warning ((t ,(list :foreground bowery-brown
                                    :bold t
                                    :inherit 'unspecified))))
   `(compilation-error ((t (:foreground ,bowery-red+1))))
   `(compilation-mode-line-fail ((t ,(list :foreground bowery-red
                                           :weight 'bold
                                           :inherit 'unspecified))))
   `(compilation-mode-line-exit ((t ,(list :foreground bowery-green
                                           :weight 'bold
                                           :inherit 'unspecified))))

   ;; Custom
   `(custom-state ((t (:foreground ,bowery-green))))

   ;; Diff
   `(diff-removed ((t ,(list :foreground bowery-red+1
                             :background nil))))
   `(diff-added ((t ,(list :foreground bowery-green
                           :background nil))))

   ;; Dired
   `(dired-directory ((t (:foreground ,bowery-niagara :weight bold))))
   `(dired-ignored ((t ,(list :foreground bowery-quartz
                              :inherit 'unspecified))))

   ;; Ebrowse
   `(ebrowse-root-class ((t (:foreground ,bowery-niagara :weight bold))))
   `(ebrowse-progress ((t (:background ,bowery-niagara))))

   ;; Egg
   `(egg-branch ((t (:foreground ,bowery-yellow))))
   `(egg-branch-mono ((t (:foreground ,bowery-yellow))))
   `(egg-diff-add ((t (:foreground ,bowery-green))))
   `(egg-diff-del ((t (:foreground ,bowery-red))))
   `(egg-diff-file-header ((t (:foreground ,bowery-wisteria))))
   `(egg-help-header-1 ((t (:foreground ,bowery-yellow))))
   `(egg-help-header-2 ((t (:foreground ,bowery-niagara))))
   `(egg-log-HEAD-name ((t (:box (:color ,bowery-fg)))))
   `(egg-reflog-mono ((t (:foreground ,bowery-niagara-1))))
   `(egg-section-title ((t (:foreground ,bowery-yellow))))
   `(egg-text-base ((t (:foreground ,bowery-fg))))
   `(egg-term ((t (:foreground ,bowery-yellow))))

   ;; ERC
   `(erc-notice-face ((t (:foreground ,bowery-wisteria))))
   `(erc-timestamp-face ((t (:foreground ,bowery-green))))
   `(erc-input-face ((t (:foreground ,bowery-red+1))))
   `(erc-my-nick-face ((t (:foreground ,bowery-red+1))))

   ;; EShell
   `(eshell-ls-backup ((t (:foreground ,bowery-quartz))))
   `(eshell-ls-directory ((t (:foreground ,bowery-niagara))))
   `(eshell-ls-executable ((t (:foreground ,bowery-green))))
   `(eshell-ls-symlink ((t (:foreground ,bowery-yellow))))

   ;; Font Lock
   `(font-lock-builtin-face ((t (:foreground ,bowery-yellow))))
   `(font-lock-comment-face ((t (:foreground ,bowery-green))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,bowery-green))))
   `(font-lock-constant-face ((t (:foreground ,bowery-niagara))))
   `(font-lock-doc-face ((t (:foreground ,bowery-green))))
   `(font-lock-doc-string-face ((t (:foreground ,bowery-brown))))
   `(font-lock-function-name-face ((t (:foreground "#0dcaef"))))
   `(font-lock-keyword-face ((t (:foreground ,bowery-yellow))))
   `(font-lock-preprocessor-face ((t (:foreground ,bowery-quartz))))
   `(font-lock-reference-face ((t (:foreground ,bowery-quartz))))
   `(font-lock-string-face ((t (:foreground ,bowery-blue))))
   `(font-lock-type-face ((t (:foreground ,bowery-quartz))))
   `(font-lock-variable-name-face ((t (:foreground ,bowery-fg+1))))
   `(font-lock-warning-face ((t (:foreground ,bowery-red))))

   ;; Flymake
   `(flymake-errline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,bowery-red)
                   :foreground unspecified
                   :background unspecified
                   :inherit unspecified))
      (t (:foreground ,bowery-red :weight bold :underline t))))
   `(flymake-warnline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,bowery-yellow)
                   :foreground unspecified
                   :background unspecified
                   :inherit unspecified))
      (t (:forground ,bowery-yellow :weight bold :underline t))))
   `(flymake-infoline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,bowery-green)
                   :foreground unspecified
                   :background unspecified
                   :inherit unspecified))
      (t (:forground ,bowery-green :weight bold :underline t))))

   ;; Flyspell
   `(flyspell-incorrect
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,bowery-red) :inherit unspecified))
      (t (:foreground ,bowery-red :weight bold :underline t))))
   `(flyspell-duplicate
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,bowery-yellow) :inherit unspecified))
      (t (:foreground ,bowery-yellow :weight bold :underline t))))

   ;; Helm
   `(helm-candidate-number ((t ,(list :background bowery-bg+2
                                      :foreground bowery-yellow
                                      :bold t))))
   `(helm-ff-directory ((t ,(list :foreground bowery-niagara
                                  :background bowery-bg
                                  :bold t))))
   `(helm-ff-executable ((t (:foreground ,bowery-green))))
   `(helm-ff-file ((t (:foreground ,bowery-fg :inherit unspecified))))
   `(helm-ff-invalid-symlink ((t ,(list :foreground bowery-bg
                                        :background bowery-red))))
   `(helm-ff-symlink ((t (:foreground ,bowery-yellow :bold t))))
   `(helm-selection-line ((t (:background ,bowery-bg+1))))
   `(helm-selection ((t (:background ,bowery-bg+1 :underline nil))))
   `(helm-source-header ((t ,(list :foreground bowery-yellow
                                   :background bowery-bg
                                   :box (list :line-width -1
                                              :style 'released-button)))))

   ;; Ido
   `(ido-first-match ((t (:foreground ,bowery-yellow :bold nil))))
   `(ido-only-match ((t (:foreground ,bowery-brown :weight bold))))
   `(ido-subdir ((t (:foreground ,bowery-niagara :weight bold))))

   ;; Info
   `(info-xref ((t (:foreground ,bowery-niagara))))
   `(info-visited ((t (:foreground ,bowery-wisteria))))

   ;; Jabber
   `(jabber-chat-prompt-foreign ((t ,(list :foreground bowery-quartz
                                           :bold nil))))
   `(jabber-chat-prompt-local ((t (:foreground ,bowery-yellow))))
   `(jabber-chat-prompt-system ((t (:foreground ,bowery-green))))
   `(jabber-rare-time-face ((t (:foreground ,bowery-green))))
   `(jabber-roster-user-online ((t (:foreground ,bowery-green))))
   `(jabber-activity-face ((t (:foreground ,bowery-red))))
   `(jabber-activity-personal-face ((t (:foreground ,bowery-yellow :bold t))))

   ;; Line Highlighting
   `(highlight ((t (:background ,bowery-bg+1 :foreground nil))))
   `(highlight-current-line-face ((t ,(list :background bowery-bg+1
                                            :foreground nil))))

   ;; Linum
   `(linum ((t `(list :foreground bowery-quartz
                      :background bowery-bg))))

   ;; Magit
   `(magit-branch ((t (:foreground ,bowery-niagara))))
   `(magit-diff-hunk-header ((t (:background ,bowery-bg+2))))
   `(magit-diff-file-header ((t (:background ,bowery-bg+4))))
   `(magit-log-sha1 ((t (:foreground ,bowery-red+1))))
   `(magit-log-author ((t (:foreground ,bowery-brown))))
   `(magit-log-head-label-remote ((t ,(list :foreground bowery-green
                                            :background bowery-bg+1))))
   `(magit-log-head-label-local ((t ,(list :foreground bowery-niagara
                                           :background bowery-bg+1))))
   `(magit-log-head-label-tags ((t ,(list :foreground bowery-yellow
                                          :background bowery-bg+1))))
   `(magit-log-head-label-head ((t ,(list :foreground bowery-fg
                                          :background bowery-bg+1))))
   `(magit-item-highlight ((t (:background ,bowery-bg+1))))
   `(magit-tag ((t ,(list :foreground bowery-yellow
                          :background bowery-bg))))
   `(magit-blame-heading ((t ,(list :background bowery-bg+1
                                    :foreground bowery-fg))))

   ;; Message
   `(message-header-name ((t (:foreground ,bowery-green))))

   ;; Mode Line
   `(mode-line ((t ,(list :background bowery-bg+1
                          :foreground bowery-white))))
   `(mode-line-buffer-id ((t ,(list :background bowery-bg+1
                                    :foreground bowery-white))))
   `(mode-line-inactive ((t ,(list :background bowery-bg+1
                                   :foreground bowery-quartz))))

   ;; Neo Dir
   `(neo-dir-link-face ((t (:foreground ,bowery-niagara))))

   ;; Org Mode
   `(org-agenda-structure ((t (:foreground ,bowery-niagara))))
   `(org-column ((t (:background ,bowery-bg-1))))
   `(org-column-title ((t (:background ,bowery-bg-1 :underline t :weight bold))))
   `(org-done ((t (:foreground ,bowery-green))))
   `(org-todo ((t (:foreground ,bowery-red-1))))
   `(org-upcoming-deadline ((t (:foreground ,bowery-yellow))))

   ;; Search
   `(isearch ((t ,(list :foreground bowery-black
                        :background bowery-fg+2))))
   `(isearch-fail ((t ,(list :foreground bowery-black
                             :background bowery-red))))
   `(isearch-lazy-highlight-face ((t ,(list
                                       :foreground bowery-fg+1
                                       :background bowery-niagara-1))))

   ;; Sh
   `(sh-quoted-exec ((t (:foreground ,bowery-red+1))))

   ;; Show Paren
   `(show-paren-match-face ((t (:background ,bowery-bg+4))))
   `(show-paren-mismatch-face ((t (:background ,bowery-red-1))))

   ;; Slime
   `(slime-repl-inputed-output-face ((t (:foreground ,bowery-red))))

   ;; Tuareg
   `(tuareg-font-lock-governing-face ((t (:foreground ,bowery-yellow))))

   ;; Speedbar
   `(speedbar-directory-face ((t ,(list :foreground bowery-niagara
                                        :weight 'bold))))
   `(speedbar-file-face ((t (:foreground ,bowery-fg))))
   `(speedbar-highlight-face ((t (:background ,bowery-bg+1))))
   `(speedbar-selected-face ((t (:foreground ,bowery-red))))
   `(speedbar-tag-face ((t (:foreground ,bowery-yellow))))

   ;; Which Function
   `(which-func ((t (:foreground ,bowery-wisteria))))

   ;; Whitespace
   `(whitespace-space ((t ,(list :background bowery-bg
                                 :foreground bowery-bg+1))))
   `(whitespace-tab ((t ,(list :background bowery-bg
                               :foreground bowery-bg+1))))
   `(whitespace-hspace ((t ,(list :background bowery-bg
                                  :foreground bowery-bg+2))))
   `(whitespace-line ((t ,(list :background bowery-bg+2
                                :foreground bowery-red+1))))
   `(whitespace-newline ((t ,(list :background bowery-bg
                                   :foreground bowery-bg+2))))
   `(whitespace-trailing ((t ,(list :background bowery-red
                                    :foreground bowery-red))))
   `(whitespace-empty ((t ,(list :background bowery-yellow
                                 :foreground bowery-yellow))))
   `(whitespace-indentation ((t ,(list :background bowery-yellow
                                       :foreground bowery-red))))
   `(whitespace-space-after-tab ((t ,(list :background bowery-yellow
                                           :foreground bowery-yellow))))
   `(whitespace-space-before-tab ((t ,(list :background bowery-brown
                                            :foreground bowery-brown))))

   ;;;;; company-mode
   `(company-tooltip ((t (:foreground ,bowery-fg :background ,bowery-bg+1))))
   `(company-tooltip-annotation ((t (:foreground ,bowery-brown :background ,bowery-bg+1))))
   `(company-tooltip-annotation-selection ((t (:foreground ,bowery-brown :background ,bowery-bg-1))))
   `(company-tooltip-selection ((t (:foreground ,bowery-fg :background ,bowery-bg-1))))
   `(company-tooltip-mouse ((t (:background ,bowery-bg-1))))
   `(company-tooltip-common ((t (:foreground ,bowery-green))))
   `(company-tooltip-common-selection ((t (:foreground ,bowery-green)))))
  `(company-scrollbar-fg ((t (:background ,bowery-bg-1))))
  `(company-scrollbar-bg ((t (:background ,bowery-bg+2))))
  `(company-preview ((t (:background ,bowery-green))))
  `(company-preview-common ((t (:foreground ,bowery-green :background ,bowery-bg-1))))

   ;;;;; Proof General

  `(proof-locked-face ((t (:background ,bowery-niagara-2))))
   )

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'bowery)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (fboundp 'rainbow-mode) (rainbow-mode +1))
;; End:

;;; bowery-theme.el ends here.
