;;https://emacs.stackexchange.com/a/52804/11934
(setq custom--inhibit-theme-enable nil)

;;; Bespoke Theme
;;;; Set Face Function
;; From Elegant Emacs
;; https://github.com/rougier/elegant-emacs/blob/master/elegance.el
;; When we set a face, we take care of removing any previous settings
(defun set-face (face style)
  "Reset a face and make it inherit style."
  (set-face-attribute face nil
                      :foreground 'unspecified :background 'unspecified
                      :family     'unspecified :slant      'unspecified
                      :weight     'unspecified :height     'unspecified
                      :underline  'unspecified :overline   'unspecified
                      :box        'unspecified :inherit    style))

;;;; Bespoke Faces

;; A theme is fully defined by these six faces
(defgroup bespoke nil
  "Faces for the bespoke theme"
  :prefix "face-")

;; Do not show prefix when displaying the bespoke group
(setq custom-unlispify-remove-prefixes t)

(defface face-default nil
  "Default face is used for regular information."
  :group 'bespoke)

(defface face-critical nil
  "Critical face is for information that requires immediate action.
It should be of high constrast when compared to other faces. This
can be realized (for example) by setting an intense background
color, typically a shade of red. It must be used scarcely."
  :group 'bespoke)

(defface face-popout nil
  "Popout face is used for information that needs attention.
To achieve such effect, the hue of the face has to be
sufficiently different from other faces such that it attracts
attention through the popout effect."
  :group 'bespoke)

(defface face-strong nil
  "Strong face is used for information of a structural nature.
It has to be the same color as the default color and only the
weight differs by one level (e.g., light/regular or
regular/bold). IT is generally used for titles, keywords,
directory, etc."
  :group 'bespoke)

(defface face-strong-bold nil
  "Strong-bold face is used for information of a structural nature.
It has to be the same color as the default color and only the
weight differs by one level (e.g., light/regular or
regular/bold). IT is generally used for titles, keywords,
directory, etc."
  :group 'bespoke)

(defface face-strong-bold-italic nil
  "Strong face is used for information of a structural nature.
It has to be the same color as the default color and only the
weight differs by one level (e.g., light/regular or
regular/bold). IT is generally used for titles, keywords,
directory, etc."
  :group 'bespoke)

(defface face-salient nil
  "Salient face is used for information that are important.
To suggest the information is of the same nature but important,
the face uses a different hue with approximately the same
intensity as the default face. This is typically used for links."

  :group 'bespoke)

(defface face-faded nil
  "Faded face is for information that are less important.
It is made by using the same hue as the default but with a lesser
intensity than the default. It can be used for comments,
secondary information and also replace italic (which is generally
abused anyway)."
  :group 'bespoke)

(defface face-faded-italic nil
  "Faded italic face is for information that are less important.
It is made by using the same hue as the default but with a lesser
intensity than the default. Allows use of slant for specific semantic emphasis."
  :group 'bespoke)

(defface face-subtle nil
  "Subtle face is used to suggest a physical area on the screen.
It is important to not disturb too strongly the reading of
information and this can be made by setting a very light
background color that is barely perceptible."
  :group 'bespoke)

(defface face-accent1 nil
  "Bright accent face"
  :group 'bespoke)

(set-face-attribute 'face-accent1 nil
                    :foreground "#BF616A")

(set-face-attribute 'face-strong-bold nil
                    :inherit'face-default
                    :weight 'bold)

(set-face-attribute 'face-strong-bold-italic nil
                    :inherit 'face-default
                    :weight 'bold
                    :slant 'italic)

(set-face-attribute 'face-faded-italic nil
                    :inherit 'face-default
                    :weight 'medium
                    :slant 'italic)

;;;; Bespoke Modeline
;; Modeline
(defun set-modeline-faces ()

  ;; Mode line at top
  (set-face 'header-line                                 'face-strong)
  (set-face-attribute 'header-line nil
                      :underline nil)
  (set-face-attribute 'mode-line nil
                      :height 10
                      :underline (face-foreground 'face-faded)
                      :overline nil
                      :box nil
                      :foreground (face-background 'default)
                      :background (face-background 'default))
  (set-face 'mode-line-inactive                            'mode-line)
  (set-face-attribute 'cursor nil
                      :background (face-foreground 'default))
  (set-face-attribute 'window-divider nil
                      :foreground (face-background 'mode-line))
  (set-face-attribute 'window-divider-first-pixel nil
                      :foreground (face-background 'default))
  (set-face-attribute 'window-divider-last-pixel nil
                      :foreground (face-background 'default))
  )

;;;; Buttons
(defun set-button-faces ()
  (set-face-attribute 'custom-button nil
                      :foreground (face-foreground 'face-faded)
                      :background (face-background 'face-subtle)
                      :box `(:line-width 1
                             :color ,(face-foreground 'face-faded)
                             :style nil))
  (set-face-attribute 'custom-button-mouse nil
                      :foreground (face-foreground 'default)
                      ;; :background (face-foreground 'face-faded)
                      :inherit 'custom-button
                      :box `(:line-width 1
                             :color ,(face-foreground 'face-subtle)
                             :style nil))
  (set-face-attribute 'custom-button-pressed nil
                      :foreground (face-background 'default)
                      :background (face-foreground 'face-salient)
                      :inherit 'face-salient
                      :box `(:line-width 1
                             :color ,(face-foreground 'face-salient)
                             :style nil)
                      :inverse-video nil))

;;;; Light theme
(defun bespoke-light ()
  (setq frame-background-mode 'light)
  (set-background-color "#FFFEF9")
  (set-foreground-color "#37474F")
  (set-face-attribute 'default nil
                      :foreground (face-foreground 'default)
                      :background (face-background 'default))
  (set-face-attribute 'face-critical nil :foreground "#ffffff"
                      :background "#ff6347")
  (set-face-attribute 'face-popout nil :foreground "#ffa07a")
  (set-face-attribute 'face-strong nil :foreground "#333333"
                      :weight 'regular)
  (set-face-attribute 'face-salient nil :foreground "#00008b"
                      :weight 'light)
  (set-face-attribute 'face-faded nil :foreground "#999999"
                      :weight 'light)
  (set-face-attribute 'face-subtle nil :background "#f0f0f0")

  (set-modeline-faces)

  (set-face-attribute 'header-line nil
                      :background "#D8DEE9"
                      :foreground "#37474F"
                      :box '(:line-width 8  :color "#D8DEE9" :height 150)
                      :overline nil
                      :underline nil
                      :height 150)

  (with-eval-after-load 'cus-edit (set-button-faces)))

;;;; Dark theme
(defun bespoke-dark ()
  (setq frame-background-mode 'dark)
  (set-background-color "#2E3440")
  (set-foreground-color "#FFFEF9")
  ;; (set-foreground-color "#dcdccc")
  (set-face-attribute 'default nil
                      :foreground (face-foreground 'default)
                      :background (face-background 'default))
  (set-face-attribute 'face-critical nil :foreground "#A3BE8C"
                      :background "#EBCB8B")
  (set-face-attribute 'face-popout nil :foreground "#D08770")
  (set-face-attribute 'face-strong nil :foreground "#dcdccc"
                      :weight 'regular)
  (set-face-attribute 'face-salient nil :foreground "#dca3a3"
                      :weight 'light)
  (set-face-attribute 'face-faded nil :foreground "#677691"
                      :weight 'light)
  (set-face-attribute 'face-subtle nil :background "#434C5E")
  (set-modeline-faces)
  (set-face-attribute 'header-line nil
                      :foreground "#ECEFF4"
                      :background "#3B4252"
                      :box '(:line-width 8 :color "#3B4252" :height 150)
                      :overline nil
                      :underline nil
                      :height 150)

  (with-eval-after-load 'cus-edit (set-button-faces)))

;;;; Faces
;; Structural
(set-face 'bold                                          'face-strong)
(set-face 'italic                                         'face-faded-italic)
(set-face 'bold-italic                                   'face-strong-bold-italic)
(set-face 'region                                        'face-subtle)
(set-face 'highlight                                     'face-subtle)
(set-face 'fixed-pitch                                       'default)
(set-face 'fixed-pitch-serif                                 'face-default)
;; set variable-pitch per group
;; (set-face 'variable-pitch                                    'default)
(set-face 'cursor                                            'default)

;;;;; Semantic
(set-face 'shadow                                         'face-faded)
(set-face 'success                                      'face-salient)
(set-face 'warning                                       'face-popout)
(set-face 'error                                       'face-critical)
(set-face 'match                                         'face-popout)

;;;;; General
(set-face 'buffer-menu-buffer                            'face-strong)
(set-face 'minibuffer-prompt                             'face-strong)
(set-face 'link                                         'face-salient)
(set-face 'fringe                                         'face-faded)
(set-face 'isearch                                       'face-strong)
(set-face 'isearch-fail                                   'face-faded)
(set-face 'lazy-highlight                                'face-subtle)
(set-face 'trailing-whitespace                           'face-subtle)
(set-face 'show-paren-match                              'face-popout)
(set-face 'show-paren-mismatch                           'face-normal)
(set-face-attribute 'tooltip nil                         :height 0.85)

;;;;; Programming mode
(set-face 'font-lock-comment-face                         'face-faded)
(set-face 'font-lock-doc-face                             'face-faded)
(set-face 'font-lock-string-face                         'face-popout)
(set-face 'font-lock-constant-face                      'face-salient)
(set-face 'font-lock-warning-face                        'face-popout)
(set-face 'font-lock-function-name-face                  'face-strong)
(set-face 'font-lock-variable-name-face                  'face-strong)
(set-face 'font-lock-builtin-face                       'face-salient)
(set-face 'font-lock-type-face                          'face-salient)
(set-face 'font-lock-keyword-face                       'face-salient)

;;;;; Documentation
(with-eval-after-load 'info
  (set-face 'info-menu-header                            'face-strong)
  (set-face 'info-header-node                            'face-normal)
  (set-face 'Info-quoted                                  'face-faded)
  (set-face 'info-title-1                                'face-strong)
  (set-face 'info-title-2                                'face-strong)
  (set-face 'info-title-3                                'face-strong)
  (set-face 'info-title-4                               'face-strong))

;;;;; Bookmarks
(with-eval-after-load 'bookmark
  (set-face 'bookmark-menu-heading                       'face-strong)
  (set-face 'bookmark-menu-bookmark                    'face-salient))

;;;;; Message
(with-eval-after-load 'message
  (set-face 'message-cited-text                           'face-faded)
  (set-face 'message-header-cc                               'default)
  (set-face 'message-header-name                         'face-strong)
  (set-face 'message-header-newsgroups                       'default)
  (set-face 'message-header-other                            'default)
  (set-face 'message-header-subject                     'face-salient)
  (set-face 'message-header-to                          'face-salient)
  (set-face 'message-header-xheader                          'default)
  (set-face 'message-mml                                 'face-popout)
  (set-face 'message-separator                           'face-faded))

;;;;; Outline
(with-eval-after-load 'outline
  (set-face 'outline-1                                   'face-strong)
  (set-face 'outline-2                                   'face-strong)
  (set-face 'outline-3                                   'face-strong)
  (set-face 'outline-4                                   'face-strong)
  (set-face 'outline-5                                   'face-strong)
  (set-face 'outline-6                                   'face-strong)
  (set-face 'outline-7                                   'face-strong)
  (set-face 'outline-8                                  'face-strong))

;;;;; Interface
(with-eval-after-load 'cus-edit
  (set-face 'widget-field                                'face-subtle)
  (set-face 'widget-button                               'face-strong)
  (set-face 'widget-single-line-field                    'face-subtle)
  (set-face 'custom-group-subtitle                       'face-strong)
  (set-face 'custom-group-tag                            'face-strong)
  (set-face 'custom-group-tag-1                          'face-strong)
  (set-face 'custom-comment                               'face-faded)
  (set-face 'custom-comment-tag                           'face-faded)
  (set-face 'custom-changed                             'face-salient)
  (set-face 'custom-modified                            'face-salient)
  (set-face 'custom-face-tag                             'face-strong)
  (set-face 'custom-variable-tag                             'default)
  (set-face 'custom-invalid                              'face-popout)
  (set-face 'custom-visibility                          'face-salient)
  (set-face 'custom-state                               'face-salient)
  (set-face 'custom-link                               'face-salient))

;;;;; Package
(with-eval-after-load 'package
  (set-face 'package-description                             'default)
  (set-face 'package-help-section-name                       'default)
  (set-face 'package-name                               'face-salient)
  (set-face 'package-status-avail-obso                    'face-faded)
  (set-face 'package-status-available                        'default)
  (set-face 'package-status-built-in                    'face-salient)
  (set-face 'package-status-dependency                  'face-salient)
  (set-face 'package-status-disabled                      'face-faded)
  (set-face 'package-status-external                         'default)
  (set-face 'package-status-held                             'default)
  (set-face 'package-status-incompat                      'face-faded)
  (set-face 'package-status-installed                   'face-salient)
  (set-face 'package-status-new                              'default)
  (set-face 'package-status-unsigned                         'default)

  ;; Button face is hardcoded, we have to redefine the relevant
  ;; function
  (defun package-make-button (text &rest properties)
    "Insert button labeled TEXT with button PROPERTIES at point.
PROPERTIES are passed to `insert-text-button', for which this
function is a convenience wrapper used by `describe-package-1'."
    (let ((button-text (if (display-graphic-p)
                           text (concat "[" text "]")))
          (button-face (if (display-graphic-p)
                           '(:box `(:line-width 1
                                    :color "#999999":style nil)
                             :foreground "#999999"
                             :background "#F0F0F0")
                         'link)))
      (apply #'insert-text-button button-text
             'face button-face 'follow-link t properties)))
  )

;;;;; Flyspell
(with-eval-after-load 'flyspell
  (set-face 'flyspell-duplicate                         'face-popout)
  (set-face 'flyspell-incorrect                         'face-popout))

;;;;; Ido
(with-eval-after-load 'ido
  (set-face 'ido-first-match                            'face-salient)
  (set-face 'ido-only-match                               'face-faded)
  (set-face 'ido-subdir                                 'face-strong))

;;;;; Diff
(with-eval-after-load 'diff-mode
  (set-face 'diff-header                                  'face-faded)
  (set-face 'diff-file-header                            'face-strong)
  (set-face 'diff-context                                    'default)
  (set-face 'diff-removed                                 'face-faded)
  (set-face 'diff-changed                                'face-popout)
  (set-face 'diff-added                                 'face-salient)
  (set-face 'diff-refine-added            '(face-salient face-strong))
  (set-face 'diff-refine-changed                         'face-popout)
  (set-face 'diff-refine-removed                          'face-faded)
  (set-face-attribute     'diff-refine-removed nil :strike-through t))

;;;;; Term
(with-eval-after-load 'term
  ;; (setq eterm-256color-disable-bold nil)
  (set-face 'term-bold                                   'face-strong)
  (set-face-attribute 'term-color-black nil
                      :foreground (face-foreground 'default)
                      :background (face-foreground 'default))
  (set-face-attribute 'term-color-white nil
                      :foreground "white" :background "white")
  (set-face-attribute 'term-color-blue nil
                      :foreground "#42A5F5" :background "#BBDEFB")
  (set-face-attribute 'term-color-cyan nil
                      :foreground "#26C6DA" :background "#B2EBF2")
  (set-face-attribute 'term-color-green nil
                      :foreground "#66BB6A" :background "#C8E6C9")
  (set-face-attribute 'term-color-magenta nil
                      :foreground "#AB47BC" :background "#E1BEE7")
  (set-face-attribute 'term-color-red nil
                      :foreground "#EF5350" :background "#FFCDD2")
  (set-face-attribute 'term-color-yellow nil
                      :foreground "#FFEE58" :background "#FFF9C4"))

;;;;; Org-agende
(with-eval-after-load 'org-agenda
  (set-face 'org-agenda-calendar-event                    'default)
  (set-face 'org-agenda-calendar-sexp                     'face-faded)
  (set-face 'org-agenda-clocking                          'face-faded)
  (set-face 'org-agenda-column-dateline                   'face-faded)
  (set-face 'org-agenda-current-time                      'face-faded)
  (set-face 'org-agenda-date                            'face-salient)
  ;; (set-face 'org-agenda-date-today        '(face-salient face-strong))
  (set-face-attribute 'org-agenda-date-today nil
                      :inherit 'variable-pitch
                      :height 1.25
                      :foreground (face-background 'face-popout))
  (with-eval-after-load 'org-super-agenda
    (set-face 'org-super-agenda-header                   'face-accent1))
  (set-face 'org-agenda-date-weekend                      'face-faded)
  (set-face 'org-agenda-diary                             'face-faded)
  (set-face 'org-agenda-dimmed-todo-face                  'face-faded)
  (set-face 'org-agenda-done                              'face-faded)
  (set-face 'org-agenda-filter-category                   'face-faded)
  (set-face 'org-agenda-filter-effort                     'face-faded)
  (set-face 'org-agenda-filter-regexp                     'face-faded)
  (set-face 'org-agenda-filter-tags                       'face-faded)
  ;; fixes issue #18 (set-face 'org-agenda-property-face                     'face-faded)
  (set-face 'org-agenda-restriction-lock                  'face-faded)
  (set-face 'org-agenda-structure                        'face-faded))

;;;;; Org mode
(with-eval-after-load 'org
  (set-face 'org-archived                                 'face-faded)
  (set-face 'org-block                                    'face-faded)
  (set-face 'org-block-begin-line                         'face-faded)
  (set-face 'org-block-end-line                           'face-faded)
  (set-face 'org-checkbox                                 'face-faded)
  (set-face 'org-checkbox-statistics-done                 'face-faded)
  (set-face 'org-checkbox-statistics-todo                 'face-faded)
  (set-face 'org-clock-overlay                            'face-faded)
  (set-face 'org-code                                     'face-faded)
  (set-face 'org-column                                   'face-faded)
  (set-face 'org-column-title                             'face-faded)
  (set-face 'org-date                                     'face-faded)
  (set-face 'org-date-selected                            'face-faded)
  (set-face 'org-default                                  'face-faded)
  (set-face 'org-document-info                            'face-faded)
  (set-face 'org-document-info-keyword                    'face-faded)
  (set-face-attribute 'org-document-title nil
                      :inherit 'variable-pitch
                      :height 1.25
                      :foreground (face-foreground 'face-salient)
                      :background (face-background 'face-default))
  (set-face 'org-done                                        'default)
  (set-face 'org-drawer                                   'face-faded)
  (set-face 'org-ellipsis                                 'face-faded)
  (set-face 'org-footnote                                 'face-faded)
  (set-face 'org-formula                                  'face-faded)
  (set-face 'org-headline-done                            'face-faded)
  ;;  (set-face 'org-hide                                     'face-faded)
  ;;  (set-face 'org-indent                                   'face-faded)
  (set-face 'org-latex-and-related                        'face-faded)
  (set-face-attribute 'org-level-1 nil
                      ;; :height 1.25
                      :inherit 'variable-pitch
                      :foreground "#BF616A"
                      ;; :background (face-background 'face-default)
                      )
  (set-face-attribute 'org-level-2 nil
                      ;; :height 1.10
                      :inherit 'variable-pitch
                      :foreground "#5E81AC"
                      ;; :foreground "#EBCB8B"
                      ;; :background (face-background 'face-default)
                      )
  (set-face-attribute 'org-level-3 nil
                      ;; :height 1.45
                      :inherit 'variable-pitch
                      :foreground "#BF616A"
                      ;; :foreground "#88C0D0"
                      ;; :background (face-background 'face-default)
                      )
  (set-face-attribute 'org-level-4 nil
                      ;; :height 1.35
                      :inherit 'variable-pitch
                      :foreground "#5E81AC"
                      ;; :foreground "#B48EAD"
                      ;; :background (face-background 'face-default)
                      )
  (set-face-attribute 'org-level-5 nil
                      ;; :height 1.25
                      :inherit 'variable-pitch
                      :foreground "#BF616A"
                      ;; :foreground "#81A1C1"
                      ;; :background (face-background 'face-default)
                      )
  (set-face-attribute 'org-level-6 nil
                      ;; :height 1.25
                      :inherit 'variable-pitch
                      :foreground "#5E81AC"
                      ;; :foreground "#8FBCBB"
                      ;; :background (face-background 'face-default)
                      )
  (set-face-attribute 'org-level-7 nil
                      ;; :height 1.25
                      :inherit 'variable-pitch
                      :foreground "#BF616A"
                      ;; :foreground "#5E81AC"
                      ;; :background (face-background 'face-default)
                      )
  (set-face-attribute 'org-level-8 nil
                      ;; :height 1.25
                      :inherit 'variable-pitch
                      :foreground "#5E81AC"
                      ;; :foreground "#A3BE8C"
                      ;; :background (face-background 'face-default)
                      )
  (set-face 'org-link                                   'face-salient)
  (set-face 'org-list-dt                                  'face-faded)
  (set-face 'org-macro                                    'face-faded)
  (set-face 'org-meta-line                                'face-faded)
  (set-face 'org-mode-line-clock                          'face-faded)
  (set-face 'org-mode-line-clock-overrun                  'face-faded)
  (set-face 'org-priority                                 'face-faded)
  (set-face 'org-property-value                           'face-faded)
  (set-face 'org-quote                                    'face-faded)
  (set-face 'org-scheduled                                'face-salient)
  (set-face 'org-scheduled-previously                     'face-salient)
  (set-face 'org-scheduled-today                          '(face-strong
                                                            face-salient))
  (set-face 'org-sexp-date                                'face-faded)
  (set-face 'org-special-keyword                          'face-faded)
  (set-face 'org-table                                    'default)
  (set-face 'org-tag                                      'face-faded)
  (set-face 'org-tag-group                                'face-faded)
  (set-face 'org-target                                   'face-faded)
  (set-face 'org-time-grid                                'face-faded)
  (set-face 'org-todo                                    'face-popout)
  (set-face 'org-upcoming-deadline                        'face-strong-bold)
  (set-face 'org-upcoming-distant-deadline                'face-strong)
  (set-face 'org-verbatim                                 'face-faded)
  (set-face 'org-verse                                    'face-faded)
  (set-face 'org-warning                                'face-popout))

;;;;; Markdown Mode
(with-eval-after-load 'markdown-mode
  (set-face 'markdown-blockquote-face              'face-salient)
  (set-face 'markdown-bold-face                     'face-strong-bold)
  (set-face 'markdown-code-face                    'face-default)
  (set-face 'markdown-comment-face                   'face-faded)
  (set-face 'markdown-footnote-marker-face         'face-default)
  (set-face 'markdown-footnote-text-face           'face-default)
  (set-face 'markdown-gfm-checkbox-face            'face-default)
  (set-face 'markdown-header-delimiter-face          'face-faded)
  (set-face-attribute 'markdown-header-face nil
                      :inherit 'variable-pitch)
  (set-face-attribute 'markdown-header-face-1 nil
                      ;; :height 1.75
                      :inherit 'variable-pitch
                      :foreground (face-foreground 'face-popout)
                      :background (face-background 'face-default))
  (set-face-attribute 'markdown-header-face-2 nil
                      ;; :height 1.6
                      :inherit 'variable-pitch
                      :foreground (face-foreground 'face-salient)
                      :background (face-background 'face-default))
  (set-face-attribute 'markdown-header-face-3 nil
                      ;; :height 1.45
                      :inherit 'variable-pitch
                      :foreground (face-foreground 'face-faded)
                      :background (face-background 'face-default))
  (set-face-attribute 'markdown-header-face-4 nil
                      ;; :height 1.35
                      :inherit 'variable-pitch
                      :foreground (face-foreground 'face-salient)
                      :background (face-background 'face-default))
  (set-face-attribute 'markdown-header-face-5 nil
                      ;; :height 1.25
                      :inherit 'variable-pitch
                      :foreground (face-foreground 'face-faded)
                      :background (face-background 'face-default))
  (set-face-attribute 'markdown-header-face-6 nil
                      ;; :height 1.25
                      :inherit 'variable-pitch
                      :foreground (face-foreground 'face-salient)
                      :background (face-background 'face-default))
  (set-face 'markdown-header-rule-face             'face-default)
  (set-face 'markdown-highlight-face               'face-default)
  (set-face 'markdown-hr-face                      'face-default)
  (set-face 'markdown-html-attr-name-face          'face-default)
  (set-face 'markdown-html-attr-value-face         'face-default)
  (set-face 'markdown-html-entity-face             'face-default)
  (set-face 'markdown-html-tag-delimiter-face      'face-default)
  (set-face 'markdown-html-tag-name-face           'face-default)
  (set-face 'markdown-inline-code-face              'face-popout)
  (set-face 'markdown-italic-face                    'face-faded-italic)
  (set-face 'markdown-language-info-face           'face-default)
  (set-face 'markdown-language-keyword-face        'face-default)
  (set-face 'markdown-line-break-face              'face-default)
  (set-face 'markdown-link-face                    'face-salient)
  (set-face 'markdown-link-title-face              'face-default)
  (set-face 'markdown-list-face                      'face-faded)
  (set-face 'markdown-markup-face                    'face-faded)
  (set-face 'markdown-math-face                    'face-default)
  (set-face 'markdown-metadata-key-face              'face-faded)
  (set-face 'markdown-metadata-value-face            'face-faded)
  (set-face 'markdown-missing-link-face            'face-default)
  (set-face 'markdown-plain-url-face               'face-default)
  (set-face 'markdown-pre-face                     'face-default)
  (set-face 'markdown-reference-face               'face-salient)
  (set-face 'markdown-strike-through-face            'face-faded)
  (set-face 'markdown-table-face                   'face-default)
  (set-face 'markdown-url-face                     'face-salient))

;;;;; Mu4e
(with-eval-after-load 'mu4e
  (set-face 'mu4e-attach-number-face                     'face-strong)
  (set-face 'mu4e-cited-1-face                            'face-faded)
  (set-face 'mu4e-cited-2-face                            'face-faded)
  (set-face 'mu4e-cited-3-face                            'face-faded)
  (set-face 'mu4e-cited-4-face                            'face-faded)
  (set-face 'mu4e-cited-5-face                            'face-faded)
  (set-face 'mu4e-cited-6-face                            'face-faded)
  (set-face 'mu4e-cited-7-face                            'face-faded)
  (set-face 'mu4e-compose-header-face                     'face-faded)
  (set-face 'mu4e-compose-separator-face                  'face-faded)
  (set-face 'mu4e-contact-face                          'face-salient)
  (set-face 'mu4e-context-face                            'face-faded)
  (set-face 'mu4e-draft-face                              'face-faded)
  (set-face 'mu4e-flagged-face                            'face-faded)
  (set-face 'mu4e-footer-face                             'face-faded)
  (set-face 'mu4e-forwarded-face                          'face-default)
  (set-face 'mu4e-header-face                                'default)
  (set-face 'mu4e-header-highlight-face                  'face-subtle)
  (set-face 'mu4e-header-key-face                        'face-strong)
  (set-face 'mu4e-header-marks-face                       'face-faded)
  (set-face 'mu4e-header-title-face                      'face-strong)
  (set-face 'mu4e-header-value-face                          'default)
  (set-face 'mu4e-highlight-face                         'face-popout)
  (set-face 'mu4e-link-face                             'face-salient)
  (set-face 'mu4e-modeline-face                           'face-faded)
  (set-face 'mu4e-moved-face                              'face-faded)
  (set-face 'mu4e-ok-face                                 'face-faded)
  (set-face 'mu4e-region-code                             'face-faded)
  (set-face 'mu4e-replied-face                          'face-salient)
  (set-face 'mu4e-special-header-value-face                  'default)
  (set-face 'mu4e-system-face                             'face-faded)
  (set-face 'mu4e-title-face                             'face-strong)
  (set-face 'mu4e-trashed-face                            'face-faded)
  (set-face 'mu4e-unread-face                            'face-strong)
  (set-face 'mu4e-url-number-face                         'face-faded)
  (set-face 'mu4e-view-body-face                             'default)
  (set-face 'mu4e-warning-face                            'face-faded))

;;;;; Company
(with-eval-after-load 'company
  (set-face 'company-scrollbar-fg                                          'face-faded)
  (set-face 'company-scrollbar-bg                                          'face-default)
  (set-face 'company-preview                             '(face-strong face-subtle))
  (set-face 'company-preview-common                                        'face-default)
  (set-face 'company-tooltip-selection                   '(face-strong face-subtle))
  (set-face 'company-tooltip                                               'face-default)
  (set-face 'company-tooltip-common                                        'face-default)
  (set-face 'company-tooltip-common-selection            '(face-strong face-subtle))
  (set-face 'company-tooltip-annotation                                    'face-default)
  (set-face 'company-tooltip-annotation-selection        '(face-strong face-subtle)))

;;;;; Selectrum
(with-eval-after-load 'selectrum
  (set-face-attribute 'selectrum-current-candidate nil
                      ;; :background color-highlight
                      :slant 'italic
                      :weight 'bold))


;;;;; Minibuffer
(defun bespoke-theme--minibuffer ()
  "Derive minibuffer / echo area faces from bespoke faces."
  ;; Minibuffer / echo area
  (dolist (buffer (list " *Minibuf-0*" " *Echo Area 0*"
                        " *Minibuf-1*" " *Echo Area 1*"))
    (when (get-buffer buffer)
      (with-current-buffer buffer
        (face-remap-add-relative 'default 'face-faded)))))
(bespoke-theme--minibuffer)

;;; Toggle Menubar
;; toggle menubar to light or dark
(defun cpm/osx-toggle-menubar-theme ()
  "toggle menubar to dark or light using shell command"
  (interactive)
  (shell-command "dark-mode"))
(defun cpm/osx-menubar-theme-light ()
  "turn dark mode off"
  (interactive)
  (shell-command "dark-mode off")
  (bespoke-light))
(defun cpm/osx-menubar-theme-dark ()
  "turn dark mode on"
  (interactive)
  (shell-command "dark-mode on")
  (bespoke-dark))

;;; Theme & menubar toggle
(defun toggle-dark-light-theme ()
  "Coordinate setting of theme with os theme and toggle"
  (interactive)
  (if (eq active-theme 'light-theme)
      (progn (cpm/osx-menubar-theme-dark)
             (setq active-theme 'dark-theme))
    (progn (cpm/osx-menubar-theme-light)
           (setq active-theme 'light-theme))))

;;; System Appearance Hook
;; See https://github.com/d12frosted/homebrew-emacs-plus#system-appearance-change
(defun cpm/system-apply-theme (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance
    ('light (progn (bespoke-light) (setq active-theme 'light-theme)))
    ('dark (progn (bespoke-dark) (setq active-theme 'dark-theme)))))

  (add-hook 'ns-system-appearance-change-functions #'cpm/system-apply-theme)


;;; End Theme

(provide 'setup-theme)
