;;; init-fonts.el --- Fonts -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(setq-default line-spacing 0.5)

(defvar kd/fixed-pitch-font "Cascadia Code"
  "The font used for `default' and `fixed-pitch' faces.")

(defvar kd/variable-pitch-font "Noto Sans"
  "The font used for `variable-pitch' face.")


(set-face-attribute 'default nil :font kd/fixed-pitch-font :height 160)
(set-face-attribute 'fixed-pitch nil :font kd/fixed-pitch-font :height 160)
(set-face-attribute 'variable-pitch nil :font kd/variable-pitch-font)

;; (when (eq system-type 'darwin)  ;; Check if running on macOS
;;   (set-face-attribute 'default nil :font kd/fixed-pitch-font :height 160)
;;   (set-face-attribute 'fixed-pitch nil :font kd/fixed-pitch-font :height 160)
;;   (set-face-attribute 'variable-pitch nil :font kd/variable-pitch-font)
;;   )



;; Set the font face based on platform
(use-package mixed-pitch
  :defer t
  :hook
  ;; If you want it in all text modes:
  (org-mode . mixed-pitch-mode))

;; (use-package ligature
;;   :config
;;   ;; Enable the "www" ligature in every possible major mode
;;   (ligature-set-ligatures 't '("www"))
;;   ;; Enable traditional ligature support in eww-mode, if the
;;   ;; `variable-pitch' face supports it
;;   (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
;;   ;; Enable all JetBrains Mono ligatures in programming modes
;;   (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
;;                                        ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
;;                                        "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
;;                                        "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
;;                                        "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
;;                                        "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
;;                                        "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
;;                                        "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
;;                                        ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
;;                                        "<$" "Avenir Next""<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
;;                                        "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
;;                                        ;; "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
;;                                        ;; "\\" "://"
;;                                        ))
;;   ;; Enables ligature checks globally in all buffers. You can also do it
;;   ;; per mode with `ligature-mode'.
;;   (global-ligature-mode t))
(use-package emojify
  :defer t
  :hook (erc-mode . emojify-mode)
  :commands emojify-mode)

(use-package unicode-fonts
  :defer t)

(provide 'init-fonts)
;;; init-fonts.el ends here
