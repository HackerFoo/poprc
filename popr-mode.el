;;; package --- Major mode for editing Popr code.
;;; Commentary:

;;; Code:

(defvar popr-bracket-face 'popr-bracket-face)

(defface popr-bracket-face
    '((((class color))
       :weight bold))
  "Face for displaying a popr bracket."
  :group 'faces)

(defvar popr-highlights
      '(("__ .*". font-lock-comment-face)
        ("_\\w.*\\w_" . font-lock-comment-face)
        ("module\\|imports" . font-lock-keyword-face)
        ("\\[\\|\\]" . popr-bracket-face)
        ("^\\s-*\\(\\w+\\):" . font-lock-function-name-face)))

(define-derived-mode popr-mode fundamental-mode "popr"
  "Major mode for editing Popr code."
  (setq font-lock-defaults '(popr-highlights)))

(provide 'popr-mode)

;;; popr-mode.el ends here
