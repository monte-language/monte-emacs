;;; monte-emacs.el --- support for editing Monte code
(provide 'monte-emacs)
(push (expand-file-name "elisp/" (file-name-directory load-file-name)) load-path)
(require 'monte)
(require 'monte-indent)
