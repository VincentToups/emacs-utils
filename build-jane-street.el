(require 'scripting)

(with-working-directory "~/jane-st/"
 (loop for f in (sh "ls *") do
  (copy-file (concat "~/elisp/utils/" f)
             f t)))