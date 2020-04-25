start /min emacs.exe -Q --iconic --eval "(require 'org)" --eval "(find-file \"README.org\")" --eval "(org-babel-tangle)" --eval "(kill-emacs)"
