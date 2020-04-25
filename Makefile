all: elisp examples

elisp:
	emacs -Q --iconic --eval "(require 'org)" --eval "(find-file \"README.org\")" --eval "(org-babel-tangle)" --eval "(find-file \"personal-site/index.org\")" --eval "(org-babel-tangle)" --eval "(kill-emacs)"

examples:
	cd personal-site && emacs --load `pwd`/../ox-thtml --load make.el --eval '(kill-emacs)'

clean:
	-rm -rf personal-site/public_html
	-rm -rf personal-site/images
	-rm personal-site/recipes/*.jpg
