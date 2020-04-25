if "%1" == "clean" goto :cleanup
if "%1" == "examples" goto :examples

:elisp
emacs.exe -Q --iconic --eval "(require 'org)" --eval "(find-file \"README.org\")" --eval "(org-babel-tangle)" --eval "(find-file \"personal-site/index.org\")" --eval "(org-babel-tangle)" --eval "(kill-emacs)"

:examples
cd personal-site
emacs.exe make.el --load "../ox-thtml.el" --load "./make.el" --eval "(kill-emacs)"
if exist public_html\index.html start public_html\index.html
cd ..
goto :exit

:cleanup
rmdir /S /Q personal-site\public_html 1>NUL 2>NUL
rmdir /S /Q personal-site\images 1>NUL 2>NUL
del /Q personal-site\recipes\*.jpg 1>NUL 2>NUL
del /Q personal-site\blog\*.inc 1>NUL 2>NUL
del /Q personal-site\recipes\*.inc 1>NUL 2>NUL

:exit
