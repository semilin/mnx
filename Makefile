build:
	sbcl --load mnx.asd \
	     --eval '(ql:quickload :mnx)' \
             --eval "(sb-ext:save-lisp-and-die #p\"mnx\" :toplevel #'mnx:new-derivation :executable t)"
