#!/bin/sh
# $Id: mkruntimedef.sh,v 1.1.2.2 2007/03/12 11:58:48 pouillar Exp $
# copied from ocaml/build to fix paths (maybe there is a way to get
# ocamlbuild to run the stock one in a different dir)
echo 'let builtin_exceptions = [|'; \
sed -n -e 's|.*/\* \("[A-Za-z_]*"\) \*/$|  \1;|p' ocaml/byterun/fail.h | \
sed -e '$s/;$//'; \
echo '|]'; \
echo 'let builtin_primitives = [|'; \
sed -e 's/.*/  "&";/' -e '$s/;$//' ocaml/byterun/primitives; \
echo '|]'
