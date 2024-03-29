AC_INIT
AC_CONFIG_SRCDIR([src/rlibjson.c])

AC_ARG_WITH(local-libjson, [use the libjson code that comes with this package rather than 
              looking for an installed version on the system],
              USE_LOCAL=$withval)

AC_ARG_WITH(prefix, [the name of the directory into which the libjson include files and library were installed],
              [ LIBJSON_PREFIX=$withval; if test -z "$USE_LOCAL" ; then USE_LOCAL="never" ; fi], 
              [ LIBJSON_PREFIX="/usr/local" ;
              ])

ORIG_USE_LOCAL="$USE_LOCAL"

if test -z "$USE_LOCAL" || test "$USE_LOCAL" = "no" ; then
  echo "Trying to find libjson.h header file"
  : ${R_HOME=`R RHOME`}
  if test -z "${R_HOME}"; then
    echo "could not determine R_HOME"
    exit 1
  fi
  CC=`"${R_HOME}/bin/R" CMD config CC`
  CFLAGS=`"${R_HOME}/bin/R" CMD config CFLAGS`
  CPPFLAGS=`"${R_HOME}/bin/R" CMD config CPPFLAGS`
  CPPFLAGS="$CPPFLAGS -I$LIBJSON_PREFIX/include"
  AC_PROG_CC
  AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <libjson/libjson.h>]], [[JSONNODE *ptr = json_parse("[1, 3]");]])],[USE_LOCAL=1; echo "Found system libjson header file libjson.h"],[
  ])


 if test -n "$USE_LOCAL" &&  ! test "$USE_LOCAL" = "no"; then
  echo "Trying to link against libjson"
  LIBS="$LIBS -L$LIBJSON_PREFIX/lib -ljson"
  AC_LINK_IFELSE([AC_LANG_PROGRAM([[#include <libjson/libjson.h>]], [[JSONNODE *ptr = json_parse("[1, 3]");]])],[USE_LOCAL=1; echo "linked against libjson"],[USE_LOCAL=""; echo "Failed to link against system libjson"
  ])
 fi

fi

echo "USE_LOCAL = \"$USE_LOCAL\""


if test -n "$USE_LOCAL" &&  ! test "$USE_LOCAL" = "" && ! test "$USE_LOCAL" = "yes"  && test -n "$LIBJSON_PREFIX" ; then
  LIBJSON_CFLAGS="-I${LIBJSON_PREFIX}/include -I."   # -I${LIBJSON_PREFIX}/Source
  LIBJSON_LIBS="-L${LIBJSON_PREFIX}/lib -ljson"
  CPP_FILES=""
else
   if test "$ORIG_USE_LOCAL" = "never" ; then
       echo "        ****************"
       echo "Couldn't find libjson on system so would use the local version of libjson in this package but you asked not to"
       echo "        ****************"
       exit 1;
   fi
   if test -z "${LIBJSON_SRC}" ; then
      LIBJSON_SRC="src/libjson/Source"
      LIBJSON_CFLAGS="-I. -Ilibjson -Ilibjson/Source"
   else
      LIBJSON_CFLAGS="-I${LIBJSON_SRC}/.. -I${LIBJSON_SRC}"
   fi
   MINOR_VERSION=`grep 'define __LIBJSON_MINOR__' $LIBJSON_SRC/JSONDefs.h | sed -e 's/.* __LIBJSON_MINOR__ //'`
   if test "$MINOR_VERSION" = "0" ; then
     LIBJSON_CFLAGS="${LIBJSON_CFLAGS} -DJSON_NO_EXCEPTIONS=1"
   else
     LIBJSON_CFLAGS="${LIBJSON_CFLAGS} -DNEW_JSON_NEW_STREAM"
   fi
   echo "Minor Version: $MINOR_VERSION"
   echo "Using local libjson code. Copying files from ${LIBJSON_SRC}"
   echo "`pwd`"
   CPP_FILES="`(cd ${LIBJSON_SRC}; ls *.cpp)`"
   echo "$CPP_FILES"
   cp ${LIBJSON_SRC}/*.cpp src/
   cp src/libjson/JSONOptions.h src/
fi

AC_SUBST(CPP_FILES)
AC_SUBST(LIBJSON_CFLAGS)
AC_SUBST(LIBJSON_LIBS)

AC_CONFIG_FILES([src/Makevars tools/cleanup])
AC_OUTPUT
mv tools/cleanup .
chmod +x cleanup
