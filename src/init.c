#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* .Call calls */
extern SEXP R_fromJSON(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP R_isValidJSON(SEXP);
extern SEXP R_jsonPrettyPrint(SEXP, SEXP, SEXP);
extern SEXP R_json_parser_init_from_con(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP R_readFromJSON(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);


static const R_CallMethodDef CallEntries[] = {
    {"R_fromJSON",                  (DL_FUNC) &R_fromJSON,                  7},
    {"R_isValidJSON",               (DL_FUNC) &R_isValidJSON,               1},
    {"R_jsonPrettyPrint",           (DL_FUNC) &R_jsonPrettyPrint,           3},
    {"R_json_parser_init_from_con", (DL_FUNC) &R_json_parser_init_from_con, 5},
    {"R_readFromJSON",              (DL_FUNC) &R_readFromJSON,              6},
    {NULL, NULL, 0}
};

void R_init_RJSONIO(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, TRUE); // for getNativeSymbolInfo calls
}
