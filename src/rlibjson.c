#include <libjson/libjson.h>
#include <Rdefines.h>

SEXP processJSONNode(JSONNODE *node, int parentType, int simplify, SEXP nullValue);

int setType(int cur, int newType);
SEXP makeVector(SEXP l, int len, int type, SEXP nullValue);

SEXP
R_fromJSON(SEXP r_str, SEXP simplify, SEXP nullValue)
{
    const char * str = CHAR(STRING_ELT(r_str, 0));
    JSONNODE *node;
    SEXP ans;
    node = json_parse(str);
    ans = processJSONNode(node, json_type(node), LOGICAL(simplify)[0], nullValue);
    json_delete(node);
    return(ans);
}

SEXP 
processJSONNode(JSONNODE *n, int parentType, int simplify, SEXP nullValue)
{
    if (n == NULL){
        PROBLEM "invalid JSON input"
	    ERROR;
    }
 
    JSONNODE *i;
    int len = 0, ctr = 0;
    int nprotect = 0;
    int numNulls = 0;
    len = json_size(n);
    char startType = parentType; // was 127
    
    int isNullHomogeneous = (TYPEOF(nullValue) == LGLSXP || TYPEOF(nullValue) == REALSXP || TYPEOF(nullValue) == STRSXP || TYPEOF(nullValue) == INTSXP);

    SEXP ans, names = NULL;
    PROTECT(ans = NEW_LIST(len)); nprotect++;

    int homogeneous = 0;
    int elType = NILSXP;
    while (ctr < len){  // i != json_end(n)

	i = json_at(n, ctr);

        if (i == NULL){
            PROBLEM "Invalid JSON Node"
		ERROR;
        }

	json_char *node_name = json_name(i);
	
	char type = json_type(i);
	if(startType == 127)
	    startType = type;

	SEXP el;
	switch(type) {
   	   case JSON_NULL:
	       el = nullValue; /* R_NilValue; */
	       numNulls++;
	       if(isNullHomogeneous) {
		   homogeneous++;
		   elType = setType(elType, TYPEOF(nullValue));
	       } else
		   elType = TYPEOF(nullValue);
	       break;
   	   case JSON_ARRAY:
  	   case JSON_NODE:
	       el = processJSONNode(i, type, simplify, nullValue);
	       if(Rf_length(el) > 1)
		   elType = VECSXP;
	       else
		   elType = setType(elType, TYPEOF(el));
	       break;
 	   case JSON_NUMBER:
	       el = ScalarReal(json_as_float(i));
	       homogeneous++;
	       elType = setType(elType, REALSXP);
	       break;
 	   case JSON_BOOL:
	       el = ScalarLogical(json_as_bool(i));
	       elType = setType(elType, LGLSXP);
	       break;
 	   case JSON_STRING:
	   {
//XXX Garbage collection
	       char *tmp = json_as_string(i);
                   // do we need to strdup here?
	       el = ScalarString(mkChar(tmp));
	       elType = setType(elType, STRSXP);
	       json_free(tmp);
	   }
	       break;
	default:
	    PROBLEM "shouldn't be here"
		WARN;
	    el = R_NilValue;
	    break;
	}
	SET_VECTOR_ELT(ans, ctr, el);

	if(parentType == JSON_NODE || (node_name && node_name[0])) {
	    if(names == NULL) {
	        PROTECT(names = NEW_CHARACTER(len)); nprotect++;
	    }
	    if(node_name && node_name[0])
		SET_STRING_ELT(names, ctr, mkChar(node_name));
	}
	json_free(node_name);
	ctr++;
    }

    /* If we have an empty object, we try to make it into a form equivalent to emptyNamedList
       if it is a {},  or as an AsIs object in R if an empty array. */
    if(len == 0 && (parentType == -1 || parentType == JSON_ARRAY || parentType == JSON_NODE)) {
        if(parentType == -1) 
            parentType = startType;

        if(parentType == JSON_NODE)
	   SET_NAMES(ans, NEW_CHARACTER(0));
        else  {
	   SET_CLASS(ans, ScalarString(mkChar("AsIs")));
	}

    } else {
        if(simplify && (homogeneous == len ||  elType == LGLSXP || elType == REALSXP || elType == STRSXP)) {
	    ans = makeVector(ans, len, elType, nullValue);
	}
    }
      

    if(names)
	SET_NAMES(ans, names);
	
    UNPROTECT(nprotect);
    return(ans);
}

SEXP
makeVector(SEXP ans, int len, int type, SEXP nullValue)
{
    SEXP tmp;
    int ctr;

    if(type == REALSXP) {
	PROTECT(tmp = NEW_NUMERIC(len)); 
	for(ctr = 0; ctr < len; ctr++) {
	    SEXP el = VECTOR_ELT(ans, ctr);
	    REAL(tmp)[ctr] = TYPEOF(el) == REALSXP ? REAL(el)[0] : Rf_asReal(el);
	}
    } else if(type == LGLSXP) {
	PROTECT(tmp = NEW_LOGICAL(len)); 
	for(ctr = 0; ctr < len; ctr++) {
	    SEXP el = VECTOR_ELT(ans, ctr);
	    LOGICAL(tmp)[ctr] = TYPEOF(el) == LGLSXP ? LOGICAL(el)[0] : Rf_asInteger(el);
	}
    } else if(type == STRSXP) {
	PROTECT(tmp = NEW_CHARACTER(len)); 
	for(ctr = 0; ctr < len; ctr++) {
	    SEXP el = VECTOR_ELT(ans, ctr);
	    if(TYPEOF(el) == STRSXP)
		SET_STRING_ELT(tmp, ctr, STRING_ELT(el, 0));
	    else if(TYPEOF(el) == LGLSXP) {
		SET_STRING_ELT(tmp, ctr, mkChar(LOGICAL(el)[0] ? "TRUE" : "FALSE"));
	    } else if(TYPEOF(el) == REALSXP) {
		char buf[70];
		sprintf(buf, "%lf", REAL(el)[0]);
		SET_STRING_ELT(tmp, ctr, mkChar(buf));
	    }
	}
    } else
	return(ans);

    UNPROTECT(1);
    return(tmp);
}

int setType(int cur, int newType)
{
    if(cur == newType)
	return(cur);

    if(newType == VECSXP || cur == VECSXP)
	return(VECSXP);

    switch(cur) {
    case INTSXP:
	switch(newType) {
	case LGLSXP:
	    return(INTSXP);
	    break;
	case REALSXP:
	    return(REALSXP);
	    break;
	case STRSXP:
	    return(STRSXP);
	    break;
	}
	break;
    case LGLSXP:
	switch(newType) {
	case INTSXP:
	    return(INTSXP);
	    break;
	case REALSXP:
	    return(REALSXP);
	    break;
	case STRSXP:
	    return(STRSXP);
	    break;
	}
	break;
    case REALSXP:
	switch(newType) {
	case INTSXP:
	    return(REALSXP);
	    break;
	case LGLSXP:
	    return(REALSXP);
	    break;
	case STRSXP:
	    return(STRSXP);
	    break;
	}
	break;
    case STRSXP:
	return(STRSXP);
	break;
    }

    return(newType);
}


SEXP
R_libjson_version()
{
    char buf[20];
    sprintf(buf, "%d.%d-%d", (int) __LIBJSON_MAJOR__,
    	                     (int) __LIBJSON_MINOR__,
	                     (int) __LIBJSON_PATCH__);

    return(ScalarString(mkChar(buf)));
}
